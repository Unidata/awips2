package ohd.hseb.sshp.var;

import java.text.SimpleDateFormat;
import java.util.*;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.MonthlyValues;
import ohd.hseb.model.RatingCurve;
import ohd.hseb.model.UnitHydrograph;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.sshp.SSHPConfig;
import ohd.hseb.sshp.SSHPSource;
import ohd.hseb.sshp.precip.BasinHrapHelper;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;

import static ohd.hseb.util.TimeHelper.*;


/*******************************************************************************************************************
 * A data manager talking with database and is specifically related with Var task. It has a delegater from SSHP data
 * manager, DataMgr.
 * 
 */
public class VarDataMgr 
{   
    private DataMgr _sshpDataMgr;
    private FileLogger _varControllerLogger;
    private String _varInputDir;
    private String _varOutputDir;
    private String _segmentsFilePath;
    
    private Map<String, RegularTimeSeries>   _precipTimeSeriesMap  = new HashMap<String, RegularTimeSeries>();
    private Map<String, IrregularTimeSeries> _stageTimeSeriesMap   = new HashMap<String, IrregularTimeSeries>();
    private Map<String, MonthlyValues>       _monthlyValuesMap     = new HashMap<String, MonthlyValues>();
    private Map<String, RatingCurve>         _ratingCurveMap       = new HashMap<String, RatingCurve>(); 
    private Map<String, String>              _basinIdToLocationIdMap    = new HashMap<String, String>(); 
    private Map<String, UnitHydrograph>      _unitHydrographMap    = new HashMap<String, UnitHydrograph>(); 
    
    private CodeTimer _dataLoadingTimer = new CodeTimer();


    /****************************************************************************************************************************
     * A constructor.
     * @param basin: String
     * @param strCurrentTime: String in the formate of "yyyy-MM-dd HH:00:00"
     */
    public VarDataMgr(FileLogger logger, String dbName, 
            String inputDir, String outputDir,
            String segmentsFilePath)
    {
        _varControllerLogger = logger;
        _varInputDir = inputDir;
        _varOutputDir = outputDir;
        _segmentsFilePath = segmentsFilePath;
        _sshpDataMgr = new DataMgr(dbName, _varControllerLogger);
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    /**********************************
     * Get the list of all possible basin ids from the SacSmaParams table.
     * Calls getSSHPConfigList in sshp project's data mgr to get the possible config details
     * and retrieves all possible basin ids from the list
     * @return list of all possible basin id's 
     */
    public List getAllBasinIdsList()
    {
        Set basinIdSet = new  HashSet();
        List basinIdList = new ArrayList();
        List<SacSmaParameters> paramList = _sshpDataMgr.loadSacSmaParamsList("order by basin_id");
        
        for(SacSmaParameters params : paramList)
        {
            if (! basinIdSet.contains(params.getBasinId()) )
            {
                //do this in multiple step to maintain basinId ordering in the list
                basinIdSet.add(params.getBasinId());
                basinIdList.add(params.getBasinId());
            }
        }
        
        return basinIdList;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------
    /**********************************************************************************************************
     * Returns the drainage area of the basin (unit: squaremiles), stored in database table linesegs. The value
     * in the database is with unit of squaremiles.
     * @return
     */
    public double getBasinArea(String basinId) throws DataAbsentException
    {
        BasinHrapHelper basinHelper = _sshpDataMgr.getBasinHrapHelper(basinId);
        if(basinHelper == null) 
        {
            _varControllerLogger.log("BasinHrapHelper for the basin " + basinId + " is not available");
            throw new DataAbsentException();
        }
        return basinHelper.getAreaSize();
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    /*******************************************************************************************************
     * Get the location name associated with the basin_id. Because the strict format required by Fortran program,
     * so if the name is longer than 20 chars, it is truncated to 20.
     * @return
     */
    public String getLocationName(String basinId) throws DataAbsentException
    {
        String locationName = _sshpDataMgr.getLocationName(basinId);

        if (locationName == null)
        {
            _varControllerLogger.log("No location name was found from database for the basin " + basinId);
            throw new DataAbsentException();       
        }

        locationName = locationName.trim();
        //control the length of the string to 20 max, because the string will be printed in deck parameter file, which restricts <= 20 chars
        if (locationName.length() > 20) 
        {
            locationName = locationName.substring(0, 20);
        }

        return locationName;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    /*****************************************************************************************************************************
     * Get the most recent SAC-SMA model parameters from database. If the file, "basinId_sacsmaparams", exists due to previous Var execuation, 
     * then update pxadj and peadj from the text file.
     * @return
     */
    public SacSmaParameters loadSacSmaParameters(String basinId, long validTimeLong) throws DataAbsentException
    {
        String header = "VarDataMgr.loadSacSmaParameters()";
        _varControllerLogger.log("In "+ header);

        SacSmaParameters sacSmaParams = null;
        sacSmaParams = _sshpDataMgr.loadSacSmaParameters(basinId, validTimeLong);

        if(sacSmaParams == null)
        {
            _varControllerLogger.log("SacSmaParameters data for the basin " + basinId + " is not available");
            throw new DataAbsentException();
        }


        //not calling it 
        //    sacSmaParams = getAdjustmentFactorsFromFile( basinId,  sacSmaParams,  varModelRunTimeLong);
        return sacSmaParams;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    /************************************************************************************
     * Returns the most recent MonthlyValues for ET-demand or PE-adjustment factor. 
     * MonthValues.getValueArray() returns an array of 12 double values. The boolean parameter 
     * will be part of SQL SELECT WHERE clause.
     * @return
     */
       
    public MonthlyValues getMonthlyValues(String basinId) throws DataAbsentException
    {
       // String header = "VarDataMgr.loadMonthlyValues(): ";
        
        boolean isAdjustmentFactor = false;
        
        MonthlyValues monthlyValues = _monthlyValuesMap.get(basinId);
        if(monthlyValues == null) 
        { 
            
            monthlyValues = _sshpDataMgr.loadMonthlyValues(basinId, isAdjustmentFactor);
            _monthlyValuesMap.put(basinId, monthlyValues);
            
        }
            
        if(monthlyValues == null) 
        {
            String message = "MonthlyValues data data for the basin " + basinId + " is not available";
            _varControllerLogger.log(message);
            throw new DataAbsentException();
        }
       
        return monthlyValues;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    /*************************************************************************************
     * From sacsmastate table, get the SAC-SMA model unadj state whose 
     * latest valid time <= (var model run time - UHG length in millis per hr).
     * @param basinId
     * @param varModelRunTimeLong
     * @return
     */
    public SacSmaState loadPrevUnadjSacSmaState(String basinId, long uhgLengthInMillis, long varModelRunTimeLong)
    {
        String header = "VarDataMgr.loadPrevUnadjSacSmaState(): ";
        
      //  System.out.println(header + "varModelRunTimeLong = " + DbTimeHelper.getDateTimeStringFromLongTime(varModelRunTimeLong));

        
        long sacSmaStateTimeLong = varModelRunTimeLong - uhgLengthInMillis; 
        
     //   System.out.println(header + "sacSmaStateTimeLong = " + varModelRunTimeLong);

        
        SacSmaState sacSmaState = _sshpDataMgr.loadSacSmaState(basinId,
                                                               SSHPSource.UNADJUSTED_VAR.getSacSmaSource(), 
                                                               sacSmaStateTimeLong);
        
        
        String sacSmaStateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(sacSmaStateTimeLong);
      //  System.out.println(header + "Source: "+ SSHPSource.UNADJUSTED_VAR.getSacSmaSource() + " with time = " + sacSmaStateTimeString);

        if(sacSmaState == null) //when corresponding record does not exist in database table.
        {
            String message = "Unadjusted SacSmaSate data for the basin " + basinId + 
                              " is not available for time: "+
                              sacSmaStateTimeString;  
           
            message += "\nRequested varModelRunTimeLong was " + DbTimeHelper.getDateTimeStringFromLongTime(varModelRunTimeLong);
            message += "Length of UHG in millis = " + uhgLengthInMillis + "\n";

            _varControllerLogger.log(message);
            System.out.println(message);
        }
        else
        {
            String message = "Unadjusted SacSmaSate data for the basin " + basinId + 
            " was found for time: "+ sacSmaStateTimeString;

            _varControllerLogger.log(message);
        }
        return sacSmaState;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public SacSmaState loadOldestUnadjSacSmaState(String basinId)
    {
         //String header = "VarDataMgr.loadOldestUnadjSacSmaState(): ";
        
         SacSmaState sacSmaState = _sshpDataMgr.loadOldestSacSmaState(basinId,
                                                               SSHPSource.UNADJUSTED_VAR.getSacSmaSource());
        
         
         return sacSmaState;
        
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    
    /*************************************************************************************
     * From sacsmastate table, get the SAC-SMA model unadj state whose 
     * latest valid time <= (var model run time - UHG length in millis per hr).
     * @param basinId
     * @param varModelRunTimeLong
     * @return
     */
    public SacSmaState loadLastAdjustedSacSmaStateAtOrBeforeATime(String basinId, long varModelRunTimeLong)
    {
        //String header = "VarDataMgr.loadLastAdjustedSacSmaStateBeforeATime(): ";
        
        long sacSmaStateTimeLong = varModelRunTimeLong;
        SacSmaState sacSmaState = _sshpDataMgr.loadSacSmaState(basinId,
                                                               SSHPSource.ADJUSTED_VAR.getSacSmaSource(), 
                                                               sacSmaStateTimeLong);
        
        
        String sacSmaStateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(sacSmaStateTimeLong);
        System.out.println("Source: "+ SSHPSource.ADJUSTED_VAR.getSacSmaSource() + " with time = " + sacSmaStateTimeString);

        return sacSmaState;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public UnitHydrograph getUnitHydrograph(String locationId, String basinId) throws DataAbsentException
    {   
        UnitHydrograph unitGraph = _unitHydrographMap.get(basinId);
        
        if (unitGraph == null)
        {
            unitGraph = _sshpDataMgr.loadUnitHydrograph(locationId, basinId, "SAC-SMA");
            _unitHydrographMap.put(basinId, unitGraph);
        }
        if(unitGraph == null) 
        {
            _varControllerLogger.log("UnitHydrograph data for the basin " + basinId + " is not available");
            throw new DataAbsentException();
        }
        return unitGraph;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------

    /******************************************************************************************
     * Load the UnitHydrograph from database.
     * @return
     */
    public UnitHydrograph loadUnitHydrograph(String basinId) throws DataAbsentException
    {   
        UnitHydrograph unitGraph = _sshpDataMgr.loadUnitHydrograph(basinId, basinId, "SAC-SMA");
        if(unitGraph == null) 
        {
            _varControllerLogger.log("UnitHydrograph data for the basin " + basinId + " is not available");
            throw new DataAbsentException();
        }
        return unitGraph;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    public RegularTimeSeries getDischargeRTSforDischargeFile(String basinId,
                                                            long startTimeLongForRTS,
                                                            long endTimeLongRTS
                                                            ) throws DataAbsentException
    {
       final boolean checkForMissingData = true;
       return getDischargeRegularTimeSeries(basinId, startTimeLongForRTS, endTimeLongRTS, checkForMissingData);
    }
    
    // ---------------------------------------------------------------------------------------------------------
    
    private IrregularTimeSeries getObservedStageTimeSeries(String locationId)
    {
        IrregularTimeSeries stageIrregularTimeSeries = _stageTimeSeriesMap.get(locationId);
            
        if (stageIrregularTimeSeries == null)
        {
            stageIrregularTimeSeries = _sshpDataMgr.loadObservedStageTimeSeries(locationId);
            _stageTimeSeriesMap.put(locationId, stageIrregularTimeSeries);
        }
        return stageIrregularTimeSeries;
    }
    // -------------------------------------------------------------------------------------------
    private RatingCurve getRatingCurve(String locationId)
    {
        RatingCurve ratingCurve = _ratingCurveMap.get(locationId);
        
        if (ratingCurve == null)
        {
            ratingCurve = _sshpDataMgr.loadRatingCurve(locationId);
            _ratingCurveMap.put(locationId, ratingCurve);
        }
        
        return ratingCurve;
    }
    
    // -------------------------------------------------------------------------------------------
    
    // -------------------------------------------------------------------------------------------
    
    /*******************************************************************************************
     * Generate the discharge time series spanning from Var start time to Var end time.
     * Load the rating curve and generate discharge data from stage data. Fill the predicted period 
     * (from current time plus one hour to the var end time) with value of -1. Convert the discharge data
     * from unit of cfs(default) to cms for Fortran.
     * @return
     */
    public RegularTimeSeries getDischargeRegularTimeSeries(String locationId,
                                                            long startTimeLongForRTS,
                                                            long endTimeLongRTS,
                                                            boolean checkForMissingData) throws DataAbsentException
    {
        
        String header = "VarDataMgr.getDischargeRegularTimeSeries(): ";
        
//      river stage data is measured irregularly and with unit of feet
        IrregularTimeSeries stageIrregularTimeSeries =  getObservedStageTimeSeries(locationId);  
        if(stageIrregularTimeSeries == null) 
        {
            String message = header + "Stage time series data for the location " + locationId + " is not available";
            System.out.println(message);
            _varControllerLogger.log(message);
            throw new DataAbsentException(message);
        }

        //Through interpolation, convert from irregular TS to regular TS and get the TS within the correct time window
        
        RegularTimeSeries fullStageRegularTimeSeries =  VarHelper.getRegularTimeSeriesFromIrregularTimeSeries(stageIrregularTimeSeries);
        RegularTimeSeries stageRegularTimeSeries = fullStageRegularTimeSeries.getSubTimeSeries(startTimeLongForRTS, endTimeLongRTS);
            
               
        //Load the rating curve
        RatingCurve rc = getRatingCurve(locationId);
        if(rc == null) 
        {
            String message = header + "Rating curve for the location " + locationId + " is not available";
            System.out.println(message);
            _varControllerLogger.log(message);
            throw new DataAbsentException(message);
        }

        //Using rating curve to convert stage data to discharge data, and convert unit from cfs to cms
        RegularTimeSeries prevDischargeRTS = rc.getDischargeRegularTimeSeries(stageRegularTimeSeries);  //returned unit: cfs
        
      //  System.out.println(header + "before conversion, prevDischargeRTS = " + prevDischargeRTS);
        
        prevDischargeRTS.convert(MeasuringUnit.cms);  
        
     //   System.out.println(header + "after conversion, prevDischargeRTS = " + prevDischargeRTS);
        
        

        //make up a dummy RTS with timing from one hour after the current time till the Var end time, with all values to be -1
        RegularTimeSeries predictDischargeRTS = new RegularTimeSeries(startTimeLongForRTS, endTimeLongRTS, 1, MeasuringUnit.cms, -1);
        
  //      System.out.println(header + "predictDischargeRTS = " + predictDischargeRTS);
        
        
 //       RegularTimeSeries totalDischargeRTS = RegularTimeSeries.add(ts1, ts2)(prevDischargeRTS, predictDischargeRTS);
        RegularTimeSeries totalDischargeRTS = RegularTimeSeries.concatenate(prevDischargeRTS, predictDischargeRTS);
        
//        System.out.println(header + "after concatenate(), totalDischargeRTS = " + totalDischargeRTS);
        
        
        //check to see if the discharge RTS spans from start time to  end time
        
        if (checkForMissingData)
        {
            if((totalDischargeRTS.getStartTime() != startTimeLongForRTS) || 
                    (totalDischargeRTS.getEndTime() != endTimeLongRTS) )
            {
                String message = header + "The discharge time series for the location " + locationId + " is not available from " + 
                DbTimeHelper.getDateTimeStringFromLongTime(startTimeLongForRTS) + " to " +
                DbTimeHelper.getDateTimeStringFromLongTime(endTimeLongRTS) + "\n." +
                "Instead, it goes from " + DbTimeHelper.getDateTimeStringFromLongTime(totalDischargeRTS.getStartTime()) + " to " +
                DbTimeHelper.getDateTimeStringFromLongTime(totalDischargeRTS.getEndTime()) + "\n" + 
                "RTS stage goes from " + DbTimeHelper.getDateTimeStringFromLongTime(stageRegularTimeSeries.getStartTime()) + " to " +
                DbTimeHelper.getDateTimeStringFromLongTime(stageRegularTimeSeries.getEndTime()) + "\n";           
                
                System.out.println(message);
                _varControllerLogger.log(message);
                throw new DataAbsentException(message);
            }
        }

        return totalDischargeRTS;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------
    public RegularTimeSeries getPrecipRTSforPrecipFile(String basinId, long startTimeLongForRTS, long endTimeLongRTS) throws DataAbsentException
    {
        return  getPrecipRTSforPrecipFile(basinId, startTimeLongForRTS, endTimeLongRTS, true);
    }
    /***********************************************************************************************************
     * Get the precip RTS spanning from Var start time to Var end time. Convert the unit from inch (default) to mm,
     * to be consistent with Fortran requirement.
     * @return
     */
    public RegularTimeSeries getPrecipRTSforPrecipFile(String basinId, long startTimeLongForRTS, long endTimeLongRTS, boolean checkForMissing ) throws DataAbsentException
    {
        String header = "VarDataMgr.getPrecipRTSforPrecipFile(): ";
        RegularTimeSeries precipRTS = _precipTimeSeriesMap.get(basinId);
        
        if (precipRTS == null)
        {
            precipRTS = _sshpDataMgr.loadObsMapTimeSeriesFromDb(basinId); //in unit of inches
            if (precipRTS !=  null)
            {
                precipRTS.convert(MeasuringUnit.mm);
                _precipTimeSeriesMap.put(basinId, precipRTS);
            }
            else //precipRTS is null
            {
                _varControllerLogger.log("Precipitation time series data for the basin " +
                                          basinId + " is not available");
                throw new DataAbsentException();
            }
        }

        precipRTS = precipRTS.getSubTimeSeries(startTimeLongForRTS, endTimeLongRTS); //for the prediction period, filled with 0 by default
     
        
        if (checkForMissing)
        {
            //check to see if the precip RTS spans from start time to end time
            if((precipRTS.getStartTime() != startTimeLongForRTS) || 
                    (precipRTS.getEndTime() != endTimeLongRTS))
            {
                String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTimeLongForRTS);
                String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTimeLongRTS);
                System.out.print(header + " precip desired start time = " + startTimeString + "  ");
                System.out.println(header + " precip desired end time = " + endTimeString);
                System.out.println(header + "actual precipRTS = " + precipRTS);

                _varControllerLogger.log(header + "The precip time series for the basin " + basinId + 
                        " is not available for " + startTimeString + " to " + endTimeString);

                _varControllerLogger.log(header + "Instead, it is available from " + 
                        DbTimeHelper.getDateTimeStringFromLongTime(precipRTS.getStartTime()) + " to " +
                        DbTimeHelper.getDateTimeStringFromLongTime(precipRTS.getEndTime()) );


                throw new DataAbsentException();
            }
        }

        return precipRTS;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------

    /**********************************************************************************************************************
     * Disconnect from database.
     *
     */
    public void disconnectDatabase()
    {
        _sshpDataMgr.disconnect();
        _varControllerLogger.log("VarDataMgr was disconnected to IHFS database");
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    public long getUhgLengthInMillis(String locationId, String basinId) throws DataAbsentException
    {
        //get the uhg time in millisecs
        long uhgTimeLong = DbTable.getNullLong();
        try
        {
            uhgTimeLong = getUnitHydrograph(locationId, basinId).getOrdinateCount() * MILLIS_PER_HOUR;
        }
        catch(DataAbsentException de)
        {
            _varControllerLogger.log("Unable to load unit hydrograph length...");
            throw new DataAbsentException();
        }
        return uhgTimeLong;

    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    public boolean generateVarInputFiles(String basinId,  int numOfFutureHours,
            long assimilationStartTimeLong, long assimilationTargetTimeLong)
    {
        
        String header = "VarDataMgr.generateVarInputFiles(): ";
        
        InputFilesGenerator inputFilesGenerator = new InputFilesGenerator(_varControllerLogger, _varInputDir, 
                                                                          _varOutputDir, _segmentsFilePath, 
                                                                          basinId, numOfFutureHours);
        
        inputFilesGenerator.setVarModelStartAndEndTimesLong(assimilationStartTimeLong,
                                                            assimilationTargetTimeLong);
        boolean isInputGenerationSuccessful = false;

        RegularTimeSeries precipRTS = null;
        RegularTimeSeries dischargeRTS = null;
        
        try
        {
            _dataLoadingTimer.restart();
            
            String locationId = getLocationIdFromBasinId(basinId);
            
            //relatively constant stuff
            String locationName = getLocationName(locationId);
            MonthlyValues monthlyValues = getMonthlyValues(basinId);
            
            Double basinArea = getBasinArea(basinId);
            
            UnitHydrograph uhg = getUnitHydrograph(locationId, basinId);
            
            //determine if unit hydrograph is a good one
            boolean isUhgGood = (uhg!= null && uhg.getMeasurementList().size() > 0);   
            if   (! isUhgGood)
            {
                String message = "ERROR - basin '" + basinId + "' is missing UnitHydrograph with model = 'SAC-SMA'";
                _varControllerLogger.log(message); 
                throw new DataAbsentException(message);
            }
              
          
            //SAC stuff
            SacSmaParameters sacSmaParams = loadSacSmaParameters(basinId, assimilationStartTimeLong);
            
            long uhgLengthInMillis = getUhgLengthInMillis(locationId, basinId);
            SacSmaState sacSmaState = loadPrevUnadjSacSmaState(basinId, uhgLengthInMillis, assimilationTargetTimeLong);
           
            //time series
            precipRTS = getPrecipRTSforPrecipFile(basinId, assimilationStartTimeLong, assimilationTargetTimeLong);
            
    
            dischargeRTS = getDischargeRTSforDischargeFile(locationId, assimilationStartTimeLong, assimilationTargetTimeLong);

            _dataLoadingTimer.stop(header + " loading static data has taken ");
   
            if(inputFilesGenerator.generateDeckFile( locationName,  sacSmaState, sacSmaParams,  monthlyValues, basinArea, uhg))
            {
                if(inputFilesGenerator.generateObsInputFiles(precipRTS, dischargeRTS))
                {
                    if(inputFilesGenerator.generateSegmentsFile())
                    {
                        isInputGenerationSuccessful = true;
                    }
                    else
                    {
                        _varControllerLogger.log("Unable to generate segmentsFile:[" + _segmentsFilePath +"]");
                    }
                }
                else
                {
                    _varControllerLogger.log("Unable to generate precip file " + basinId + ".mapx_ts_var_dat");
                    _varControllerLogger.log("Unable to generate discharge file " + basinId + ".qin01_ts_var_dat");
                }
            }
            else
            {
                _varControllerLogger.log("Unable to generate parameter deck file " + basinId + ".var_deck");
            }
        }
        catch(DataAbsentException e)
        {
            _varControllerLogger.log("VarDataMgr.generateVarInputFiles(): "+ e);
        }

        return isInputGenerationSuccessful;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public String getLocationIdFromBasinId(String basinId)
    {
        
        //if the cache is empty, then fill it up with data from the SSHPConfig table.
        if (_basinIdToLocationIdMap.isEmpty())
        {
            List<SSHPConfig> configList = _sshpDataMgr.loadSSHPConfigList("");
            
            for (SSHPConfig config : configList)
            {
                _basinIdToLocationIdMap.put(config.getBasinId(), config.getLid());
            }  
        }
        
        //get the data from the cache
        String locationId = _basinIdToLocationIdMap.get(basinId);
   
        return locationId;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------
    public boolean areDischargeAndPrecipDataAvailable(RegularTimeSeries dischargeTimeSeries, RegularTimeSeries precipTimeSeries, long time)
    {
        boolean result = false;
        
        
        result = ( isRegularTimeSeriesDataAvailableAtTime(dischargeTimeSeries, time, "discharge")  &&
                    isRegularTimeSeriesDataAvailableAtTime(precipTimeSeries, time, "precip")  );
        
        return result;
        
      
    }
    
    public boolean isRegularTimeSeriesDataAvailableAtTime(RegularTimeSeries rts, long time, String dataTypeString)
    {
        String header = "VarDataMgr.isRegularTimeSeriesDataAvailableAtTime(): ";
        
        boolean result = false;
               
        AbsTimeMeasurement measurement = rts.getAbsTimeMeasurementByTime(time);
         
        if ((measurement != null) && (measurement.getValue() >= 0.0))
        {
            String message = header + "available " + dataTypeString + " =  " + measurement.getValue() + " at " + DbTimeHelper.getDateTimeStringFromLongTime(time) ;
            _varControllerLogger.log(message);
            System.out.println(message);
            result = true;
        }
        else if (measurement != null) //value < 0.0
        {
            String message = header + dataTypeString + " not available. Bogus value = " + measurement.getValue() + " at " + DbTimeHelper.getDateTimeStringFromLongTime(time) ;
            _varControllerLogger.log(message);
            System.out.println(message);
        }
        else //measurement is null
        {
            String message = header + dataTypeString + " not available. Measurement is null at " + DbTimeHelper.getDateTimeStringFromLongTime(time) ;
            _varControllerLogger.log(message);
            System.out.println(message);
            
        }
        
        return result;
    }
    
//  -------------------------------------------------------------------------------------------------   

    public long getLatestVarSacSmaStatePostingTime()
    {
        //if none found, then -1 is returned
        long latestPostingTime = _sshpDataMgr.getLatestVarSacSmaStatePostingTime(SSHPSource.ADJUSTED_VAR);
        
        return latestPostingTime;
    }
//  -------------------------------------------------------------------------------------------------   
    public List getNewerBestQpeXmrgFileTimeList(long lastPostingTime)
    {
        return _sshpDataMgr.getNewerBestQpeXmrgFileTimeList(lastPostingTime);
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public List getNewerRfcXmrgFileTimeList(long lastPostingTime)
    {      
        return _sshpDataMgr.getNewerRfcXmrgFileTimeList(lastPostingTime);
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public boolean readAndSaveGeneratedResults(String basinId, long assimilationStartTimeLong, long assimilationTargetTimeLong) 
    {
     //   long basisTime = System.currentTimeMillis();
        long basisTime = assimilationTargetTimeLong;

        boolean isSaveDataSuccessful = false;
        //reads the file basinId_adj_states_02_var & basinId_states_02_var and save them into 
        // sacsmastate, fcstdischarge tables with PRODUCT_ID "ADJRUNOFF" & "UADJRUNOFF"
        String header = "VarDataMgr.saveGeneratedResults(): ";
        _varControllerLogger.log("In "+ header);
        int uhgOrdinateNum ;
        try
        {
            
            String locationId = getLocationIdFromBasinId(basinId);
            
            uhgOrdinateNum  =  getUnitHydrograph(locationId, basinId).getMeasurementList().size();

            OutputFileManager outputFileManager = new OutputFileManager(_varControllerLogger, basinId, 
                    assimilationStartTimeLong, assimilationTargetTimeLong, uhgOrdinateNum);

            boolean unadjustedSave = readAndSaveUnadjustedStateAndRunOffValues( outputFileManager,
                                                                                basinId, basisTime,
                                                                                assimilationStartTimeLong, assimilationTargetTimeLong );    

            boolean adjustedSave = readAndSaveAdjustedStateAndRunOffValues( outputFileManager,
                                                                            basinId, basisTime, 
                                                                            assimilationStartTimeLong, assimilationTargetTimeLong);

            if (unadjustedSave &&  adjustedSave)
            {
                isSaveDataSuccessful = true;
            }
        }
        catch(DataAbsentException e)
        {
            _varControllerLogger.log("VarDataMgr.saveGeneratedResults(): DataAbsent"+ e);
        }
        
       //may be used when we do var with adjusted var as input
        //save the adjusted SacSma parameters(peadj, pxadj) into a text file
      //   isSaveDataSuccessful = outputFileManager.saveSacSmaParamsFromAdjFileToParamsFile(_varOutputDir, basinId);
       
        return isSaveDataSuccessful;
    }
//----------------------------------------------------------------------------------------------------------------------------------
    
    private boolean readAndSaveStatesAndRunOffValues(OutputFileManager outputFileManager, String fileNameEndingPart, String basinId,
          long basisTime, long assimilationStartTimeLong, long assimilationTargetTimeLong, SSHPSource sshpSource, boolean saveSecondValue)
    {
        boolean isSaveSuccess = false;

        //      reads the file "basinId + fileNameEndingPart and saves some SacSmaStates and the runoff (tci) values into
        //      the IHFS DB tables: sacsmastate, fcstdischarge 
       // String header = "readAndSaveStatesAndRunOffValues(): ";
        
       // System.out.println(header + "Saving Var variables to database ...");
        String fileName = _varOutputDir + basinId + fileNameEndingPart;
       
        boolean isReadSuccess = outputFileManager.readVarOutputFileForStateAndRunoffValues(fileName);
        if(isReadSuccess)
        {
            long postingTime = System.currentTimeMillis();
            List sacSmaStateList = outputFileManager.getSacSmaStateList();
            for(int i=0; i < sacSmaStateList.size(); i++)
            {
                SacSmaState sacSmaState = (SacSmaState) sacSmaStateList.get(i);
                long validTimeLong = sacSmaState.getValidTime();

                //if(validTimeLong >= assimilationStartTimeLong || validTimeLong <= assimilationTargetTimeLong)
                if (
                    (validTimeLong == assimilationTargetTimeLong)  || 
                    (saveSecondValue &&
                    (validTimeLong == assimilationStartTimeLong + MILLIS_PER_HOUR)   ) 
                   )
                {
                    sacSmaState.setPostingTime(postingTime);
                    sacSmaState.setBasisTime(basisTime);
                    sacSmaState.setSource( sshpSource.getSacSmaSource());
                    _sshpDataMgr.saveSacState(sacSmaState);
                }
            }

            RegularTimeSeries runOffRegularTimeSeries = outputFileManager.getRunOffRegularTimeSeries();
            
       //     printRunoffInfo(assimilationStartTimeLong, assimilationTargetTimeLong, runOffRegularTimeSeries);

            String locationId = getLocationIdFromBasinId(basinId);
            _sshpDataMgr.saveRunoffTimeSeries(locationId, sshpSource.getTypeSource(), basisTime,
                                              sshpSource.getProductId(), runOffRegularTimeSeries);
            isSaveSuccess = true;
        }
        else
        {   
            String message = "Error: unable to complete the reading of the data for " + basinId + ".";
            System.out.println(message);
            _varControllerLogger.log(message);
        }
        return isSaveSuccess;    

    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    private void printRunoffInfo(long assimilationStartTimeLong, long assimilationTargetTimeLong, RegularTimeSeries runoffTimeSeries)
    {
        String assimilationStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(assimilationStartTimeLong);
        String assimilationTargetTimeString = DbTimeHelper.getDateTimeStringFromLongTime(assimilationTargetTimeLong);
        
        System.out.println("---------------------------------------------------------------------------------------");   
        System.out.println("Runoff time series info for assimilationStartTime = " + 
                            assimilationStartTimeString + " and assimilationTargetTime = " + assimilationTargetTimeString);
            
        String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(runoffTimeSeries.getStartTime());
        String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(runoffTimeSeries.getEndTime());
        
        System.out.println("Start time = " + startTimeString + " and end time = " + endTimeString);
        System.out.println("---------------------------------------------------------------------------------------");
        
        return;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    private boolean readAndSaveAdjustedStateAndRunOffValues(OutputFileManager outputFileManager, String basinId,
            long basisTime, long assimilationStartTimeLong, long assimilationTargetTimeLong)
    {
        
        return readAndSaveStatesAndRunOffValues(outputFileManager, "_adj_states_02_var", basinId,  
                                         basisTime,assimilationStartTimeLong,  assimilationTargetTimeLong, SSHPSource.ADJUSTED_VAR, false);
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------
    private boolean readAndSaveUnadjustedStateAndRunOffValues(OutputFileManager outputFileManager, String basinId,
            long basisTime,long assimilationStartTimeLong, long assimilationTargetTimeLong)
    {
        
        return readAndSaveStatesAndRunOffValues(outputFileManager, "_states_02_var", basinId, 
                                         basisTime, assimilationStartTimeLong, assimilationTargetTimeLong, SSHPSource.UNADJUSTED_VAR, true);
    }
    
    
//  ----------------------------------------------------------------------------------------------------------------------------------
} //close class
