/*
 * Created on Jul 30, 2003
 *
 * 
 */
package ohd.hseb.sshp;


import ohd.hseb.db.*;
import ohd.hseb.util.*;

//These are explicity imported in order to show what
//database tables are being used.

import ohd.hseb.ihfsdb.generated.ArealFcstRecord;
import ohd.hseb.ihfsdb.generated.ArealFcstTable;
import ohd.hseb.ihfsdb.generated.ArealObsRecord;
import ohd.hseb.ihfsdb.generated.ArealObsTable;
import ohd.hseb.ihfsdb.generated.ContingencyValueRecord;
import ohd.hseb.ihfsdb.generated.ContingencyValueTable;
import ohd.hseb.ihfsdb.generated.DischargeRecord;
import ohd.hseb.ihfsdb.generated.DischargeTable;
import ohd.hseb.ihfsdb.generated.FcstDischargeRecord;
import ohd.hseb.ihfsdb.generated.FcstDischargeTable;
import ohd.hseb.ihfsdb.generated.FcstHeightRecord;
import ohd.hseb.ihfsdb.generated.FcstHeightTable;
import ohd.hseb.ihfsdb.generated.FcstOtherRecord;
import ohd.hseb.ihfsdb.generated.FcstOtherTable;
import ohd.hseb.ihfsdb.generated.FcstPrecipRecord;
import ohd.hseb.ihfsdb.generated.FcstPrecipTable;
import ohd.hseb.ihfsdb.generated.FloodcatRecord;
import ohd.hseb.ihfsdb.generated.FloodcatTable;
import ohd.hseb.ihfsdb.generated.HeightRecord;
import ohd.hseb.ihfsdb.generated.HeightTable;
import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.LineSegsRecord;
import ohd.hseb.ihfsdb.generated.LineSegsTable;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.MonthlyValuesRecord;
import ohd.hseb.ihfsdb.generated.MonthlyValuesTable;
import ohd.hseb.ihfsdb.generated.ProcValueRecord;
import ohd.hseb.ihfsdb.generated.ProcValueTable;
import ohd.hseb.ihfsdb.generated.RWResultRecord;
import ohd.hseb.ihfsdb.generated.RWResultTable;
import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftTable;
import ohd.hseb.ihfsdb.generated.RatingTable;
import ohd.hseb.ihfsdb.generated.RiverStatusRecord;
import ohd.hseb.ihfsdb.generated.RiverStatusTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.SacSmaParamsRecord;
import ohd.hseb.ihfsdb.generated.SacSmaParamsTable;
import ohd.hseb.ihfsdb.generated.SacSmaStateRecord;
import ohd.hseb.ihfsdb.generated.SacSmaStateTable;
import ohd.hseb.ihfsdb.generated.ShefPeRecord;
import ohd.hseb.ihfsdb.generated.ShefPeTable;
import ohd.hseb.ihfsdb.generated.SshpConfigRecord;
import ohd.hseb.ihfsdb.generated.SshpConfigTable;
import ohd.hseb.ihfsdb.generated.UnitGraphRecord;
import ohd.hseb.ihfsdb.generated.UnitGraphTable;
import ohd.hseb.measurement.*;
import ohd.hseb.model.*;
import ohd.hseb.model.sacsma.*;
import ohd.hseb.sshp.precip.*;

//import whfs.db.gen.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Date;
import java.io.*;
import java.sql.*;




/**
 * @author Chip Gobs
 *
 * DataMgr manages the data for the SSHP application
 */
public class DataMgr
{
    public final double MISSING = -999.0;
    
    private static final int _defaultQualityCode = 1879048191;
    private static final int _questionable_bad_threshold = 	1073741824;
    private static final long _millisPerHour = 60*60*1000;     
	private static final long _millisPerYear = (365L * 24L * 60L * 60L * 1000L);

    private Database _db = null;
    private Logger _logger = null;
    private Map _locationDescriptorMap =  new HashMap();;
    private Map _sshpConfigMap =  new HashMap();
    private Map _basinHelperMap = new HashMap();
    private Map _priorRunoffRecordMap = new HashMap();
    private Map _ratingCurveMap = new HashMap();
    private Map _sigRiverLevelsMap = new HashMap();
    private Map _observedStageTimeSeriesMap = new HashMap();
    private Map _priorRunoffTimeSeriesMap = new HashMap();
    private Map _sacDescriptorListMap = new HashMap();
    private List _sshpConfigList = null;
    private TimeSeriesFileManager _reader = new TimeSeriesFileManager();
    private AppsDefaults _appsDefaults = null;
    
    private boolean _shouldSaveCrestMeasurement = true;
    private static final String SHOULD_SAVE_FCST_CREST_TOKEN_NAME_STRING = "sshp_should_save_fcst_crest";
    
    private static int _referenceCount = 0;
     
    private static CodeTimer _priorRunoffCodeTimer = new CodeTimer();
    
// ------------------------------------------------------------------------
    

// ------------------------------------------------------------------------	
    
    public DataMgr()
    {
        
        this(null, null);
        _logger = new FileLogger("DataMgr.log");
        
        _referenceCount++;
        
        if (_referenceCount > 1)
        {
            throw new Error("Singleton violation error!");    
        }
        
    }

// ------------------------------------------------------------------------
    
    public DataMgr(String baseConnectionString, Logger logger)
    {		
        super();
        
        _appsDefaults = new AppsDefaults();
        
    	_db = new Database();
        _logger = logger;
        
        if (baseConnectionString == null)
        {
            baseConnectionString = getBaseConnectionString();  
        }
        
    	_db.connectWithDriverSearch(baseConnectionString);	
    	
    	_shouldSaveCrestMeasurement = 
    	        _appsDefaults.getBoolean(SHOULD_SAVE_FCST_CREST_TOKEN_NAME_STRING, true);

    }
    
	// ------------------------------------------------------------------------
  
    public String getDatabaseName()
    {
            String dbName = _db.getDatabaseName();
            return dbName;
    }
	
    // ------------------------------------------------------------------------
  	
  	private String getBaseConnectionString()
	{
  	    //this is used in testing mode, not the production mode
		String connectionURLPart = "jdbc:informix-sqli://ds1-nhdr:1530/hd_ob5ounx:informixserver=ONLINE;";
		return connectionURLPart;
	}

    // ------------------------------------------------------------------------
    public void disconnect()
    {
        _db.disconnect();   
    }
    
        //  ------------------------------------------------------------------------
    public void finalize()
    {
        disconnect();   
    }
    
    //  ------------------------------------------------------------------------
    public String getLocationName(String locationId)
    {   
        String locationName = null;
        
        LocationDescriptor desc = getLocationDescriptor(locationId);
        if (desc != null)
        {
            locationName = desc.getLocationName();         
        }     
        return locationName;
    }    
//  ------------------------------------------------------------------------
    private void loadLocationInfo(LocationDescriptor desc, String locationId)
    {               
        String locationName = null;

        LocationRecord record = null;
        LocationTable table = new LocationTable(_db);

        try
        {
            String whereClause = "WHERE lid = '" + locationId + "'";
            List recordList = table.select(whereClause);


            if (recordList.size() > 0)
            {
                record = (LocationRecord) recordList.get(0);
                desc.setLocationName(record.getName());
                desc.setHsa(record.getHsa());
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        return;      
    }
    //  ------------------------------------------------------------------------
  
    private String loadStreamName(String locationId)
    {          
        String streamName = null;

        RiverstatRecord record = null;
        RiverstatTable table = new RiverstatTable(_db);

        try
        {
            String whereClause = "WHERE lid = '" + locationId + "'";
            List recordList = table.select(whereClause);


            if (recordList.size() > 0)
            {
                record = (RiverstatRecord) recordList.get(0);
                streamName = record.getStream();
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        return streamName;    
    }
    //  ------------------------------------------------------------------------
    
    public LocationDescriptor loadLocationDescriptor(String locationId)
    {
        LocationDescriptor desc = new LocationDescriptor();
        String basinId = null;
        String modelPreference = null;
        SSHPConfig config = null;
        String streamName = null;
        
        loadLocationInfo(desc, locationId); //load name and hsa from Location Table
       
        streamName = loadStreamName(locationId);
        
        config = getSSHPConfig(locationId);
        if (config != null)
        {
            basinId = config.getBasinId();
            modelPreference = config.getModelPref();
        }
                
        desc.setId(locationId);
        desc.setStreamName(streamName);
        desc.setBasinId(basinId);
        desc.setModelPreference(modelPreference);

        return desc;
    }

    //  ------------------------------------------------------------------------
   
    public LocationDescriptor getLocationDescriptor(String locationId)
    {
        LocationDescriptor desc = null;    
        
        desc = (LocationDescriptor) _locationDescriptorMap.get(locationId);
        
        if (desc == null)
        {
            desc = loadLocationDescriptor(locationId);
            _locationDescriptorMap.put(locationId, desc);
        }
        
        return desc;
    }
    
//  ------------------------------------------------------------------------
    
    public List loadAllSshpBasinIds()
    {
        
        String header = "DataMgr.loadAllSshpBasinIds(): ";
        
        // This method just reads in all the valid basinIds from the SshpConfig table
        // and returns a list of the basins
        HashSet basinSet = new HashSet();
        List basinIdList = new ArrayList();
        
        List recordList = null;
     
        SshpConfigTable table = new SshpConfigTable(_db);
        SshpConfigRecord record = null;
        
        String whereClause = null;
        
        try
        {
           
           whereClause = " ORDER BY basin_id ";
    
           recordList = table.select(whereClause);

           for (int i = 0; i < recordList.size(); i++)
           {
                record = (SshpConfigRecord) recordList.get(i);
                String basinId = record.getBasin_id();
                
                if (! basinSet.contains(basinId))
                {    
                    basinSet.add(basinId);
                    basinIdList.add(basinId);
                }
                
              //  _logger.log(header + "basinId = " + record.getBasin_id());
           }
  
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }               
  
        return basinIdList;           
    }
    
//  ------------------------------------------------------------------------
    public boolean isLocationIdValidForSSHP(String locationId)
    {
        boolean isValid = false;
        String header = "DataMgr.isLocationIdValidForSSHP():";
        List validLocationIdList = loadLocationIdsForForecasts();
        
        if (validLocationIdList != null)
        {
            isValid = validLocationIdList.contains(locationId);
            _logger.log(header + " locationId = " + locationId);
        }
        
        return isValid;      
    }
 
//  -----------------------------------------------------------------------
    public List loadLocationIdsForForecasts()
    {
           // much of this method's select clause taken from Russ Erb's version
           List locationIdList = new ArrayList();
      
           List recordList = null;
   
           RiverstatTable table = new RiverstatTable(_db);
           RiverstatRecord record = null;
           String whereClause = null;
           
           try
           {  
               whereClause = " WHERE lid IN (SELECT lid FROM SshpConfig)" +
               " AND lid IN (SELECT distinct(lid) FROM Rating) " +
               " AND lid IN (SELECT distinct(lid) FROM UnitGraph) " +
               " AND ((fs IS NOT NULL) OR (fq is NOT NULL)) ORDER BY lid ";
       
               recordList = table.select(whereClause);
               
               for (int i = 0; i < recordList.size(); i++)
               {
                   record = (RiverstatRecord) recordList.get(i);
                   
                   locationIdList.add(record.getLid());
                   // _locationNameMap.add(record.getLid(), record.get());
               }
               
           }
           catch (SQLException e)
           {
               logSQLException(e);
           }    
    
           return locationIdList;         
    }     
//    -------------------------------------------------------     
    public void saveRatingCurve(RatingCurve ratingCurve)
    {
        RatingTable table = new RatingTable(_db);

        RatingRecord record = new RatingRecord();

        record.setLid(ratingCurve.getLocationId());

        try
        {

            for (int i = 0; i < ratingCurve.getRatingPointCount(); i++)
            {
                RatingPoint point = ratingCurve.getRatingPoint(i);
                record.setStage(point.getUnshiftedStage());
                record.setDischarge(point.getDischarge());

                table.insertOrUpdate(record);

            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }


        return;


    }
// -------------------------------------------------------
    public RatingCurve loadRatingCurve(String locationId)
    {
        RatingCurve ratingCurve = null;
         
        RatingTable ratingTable = new RatingTable(_db); 
        RatingShiftTable ratingShiftTable = new RatingShiftTable(_db);
       
        RiverstatTable riverstatTable = new RiverstatTable(_db);
        
        List ratingRecordList = null;
        List ratingShiftList = null;
        List riverstatList = null;
        
        
        try
        {
            ratingRecordList = ratingTable.select("WHERE LID = '" +
            								   locationId + "' ORDER by stage");
            								   
        	ratingShiftList  = ratingShiftTable.selectNRecords("WHERE LID = '" + 
        									   locationId + "' AND active = 'T' ORDER BY date desc", 1 );
                                               
                  
            riverstatList = riverstatTable.select("WHERE LID =  '" +  locationId + "'");  
      
   			//determine total amount of active 
			//rating shifts 
			double totalShiftAmount = 0.0;
			
        	for (int i = 0; i < ratingShiftList.size(); i++)
        	{
        	   RatingShiftRecord shiftRecord = (RatingShiftRecord) ratingShiftList.get(i);
        	   totalShiftAmount = shiftRecord.getShift_amount(); 	

			   //System.out.println("totalShiftAmount = " + totalShiftAmount);
           	}
        	

			//initialize the RatingCurve object with data from the database, including
			// the totalShiftAmount
        	ratingCurve = new RatingCurve(locationId);
        	
    
            ratingCurve.setShiftAmount(totalShiftAmount);
        
            for (int i = 0; i < ratingRecordList.size(); i++)
            {
                RatingRecord record = (RatingRecord) ratingRecordList.get(i);
            
                RatingPoint ratingPoint = new RatingPoint();
                ratingPoint.setDischarge(record.getDischarge());
                ratingPoint.setUnshiftedStage(record.getStage());
    
                //done by addRatingPoint
                //ratingCurve.setShiftAmount(totalShiftAmount);
            	
                ratingCurve.addRatingPoint(ratingPoint);
            } //end for
            
            
            // set the USGS fields in the rating curve
            if (riverstatList.size() > 0)
            {
                  RiverstatRecord riverstatRecord = (RiverstatRecord) riverstatList.get(0);
                  ratingCurve.setUsgsRatingNumber(riverstatRecord.getUsgs_ratenum());
                  ratingCurve.setRatingDate(riverstatRecord.getRatedat());  
            }    
    
        }
        catch (SQLException e)
        {
        	logSQLException(e);
        	ratingCurve = null;
        }
        
        return ratingCurve;
    }
// -------------------------------------------------------	
    public UnitHydrograph loadUnitHydrograph(String locationId, String areaId, String modelName)
    {	
      //  String header = "DataMgr.loadUnitHydrograph(): ";
      //  System.out.println(header + "loading unit hydrograph for modelName = " + modelName);
    	int intervalInHours = 1;
        UnitHydrograph unitHydrograph =  new UnitHydrograph(MeasuringUnit.cfs,
        		  				   			  				intervalInHours);
        unitHydrograph.setLocationId(locationId); 	
        unitHydrograph.setRainfallRunoffModel(modelName);

        UnitGraphTable table = new UnitGraphTable(_db);
		String whereClause = "WHERE lid = '" + locationId + "' AND " +
		                     " area_id = '" + areaId + "' AND " +
		                     " model = '" + modelName + "' AND " +
		                     " dur = 1001 " +
							 " ORDER BY area_id, ordinal";
        	
  		List recordList = null;
  	
        try
        {
            
            recordList = table.select(whereClause);	
        
            for (int i = 0; i < recordList.size(); i++)
            {
                UnitGraphRecord record = (UnitGraphRecord) recordList.get(i);

				long relativeTime = (i * _millisPerHour); 
                RelTimeMeasurement measurement = new RelTimeMeasurement(record.getDischarge(),
																		relativeTime,
                                                          				MeasuringUnit.cfs);
                                                          				 
                unitHydrograph.addMeasurement(measurement);	
            }	  				   
        }
        catch(SQLException e)
        {
        	logSQLException(e);
        }

		return unitHydrograph;

	} //end getUnitHydrograph()

//  -------------------------------------------------------	
    
    private UnitHydrograph loadUnitHydrograph_unused(String locationId)
    {	
    	int intervalInHours = 1;
        UnitHydrograph unitHydrograph =  new UnitHydrograph(
        											MeasuringUnit.cfs,
        		  				   			  		intervalInHours);
        unitHydrograph.setLocationId(locationId); 		  				   

        UnitGraphTable table = new UnitGraphTable(_db);
		String whereClause = "WHERE lid = '" + locationId + 
							 " ORDER BY area_id, ordinal";
        	
  		List recordList = null;
  	
        try
        {
            String areaId = null;

            recordList = table.select(whereClause);	

            if (recordList.size() > 0)
            {
                  UnitGraphRecord record = (UnitGraphRecord) recordList.get(0);
                  areaId = record.getArea_id();
                  unitHydrograph.setAreaId(areaId);
            }
            
            for (int i = 0; i < recordList.size(); i++)
            {
                UnitGraphRecord record = (UnitGraphRecord) recordList.get(i);

                //if a unithydrograph for a different area, but the same location,
                // ignore the later ones.
                if (! areaId.equals(record.getArea_id()))
                {
                    break;
                }

				long relativeTime = (i * _millisPerHour); 
                RelTimeMeasurement measurement = new RelTimeMeasurement(record.getDischarge(),
																		relativeTime,
                                                          				MeasuringUnit.cfs);
                                                          				 
                unitHydrograph.addMeasurement(measurement);	
            }	  				   
        }
        catch(SQLException e)
        {
        	logSQLException(e);
        }

       // System.out.println(header + "ugh = " + unitHydrograph);

		return unitHydrograph;

	} //end getUnitHydrograph()
 
//	-----------------------------------------------------------------------	
 /*   
    private  RegularTimeSeries _unused_getFileBasedPrecipTimeSeries(long endTime,
    													   double valueScaling)
    {
    	MeasuringUnit unit = MeasuringUnit.mm;
    	RegularTimeSeries ts = null;
		double[] precipArray = null;
    	
    	try
    	{
    
			String fileName = "D:/Data/code/exampleCode/sacsma/rainfile";
		    precipArray = DataFileReader.readPrecipArray(fileName);
		    
			int hoursBack = precipArray.length;
			
			long startTime = endTime - (hoursBack * _millisPerHour);
			startTime =  TimeHelper.truncateTimeInMillisToNearestHour(startTime, 1);
    		
    		
    		int intervalInHours = 1;
    			
 		
    		ts = new  RegularTimeSeries(startTime, 
    									endTime,
    									intervalInHours,
    									unit);
    									
			
			long millisPerInterval = (intervalInHours *_millisPerHour);
			
			
			//System.out.println("DataMgr.getFileBasedPrecipTimeSeries(): precipArray.length = " + precipArray.length );
			           
			long currentTime = startTime;
			for (int i = 0; i < precipArray.length; i++)
    		{
			//	System.out.println("DataMgr.getFileBasedPrecipTimeSeries(): i = " + i +
			//			" currentTime = " + DbTimeHelper.getStringFromLongTime(currentTime));
    			
    			double newValue = precipArray[i]* valueScaling;
				Measurement measurement =  new Measurement( newValue, unit);				       
				
				ts.setMeasurementByTime(measurement, currentTime);	
				
				currentTime += millisPerInterval;														  
    		}
    	}
    	catch(IOException e)
    	{
    		e.printStackTrace();
    	}
    	
    	return ts;
    } // end getFileBasedPrecipTimeSeries()

//	-------------------------------------------------------	
	private RegularTimeSeries _unused_getFileBasedEvaporationTimeSeries(long timeShift,
								 					            double valueScaling)
	   {
		   MeasuringUnit unit = MeasuringUnit.mm;
			RegularTimeSeries ts = null;
		   double[] evapArray = null;
    	
		   try
		   {
			   int hoursBack = 72;
			   long startTime = System.currentTimeMillis() - (hoursBack * _millisPerHour);
			   startTime =  TimeHelper.truncateTimeInMillisToNearestHour(startTime, 1);
			
			   startTime += timeShift;
			
			   String fileName = "D:/Data/code/exampleCode/sacsma/evapfile";
			   evapArray = DataFileReader.readEvapArray(fileName);
    		
			   int intervalInHours = 1;
			   long intervalInMillis = intervalInHours * _millisPerHour;
    			
			   long endTime = startTime + ( intervalInMillis * (evapArray.length -1)); 
    		
			   ts = new  RegularTimeSeries(startTime, 
													endTime,
													intervalInHours,
													unit);
			   long currentTime = startTime;
			
			   long millisPerInterval = (intervalInHours *_millisPerHour);
			
			
			   //System.out.println("DataMgr.getFileBasedEvapTimeSeries(): precipArray.length = " + precipArray.length );
			           
			
			   for (int i = 0; i < evapArray.length; i++)
			   {
    		
				   currentTime = startTime + (i * millisPerInterval);	
			   //	System.out.println("DataMgr.getFileBasedPrecipTimeSeries(): i = " + i +
			   //			" currentTime = " + DbTimeHelper.getStringFromLongTime(currentTime));
    			
				   double newValue = evapArray[i]* valueScaling;
				   Measurement measurement =  new Measurement( newValue, unit);				       
				
				   ts.setMeasurementByTime(measurement, currentTime);														  
			   }
		   }
		   catch(IOException e)
		   {
			   e.printStackTrace();
		   }
    	
		   return ts;
	   } // end getFileBasedEvaporationTimeSeries()
*/	   
//	-------------------------------------------------------	
	public double loadThresholdRunoffValue(String locationId)
    {
        double value = MISSING;
        try
        {
            RiverstatTable table = new RiverstatTable(_db);
            RiverstatRecord record = null;
            String whereClause = "WHERE lid = '" + locationId +
                           "' AND threshold_runoff is not null"; 

            List list = table.selectNRecords(whereClause, 1);
            if (list.size() > 0)
            {
                record = (RiverstatRecord) list.get(0);
                value = record.getThreshold_runoff();    
            }
            
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        if (value != MISSING)
        {
            _logger.log("DataMgr.getThresholdRunoffValue(): value = " + value);    
        }


        return value;
    }   

//  ------------------------------------------------------- 
	public SigRiverLevels getSigRiverLevels(String locationId)
	{
	    SigRiverLevels sigRiverLevels = (SigRiverLevels) _sigRiverLevelsMap.get(locationId);
        
        if (sigRiverLevels == null)
        {
            sigRiverLevels = setUpSigRiverLevels(locationId);
            _sigRiverLevelsMap.put(locationId, sigRiverLevels);
        }
        
        return sigRiverLevels;
	}

//  ------------------------------------------------------- 

	public SigRiverLevels setUpSigRiverLevels(String locationId)
	{
	    SigRiverLevels sigRiverLevels = null;
	    // if needed, retrieve and cache the data 
	    // then return the value

	    sigRiverLevels =  loadSigRiverLevels(locationId);   

	    boolean stagesAreMissing = false;
	    if (! sigRiverLevels.hasFloodStage() || ! sigRiverLevels.hasActionStage())
	    {
	        stagesAreMissing = true;
	    }

	    // if the site is set up to use discharge (flow)  instead of stage,
	    // convert from flow to stage and populate the floodStage and actionStage 
	    //(if possible),
	    // so that they are available to other parts of the program
	    if (stagesAreMissing)
	    {
	        RatingCurve ratingCurve = getRatingCurve(locationId);
	        if (sigRiverLevels.hasFloodFlow())
	        {
	            double floodFlow = sigRiverLevels.getFloodFlow(); 
	            double floodStage =  ratingCurve.getStageFromDischarge(floodFlow);
	            sigRiverLevels.setFloodStage(floodStage);
	        }
	        if (sigRiverLevels.hasActionFlow())
	        {
	            double actionFlow = sigRiverLevels.getActionFlow(); 
	            double actionStage =  ratingCurve.getStageFromDischarge(actionFlow);
	            sigRiverLevels.setActionStage(actionStage);
	        }
	    }



	    return sigRiverLevels;
    }

//	-------------------------------------------------------	
    public RatingCurve getRatingCurve(String locationId)
    {
  
        RatingCurve ratingCurve = (RatingCurve) _ratingCurveMap.get(locationId);
        
        if (ratingCurve == null)
        {
        
            ratingCurve = loadRatingCurve(locationId);   
            
            _ratingCurveMap.put(locationId, ratingCurve);
        }
        
        return ratingCurve;

    }
    
    
//  ------------------------------------------------------- 
    public IrregularTimeSeries getCachedObservedStageTimeSeriesFromStageOrDischarge(String locationId, boolean reload)
    {
        IrregularTimeSeries observedStageTimeSeries = (IrregularTimeSeries) _observedStageTimeSeriesMap.get(locationId);

        
        if ((observedStageTimeSeries == null) || (reload) )
        {
            observedStageTimeSeries = loadObservedStageTimeSeriesFromStageOrDischarge(locationId);
            _observedStageTimeSeriesMap.put(locationId, observedStageTimeSeries);
        }
        
        return observedStageTimeSeries;
    }
 
//  ------------------------------------------------------- 
    public IrregularTimeSeries loadObservedStageTimeSeriesFromStageOrDischarge(String locationId)
    {
        IrregularTimeSeries observedStageTimeSeries = null;

        if (getSigRiverLevels(locationId).useStage())
        {
            observedStageTimeSeries = loadObservedStageTimeSeries(locationId);
        }
        else //use flow and then convert it
        {
            IrregularTimeSeries observedDischargeTimeSeries = null;
            observedDischargeTimeSeries = loadObservedDischargeTimeSeries(locationId);
            if (observedDischargeTimeSeries.getMeasurementCount() > 0)
            {
                observedStageTimeSeries = getRatingCurve(locationId).getStageTimeSeries(observedDischargeTimeSeries);
            }
            else //there is no discharge time series
            {
                observedStageTimeSeries = loadObservedStageTimeSeries(locationId);         
            }
        }

        return observedStageTimeSeries;
    }

//  ------------------------------------------------------- 

	public IrregularTimeSeries loadObservedStageTimeSeries(String locationId)
	{
	    String header = "DataMgr.loadObservedStageTimeSeries(): ";
		IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(MeasuringUnit.feet);
		
        final double missingValue = -9999.0;
		HeightTable table = new HeightTable(_db);
		HeightRecord record = null;
		List recordList = null;
		AbsTimeMeasurement measurement = null;
		
		final long maxLong = Long.MAX_VALUE;
		long currentTime = System.currentTimeMillis();
		long earlyTime = currentTime - _millisPerYear;
		long lateTime = currentTime + _millisPerYear;
			  
   	/*
		String whereClause = " WHERE lid = '" + locationId + "'" +
						     " AND pe = '" + pe + "'" +
						     " AND dur = " + duration +
						     " AND ts = '" + typeSource + "'" +
						     " AND extremum = 'Z' ORDER by obstime ASC";
	*/	
		
	
		String whereClause = " WHERE lid = '" + locationId + "'" +
         " AND pe <> 'HI' " +
	     " AND dur = 0" +
	     " AND extremum = 'Z'" +
	     " AND quality_code >= " + _questionable_bad_threshold;
        
     //   System.out.println(header + "whereClause = " + whereClause);
	
		try
		{	    
		    recordList = table.select(whereClause);
			
			for (int i = 0; i < recordList.size(); i++)
			{
				record = (HeightRecord) recordList.get(i);
				
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
				    measurement = new AbsTimeMeasurement(record.getValue(),
				                                     record.getObstime(),
				                                     MeasuringUnit.feet); 
				                                     
				    stageTimeSeries.insertMeasurement(measurement);
                }   
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
		}
	    
		return stageTimeSeries;
 	
	}
//	-------------------------------------------------------	      
	public IrregularTimeSeries loadObservedStageTimeSeriesOld(String locationId)
	{
	    //return the best observed stage time series available, that
	    // based on ts_rank usage
	    final String header = "DataMgr.loadObservedStageTimeSeries(): ";
		IrregularTimeSeries stageTimeSeries = null;
		
		String pe = "HG";
		int duration = 0;
		
		String[] preferredObsTsArray = loadPreferredObsTypeSourceArray(locationId, pe, duration);
		
		boolean success = false;
		
		for (int i = 0; i <  preferredObsTsArray.length && !success; i++)
		{
		    String ts = preferredObsTsArray[i];
		    System.out.println(header + "locationId = " + locationId + " ts = " + ts);
		    stageTimeSeries = loadObservedStageTimeSeriesByTsOld(locationId, pe, duration, ts);
		    if (stageTimeSeries.getMeasurementCount() > 0)
		    {
		        System.out.println(header + "found " + stageTimeSeries.getMeasurementCount() + " records.");
		        success = true;
		    }
		}
		
        return stageTimeSeries;
	}
    
    
//	-------------------------------------------------------	      
    
//	-------------------------------------------------------	
	public IrregularTimeSeries loadObservedStageTimeSeriesByTsOld(String locationId, String pe, int duration, String typeSource)
	{
	    String header = "DataMgr.loadObservedStageTimeSeriesByTs(): ";
		IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(MeasuringUnit.feet);
		
        final double missingValue = -9999.0;
		HeightTable table = new HeightTable(_db);
		HeightRecord record = null;
		List recordList = null;
		AbsTimeMeasurement measurement = null;
		
		final long maxLong = Long.MAX_VALUE;
		long currentTime = System.currentTimeMillis();
		long earlyTime = currentTime - _millisPerYear;
		long lateTime = currentTime + _millisPerYear;
		//String earlyTimeString = DbTimeHelper.getDateTimeStringFromLongTime(earlyTime);
		//String lateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(lateTime);
		String earlyTimeString = "2001-01-01 00:00:00";
		String lateTimeString = "3001-01-01 00:00:00";
		  
   	/*
		String whereClause = " WHERE lid = '" + locationId + "'" +
						     " AND pe = '" + pe + "'" +
						     " AND dur = " + duration +
						     " AND ts = '" + typeSource + "'" +
						     " AND extremum = 'Z' ORDER by obstime ASC";
	*/	
		
	
		String whereClause = " WHERE lid = '" + locationId + "'" +
	     " AND pe = '" + pe + "'" +
	     " AND dur = " + duration +
	     " AND ts = '" + typeSource + "'" +
	     " AND extremum = 'Z'" +
	     " AND shef_qual_code != 'F' ";
	     

		
		try
		{	
		    CodeTimer timer = new CodeTimer();
			timer.start();
			_db.getStatement().execute("SET EXPLAIN ON");
		    recordList = table.select(whereClause);
			_db.getStatement().execute("SET EXPLAIN OFF");
			timer.stop(header + "Height select with " + whereClause + "\n took ");
    	
			for (int i = 0; i < recordList.size(); i++)
			{
				record = (HeightRecord) recordList.get(i);
				
                //don't let the missing values be included          
                if (record.getValue() != missingValue)
                {
				    measurement = new AbsTimeMeasurement(record.getValue(),
				                                     record.getObstime(),
				                                     MeasuringUnit.feet); 
				                                     
				    stageTimeSeries.insertMeasurement(measurement);
                }   
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
		}
	    
		return stageTimeSeries;
 	
	}
    
	
//	  -----------------------------------------------------------------------------------

	public IrregularTimeSeries loadObservedDischargeTimeSeries(String locationId)
	                                                         
	    {
	        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(
	                MeasuringUnit.cfs);

	        final double missingValue = -9999.0;
	        DischargeTable table = new DischargeTable(_db);
	        DischargeRecord record = null;
	        List recordList = null;
	        AbsTimeMeasurement measurement = null;


	        String whereClause = 
	                 " WHERE lid = '" + locationId + "'" +
	        		 " AND dur = 0" +
	                 " AND extremum = 'Z'" +
	                 " AND quality_code >= " + _questionable_bad_threshold;

	        try
	        {
	            recordList = table.select(whereClause);

	            for (int i = 0; i < recordList.size(); i++)
	            {
	                record = (DischargeRecord) recordList.get(i);

	                //don't let the missing values be included
	                if (record.getValue() != missingValue)
	                {
	                    measurement = new AbsTimeMeasurement(record.getValue(),
	                            record.getObstime(), MeasuringUnit.cfs);

	                    dischargeTimeSeries.insertMeasurement(measurement);
	                }
	            }
	        }
	        catch (SQLException e)
	        {
	            logSQLException(e);
	        }

	        return dischargeTimeSeries;

	    }
		    
		    
//	  -----------------------------------------------------------------------------------
	public IrregularTimeSeries loadObservedDischargeTimeSeriesByTsOld(
                                                                   String locationId,
                                                                   String pe,
                                                                   int duration,
                                                                   String typeSource)
    {
        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(
                MeasuringUnit.cfs);

        final double missingValue = -9999.0;
        DischargeTable table = new DischargeTable(_db);
        DischargeRecord record = null;
        List recordList = null;
        AbsTimeMeasurement measurement = null;


        String whereClause = " WHERE lid = '" + locationId + "'"
                + " AND pe = '" + pe + "'" + " AND dur = " + duration
                + " AND ts = '" + typeSource + "'"
                + " AND extremum = 'Z' ORDER by obstime ASC";

        try
        {
            recordList = table.select(whereClause);

            for (int i = 0; i < recordList.size(); i++)
            {
                record = (DischargeRecord) recordList.get(i);

                //don't let the missing values be included
                if (record.getValue() != missingValue)
                {
                    measurement = new AbsTimeMeasurement(record.getValue(),
                            record.getObstime(), MeasuringUnit.cfs);

                    dischargeTimeSeries.insertMeasurement(measurement);
                }
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        return dischargeTimeSeries;

    }
	    
	    
//		-------------------------------------------------------	  
	public IrregularTimeSeries loadObservedDischargeTimeSeriesOld(String locationId)
	{
	    //return the best observed discharge time series available, that
	    // based on ts_rank usage
	    
		IrregularTimeSeries dischargeTimeSeries = null;
		
		String pe = "QR";
		int duration = 0;
		
		String[] preferredObsTsArray = loadPreferredObsTypeSourceArray(locationId, pe, duration);
		
		boolean success = false;
		
		for (int i = 0; i <  preferredObsTsArray.length && !success; i++)
		{
		    String ts = preferredObsTsArray[i];
		    dischargeTimeSeries = loadObservedDischargeTimeSeriesByTsOld(locationId, pe, duration, ts);
		    if (dischargeTimeSeries.getMeasurementCount() > 0)
		    {
		        success = true;
		    }
		}
		
        return dischargeTimeSeries;
	}
    
    
//	-------------------------------------------------------	  
	public IrregularTimeSeries loadFcstStageTimeSeries(String locationId, String pe)
	{
	    MeasuringUnit unit = MeasuringUnit.feet;
	    String header = "DataMgr.loadFcstStageTimeSeries(): ";
	    IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(unit);

	    final double missingValue = -9999.0;
	    FcstHeightTable table = new FcstHeightTable(_db);
	    FcstHeightRecord record = null;
	    List recordList = null;
	    AbsTimeMeasurement measurement = null;

	    final long maxLong = Long.MAX_VALUE;


	    String whereClause = " WHERE lid = '" + locationId + "'" +
	    " AND pe = '" + pe + "'" +  
	     " ORDER BY basistime DESC ";

	    long originalBasisTime = 0;

	    try
	    {       
	        recordList = table.select(whereClause);

	        for (int i = 0; i < recordList.size(); i++)
	        {


	            record = (FcstHeightRecord) recordList.get(i);

	            if (i == 0)
	            {
	                originalBasisTime = record.getBasistime();   
	            }
	            else
	            {
	                if (record.getBasistime() != originalBasisTime)
	                {
	                    break;    
	                }
	            }

//	            don't let the missing values be included          
	            if (record.getValue() != missingValue)
	            {
	                measurement = new AbsTimeMeasurement(record.getValue(),
	                        record.getValidtime(),
	                        unit); 

	                stageTimeSeries.insertMeasurement(measurement);

//	                System.out.println(header + "measurement = " + measurement);

	            }   
	        }
	    }
	    catch(SQLException e)
	    {
	        logSQLException(e);
	    }

	    return stageTimeSeries;
	}
    
//  -----------------------------------------------------------------------------------   

    
	public IrregularTimeSeries loadFcstDischargeTimeSeries(String locationId,
	        String pe)
	{
	    MeasuringUnit unit = MeasuringUnit.feet;
	    String header = "PdcTslDataManager.loadFcstDischargeTimeSeries(): ";
	    IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(unit);

	    final double missingValue = -9999.0;
	    FcstDischargeTable table = new FcstDischargeTable(_db);
	    FcstDischargeRecord record = null;
	    List recordList = null;
	    AbsTimeMeasurement measurement = null;

	    final long maxLong = Long.MAX_VALUE;


	    String whereClause = " WHERE lid = '" + locationId + "'" +
	    " AND pe = '" +pe + "'" +  
	    " ORDER BY basistime DESC ";
        
	    long originalBasisTime = 0;

	    try
	    {       
	        recordList = table.select(whereClause);

	        for (int i = 0; i < recordList.size(); i++)
	        {


	            record = (FcstDischargeRecord) recordList.get(i);

	            if (i == 0)
	            {
	                originalBasisTime = record.getBasistime();   
	            }
	            else
	            {
	                if (record.getBasistime() != originalBasisTime)
	                {
	                    break;    
	                }
	            }

//	            don't let the missing values be included          
	            if (record.getValue() != missingValue)
	            {
	                measurement = new AbsTimeMeasurement(record.getValue(),
	                        record.getValidtime(),
	                        unit); 

	                dischargeTimeSeries.insertMeasurement(measurement);

//	                System.out.println(header + "measurement = " + measurement);

	            }   
	        }
	    }
	    catch(SQLException e)
	    {
	        logSQLException(e);
	    }

	    return dischargeTimeSeries;
}


    // ---------------------------------------------------------------------------------------------

	public Measurement _unused_getHeightMeasurement2(String locationId)
	{
		RiverStatusTable table = new RiverStatusTable(_db);
		RiverStatusRecord record = null;
		List recordList = null;
		Measurement measurement = null;
	
		String whereClause = " WHERE lid = '" + locationId + "'";
	
		try
		{	
			recordList = table.select(whereClause);
	
			if (recordList.size() > 0)
			{
				record = (RiverStatusRecord) recordList.get(0);	
		
				measurement = new Measurement(record.getValue(), MeasuringUnit.feet);    
			}
		}
		catch(Exception e)
		{
			System.err.println("error retrieving RiverStatusRecord " + whereClause);
			e.printStackTrace();
		}
    
		return measurement;


	}

//	-------------------------------------------------------	

    public SacSmaParameters loadSacSmaParameters(String basinId, long modelRunStartTime)
    {
        String header = "DataMgr.getSacSmaParameters(): ";
    	SacSmaParameters sacParams = null;
       
    	
    	SacSmaParamsTable table = new SacSmaParamsTable(_db);
    	
    	String whereClause = "WHERE basin_id = '" + basinId + "' ORDER by validtime DESC";
    	
    	try
    	{
    	    List recordList = table.select(whereClause);
    	
        
            for (int i = 0 ; i < recordList.size(); i++)
            {
			
    	 	    SacSmaParamsRecord record = (SacSmaParamsRecord) recordList.get(i);	
    		
                if (record.getValidtime() <= modelRunStartTime)
                {
                    //String dateTime =  DbTimeHelper.getDateTimeStringFromLongTime(record.getValidtime());
                   
                   // System.out.println(header + " validtime found = " + dateTime);
                    sacParams = new SacSmaParameters();      
    		        setSacSmaParametersFromRecord(sacParams, record);
                    break;
                }
                // else, keep looking for a record before the modelRunStartTime
            }
    	}
    	catch(SQLException e)
    	{
            logSQLException(e);
    	}
    	return sacParams;
    }
    
//	-------------------------------------------------------	
	public void setSacSmaParametersFromRecord(SacSmaParameters sacParams,
											  SacSmaParamsRecord record)
	{
       
        sacParams.setBasinId(record.getBasin_id());
        sacParams.setSource(record.getSource());
        sacParams.setValidTime(record.getValidtime());
        sacParams.setPostingTime(record.getPostingtime());
        
		sacParams.setUztwm(record.getUztwm());
		sacParams.setUzfwm(record.getUzfwm());
		sacParams.setUzk(record.getUzk());
			
		sacParams.setPctim(record.getPctim());
	
		sacParams.setAdimp(record.getAdimp());
		
		sacParams.setRiva(record.getRiva());
	
		sacParams.setZperc(record.getZperc());
		sacParams.setRexp(record.getRexp());
		sacParams.setLztwm(record.getLztwm());
		sacParams.setLzfsm(record.getLzfsm());
		
		sacParams.setLzfpm(record.getLzfpm());
	
		sacParams.setLzsk(record.getLzsk());
		sacParams.setLzpk(record.getLzpk());

		sacParams.setPfree(record.getPfree());
	
		sacParams.setRserv(record.getRserv());
		sacParams.setSide(record.getSide());
        
        sacParams.setPeadj(record.getPeadj());
        sacParams.setPxadj(record.getPxadj());
        sacParams.setEfc(record.getEfc());
        
	
		return;
	}		
//	-------------------------------------------------------	
    public SacSmaState loadSacSmaState(String basinId, long modelRunStartTime)
    {
        SacSmaState sacState = loadSacSmaState(basinId, null, modelRunStartTime);
        
        return sacState;
    }
  
//  ------------------------------------------------------- 

    
    public SacSmaState loadSacSmaState(String basinId, String source, long modelRunStartTime)
    {
        // returns null when no state is found
      //  String header = "DataMgr.loadSacSmaState(): ";
        SacSmaState sacState = null;
        SacSmaStateTable table = new SacSmaStateTable(_db);
        
        String sourceClause = null;
         
        if (source == null)
        {
            sourceClause = "";
        }
        else // source is not null
        {
            sourceClause = " AND source = '" + source + "'";
        }
        
        String whereClause =  "WHERE basin_id = '" + basinId + "'" + sourceClause + " ORDER by validtime DESC";
        
        
        try
        {
            List recordList = table.select(whereClause);

            for (int i = 0 ; i < recordList.size(); i++)
            {
            
                SacSmaStateRecord record = (SacSmaStateRecord) recordList.get(i);   
            
                if (record.getValidtime() <= modelRunStartTime)
                {
                    String dateTime =  DbTimeHelper.getDateTimeStringFromLongTime(record.getValidtime());
                   
 //                   System.out.println(header + " validtime found = " + dateTime);
                    
                    sacState= new SacSmaState();
                    setSacSmaStateFromRecord(sacState, record);
                    break;    
                }
                //else, keep looking for a record with a validtime before the modelRunStartTime
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
    
        return sacState;
    }
    
    //------------------------------------------------------------------------------------------------------------------
    
    
    public SacSmaState loadOldestSacSmaState(String basinId, String source)
    {
        // returns null when no state is found
        String header = "DataMgr.loadOldestSacSmaState(): ";
        SacSmaState sacState = null;
        SacSmaStateTable table = new SacSmaStateTable(_db);
        
        String sourceClause = null;
         
        if (source == null)
        {
            sourceClause = "";
        }
        else // source is not null
        {
            sourceClause = " AND source = '" + source + "'";
        }
        
        String whereClause =  "WHERE basin_id = '" + basinId + "'" + sourceClause + " ORDER by validtime ASC";
        
        
        try
        {
            List recordList = table.select(whereClause);

            if (recordList.size() > 0)
            {
                SacSmaStateRecord record = (SacSmaStateRecord) recordList.get(0);   

                sacState= new SacSmaState();
                setSacSmaStateFromRecord(sacState, record);
            }
 
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
    
        return sacState;
    }
    
    
    //------------------------------------------------------------------------------------------------------------------
    
	public SacSmaState oldLoadSacSmaState(String basinId, long modelRunStartTime)
	{
        // returns null when no state is found
        String header = "DataMgr.getSacSmaState(): ";
		SacSmaState sacState = null;
		
		SacSmaStateTable table = new SacSmaStateTable(_db);
    	
		String whereClause = "WHERE basin_id = '" + basinId + "' ORDER by validtime DESC";
    	
		try
		{
			List recordList = table.select(whereClause);

            for (int i = 0 ; i < recordList.size(); i++)
            {
    		
				SacSmaStateRecord record = (SacSmaStateRecord) recordList.get(i);	
    		
                if (record.getValidtime() <= modelRunStartTime)
                {
                    String dateTime =  DbTimeHelper.getDateTimeStringFromLongTime(record.getValidtime());
                   
                    System.out.println(header + " validtime found = " + dateTime);
				    
                    sacState= new SacSmaState();
                    setSacSmaStateFromRecord(sacState, record);
                    break;    
                }
                //else, keep looking for a record with a validtime before the modelRunStartTime
    		}
		}
		catch(SQLException e)
		{
            logSQLException(e);
		}
	
		return sacState;
	}
    
//  ------------------------------------------------------- 
    public long loadTimeOfBestRecentSacState(String locationId)
    {
        long bestTime = 0;
        
        long hourLimit = 25 ;  // a little over 24 hours
        
        long preferredSourceValidTime = 0;
        long allSourceValidTime = 0;
        
        // 
        SSHPConfig config = this.getSSHPConfig(locationId);
        String basinId = null;
        String preferredSource = null;
        
        if (config != null)
        {
           basinId = config.getBasinId();
           preferredSource = config.getSourcePref();   
        }
        else
        {
           throw new Error("DataMgr.getTimeOfBestRecentState(): no SSHPConfig record for locationId = " + locationId); 
        }
        
        String preferredSourceWhereClause = " WHERE basin_id = '" + basinId + "' " +
                             " AND source = '" + preferredSource + "' " +
                             " ORDER BY validtime DESC ";
           
        String allSourceWhereClause = " WHERE basin_id = '" + basinId + "' " +  
                                      " ORDER BY validtime DESC ";
                          
                             
        SacSmaStateTable table = new SacSmaStateTable(_db);  
        
        try
        {
                    
            //select the most recent record with the preferred source               
            List preferredRecordList = table.selectNRecords(preferredSourceWhereClause,1);
            
            if (preferredRecordList.size() > 0)
            {
               SacSmaStateRecord record = (SacSmaStateRecord) preferredRecordList.get(0);
               
               preferredSourceValidTime = record.getValidtime();      
            }
            
            
            // select all the records, regardless of source
            List allRecordList = table.selectNRecords(allSourceWhereClause,1);
            
            if (allRecordList.size() > 0)
            {
                SacSmaStateRecord record = (SacSmaStateRecord) allRecordList.get(0);
               
                allSourceValidTime = record.getValidtime();      
            }
            
            //get current system time
            long currentTime = System.currentTimeMillis();
       
              
            // select the preferredSourceValidTime as the best if it is within the time window
            long diffTime = currentTime - preferredSourceValidTime;
            
            if (diffTime < _millisPerHour * hourLimit)
            {
                bestTime = preferredSourceValidTime;     
            }
            else //the preferred is not available, so use the next best thing
            {
                bestTime = allSourceValidTime;
            }
        
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        
        
        return bestTime;
    }
//	-------------------------------------------------------	
    public SacSmaState loadBestRecentSacState(String locationId)
       {
           
           SacSmaState preferredSourceState = null;
           SacSmaState allSourceState = null;
           SacSmaState bestState = null;
           
           long bestTime = 0;
        
           long hourLimit = 25 ;  // a little over 24 hours
        
           long preferredSourceValidTime = 0;
           long allSourceValidTime = 0;
        
           // 
           SSHPConfig config = this.getSSHPConfig(locationId);
           String basinId = null;
           String preferredSource = null;
        
           if (config != null)
           {
              basinId = config.getBasinId();
              preferredSource = config.getSourcePref();   
           }
           else
           {
              throw new Error("DataMgr.loadBestRecentSacState(): no SSHPConfig record for locationId = " + locationId); 
           }
        
           String preferredSourceWhereClause = " WHERE basin_id = '" + basinId + "' " +
                                " AND source = '" + preferredSource + "' " +
                                " ORDER BY validtime DESC ";
           
           String allSourceWhereClause = " WHERE basin_id = '" + basinId + "' " +  
                                         " ORDER BY validtime DESC, source ";
                          
                             
           SacSmaStateTable table = new SacSmaStateTable(_db);  
        
           try
           {
                    
               //select the most recent record with the preferred source               
               List preferredRecordList = table.selectNRecords(preferredSourceWhereClause,1);
            
               if (preferredRecordList.size() > 0)
               {
                  SacSmaStateRecord record = (SacSmaStateRecord) preferredRecordList.get(0);
               
                  preferredSourceState = getSacSmaStateFromRecord(record);
                  preferredSourceValidTime = record.getValidtime();      
               }
            
            
               // select all the records, regardless of source
               List allRecordList = table.selectNRecords(allSourceWhereClause,1);
            
               if (allRecordList.size() > 0)
               {
                   SacSmaStateRecord record = (SacSmaStateRecord) allRecordList.get(0);
                   allSourceState = getSacSmaStateFromRecord(record);
               
                   allSourceValidTime = record.getValidtime();      
               }
            
               //get current system time
               long currentTime = System.currentTimeMillis();
       
              
               // select the preferredSourceValidTime as the best if it is within the time window
               long diffTime = currentTime - preferredSourceValidTime;
            
               if (diffTime < _millisPerHour * hourLimit)
               {
                   bestState = preferredSourceState;
                   bestTime = preferredSourceValidTime;     
               }
               else //the preferred is not available, so use the next best thing
               {
                   bestState = allSourceState;
                   bestTime = allSourceValidTime;
               }
        
           }
           catch (SQLException e)
           {
               logSQLException(e);
           }
        
        
        
           return bestState;
       }
//     -------------------------------------------------------  

    
    public SacSmaState loadBestRecentSacStateForForecasts(String locationId)
    {
        SacSmaState state = null;
        Map<String, SacSmaState> stateMap = new HashMap<String, SacSmaState>();

        
        long bestTime = 0;
  
        //  load configuration data
        SSHPConfig config = this.getSSHPConfig(locationId);
        
        String basinId = null;      
        boolean useAdjustment = false;
        
        if (config != null)
        {
           basinId = config.getBasinId();
           useAdjustment = config.useBlend();
        }
        else
        {
           throw new Error("DataMgr.loadBestRecentSacStateForForecasts(): no SSHPConfig record for locationId = " + locationId); 
        }
        
        SacSmaStateTable table = new SacSmaStateTable(_db);  
        
        String allSourceWhereClause = " WHERE basin_id = '" + basinId + "' " +  
                                      " ORDER BY validtime DESC, source ";
                       
           
        long latestValidTime = 0;
        int maxRecords = 10;
        
        try
        {
             // select all the records, regardless of source
            List<SacSmaStateRecord> allRecordList = table.selectNRecords(allSourceWhereClause, maxRecords);
         
            int recordCount = 0;
            
            // put all the records into a map
            // also, record the time of the first record.
            for (SacSmaStateRecord record : allRecordList)
            {
                state = getSacSmaStateFromRecord(record);  
                
                String key = getStateMapKeyFromState(state);
                
                stateMap.put(key, state);
                
                if (recordCount == 0)
                {
                    latestValidTime = state.getValidTime(); //use the first record to set the latest time
                }
                
                recordCount++;
               
            }
          
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
     
        
        SacSmaState bestState = getBestState(stateMap, latestValidTime);
        
     
        return bestState;
    }
//  -------------------------------------------------------  
    private SacSmaState getBestState(Map<String, SacSmaState> stateMap, long latestValidTime)
    {
        //This method determines what is the best SAC-SMA state to use for the background forecast.
        SacSmaState bestState = null;
        List<SSHPSource> sshpSourceList = new ArrayList<SSHPSource>();
        
        sshpSourceList.add(SSHPSource.ADJUSTED_VAR);
        sshpSourceList.add(SSHPSource.RFC);
        sshpSourceList.add(SSHPSource.UNADJUSTED_VAR);
        sshpSourceList.add(SSHPSource.SSHP_UPDATER);
        
        
        //run through the preferred order of state sources
        // when find one that matches what I want, set it and break;
        for (SSHPSource sshpSource : sshpSourceList)
        {
            SacSmaState state = getStateBySSHPSource(sshpSource, stateMap, latestValidTime);
            if (state != null)
            {
                bestState = state;
                break;
            }

        }
 
        return bestState;
    }
//  -------------------------------------------------------  
    private SacSmaState getStateBySSHPSource(SSHPSource sshpSource, 
                                             Map<String, SacSmaState> stateMap,
                                             long latestValidTime)
    {
        String key = getStateMapKey(sshpSource.getSacSmaSource(), latestValidTime);
        
        SacSmaState  state = stateMap.get(key);
        
        return state;
    }
    
//  -------------------------------------------------------  
    
    private String getStateMapKeyFromState(SacSmaState state)
    {
     
        return getStateMapKey(state.getSource(), state.getValidTime());
    }
    
 //  -------------------------------------------------------  

    private String getStateMapKey(String source, long validTime)
    {
        String validTimeString = DbTimeHelper.getDateTimeStringFromLongTime(validTime);
        
        String key = source + "|" + validTimeString;
   
        return key;
    }
//  -------------------------------------------------------  

    public long getTimeOfBestRecentFFH(String locationId, String basinId)
    {
        long bestTime = 0;
        
       
       
        long currentTime = System.currentTimeMillis();
        AbsTimeMeasurement ffhMeasurement = loadMostCurrentFfhMeasurement(locationId, basinId, currentTime);  
        if (ffhMeasurement != null)
        {
            bestTime = ffhMeasurement.getTime();    
        }
     
        return bestTime;
    }
 
//  ------------------------------------------------------- 
      
    public long getLatestHourTime()
    {
        return TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(), 1);
    }
 
//  ------------------------------------------------------- 

	public void setSacSmaStateFromRecord(SacSmaState sacState, SacSmaStateRecord record)
	{
        
      
        sacState.setBasinId(record.getBasin_id());
        sacState.setSource(record.getSource());
        sacState.setBasisTime(record.getBasistime());
        sacState.setValidTime(record.getValidtime());
        sacState.setPostingTime(record.getPostingtime());
     
        
		sacState.setUztwc(record.getUztwc());
		sacState.setUzfwc(record.getUzfwc());
		sacState.setLztwc(record.getLztwc());
		sacState.setLzfsc(record.getLzfsc());
		sacState.setLzfpc(record.getLzfpc());
		sacState.setAdimc(record.getAdimc());
		
	//	System.out.println("DataMgr.setSacSmaStageFromRecord() sacState = " + sacState);
		
		return;
	}
   
   
//  ------------------------------------------------------- 
/*
    
    public void testGeneratedCodeWithNulls()
    {
        TestHeightTable heightTable = new TestHeightTable(_db);
        TestHeightRecord record = null;
        List recordList = null;
        CodeTimer timer = new CodeTimer();
    
        try
        {
            timer.start();
            recordList = heightTable.select("where lid = 'BLUO2' order by obstime DESC ");
            timer.stop("after Select from Height Table");
        
            if (recordList.size() > 0)
            {
               record = (TestHeightRecord) recordList.get(0);
               System.out.println("1 retrieved record = " + record);
           
               heightTable.delete("Where lid = 'CHIP1'");
               heightTable.delete("Where lid = 'CHIP3'");
           
               record.setLid("CHIP1");
           
               heightTable.insert(record);
               
           
               recordList = heightTable.select("where lid = 'CHIP1'" );
               record = (TestHeightRecord) recordList.get(0);
           
               System.out.println("2 retrieved record = " + record);
           
               TestHeightRecord newRecord = new TestHeightRecord(record);
               
               System.out.println("3 after copy record orig = " + record);
               System.out.println("4 after copy record new = " + newRecord);
           
               newRecord.setLid("CHIP3");
               newRecord.setNullDur(true);
           
               heightTable.update(record, newRecord );
                 
               recordList = heightTable.select("where lid = '" + newRecord.getLid() + "'" );
               record = (TestHeightRecord) recordList.get(0);
           
               System.out.println("5 retrieved updated record with null Dur = " + record);
           
               // insertOrUpdate Test
               newRecord.setLid("Chip4");
               heightTable.insertOrUpdate(newRecord);
                 
               recordList = heightTable.select("where lid = '" + newRecord.getLid() + "'" );
               record = (TestHeightRecord) recordList.get(0);
           
               System.out.println("6 retrieved insertedOrUpdated record = " + record);
            
               
                   timer.start();
                   Measurement height2Measurement = 
                                          _dataMgr.getHeightMeasurementWithShortCircuit(_locationId);
                   timer.stop("AppController.runModel(): Short Circuited Height retrieval took");
 
   
                   timer.start();
                   heightMeasurement = _dataMgr.getHeightMeasurement(_locationId);
                   timer.stop("AppController.runModel(): Normal Height retrieval took");
                   heightMeasurement.convert(MeasuringUnit.feet);      
               
                        
            
            }
        
        }
        catch (SQLException e)
        {
             logSQLException(e);
        }

    }
   */
//	-------------------------------------------------------		
    public void testGeneratedCode()
    {
        HeightTable heightTable = new HeightTable(_db);
        HeightRecord record = null;
        List recordList = null;
        CodeTimer timer = new CodeTimer();
        
        try
        {
        	timer.start();
        	recordList = heightTable.select("where lid = 'BLUO2' order by obstime DESC ");
        	timer.stop("after Select from Height Table");
        	
        	if (recordList.size() > 0)
        	{
        	   record = (HeightRecord) recordList.get(0);
			   System.out.println("1 retrieved record = " + record);
			   
			   heightTable.delete("Where lid = 'CHIP1'");
			   heightTable.delete("Where lid = 'CHIP3'");
			   
        	   record.setLid("CHIP1");
        	   
        	   heightTable.insert(record);
        	   
        	   recordList = heightTable.select("where lid = 'CHIP1'" );
			   record = (HeightRecord) recordList.get(0);
			   
			   System.out.println("2 retrieved record = " + record);
			   
			   HeightRecord newRecord = new HeightRecord(record);
			   System.out.println("3 after copy record orig = " + record);
			   System.out.println("4 after copy record new = " + newRecord);
			   
			   newRecord.setLid("CHIP3");
			   
			   heightTable.update(record, newRecord );
			   		 
			   recordList = heightTable.select("where lid = '" + newRecord.getLid() + "'" );
			   record = (HeightRecord) recordList.get(0);
			   
			   System.out.println("5 retrieved updated record = " + record);
			   
			   // insertOrUpdate Test
			   newRecord.setLid("Chip4");
			   heightTable.insertOrUpdate(newRecord);
			   		 
			   recordList = heightTable.select("where lid = '" + newRecord.getLid() + "'" );
			   record = (HeightRecord) recordList.get(0);
			   
			   System.out.println("6 retrieved insertedOrUpdated record = " + record);
			    
			   /*
			   timer.start();
			   Measurement height2Measurement = 
			 						  _dataMgr.getHeightMeasurementWithShortCircuit(_locationId);
			   timer.stop("AppController.runModel(): Short Circuited Height retrieval took");
 
   
			   timer.start();
			   heightMeasurement = _dataMgr.getHeightMeasurement(_locationId);
	  		   timer.stop("AppController.runModel(): Normal Height retrieval took");
   			   heightMeasurement.convert(MeasuringUnit.feet);	   
			   
			   **/		    
			    
        	}
        	
        }
        catch (SQLException e)
        {
        	 logSQLException(e);
        }
    
    }
//	-------------------------------------------------------	


// -------------------------------------------------------	
    private void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
                    exception.getErrorCode() +  " " +
                    exception.getMessage());
                         
        exception.printStackTrace(_logger.getPrintWriter());
        
        _logger.log("End of stack trace");
         
    }
    
//	-------------------------------------------------------		
	public SigRiverLevels loadSigRiverLevels(String locationId)
	{
		RiverstatTable table = new RiverstatTable(_db);
		RiverstatRecord record = null;
		
		SigRiverLevels levels = null;
		
		String whereClause = "WHERE lid = '" + locationId + "'";
	
		try
		{
			List recordList = table.select(whereClause);
			if (recordList.size() > 0)
			{
				record = (RiverstatRecord) recordList.get(0);
				
				String primaryPe = record.getPrimary_pe();
				if (primaryPe == null)
                {
                    primaryPe = "HG";   
                }
				
				double floodStage = record.getFs();
				double actionStage = record.getWstg();
				double floodFlow = record.getFq();
				double actionFlow = record.getAction_flow();
				
                levels = createSigRiverLevels(locationId, 
                            primaryPe,
                            floodStage, 
                            actionStage,
                            floodFlow,
                            actionFlow);
                
                loadFloodCategories(levels);
			}	
		}
		catch(SQLException e)
		{
			 logSQLException(e);
		}
		
		return levels;
	}

//	-------------------------------------------------------	
	private SigRiverLevels createSigRiverLevels(String locationId, String primaryPe,
	                                            double floodStage, double actionStage,
	                                            double floodFlow,  double actionFlow)
	{
	    RatingCurve ratingCurve = getRatingCurve(locationId);
	    SigRiverLevels levels = null;
	    
	    boolean isNullFloodStage = DbTable.isNull(floodStage);
	    boolean isNullActionStage = DbTable.isNull(actionStage);
	    boolean isNullFloodFlow = DbTable.isNull(floodFlow);
	    boolean isNullActionFlow = DbTable.isNull(actionFlow);

	    
	    floodStage = getStage(locationId, floodFlow, floodStage);
	    actionStage = getStage(locationId, actionFlow, actionStage);
	   
	    
	    levels = new SigRiverLevels(locationId, 
                primaryPe,
                floodStage, 
                actionStage,
                floodFlow,
                actionFlow);
	    
	    return levels;
	}
	
	
// ------------------------------------------------------- 
	private double getStage(String locationId, double flow, double stage)
	{
	    if ( (DbTable.isNull(stage)) && (! DbTable.isNull(flow)) )
	    {
	        RatingCurve ratingCurve = getRatingCurve(locationId);
	        stage = ratingCurve.getStageFromDischarge(flow);	        
	    }
	    
	    return stage;
	}
	
	// ------------------------------------------------------- 
	
	
    private void loadFloodCategories(SigRiverLevels levels)
    {
        FloodcatTable table = new FloodcatTable(_db);
        FloodcatRecord record = null;
          
        String whereClause = "WHERE lid = '" + levels.getLocationId() + "'";
    
        try
        {
            List recordList = table.select(whereClause);
            if (recordList.size() > 0)
            {
                record = (FloodcatRecord) recordList.get(0);
                
                levels.setMinorFloodFlow(record.getMinor_flow());
                levels.setModerateFloodFlow(record.getModerate_flow());
                levels.setMajorFloodFlow(record.getMajor_flow());
                
                
                double minorStage =  getStage(levels.getLocationId(), record.getMinor_flow(), record.getMinor_stage());
                double moderateStage =  getStage(levels.getLocationId(), record.getModerate_flow(), record.getModerate_stage());
                double majorStage =  getStage(levels.getLocationId(), record.getMajor_flow(), record.getMajor_stage());
                
                             
                levels.setMinorFloodStage(minorStage);
                levels.setModerateFloodStage(moderateStage);
                levels.setMajorFloodStage(majorStage);
            }   
        }
        catch(SQLException e)
        {
             logSQLException(e);
        }
        
        return;
    }

//  -------------------------------------------------------     
	public String[] loadPhysicalElementArray(char firstLetterOfPhysicalElement)
	{
	    String[] peArray = null;
	    
		ShefPeTable table = new ShefPeTable(_db);
		ShefPeRecord record = null;
		
		String primaryPe = null;
		
		String whereClause = "WHERE pe LIKE '" + 
		                     firstLetterOfPhysicalElement +
		                     "%' ORDER BY pe";
	
		try
		{
			List recordList = table.select(whereClause);
			if (recordList.size() > 0)
			{
			    for (int i = 0; i < recordList.size(); i++)
			    {
			        record = (ShefPeRecord) recordList.get(i);
			        if (i == 0)
			        {
			            peArray = new String[recordList.size()];   
			        }
			        peArray[i] = record.getPe();
			    }
			}	
		}
		catch(SQLException e)
		{
			 logSQLException(e);
		}
		
		return peArray;
	}

//  -------------------------------------------------------     
	public String loadPrimaryPe(String locationId)
	{
		RiverstatTable table = new RiverstatTable(_db);
		RiverstatRecord record = null;
		
		String primaryPe = null;
		
		String whereClause = "WHERE lid = '" + locationId + "'";
	
		try
		{
			List recordList = table.select(whereClause);
			if (recordList.size() > 0)
			{
				record = (RiverstatRecord) recordList.get(0);
				
				 primaryPe = record.getPrimary_pe();
			}	
		}
		catch(SQLException e)
		{
			 logSQLException(e);
		}
		
		return primaryPe;
	}

//  -------------------------------------------------------     
	
    public long getLatestObsTimeForMAPPreprocessor()
    {
        ArealObsTable table = new ArealObsTable(_db);
       
        ArealObsRecord record = null;
        List recordList = null;
        RegularTimeSeries ts = null;
        
        long latestPostingTime = 0;
    
        //String whereClause = "WHERE  product_id like 'SSHP%' " +
        //                       " ORDER BY postingtime DESC";
   
        String whereClause = "WHERE  product_id = 'SSHP_MAP' " +
        " ORDER BY obstime DESC";
        
        try
        {
            recordList = table.selectNRecords(whereClause, 1);    
   
            if (recordList.size() > 0)
            {
                  record = (ArealObsRecord ) recordList.get(0);
                 // latestPostingTime = record.getPostingtime();
                  latestPostingTime = record.getObstime();
            }
         
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return latestPostingTime;
    }


//  -----------------------------------------------------------------------------------  
    public long getLatestPostingTimeForMAPPreprocessor()
    {
        ArealObsTable table = new ArealObsTable(_db);
       
        ArealObsRecord record = null;
        List recordList = null;
        RegularTimeSeries ts = null;
        
        long latestPostingTime = 0;
    
        String whereClause = "WHERE  product_id like 'SSHP_MAP' " +
                              " ORDER BY postingtime DESC";
        
        try
        {
            recordList = table.selectNRecords(whereClause, 1);    
   
            if (recordList.size() > 0)
            {
                  record = (ArealObsRecord ) recordList.get(0);
                  latestPostingTime = record.getPostingtime();
             //     latestPostingTime = record.getObstime();
            }
         
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        
        return latestPostingTime;
    }


//  -----------------------------------------------------------------------------------  
    
   
    public RegularTimeSeries loadObsMapTimeSeriesFromDb(String areaId)
    {
        String header = "DataMgr.loadObsMapTimeSeriesFromDb(): ";
       
        MeasuringUnit precipUnit = MeasuringUnit.inches;
        ArealObsTable table = new ArealObsTable(_db);
        ArealObsRecord record = null;
        List recordList = null;
        RegularTimeSeries ts = null;
        
        String whereClause = "WHERE lid = '" + areaId + "' AND dur = 1001 AND pe='PP' AND ts = 'PM' " +
                           " ORDER BY obstime ASC";
       
        //  System.out.println(header + " where string = " + whereClause);
           
        long startTime = 0;
        long endTime = 0; 
        
        try
        {
            recordList = table.select(whereClause);    
       
      
            if (recordList.size() > 0)
            {
                record = (ArealObsRecord ) recordList.get(0);
                startTime = record.getObstime();
                
                record = (ArealObsRecord ) recordList.get(recordList.size() - 1); 
                endTime = record.getObstime();   
            }
            else
            {
                endTime = getLatestHourTime();
                startTime = endTime - (3 * 24 * _millisPerHour);
               
            }
           
           
            ts = new RegularTimeSeries(startTime, endTime, 1, precipUnit);
        
           
        //   System.out.println(header + " startTime = " + DbTimeHelper.getDateTimeStringFromLongTime(startTime) +
        //                       " endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
        
           
            
          // String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime); 
          //String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTime);
            
           for (int i = 0; i < recordList.size() ; i++)
           {
                
               record = (ArealObsRecord) recordList.get(i);
               double value = record.getValue();
               long time = record.getObstime();
               
               if  ((time >= startTime) && (time <= endTime))
               {            
                   Measurement m = new Measurement(value, precipUnit);
                
                   ts.setMeasurementByTime(m, time);
               }
               else if (time > endTime)
               {
                   break; //done, since we went past the last one that we want
               }
    
           } //end for
       
       } //end try
       
       catch(SQLException e)
       {
           logSQLException(e); 
         
           endTime = getLatestHourTime();
           startTime = endTime - (3 * 24 * _millisPerHour);
          
           ts = new RegularTimeSeries(startTime, endTime, 1, precipUnit);
       }
       
          
       return ts;
      
    }
//  ----------------------------------------------------------------------------------     
    
    public RegularTimeSeries loadObsMapTimeSeriesFromDb_old(String areaId)
    {
//      we don't use ProcValue anymore for this; we use ArealObsPrecip.
        String header = "DataMgr.loadObsMapTimeSeriesFromDb(): ";
       
        MeasuringUnit precipUnit = MeasuringUnit.inches;

        ProcValueTable table = new ProcValueTable(_db);
        ProcValueRecord record = null;
        List recordList = null;
        RegularTimeSeries ts = null;
        
        String whereClause = "WHERE lid = '" + areaId + "' AND dur = 1001 AND pe='PP' AND ts = 'PM' " +
                           " ORDER BY obstime ASC";
       
        //  System.out.println(header + " where string = " + whereClause);
           
        long startTime = 0;
        long endTime = 0; 
        
        try
        {
            recordList = table.select(whereClause);    
       
      
            if (recordList.size() > 0)
            {
                record = (ProcValueRecord ) recordList.get(0);
                startTime = record.getObstime();
                
                record = (ProcValueRecord ) recordList.get(recordList.size() - 1); 
                endTime = record.getObstime();   
            }
            else
            {
                endTime = getLatestHourTime();
                startTime = endTime - (3 * 24 * _millisPerHour);
               
            }
           
           
            ts = new RegularTimeSeries(startTime, endTime, 1, precipUnit);
        
           
        //   System.out.println(header + " startTime = " + DbTimeHelper.getDateTimeStringFromLongTime(startTime) +
        //                       " endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
        
           
            
          // String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime); 
          //String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTime);
            
           for (int i = 0; i < recordList.size() ; i++)
           {
                
               record = (ProcValueRecord) recordList.get(i);
               double value = record.getValue();
               long time = record.getObstime();
               
               if  ((time >= startTime) && (time <= endTime))
               {            
                   Measurement m = new Measurement(value, precipUnit);
                
                   ts.setMeasurementByTime(m, time);
               }
               else if (time > endTime)
               {
                   break; //done, since we went past the last one that we want
               }
    
           } //end for
       
       } //end try
       
       catch(SQLException e)
       {
           logSQLException(e); 
         
           endTime = getLatestHourTime();
           startTime = endTime - (3 * 24 * _millisPerHour);
          
           ts = new RegularTimeSeries(startTime, endTime, 1, precipUnit);
       }
       
          
       return ts;
      
    }
//  -------------------------------------------------------
   
//      -------------------------------------------------------  
    private RegularTimeSeries loadFcstMapTimeSeriesByTs(String areaId, long forecastStartTime, String typeSource, BooleanHolder successHolder)
    {
        String header = "DataMgr.loadFcstMapTimeSeriesByTs(): ";
   
        MeasuringUnit precipUnit = MeasuringUnit.inches;

       // System.out.println(header + "running"); 

        ArealFcstTable table = new ArealFcstTable(_db);
        ArealFcstRecord record = null;
        List recordList = null;
        RegularTimeSeries ts = null;
   
   
        String whereClause = "WHERE lid = '" + areaId +
                            "' AND dur = 1001 AND pe= 'PP'" +
                           " AND ts = '" + typeSource + "'" + 
                           " ORDER BY basistime DESC, validtime ASC"; 
   
     //  System.out.println(header + " where string = " + whereClause);
   
      
        long startTime = 0;
        long endTime = 0; 
        
//      initialize the fcst time series    
        startTime = forecastStartTime;
        endTime = startTime;
      
        ts = new RegularTimeSeries(startTime, endTime, 1, precipUnit);
        
        successHolder.setValue(false);

        try
        {
            recordList = table.select(whereClause);    
    
            for (int i = 0; i < recordList.size() ; i++)
            {
    
                record = (ArealFcstRecord) recordList.get(i);
                double value = record.getValue();
                long time = record.getValidtime();
   

                if  (time >= startTime)
                {            
                     Measurement m = new Measurement(value, precipUnit);
        
                     ts.setMeasurementByTime(m, time);
                     successHolder.setValue(true);
                }
                else if (time > endTime)
                {
                     break; //done, since we went past the last one that we want
                }
            } //end for
       
       } // end try

       catch(SQLException e)
       {
           logSQLException(e);
       }
  
       return ts;
 
         
    } // end loadFcstMapTimeSeriesByTs()
//  -------------------------------------------------------     
    public String[] loadPreferredFcstTypeSourceArray(String areaId, String physicalElement, int duration)
    {
        IngestFilterTable table = new IngestFilterTable(_db);
        
        IngestFilterRecord record = null;
        
        List recordList = null;
        
        String whereString = " WHERE lid = '" + areaId + 
                             "' AND pe = '" + physicalElement +
                             "' AND dur = " + duration + 
                             " AND ts LIKE 'F%'" + 
                             " ORDER by ts_rank ASC ";
    
        String[] tsArray = new String[0];
        
        try
        {
           recordList = table.select(whereString); 
           
           int lowestRank = 0;

           tsArray = new String[recordList.size()];
           for (int i = 0; i < recordList.size(); i++)
           {
               record = (IngestFilterRecord) recordList.get(i);
               
               tsArray[i] = record.getTs();
           }
           
        }
        catch (SQLException e)
        {
           logSQLException(e);    
        }  
        
        
        return tsArray;  
        
    } 

//  -------------------------------------------------------     
    public RegularTimeSeries loadFcstMapTimeSeries(String areaId, long forecastStartTime)
    {
        String header = "DataMgr.loadFcstMapTimeSeries(): ";
        
        RegularTimeSeries fcstPrecipTimeSeries = null;
        MeasuringUnit precipUnit = MeasuringUnit.inches;
        
        ArealFcstTable table = new ArealFcstTable(_db);
        ArealFcstRecord record = null;
        List recordList = null;
        BooleanHolder successHolder = new BooleanHolder(false);
        
        
        String pe = "PP";
        int dur = 1001;
        String[] preferredTypeSourceArray = loadPreferredFcstTypeSourceArray(areaId, pe, dur);
        boolean success = false;
        
        // in priority of the preferred sources, try to load the FCST MAP time series
        // stop after successfully loading the time series
        for (int i = 0; i < preferredTypeSourceArray.length && !success; i++)
        {
            String preferredTypeSource = preferredTypeSourceArray[i];
            RegularTimeSeries tempFcstPrecipTimeSeries = loadFcstMapTimeSeriesByTs(areaId, forecastStartTime, preferredTypeSource, successHolder);
            
            
            if (successHolder.getValue() == true)
            {
                fcstPrecipTimeSeries = tempFcstPrecipTimeSeries;
                // System.out.println("chosen preferredTypeSource = " + preferredTypeSource);
                success = true;
            }
            else
            {
                //System.out.println("NOT chosen preferredTypeSource = " + preferredTypeSource);
            }   
        }
        
        if (!success)
        {    
            fcstPrecipTimeSeries = new RegularTimeSeries(forecastStartTime, forecastStartTime, 1, precipUnit);
        }
        
        return fcstPrecipTimeSeries;
    }
//      -------------------------------------------------------  
    
    public RegularTimeSeries loadFileMapTimeSeries(String filePath)
    {
        String header = "DataMgr.loadFileMapTimeSeries(): ";
        
        TimeSeriesFileManager reader = new TimeSeriesFileManager();
        
        RegularTimeSeries precipTimeSeries = null;
        MeasuringUnit precipUnit = MeasuringUnit.inches;
        
        precipTimeSeries = reader.readRegularTimeSeries(filePath, MeasuringUnit.inches);
        
        return precipTimeSeries;
    }
//      -------------------------------------------------------  
    
    public RegularTimeSeries loadFcstMapTimeSeries_old(String areaId, long forecastStartTime)
    {
        String header = "DataMgr.loadFcstMapTimeSeries(): ";
   
        RegularTimeSeries fcstPrecipTimeSeries = null;
        MeasuringUnit precipUnit = MeasuringUnit.inches;

        FcstPrecipTable table = new FcstPrecipTable(_db);
        FcstPrecipRecord record = null;
        List recordList = null;
        BooleanHolder successHolder = new BooleanHolder(false);
       
       
        String pe = "PP";
        int dur = 1001;
        String[] preferredTypeSourceArray = loadPreferredFcstTypeSourceArray(areaId, pe, dur);
        boolean success = false;

        for (int i = 0; i < preferredTypeSourceArray.length; i++)
        {
            String preferredTypeSource = preferredTypeSourceArray[i];
            RegularTimeSeries tempFcstPrecipTimeSeries = loadFcstMapTimeSeriesByTs(areaId, forecastStartTime, preferredTypeSource, successHolder);
        

            if (successHolder.getValue() == true)
            {
                 fcstPrecipTimeSeries = tempFcstPrecipTimeSeries;
                // System.out.println("chosen preferredTypeSource = " + preferredTypeSource);
                 success = true;
                 break;
            }
            else
            {
                 //System.out.println("NOT chosen preferredTypeSource = " + preferredTypeSource);
            }   
        }

        if (!success)
        {    
             fcstPrecipTimeSeries = new RegularTimeSeries(forecastStartTime, forecastStartTime, 1, precipUnit);
        }
 
       return fcstPrecipTimeSeries;
    }
//      -------------------------------------------------------     
   
    
   
//      ------------------------------------------------------- 
 /*   public RegularTimeSeries loadMapTimeSeriesFromFiles(String areaId, long startTime, long endTime)
    {
             
           String header = "DataMgr.getMapTimeSeriesFromFiles(): ";
        
           _logger.log(header + " startTime = " + DbTimeHelper.getDateTimeStringFromLongTime(startTime) +
                           " endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
        
           CodeTimer timer = new CodeTimer(_logger);
        
           timer.start();
           MAP map = new MAP(_db, areaId, startTime, endTime);
           timer.stop("reading xmrg files for " + areaId + " took");
        
           RegularTimeSeries ts =  map.getMpeMapTimeSeries();
        
          
       
           return ts;
         
    }
*/

// ------------------------------------------------------- 
   public void saveMapTimeSeries_old(String areaId, RegularTimeSeries ts, Set timeSet, Logger localLogger)
   {
//     we don't use ProcValue anymore for this; we use ArealObsPrecip.
       String header = "DataMgr.saveMapTimeSeries(): ";
       
       ProcValueTable table = new ProcValueTable(_db);
       ProcValueRecord record = null;

       short durationCode = 1001; //one hour only
       String peCode = "PP";
       String extremumCode = "Z";
       String tsCode = "PM";

       //make sure record is in ingestfilter
       ProcTsDescriptor descriptor = new ProcTsDescriptor("ProcValue");
       descriptor.setLid(areaId);
       descriptor.setPe(peCode);
       descriptor.setDur(durationCode);      
       descriptor.setExtremum(extremumCode);
       descriptor.setTs(tsCode);
       
       saveToIngestFilterIfNeeded(descriptor);
    
    
       // initialize the constant record fields
       long productTime = System.currentTimeMillis();
 
       record = new ProcValueRecord();
       record.setLid(areaId);
       record.setQuality_code(_defaultQualityCode);
            
       record.setDur(durationCode); //one hour

       record.setPe(peCode);
       record.setTs(tsCode);
       record.setExtremum(extremumCode);
       
       record.setProduct_id("SSHP_T2");
       record.setProducttime(productTime);
       
       long postingTime = System.currentTimeMillis();
       record.setPostingtime(postingTime);
       
       record.setRevision((short)0);
       record.setShef_qual_code("Z");


       for (int i = 0; i < ts.getMeasurementCount(); i++)
       {
        
           Measurement m = ts.getMeasurementByIndex(i);
           long time = ts.getMeasurementTimeByIndex(i);
           
           if (timeSet.contains(new Long(time)))
           {
               double value = m.getValue(MeasuringUnit.inches);
                    
               record.setObstime(time);
               record.setValue(value);
          
               try
               {
                  if (value != MISSING)
                  {
                      if (value > 0.0)
                      {
                         localLogger.log(header + "record with precip = " + record);    
                      }
                      else
                      {
                         //localLogger.log(header + "record with no precip = " + record);
                      }
                      
                      //only save records that might be different from before
                      // see the outer if block
                      table.insertOrUpdate(record);
                   
                  }
                  else
                  {
                      /*
                      String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                      localLogger.log(header + "missing data for period ending at " +
                                            timeString + 
                                            ", so  not saving (relying on previously saved data)");  
                      */ 
                  }
              }
              catch (SQLException e)
              {
                  logSQLException(e);
              }
          }
       } //end for
   }
// ------------------------------------------------------- 
   public void saveMapTimeSeries(String areaId, RegularTimeSeries ts, Set timeSet, Logger localLogger)
   {
       String header = "DataMgr.saveMapTimeSeries(): ";
       
       ArealObsTable table = new ArealObsTable(_db);
       ArealObsRecord record = null;

       short durationCode = 1001; //one hour only
       String peCode = "PP";
       String extremumCode = "Z";
       String tsCode = "PM";

       //make sure record is in ingestfilter
       ProcTsDescriptor descriptor = new ProcTsDescriptor("ArealObsPrecip");
       descriptor.setLid(areaId);
       descriptor.setPe(peCode);
       descriptor.setDur(durationCode);      
       descriptor.setExtremum(extremumCode);
       descriptor.setTs(tsCode);
       
       saveToIngestFilterIfNeeded(descriptor);
    
    
       // initialize the constant record fields
       long productTime = System.currentTimeMillis();
 
       record = new ArealObsRecord();
       record.setLid(areaId);
       record.setQuality_code(_defaultQualityCode);
            
       record.setDur(durationCode); //one hour

       record.setPe(peCode);
       record.setTs(tsCode);
       record.setExtremum(extremumCode);
       
       record.setProduct_id("SSHP_T2");
       record.setProducttime(productTime);
       
       long postingTime = System.currentTimeMillis();
       record.setPostingtime(postingTime);
       
       record.setRevision((short)0);
       record.setShef_qual_code("Z");


       for (int i = 0; i < ts.getMeasurementCount(); i++)
       {
        
           Measurement m = ts.getMeasurementByIndex(i);
           long time = ts.getMeasurementTimeByIndex(i);
           
           if (timeSet.contains(new Long(time)))
           {
               double value = m.getValue(MeasuringUnit.inches);
                    
               record.setObstime(time);
               record.setValue(value);
          
               try
               {
                  if (value != MISSING)
                  {
                      if (value > 0.0)
                      {
                         localLogger.log(header + "record with precip = " + record);    
                      }
                      else
                      {
                         //localLogger.log(header + "record with no precip = " + record);
                      }
                      
                      //only save records that might be different from before
                      // see the outer if block
                      table.insertOrUpdate(record);
                   
                  }
                  else
                  {
                      /*
                      String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                      localLogger.log(header + "missing data for period ending at " +
                                            timeString + 
                                            ", so  not saving (relying on previously saved data)");  
                      */ 
                  }
              }
              catch (SQLException e)
              {
                  logSQLException(e);
              }
          }
       } //end for
   }

// ---------------------------------------------------------------------------------- 
   public void saveMapTimeSeries_old(String areaId, RegularTimeSeries ts,  Logger localLogger)
      {
//     we don't use ProcValue anymore for this; we use ArealObsPrecip.
          String header = "DataMgr.saveMapTimeSeries(): ";
       
          ProcValueTable table = new ProcValueTable(_db);
          ProcValueRecord record = null;

          short durationCode = 1001; //one hour only
          String peCode = "PP";
          String extremumCode = "Z";
          String tsCode = "PM";

          //make sure record is in ingestfilter

          ProcTsDescriptor descriptor = new ProcTsDescriptor("ProcValue");
          descriptor.setLid(areaId);
          descriptor.setPe(peCode);
          descriptor.setDur(durationCode);      
          descriptor.setExtremum(extremumCode);
          descriptor.setTs(tsCode);
          saveToIngestFilterIfNeeded(descriptor);
    
          long productTime = System.currentTimeMillis();
          
          // initial the record object
          record = new ProcValueRecord();
          record.setLid(areaId);
         
          record.setQuality_code(_defaultQualityCode);
        
          record.setDur(durationCode); //one hour

          record.setPe(peCode);
          record.setTs(tsCode);
          record.setExtremum(extremumCode);
   
          record.setProduct_id("SSHP_MAP");
          record.setProducttime(productTime);
   
          long postingTime = System.currentTimeMillis();
          record.setPostingtime(postingTime);
   
          record.setRevision((short)0);
          record.setShef_qual_code("Z");
   
          for (int i = 0; i < ts.getMeasurementCount(); i++)
          {
        
              Measurement m = ts.getMeasurementByIndex(i);
              long time = ts.getMeasurementTimeByIndex(i);
              double value = m.getValue(MeasuringUnit.inches);
        
              record.setObstime(time);
              record.setValue(value);
     
              try
              {
                 if (value != MISSING)
                 {
                     if (value > 0.0)
                     {
                        localLogger.log(header + "record with precip = " + record);    
                     }
                     else
                     {
                        //localLogger.log(header + "record with no precip = " + record);
                     }
                   
                     table.insertOrUpdate(record);
                 }
                 else
                 {
                     /*
                     String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                     localLogger.log(header + "missing data for period ending at " +
                                           timeString + 
                                           ", so  not saving (relying on previously saved data)");  
                     */ 
                 }
             }
             catch (SQLException e)
             {
                 logSQLException(e);
             }
          } //end for
      }
   
// ---------------------------------------------------------------------------------- 
   public void saveMapTimeSeriesToFile(RegularTimeSeries regularTimeSeries, String filePath)
   {
       _reader.saveRegularTimeSeries(regularTimeSeries, filePath);
       
       return;
   }
   
// ---------------------------------------------------------------------------------- 
   
    public void saveMapTimeSeries(String areaId, RegularTimeSeries ts,  Logger localLogger)
    {
       String header = "DataMgr.saveMapTimeSeries(): ";
    
       ArealObsTable table = new ArealObsTable(_db);
       ArealObsRecord record = null;

       short durationCode = 1001; //one hour only
       String peCode = "PP";
       String extremumCode = "Z";
       String tsCode = "PM";

       //make sure record is in ingestfilter

       ProcTsDescriptor descriptor = new ProcTsDescriptor("ArealObsPrecip");
       descriptor.setLid(areaId);
       descriptor.setPe(peCode);
       descriptor.setDur(durationCode);      
       descriptor.setExtremum(extremumCode);
       descriptor.setTs(tsCode);
       saveToIngestFilterIfNeeded(descriptor);
 
       long productTime = System.currentTimeMillis();
       
       // initial the record object
       record = new ArealObsRecord();
       record.setLid(areaId);
      
       record.setQuality_code(_defaultQualityCode);
     
       record.setDur(durationCode); //one hour

       record.setPe(peCode);
       record.setTs(tsCode);
       record.setExtremum(extremumCode);

       record.setProduct_id("SSHP_MAP");
       record.setProducttime(productTime);

       long postingTime = System.currentTimeMillis();
       record.setPostingtime(postingTime);

       record.setRevision((short)0);
       record.setShef_qual_code("Z");

       for (int i = 0; i < ts.getMeasurementCount(); i++)
       {
     
           Measurement m = ts.getMeasurementByIndex(i);
           long time = ts.getMeasurementTimeByIndex(i);
           double value = m.getValue(MeasuringUnit.inches);
     
           record.setObstime(time);
           record.setValue(value);
  
           try
           {
              if (value != MISSING)
              {
                  if (value > 0.0)
                  {
                     localLogger.log(header + "record with precip = " + record);    
                  }
                  else
                  {
                     //localLogger.log(header + "record with no precip = " + record);
                  }
                
                  table.insertOrUpdate(record);
              }
              else
              {
                  /*
                  String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                  localLogger.log(header + "missing data for period ending at " +
                                        timeString + 
                                        ", so  not saving (relying on previously saved data)");  
                  */ 
              }
          }
          catch (SQLException e)
          {
              logSQLException(e);
          }
       } //end for
   }
         
//  ------------------------------------------------------- 
    public void saveMapRowsToIngestFilterAsNeeded(List areaIdList)
    {
        saveMapRowsToIngestFilterAsNeeded(areaIdList, "PM");
    }
    
//  -------------------------------------------------------------------------------------------------   

    public void saveMapRowsToIngestFilterAsNeeded(List areaIdList, String typeSourceCode)
    {
        CodeTimer timer = new CodeTimer(_logger);
        timer.start();
        
        String areaId = null;
        
        short durationCode = 1001; //one hour only
        String peCode = "PP";
        String extremumCode = "Z";
        String tsCode = typeSourceCode;

                 //make sure record is in ingestfilter

        ProcTsDescriptor descriptor = new ProcTsDescriptor("ArealObsPrecip");
      
        descriptor.setPe(peCode);
        descriptor.setDur(durationCode);      
        descriptor.setExtremum(extremumCode);
        descriptor.setTs(tsCode);
        
        //select all the MAP rows to Check
        String whereString = "WHERE pe = '" + descriptor.getPe() + 
                             "' AND  dur = " + descriptor.getDur() + " AND extremum = '" +
                             descriptor.getExtremum() + "' AND ts = '" + descriptor.getTs() + "'";
        
        
        System.out.println("whereString = " + whereString);
        
        IngestFilterRecord record;
        IngestFilterTable table = new IngestFilterTable(_db);
        List recordList = null;
        HashSet filterSet = new HashSet();
        
        // select from the ingestfilter table and
        //add all of the lids to a hashset
        try
        {
            recordList = table.select(whereString);
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (IngestFilterRecord) recordList.get(i);
                filterSet.add(record.getLid());
                _logger.log("Adding " + record.getLid() + " to the filter set");
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        
        // for each areaId in the list,
        // check to see if it is in the hashset (which indicates it is already in the db)
        // if not, then add it to the db
        for (int i = 0; i < areaIdList.size(); i++)
        {
            areaId = (String) areaIdList.get(i);
            
            System.out.println("areaId to check = :" + areaId + ":");
            
            if (! filterSet.contains(areaId))
            {
                // add it to the db
                descriptor.setLid(areaId);
                saveToIngestFilter(descriptor);   
            }
        }
    
        timer.stop("DataMgr.saveMapRowsToIngestFilterAsNeeded for all ids took ");
                   
    } 
  
    
//  ------------------------------------------------------- 

    
    public void saveMapMeasurement(String areaId, Measurement mapMeasurement,
            long obsTime, Logger localLogger)
    {
        long productTime = System.currentTimeMillis();
        saveMapObsMeasurement(areaId, "PM", mapMeasurement, obsTime, productTime, localLogger);
        
        return;
    }
//  ------------------------------------------------------- 
  
    public void saveMapMeasurement(String areaId, String typeSource, Measurement mapMeasurement,
                                   long measurementTime, long basisTime, long productTime, Logger localLogger)
    {
         
        if (typeSource.charAt(0) == 'F' )
        {
            saveMapFcstMeasurement(areaId, typeSource, mapMeasurement, measurementTime, basisTime, productTime, localLogger);
        }
        else
        {
            saveMapObsMeasurement(areaId, typeSource, mapMeasurement, measurementTime, productTime, localLogger);
        }
           
    }     
//  ------------------------------------------------------------------------------------------------ 
 
    private void saveMapObsMeasurement(String areaId, String typeSource, Measurement mapMeasurement,
            long obsTime, long productTime, Logger localLogger)
    {
        
        final String productId = "SSHP_MAP";
        MeasuringUnit dbPrecipUnit = MeasuringUnit.inches;
        String header = "DataMgr.saveMapObsMeasurement(): ";

        ArealObsTable table = new ArealObsTable(_db);
        ArealObsRecord record = new ArealObsRecord();

        short durationCode = 1001; //one hour only
        String peCode = "PP";
        String extremumCode = "Z";
        String tsCode = typeSource;

//      make sure record is in ingestfilter

        double value = mapMeasurement.getValue(dbPrecipUnit);

        record.setLid(areaId);

        record.setObstime(obsTime);
        record.setValue(value);

        record.setQuality_code(_defaultQualityCode);

        record.setDur(durationCode); //one hour

        record.setPe(peCode);
        record.setTs(tsCode);
        record.setExtremum(extremumCode);

        record.setProduct_id(productId);
        record.setProducttime(productTime);

        long postingTime = System.currentTimeMillis();
        record.setPostingtime(postingTime);

        record.setRevision((short)0);
        record.setShef_qual_code("Z");


        try
        {
            if (value != MISSING)
            {
                if (value > 0.0)
                {
                    localLogger.log(header + "record with precip = " + record);    
                }
                else
                {
//                  localLogger.log(header + "record with no precip = " + record);
                }

                table.insertOrUpdate(record);

            }
            else
            {
                /*
String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
localLogger.log(header + "missing data for period ending at " +
                 timeString + 
                 ", so  not saving (relying on previously saved data)");  
                 */ 
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

    } //end saveMAPObsMeasurement
//  ------------------------------------------------------------------------------------------------ 
    private void saveMapFcstMeasurement(String areaId, String typeSource, Measurement mapMeasurement,
            long validTime, long basisTime, long productTime, Logger localLogger)
    {
        
        
       // saveMapFcstMeasurement(areaId, typeSource, mapMeasurement, measurementTime, basisTime, productTime, localLogger);
        
        final String productId = "SSHP_HPN";
        MeasuringUnit dbPrecipUnit = MeasuringUnit.inches;
        String header = "DataMgr.saveMapFcstMeasurement(): ";

        ArealFcstTable table = new ArealFcstTable(_db);
        ArealFcstRecord record = new ArealFcstRecord();

        short durationCode = 1001; //one hour only
        String peCode = "PP";
        String extremumCode = "Z";
        String tsCode = typeSource;

//      make sure record is in ingestfilter

   
        double value = mapMeasurement.getValue(dbPrecipUnit);

        record.setLid(areaId);

        record.setValidtime(validTime);
        record.setValue(value);

        record.setQuality_code(_defaultQualityCode);

        record.setDur(durationCode); //one hour

        record.setPe(peCode);
        record.setTs(tsCode);
        record.setExtremum(extremumCode);

        record.setProduct_id(productId);
        record.setProducttime(productTime);

        long postingTime = System.currentTimeMillis();
        record.setPostingtime(postingTime);
        
        record.setBasistime(basisTime);
        
        record.setRevision((short)0);
        record.setShef_qual_code("Z");
        record.setProbability(-1);
        
        


        try
        {
            if (value != MISSING)
            {
                if (value > 0.0)
                {
                    localLogger.log(header + "record with precip = " + record);    
                }
                else
                {
//                  localLogger.log(header + "record with no precip = " + record);
                }

                table.insertOrUpdate(record);

            }
            else
            {
                /*
String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
localLogger.log(header + "missing data for period ending at " +
                 timeString + 
                 ", so  not saving (relying on previously saved data)");  
                 */ 
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

    } //end saveMAPFcstMeasurement
//  ------------------------------------------------------------------------------------------------ 
    
// -------------------------------------------------------------------------------------------------    
 
    public AbsTimeMeasurement loadMostCurrentFfhMeasurement(String lid,
                                                            String basinId,
                                                            long endTime)
    {
        Measurement measurement = null;
        AbsTimeMeasurement ffhMeasurement = null;
        
        List ffhDescriptorList = loadFfhDescriptorList(lid, basinId);
        for (int i = 0; i < ffhDescriptorList.size(); i++)
        {
            FFHDescriptor descriptor = (FFHDescriptor) ffhDescriptorList.get(i);
            if (descriptor.getTime() <= endTime)
            {
                measurement = descriptor.getFfhMeasurement();
                ffhMeasurement = new AbsTimeMeasurement(measurement, descriptor.getTime());
                break;
            }
        }
    
        return ffhMeasurement;
    }
//     ------------------------------------------------------- 
/*
   public AbsTimeMeasurement loadMostCurrentFfhMeasurement(String lid, long endTime)
   {
       //String header = "DataMgr.getFfgValue(): ";
       
       AbsTimeMeasurement measurement = null;
       double ffhValue = -1.0;
       
       ContingencyValueTable table = new ContingencyValueTable(this._db);
       ContingencyValueRecord record = null;
    
       // Go to  UnitGraph table to get basin id
       String whereClause = null;
       // " WHERE lid='" + basinId + "' ";
     
       List recordList = null;
       
       // Go to ContingencyValue table to get the FFH
       //whereClause = " WHERE lid='" + lid + "'" +
       //          " AND dur=1001 AND pe='PP' AND ts IN ('CF','CP') " +
       //          " AND validtime > (TODAY - 5) " + 
       //          " ORDER BY validtime DESC, ts ";
                 
       whereClause = " WHERE lid='" + lid + "'" +
                        " AND dur=1001 AND pe='PP' AND ts IN ('CF','CP') " +
                        " ORDER BY validtime DESC, basistime DESC, ts ";
                       
		// System.out.println( "DataMgr.getFfhMeasurement.whereClause = " + whereClause );
        
       try
       {
          recordList = table.select(whereClause);
         
          // find the value that is the latest that is before or equal to the end time
          for (int i = 0; i < recordList.size(); i++)
          {
               record = (ContingencyValueRecord) recordList.get(i);
     		
               if (record.getValidtime() <= endTime)
               {
                    ffhValue = record.getValue();
                    long time = record.getValidtime();
                    measurement = new AbsTimeMeasurement(ffhValue, time, MeasuringUnit.inches);
                    break;  //don't look anymore, since the first one is the one I want
               }
          } //end for i

       }
       catch(SQLException e)
       {
            logSQLException(e);
       }
    
       
       return measurement;
    } // end of loadFFHMeasurement method
*/
// -------------------------------------------------------------------------
 
    public List loadFfhDescriptorList(String lid, String basinId)
    {
        String header = "DataMgr.loadFfhDescriptorList(): ";
   
        FFHDescriptor descriptor = null;
        AbsTimeMeasurement measurement = null;
        double ffhValue = -1.0;
        int durationCode = 0;
   
        ContingencyValueTable table = new ContingencyValueTable(this._db);
        ContingencyValueRecord record = null;

        String whereClause = null;
     
        List recordList = null;

        List descriptorList = new ArrayList();
   
              
        whereClause = " WHERE (lid='" + lid + "' OR lid = '" + basinId + "') " +
                        " AND dur in (1001, 1003, 1006) AND pe='PP' AND ts IN ('CF','CP') " +
                        " ORDER BY validtime DESC, basistime DESC, ts, dur ";
     
        long oldValidTime = 0;
        String oldTypeSource="";
     
        try
        {
            recordList = table.select(whereClause);
     
            // find the value that is the latest that is before or equal to the end time
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (ContingencyValueRecord) recordList.get(i);
                 
 //               System.out.println(header + " FFH record = " + record);
                
                long validTime = record.getValidtime();
                String typeSource = record.getTs();

                oldValidTime = validTime;    
                oldTypeSource = typeSource;

                ffhValue =  record.getValue();

                measurement = new AbsTimeMeasurement(ffhValue, validTime, MeasuringUnit.inches);

                durationCode = record.getDur();

                boolean isGridded = false;
                if ( record.getTs().toUpperCase().equals("CF")) //areal
                {
                    isGridded = false;
                }
                else //gridded
                {
                    isGridded = true;
                }

                descriptor = new FFHDescriptor(lid, measurement, durationCode, isGridded);
                descriptorList.add(descriptor);

            } //end for i

       }
       catch(SQLException e)
       {
            logSQLException(e);
       }
    
       
       return descriptorList;

    } // end of load FFHDescriptor Listmethod

  
//      -------------------------------------------------------     

    public void saveSacState( SacSmaState sacSmaState )
    {
        
        saveSacState(sacSmaState, sacSmaState.getSource());
        
        return;
    }

    //  -------------------------------------------------------     

    public void saveSacState( SacSmaState sacSmaState, String source)
    {
        SacSmaStateTable table = new SacSmaStateTable( _db );
        SacSmaStateRecord record = new SacSmaStateRecord();

        record.setBasin_id( sacSmaState.getBasinId() );
        record.setSource( source );
        record.setValidtime( sacSmaState.getValidTime() );
        record.setBasistime( sacSmaState.getBasisTime() );
        
        long currentTime = System.currentTimeMillis();
             
        record.setPostingtime( currentTime );
    
        record.setUztwc( sacSmaState.getUztwc() );
        record.setUzfwc( sacSmaState.getUzfwc() );
        record.setLztwc( sacSmaState.getLztwc() );
        record.setLzfsc( sacSmaState.getLzfsc() );
        record.setLzfpc( sacSmaState.getLzfpc() );
        record.setAdimc( sacSmaState.getAdimc() );

        try
        {       
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }   
    }
  
// ------------------------------------------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------------------    
    public List getPriorRunoffRecordList(String locationId, String typeSource, boolean forceCacheRefresh)
    {
        
        String key = locationId + '|' + typeSource;
        
        List list = (List) _priorRunoffRecordMap.get(key);
        if ((list == null) || (forceCacheRefresh) )
        {
            list = loadPriorRunoffRecordList(locationId, typeSource);
            _priorRunoffRecordMap.put(key, list);
        }
        
         return list;
    }
    
    
//  ------------------------------------------------------------------------------------------------------------------------------    

    public List loadPriorRunoffRecordList(String locationId, String typeSource)
    {
        
        String header = "DataMgr.loadPriorRunoffRecordList(): ";
        
        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;

        String typeSourceClause = null;
        
        if (typeSource == null)
        {
            typeSourceClause = "";
        }
        else
        {
            typeSourceClause = " AND ts = '" + typeSource + "'";    
        }

        
        String whereClause = "WHERE lid = '" + locationId + "'" +
        " AND pe = 'QB' AND dur = 1001" +
        typeSourceClause +
        " ORDER BY basistime DESC, validtime ASC";
        
        System.out.println(header + "whereClause = " + whereClause);
        
        try
        {
            recordList = table.select(whereClause);
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
        
        return recordList;
        
        
    }
    // -------------------------------------------------------------------------------------------------------------
    public void clearPriorRunoffTimeSeriesCache()
    {
        _priorRunoffTimeSeriesMap.clear();
    }
    
    // -------------------------------------------------------------------------------------------------------------

    
    public RegularTimeSeries getPriorRunoffTimeSeries(String locationId,
            String typeSource,
            long modelRunStartTime,
            long unitHydrographWholeDuration)
    {
        
        String header = "DataMgr.getPriorRunoffTimeSeries";
        
        String key = locationId + "|" + typeSource + "|" + DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime);
        RegularTimeSeries priorRunoffTimeSeries =  (RegularTimeSeries) _priorRunoffTimeSeriesMap.get(key);
        
        System.out.println(header + " key = " + key);
        
        if (priorRunoffTimeSeries == null)
        {
            priorRunoffTimeSeries = loadPriorRunoffTimeSeries(locationId, typeSource, modelRunStartTime, unitHydrographWholeDuration);
            
           // this would access a cache of all PriorRunoffTimeSeries records,
           // this is a problem when new records are being added while the user is runninng SSHP
           // priorRunoffTimeSeries = loadPriorRunoffTimeSeries2(locationId, typeSource, modelRunStartTime, unitHydrographWholeDuration);
            _priorRunoffTimeSeriesMap.put(key, priorRunoffTimeSeries);
        }
        
        return priorRunoffTimeSeries;
    }
    // -------------------------------------------------------------------------------------------------------------

    public RegularTimeSeries loadPriorRunoffTimeSeries2(String locationId,
            String typeSource,
            long modelRunStartTime,
            long unitHydrographWholeDuration)
    {
        final String header = "DataMgr.loadPriorRunoffTimeSeries2(): ";
        FcstDischargeRecord record = null;

    //    _priorRunoffCodeTimer.restart();

        List recordList = getPriorRunoffRecordList(locationId, typeSource, false);
        //      selectTimer.stop(header + " select() took ");

    //    _priorRunoffCodeTimer.stop("getPriorRunoffRecordList() has so far taken: ");


   
        String typeSourceClause = null;


        long initialTime = modelRunStartTime - (unitHydrographWholeDuration * _millisPerHour);
        String initialTimeString = DbTimeHelper.getDateTimeStringFromLongTime(initialTime);
        String modelRunStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime);



        RegularTimeSeries ts = null;

        long desiredBasisTime = 0; // use the most recent basistime returned from the db
        long startTime = modelRunStartTime - (unitHydrographWholeDuration * _millisPerHour);

        
        List subRecordList = getSubListForPriorRunoffTimeSeries(recordList,
                initialTime,
                modelRunStartTime);


        final MeasuringUnit tsRunoffUnit = MeasuringUnit.mm;
        final MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;


        if (subRecordList == null)
        {
            System.out.println(header + "subRecordList is null for " + locationId);
        }

        else if (subRecordList.size() > 0)
        {    

            record = (FcstDischargeRecord) subRecordList.get(0);
            //desiredBasisTime = record.getBasistime();
//          startTime = record.getValidtime();

            long endTime = modelRunStartTime;
            int intervalInHours = 1;

            ts = new RegularTimeSeries(startTime, endTime,
                    intervalInHours, tsRunoffUnit);

            //      System.out.println("******* " + header + "desiredBasisTime = " + DbTimeHelper.getDateTimeStringFromLongTime(desiredBasisTime));                           
        }

//      for the selected subRecordList, create a runoff time series



        boolean done = false;
        for (int i = 0 ; !done &&  (subRecordList != null) && (i < subRecordList.size()); i++)
        {                  
            record = (FcstDischargeRecord) subRecordList.get(i);

            if (record.getValidtime() > modelRunStartTime)
            {  
                done = true;
            }
            else
            {         
                double value = record.getValue();
                long validTime = record.getValidtime();
                Measurement m = new Measurement(value, dbRunoffUnit);

                //            System.out.print(header + " m = " + m);

                ts.setMeasurementByTime(m, validTime);        
            }
        } //end for i

 
        return ts;
    }

    // -------------------------------------------------------------------------------------------------------------
    
    
    public RegularTimeSeries loadPriorRunoffTimeSeriesNew(String locationId,
            String typeSource,
            long modelRunStartTime,
            long unitHydrographWholeDuration)
    {
        String header = "DataMgr.loadPriorRunoffTimeSeries(): ";

        System.out.println(header + "loading prior runoff time series for type source = " + typeSource);

        final MeasuringUnit tsRunoffUnit = MeasuringUnit.mm;
        final MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;

        String typeSourceClause = null;

        if (typeSource == null)
        {
            typeSourceClause = "";
        }
        else
        {
            typeSourceClause = " AND ts = '" + typeSource + "'";    
        }

        long initialTime = modelRunStartTime - (unitHydrographWholeDuration * _millisPerHour);
        String initialTimeString = DbTimeHelper.getDateTimeStringFromLongTime(initialTime);
        String modelRunStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime);

  //      System.out.println(header + "validtimes must be between " + initialTimeString + " and " + modelRunStartTimeString);

//      create the select where clause
        String whereClause = "WHERE lid = '" + locationId + "'" +
            " AND pe = 'QB' AND dur = 1001" +
            typeSourceClause + " AND " +
            " basistime = '" + modelRunStartTimeString + "' " +
            " ORDER BY basistime DESC, validtime ASC";

        System.out.println(header + "whereClause = " + whereClause);

        RegularTimeSeries ts = null;

        long desiredBasisTime = 0; // use the most recent basistime returned from the db
        long startTime = modelRunStartTime - 
        (unitHydrographWholeDuration * _millisPerHour);

        try
        {
            recordList = table.select(whereClause);

            List subRecordList = getSubListForPriorRunoffTimeSeries(recordList,
                                                                    initialTime,
                                                                    modelRunStartTime);


            if (subRecordList == null)
            {
                System.out.println(header + "subRecordList is null for " + locationId);
            }

            else if (subRecordList.size() > 0)
            {    

                record = (FcstDischargeRecord) subRecordList.get(0);
                desiredBasisTime = record.getBasistime();
//              startTime = record.getValidtime();

                long endTime = modelRunStartTime;
                int intervalInHours = 1;

                ts = new RegularTimeSeries(startTime, endTime,
                        intervalInHours, tsRunoffUnit);
            }

//          for the selected subRecordList, create a runoff time series

            boolean done = false;
            for (int i = 0 ; !done &&  (subRecordList != null) && (i < subRecordList.size()); i++)
            {                  
                record = (FcstDischargeRecord) subRecordList.get(i);

                if (record.getValidtime() > modelRunStartTime)
                {  
                    done = true;
                }
                else
                {         
                    double value = record.getValue();
                    long validTime = record.getValidtime();
                    Measurement m = new Measurement(value, dbRunoffUnit);

        //            System.out.print(header + " m = " + m);
                    
                    ts.setMeasurementByTime(m, validTime);        
                }
            } //end for i
              
        } //end try

        catch ( SQLException e )
        {
            logSQLException(e);
        }
        
        return ts;
    }

    // -------------------------------------------------------------------------------------------------------------
//  -----------------------------------------------------------------------------------------------------------------     
    public RegularTimeSeries loadPriorRunoffTimeSeries(String locationId,
                                                       String typeSource,
                                                       long modelRunStartTime,
                                                       long unitHydrographWholeDuration)
    {
        String header = "DataMgr.loadPriorRunoffTimeSeries(): ";

      //  System.out.println(header + "loading prior runoff time series for type source = " + typeSource);

        final MeasuringUnit tsRunoffUnit = MeasuringUnit.mm;
        final MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;

        String typeSourceClause = null;

        if (typeSource == null)
        {
            typeSourceClause = "";
        }
        else
        {
            typeSourceClause = " AND ts = '" + typeSource + "'";    
        }

        long initialTime = modelRunStartTime - (unitHydrographWholeDuration * _millisPerHour);
        String initialTimeString = DbTimeHelper.getDateTimeStringFromLongTime(initialTime);
        String modelRunStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime);

  //      System.out.println(header + "validtimes must be between " + initialTimeString + " and " + modelRunStartTimeString);


//      create the select where clause
        String whereClause = "WHERE lid = '" + locationId + "'" +
        " AND pe = 'QB' AND dur = 1001" +
        typeSourceClause + " AND " +
        " validtime >= '" + initialTimeString + "' " + " AND " +
        " validtime <= '" + modelRunStartTimeString + "' " +
        " ORDER BY basistime DESC, validtime ASC";

      //  System.out.println(header + "whereClause = " + whereClause);

        RegularTimeSeries ts = null;

        long desiredBasisTime = 0; // use the most recent basistime returned from the db
        long startTime = modelRunStartTime - 
        (unitHydrographWholeDuration * _millisPerHour);

        try
        {
            recordList = table.select(whereClause);

            List subRecordList = getSubListForPriorRunoffTimeSeries(recordList,
                    initialTime,
                    modelRunStartTime);


            if (subRecordList == null)
            {
                System.out.println(header + "subRecordList is null for " + locationId);
            }

            else if (subRecordList.size() > 0)
            {    

                record = (FcstDischargeRecord) subRecordList.get(0);
                desiredBasisTime = record.getBasistime();
//              startTime = record.getValidtime();

                long endTime = modelRunStartTime;
                int intervalInHours = 1;

                ts = new RegularTimeSeries(startTime, endTime,
                        intervalInHours, tsRunoffUnit);
            }

//          for the selected subRecordList, create a runoff time series

            boolean done = false;
            for (int i = 0 ; !done &&  (subRecordList != null) && (i < subRecordList.size()); i++)
            {                  
                record = (FcstDischargeRecord) subRecordList.get(i);

                if (record.getValidtime() > modelRunStartTime)
                {  
                    done = true;
                }
                else
                {         
                    double value = record.getValue();
                    long validTime = record.getValidtime();
                    Measurement m = new Measurement(value, dbRunoffUnit);

        //            System.out.print(header + " m = " + m);
                    
                    ts.setMeasurementByTime(m, validTime);        
                }
            } //end for i
              
        } //end try

        catch ( SQLException e )
        {
            logSQLException(e);
        }
        
        return ts;
    }

    // -------------------------------------------------------------------------------------------------------------
    public RegularTimeSeries loadPriorRunoffTimeSeriesOld(String locationId,
                                                       String typeSource,
                                                       long modelRunStartTime,
                                                       long unitHydrographWholeDuration)
    {
        
        
        
        
        String header = "DataMgr.loadPriorRunoffTimeSeries(): ";
        
        CodeTimer overallTimer = new CodeTimer();
        CodeTimer selectTimer = new CodeTimer();
        CodeTimer sublistTimer = new CodeTimer();
        
        overallTimer.start();
       // System.out.println(header + "loading prior runoff time series");

        final MeasuringUnit tsRunoffUnit = MeasuringUnit.mm;
        final MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;
        
        String typeSourceClause = null;
        
        if (typeSource == null)
        {
            typeSourceClause = "";
        }
        else
        {
            typeSourceClause = " AND ts = '" + typeSource + "'";    
        }
 
        long initialTime = modelRunStartTime - (unitHydrographWholeDuration * _millisPerHour);
        String initialTimeString = DbTimeHelper.getDateTimeStringFromLongTime(initialTime);
        String modelRunStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime);
        
        //create the select where clause
        String whereClause = "WHERE lid = '" + locationId + "'" +
                             " AND pe = 'QB' AND dur = 1001" +
                             typeSourceClause + 
                             " ORDER BY basistime DESC, validtime ASC";
                
        RegularTimeSeries ts = null;
         
        long desiredBasisTime = 0; // use the most recent basistime returned from the db
        long startTime = modelRunStartTime - 
                         (unitHydrographWholeDuration * _millisPerHour);

        try
        {
            
            selectTimer.start();
            recordList = table.select(whereClause);
            selectTimer.stop(header + " select() took ");
            
            
            System.out.println(header + "validtimes must be between " + initialTimeString + " and " + modelRunStartTimeString);
            
            sublistTimer.start();
            
            List subRecordList = getSubListForPriorRunoffTimeSeries(recordList,
                                                               initialTime,
                                                               modelRunStartTime);
            sublistTimer.stop(header + " getSubListForPriorRunoffTimeSeries() took ");
            
            if (subRecordList == null)
            {
                System.out.println(header + "subRecordList is null for " + locationId);
            }
            
            else if (subRecordList.size() > 0)
            {    
             
                record = (FcstDischargeRecord) subRecordList.get(0);
                desiredBasisTime = record.getBasistime();
                startTime = record.getValidtime();
                
                long endTime = modelRunStartTime;
                int intervalInHours = 1;
                
                ts = new RegularTimeSeries(startTime, endTime,
                                           intervalInHours, tsRunoffUnit);
                                           
                //System.out.println(header + "record count = " + recordList.size());                           
            }

            // for the selected subRecordList, create a runoff time series

            boolean done = false;
            for (int i = 0 ; !done &&  (subRecordList != null) && (i < subRecordList.size()); i++)
            {                  
                record = (FcstDischargeRecord) subRecordList.get(i);

                if (record.getValidtime() > modelRunStartTime)
                {  
                     done = true;
                }
                else
                {         
                    double value = record.getValue();
                    long validTime = record.getValidtime();
                    Measurement m = new Measurement(value, dbRunoffUnit);
                     
                    ts.setMeasurementByTime(m, validTime);        
                }
            } //end for i
        } //end try

        catch ( SQLException e )
        {
            logSQLException(e);
        }
        
        
        overallTimer.stop(header + "overall, took ");
        return ts;
    }
    
    //  -------------------------------------------------------------------------------

    private List createListOfFcstDischargeRecordLists( List <FcstDischargeRecord> recordList)
    {
         String header = "DataMgr.createListOfFcstDischargeRecordLists()";
         List listOfLists = new ArrayList();
         List subRecordList = null;
         
         long previousBasisTime = -1;
         
         for (FcstDischargeRecord record: recordList)
         {
             //if the record is the first one of its basistime, then create a new sublist and add it to the listOfLists
             if (record.getBasistime() != previousBasisTime )
             {
                 subRecordList = new ArrayList();
                 listOfLists.add(subRecordList);
                 previousBasisTime = record.getBasistime();
                 //System.out.println(header + "basisTime = " + DbTimeHelper.getDateTimeStringFromLongTime(record.getBasistime()));
             }
             
             //add the record to the current subRecordList,
             //which itself has already been added to the listOfLists
             subRecordList.add(record);
             
         }
         
        // System.out.println(header + "listOfLists has " +listOfLists.size() + " sublists.");
         
         return listOfLists;
    }   
    
    //  ------------------------------------------------------------------------------- 
    
    private List<FcstDischargeRecord> getSubListForPriorRunoffTimeSeries(List<FcstDischargeRecord> recordList,
                                                                         long initialTime,
                                                                         long endTime)
    {
        String header = "DataMgr.getSubListForPriorRunoffTimeSeries(): ";
        //This method assumes:
        //recordList is ordered by basistime DESC (most recent basistime first), validtime ASC (oldest first)
        //
        List subList = null;
        
        
        //put all time series into a List of Lists
        List<List<FcstDischargeRecord>> listOfFcstDischargeRecordLists = createListOfFcstDischargeRecordLists(recordList);
        
        
        //iterate through the list of lists until you find the subList that includes a validtime
        // set that overlaps the time from initialTime to endTime (inclusive)
        //Take that sublist and quit
        
        for (List<FcstDischargeRecord> subRecordList : listOfFcstDischargeRecordLists)
        {
            int lastIndex = subRecordList.size() - 1;
            if (lastIndex > -1)
            {
                FcstDischargeRecord firstRecord = subRecordList.get(0);
                FcstDischargeRecord lastRecord = subRecordList.get(lastIndex);

                String time1String = DbTimeHelper.getDateTimeStringFromLongTime(firstRecord.getValidtime());
                String time2String = DbTimeHelper.getDateTimeStringFromLongTime(lastRecord.getValidtime());

           //     System.out.println(header + "subRecordList's validtime 1 = " + time1String +  "  validtime 2 = " + time2String);

                if (
                        (firstRecord.getValidtime() <= initialTime)  &&
                        (lastRecord.getValidtime()  >= endTime) 
                )
                {
                    subList = subRecordList;
                    break;
                }
            }

        }
        
        return subList;
    }

    //	-------------------------------------------------------------------------------	
 /*   public RegularTimeSeries oldLoadPriorRunoffTimeSeries(String locationId,
            long modelRunStartTime,
            long unitHydrographWholeDuration)
    {
//      String header = "DataMgr.getPriorRunoffTimeSeries(): ";
//      System.out.println(header + "loading prior runoff time series");

        MeasuringUnit tsRunoffUnit = MeasuringUnit.mm;
        MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

        FcstDischargeTable table = new FcstDischargeTable(_db);
        FcstDischargeRecord record = null;
        List recordList = null;

//      NOTE:   We may at some point have to select based on the Type/source whose
//      prior runoff is favored
        String whereClause = "WHERE lid = '" + locationId + "'" +
        " AND pe = 'QB' AND dur = 1001 " + 
        " ORDER BY basistime DESC, validtime ASC";

        RegularTimeSeries ts = null;

        long desiredBasisTime = 0; // use the most recent basistime in the db
        long startTime = modelRunStartTime - 
        (unitHydrographWholeDuration * _millisPerHour);

        try
        {
            recordList = table.select(whereClause);

            if (recordList.size() > 0)
            {
                record = (FcstDischargeRecord) recordList.get(0);
                desiredBasisTime = record.getBasistime();
                startTime = record.getValidtime();

                long endTime = modelRunStartTime;
                int intervalInHours = 1;

                ts = new RegularTimeSeries(startTime, endTime,
                        intervalInHours, tsRunoffUnit);

//              System.out.println(header + "record count = " + recordList.size());                           
            }

//          for the desiredBasisTime, create a runoff time series

            boolean done = false;
            for (int i = 0 ; !done &&  (i < recordList.size()); i++)
            {
//              System.out.println(header + "i = " + i);                           
                record = (FcstDischargeRecord) recordList.get(i);

                if (record.getBasistime() != desiredBasisTime)
                {
//                  System.out.println(header + "part1");    
//                  we are done, since the desiredBasisTime comes first in the 
//                  list
                    done = true;
                }
                else if (record.getValidtime() > modelRunStartTime)
                {
//                  System.out.println(header + "part2, validtime = " + 
//                  DbTimeHelper.getDateTimeStringFromLongTime(record.getValidtime()) + 
//                  " modelRunStartTime = " + DbTimeHelper.getDateTimeStringFromLongTime(modelRunStartTime));
//                  we only want PRIOR runoff to the model StartTime, so
//                  stop now
                    done = true;
                }
                else
                {
//                  System.out.println(header + "part3");
                    double value = record.getValue();
                    long validTime = record.getValidtime();
                    Measurement m = new Measurement(value, dbRunoffUnit);

                    ts.setMeasurementByTime(m, validTime);  

//                  System.out.println(header + "measurement = " + m);
                }
            } //end for i
        } //end try

        catch ( SQLException e )
        {
            logSQLException(e);
        }

        return ts;
    }
*/
//  ------------------------------------------------------------------------------- 
    public String[] loadPreferredObsTypeSourceArray(String areaId,
                                                    String physicalElement,
                                                    int duration)
    {
        IngestFilterTable table = new IngestFilterTable(_db);
    
        IngestFilterRecord record = null;
    
        List recordList = null;
    
        String whereString = " WHERE lid = '" + areaId + "'" + " AND pe = '"
                + physicalElement + "'" + " AND dur = " + duration
                + " AND ts LIKE 'R%'" + " ORDER by ts_rank ASC ";
    
        String[] tsArray = new String[0];
    
        try
        {
            recordList = table.select(whereString);
    
            int lowestRank = 0;
    
            tsArray = new String[recordList.size()];
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (IngestFilterRecord) recordList.get(i);
    
                tsArray[i] = record.getTs();
            }
    
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
    
        return tsArray;
    }  
//  ------------------------------------------------------- 
    public RegularTimeSeries loadPotentialEvaporationTimeSeries(String basinId,
                                                               long modelRunStartTime,
                                                               long modelRunEndTime)
    {
        RegularTimeSeries ts = null;
        MeasuringUnit tsEvapUnit = MeasuringUnit.mm;
        MeasuringUnit dbEvapUnit = MeasuringUnit.inches;
        
        FcstOtherTable table = new FcstOtherTable(_db);
        FcstOtherRecord record = null;
        List recordList = null;
        
        long desiredBasisTime = 0; // use the most recent basistime in the db
        long startTime = modelRunStartTime;
        long endTime = modelRunEndTime;

        int intervalInHours = 1;
        ts = new RegularTimeSeries(startTime, endTime,
                                   intervalInHours, tsEvapUnit);
        
        String whereClause =  " WHERE lid = '" + basinId + "' AND " +
                              "pe = 'EA' AND dur = 1001 ORDER BY basistime DESC, validtime ASC";   
   
   
        try
        {
            recordList =  table.select(whereClause);
            
            if (recordList.size() > 0)
            {
                record = (FcstOtherRecord) recordList.get(0);
                desiredBasisTime = record.getBasistime();
            }
            
            boolean done = false;
            for (int i = 0 ; !done &&  (i < recordList.size()); i++)
            {
                record = (FcstOtherRecord) recordList.get(i);

                if (record.getBasistime() != desiredBasisTime)
                {
                    // we are done, since the desiredBasisTime comes first in the 
                    // list
                    done = true;
                }
                else if (record.getValidtime() > modelRunEndTime)
                {
                    //we are done, since we don't need the data past this time
                    done = true;
                }
                else if (record.getValidtime() >= modelRunStartTime)
                {
                    double value = record.getValue();
                    long validTime = record.getValidtime();
                    Measurement m = new Measurement(value, dbEvapUnit);
                     
                    ts.setMeasurementByTime(m, validTime);  
                }
                //else, the data comes from before the modelRunStartTime and we are
                // not interested.
            
         } //end for i
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
         
        return ts;    
        
    }
    
//  ---------------------------------------------------------------------------------------    

    public void saveRunoffTimeSeries(String locationId, RegularTimeSeries runoffTimeSeries)
    {
          saveRunoffTimeSeries(locationId, "SSHPRUNOFF", runoffTimeSeries);
    } //end saveRunoffTimeSeries (2 arg version)


//  ---------------------------------------------------------------------------------------
    public void saveRunoffTimeSeries(String locationId, String productId, RegularTimeSeries runoffTimeSeries)
    {
        long basisTime = System.currentTimeMillis();  
        String typeSource = "FZ";
        saveRunoffTimeSeries(locationId, typeSource,  basisTime, productId, runoffTimeSeries);
        
        return;
    } //end saveRunoffTimeSeries (3 arg version)
//  ----------------------------------------------------------------------------------------     
    public void saveRunoffTimeSeries(String locationId, String typeSource, long basisTime,
                                     String productId, RegularTimeSeries runoffTimeSeries)
    {
        
        String header = "DataMgr.saveRunoffTimeSeries(): ";
        
        String basisTimeString = DbTimeHelper.getDateTimeStringFromLongTime(basisTime);
        System.out.println(header + " basisTime = " + basisTimeString);
        
        MeasuringUnit dbRunoffUnit = MeasuringUnit.inches;

        FcstDischargeTable table = new FcstDischargeTable(_db);  
        FcstDischargeRecord record = null;
         
        int length = runoffTimeSeries.getMeasurementCount();
        short durationCode = 1001;
        record = new FcstDischargeRecord();
        record.setLid(locationId);
        record.setPe("QB");
          
        long currentTime = System.currentTimeMillis();                
        record.setBasistime(basisTime);
        record.setProducttime(basisTime);
        record.setPostingtime(currentTime);

        record.setTs(typeSource);
        record.setDur(durationCode);
        record.setExtremum("Z");
        record.setProbability(-1.0f);
        record.setProduct_id(productId);
        record.setShef_qual_code("");

        FcstTsDescriptor descriptor = new FcstTsDescriptor("FcstDischarge");
        descriptor.setLid(record.getLid());
        descriptor.setPe(record.getPe());
        descriptor.setTs(record.getTs());
        descriptor.setDur(record.getDur());
        descriptor.setExtremum(record.getExtremum());  
        
        saveToIngestFilterIfNeeded(descriptor);      
    
      
      //  System.out.println(header + " runoff time series length = " + length);
        
        for (int i = 0; i < length; i++)
        {
            Measurement measurement = runoffTimeSeries.getMeasurementByIndex(i);
            long time = runoffTimeSeries.getMeasurementTimeByIndex(i);
         
            //make sure my units are correct
            measurement = measurement.getCopy(dbRunoffUnit);

            
            double value = measurement.getValue();
          
           /* 
            if (value > 0.0)
            {
                System.out.println(header + " value = " + value + " time = " + DbTimeHelper.getDateTimeStringFromLongTime(time));
            }
           */
            
            record.setValidtime(time);
            record.setValue(value);

            
            try
            {
                table.insertOrUpdate(record);
            }
            catch (SQLException e)
            {
                e.printStackTrace();
                _logger.log("Failed to save record = " + record );
                logSQLException(e);
            }    
            
            
        }

    } //end saveRunoffTimeSeries (6 arg version)
    
//  -------------------------------------------------------     
    
    public void deleteLaterSacSmaVarStatesAndRunoff(String lid, String basinId, long validTime) throws SQLException
    {
        deleteLaterRunoff(lid, SSHPSource.UNADJUSTED_VAR, validTime);
        deleteLaterRunoff(lid, SSHPSource.ADJUSTED_VAR, validTime);
        
        deleteLaterSacStates(basinId, SSHPSource.UNADJUSTED_VAR, validTime);
        deleteLaterSacStates(basinId, SSHPSource.ADJUSTED_VAR, validTime);
    }
    
    private void deleteLaterRunoff(String lid, SSHPSource source, long validTime) throws SQLException
    {
        FcstDischargeTable table = new FcstDischargeTable(_db);
        
        String whereString = " WHERE lid = '" + lid + "' AND pe = 'QB' AND ts = '" + source.getTypeSource() + "' AND validtime >" +
        "'" + DbTimeHelper.getDateTimeStringFromLongTime(validTime) + "'";
        
        table.delete(whereString);
        
    }
    
    private void deleteLaterSacStates(String basinId, SSHPSource source, long validTime) throws SQLException
    {
        SacSmaStateTable table = new SacSmaStateTable(_db);
        
        String whereString = " WHERE basin_id = '" + basinId + "'  AND source = '" + source.getSacSmaSource() + "' AND validtime >" +
        "'" + DbTimeHelper.getDateTimeStringFromLongTime(validTime) + "'";
        
        table.delete(whereString);
        
    }
    
    
//  -------------------------------------------------------     
    public List loadSacSmaParamsList( String where )
    {
        List sacSmaParamsRecordList = null;
        SacSmaParamsTable table = new SacSmaParamsTable( _db );
        
        try 
        {     
            sacSmaParamsRecordList = table.select( where );     
        }
        catch( SQLException e )
        {
            logSQLException(e);
        }

        return getSacSmaParamsListFromSacSmaParamsRecordList( sacSmaParamsRecordList );
    }
    
//  -------------------------------------------------------     
    private SacSmaState getSacSmaStateFromRecord(SacSmaStateRecord record)
    {
       SacSmaState state = new SacSmaState();
       
       state.setBasinId(record.getBasin_id());
       
       state.setValidTime(record.getValidtime());
       state.setBasisTime(record.getBasistime());
       state.setPostingTime(record.getPostingtime());
       
       state.setSource(record.getSource());
       
       state.setAdimc(record.getAdimc());
       state.setLzfpc(record.getLzfpc());
       state.setLzfsc(record.getLzfsc());
       state.setLztwc(record.getLztwc());
   
  
       state.setUzfwc(record.getUzfwc());
       state.setUztwc(record.getUztwc());
 
       return state;
           
    }

    private List getSacSmaParamsListFromSacSmaParamsRecordList( List sacSmaParamsRecordList )
    {
        List sacSmaParamsList = new ArrayList();
        
        for( int i = 0; i < sacSmaParamsRecordList.size(); i++ )
        {
            SacSmaParameters addSacSmaParams = new SacSmaParameters();
            SacSmaParamsRecord sacSmaParamsRecord = (SacSmaParamsRecord) sacSmaParamsRecordList.get( i );
            
            addSacSmaParams.setBasinId(sacSmaParamsRecord.getBasin_id());
            addSacSmaParams.setSource(sacSmaParamsRecord.getSource());
            addSacSmaParams.setValidTime(sacSmaParamsRecord.getValidtime());
            addSacSmaParams.setPostingTime(sacSmaParamsRecord.getPostingtime());
            addSacSmaParams.setUztwm(sacSmaParamsRecord.getUztwm());
            addSacSmaParams.setUzfwm(sacSmaParamsRecord.getUzfwm());
            addSacSmaParams.setUzk(sacSmaParamsRecord.getUzk());
            addSacSmaParams.setPctim(sacSmaParamsRecord.getPctim());
            addSacSmaParams.setAdimp(sacSmaParamsRecord.getAdimp());
            addSacSmaParams.setRiva(sacSmaParamsRecord.getRiva());
            addSacSmaParams.setZperc(sacSmaParamsRecord.getZperc());
            addSacSmaParams.setRexp(sacSmaParamsRecord.getRexp());
            addSacSmaParams.setLztwm(sacSmaParamsRecord.getLztwm());
            addSacSmaParams.setLzfsm(sacSmaParamsRecord.getLzfsm());
            addSacSmaParams.setLzfpm(sacSmaParamsRecord.getLzfpm());
            addSacSmaParams.setLzsk(sacSmaParamsRecord.getLzsk());
            addSacSmaParams.setLzpk(sacSmaParamsRecord.getLzpk());
            addSacSmaParams.setPfree(sacSmaParamsRecord.getPfree());
            addSacSmaParams.setRserv(sacSmaParamsRecord.getRserv());
            addSacSmaParams.setSide(sacSmaParamsRecord.getSide());
            
            addSacSmaParams.setPeadj(sacSmaParamsRecord.getPeadj());
            addSacSmaParams.setPxadj(sacSmaParamsRecord.getPxadj());
            addSacSmaParams.setEfc(sacSmaParamsRecord.getEfc());
        
            sacSmaParamsList.add( addSacSmaParams );
        }
    
    return sacSmaParamsList;    
    }

//  ------------------------------------------------------- 
    public List getSacSmaStateDescriptorListByBasinId(String basinId)
    {
            return getSacSmaStateDescriptorListByBasinId(basinId, false);
    }
//  -------------------------------------------------------   

    public List getSacSmaStateDescriptorListByBasinId(String basinId, boolean forceCacheUpdate)
    {
        
        List descriptorList = null;
        if (forceCacheUpdate)
        {
            descriptorList = loadSacSmaStateDescriptorListByBasinId(basinId);
            _sacDescriptorListMap.put(basinId, descriptorList);
        }
        else //may use the cache
        {
            descriptorList =  (List) _sacDescriptorListMap.get(basinId);
        }
        
        
        if (descriptorList == null) 
        {
            descriptorList = loadSacSmaStateDescriptorListByBasinId(basinId);
            _sacDescriptorListMap.put(basinId, descriptorList);
        }
        
        return descriptorList;
    }
//  -------------------------------------------------------     
    public List loadSacSmaStateDescriptorListByBasinId(String basinId)
    {
        String whereString = "WHERE basin_id = '" + basinId + "' ORDER BY validtime desc, source";
        List stateList = loadSacSmaStateList(whereString);  
        
        List descriptorList = new ArrayList();
        
        for (int i = 0; i < stateList.size(); i++)
        {
            SacSmaState state = (SacSmaState) stateList.get(i);
            
            SacSmaStateDescriptor descriptor = new SacSmaStateDescriptor(state);
            descriptorList.add(descriptor);    
            
        }
        
        return descriptorList;  
        
    } 

    public List loadSacSmaStateList( String where )
    {
        List sacSmaStateRecordList = null;
        SacSmaStateTable table = new SacSmaStateTable( _db );
        
        try
        {
            sacSmaStateRecordList = table.select( where );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }
        return getSacSmaStateListFromSacSmaStateRecordList( sacSmaStateRecordList );
    }
    
  //  -------------------------------------------------------     

    
    private List getSacSmaStateListFromSacSmaStateRecordList( List sacSmaStateRecordList )
    {
        List sacSmaStateList = new ArrayList();
        
        if (sacSmaStateRecordList != null)
        {      
            for ( int i = 0; i < sacSmaStateRecordList.size(); i++ )
            {
                SacSmaState addSacSmaState = new SacSmaState(); 
                SacSmaStateRecord sacSmaStateRecord = (SacSmaStateRecord) sacSmaStateRecordList.get( i );
                
                addSacSmaState.setBasinId( sacSmaStateRecord.getBasin_id());
                addSacSmaState.setSource( sacSmaStateRecord.getSource());
                addSacSmaState.setValidTime( sacSmaStateRecord.getValidtime() );
                addSacSmaState.setBasisTime( sacSmaStateRecord.getBasistime() );
                addSacSmaState.setPostingTime( sacSmaStateRecord.getPostingtime() );
                addSacSmaState.setUztwc( sacSmaStateRecord.getUztwc() );            
                addSacSmaState.setUzfwc( sacSmaStateRecord.getUzfwc() );
                addSacSmaState.setLztwc( sacSmaStateRecord.getLztwc() );
                addSacSmaState.setLzfsc( sacSmaStateRecord.getLzfsc() );
                addSacSmaState.setLzfpc( sacSmaStateRecord.getLzfpc() );
                addSacSmaState.setAdimc( sacSmaStateRecord.getAdimc() );
                
                sacSmaStateList.add( addSacSmaState ); 
            }
        }
        
        return sacSmaStateList;
    }

    
//  ------------------------------------------------------- 
    private SacSmaParamsRecord getSacSmaParamsRecord(SacSmaParameters sacSmaParams)
    {
        SacSmaParamsRecord record = new SacSmaParamsRecord();
      
        record.setBasin_id( sacSmaParams.getBasinId() );
        record.setSource(sacSmaParams.getSource());
        record.setValidtime(sacSmaParams.getValidTime() );
        record.setPostingtime(sacSmaParams.getPostingTime());

        record.setUztwm(sacSmaParams.getUztwm());
        record.setUzfwm(sacSmaParams.getUzfwm());
        record.setUzk(sacSmaParams.getUzk());
        record.setPctim(sacSmaParams.getPctim());
        record.setAdimp(sacSmaParams.getAdimp());
        record.setRiva(sacSmaParams.getRiva());
        record.setZperc(sacSmaParams.getZperc());
        record.setRexp(sacSmaParams.getRexp());
        record.setLztwm(sacSmaParams.getLztwm());
        record.setLzfsm(sacSmaParams.getLzfsm());
        record.setLzfpm(sacSmaParams.getLzfpm());
        record.setLzsk(sacSmaParams.getLzsk());
        record.setLzpk(sacSmaParams.getLzpk());
        record.setPfree(sacSmaParams.getPfree());
        record.setRserv(sacSmaParams.getRserv());
        record.setSide(sacSmaParams.getSide());
        
       
        
        record.setPeadj(sacSmaParams.getPeadj());
        record.setPxadj(sacSmaParams.getPxadj());
        record.setEfc(sacSmaParams.getEfc());
        
        return record;
    }


//  ------------------------------------------------------- 


    public void saveParams( SacSmaParameters sacSmaParams )
    {
        SacSmaParamsTable table = new SacSmaParamsTable( _db );
        SacSmaParamsRecord record = getSacSmaParamsRecord(sacSmaParams);
        
        
        try
        {   
            table.insertOrUpdate( record ); 
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }   
    }   
//  -------------------------------------------------------     

    public void saveState( SacSmaState sacSmaState )
    {
        SacSmaStateTable table = new SacSmaStateTable( _db );
        SacSmaStateRecord record = new SacSmaStateRecord();

        record.setBasin_id( sacSmaState.getBasinId() );
        record.setSource( sacSmaState.getSource() );
        record.setValidtime( sacSmaState.getValidTime() );
        record.setBasistime( sacSmaState.getBasisTime() );
        record.setPostingtime( sacSmaState.getPostingTime() );
        
        record.setUztwc( sacSmaState.getUztwc() );
        record.setUzfwc( sacSmaState.getUzfwc() );
        record.setLztwc( sacSmaState.getLztwc() );
        record.setLzfsc( sacSmaState.getLzfsc() );
        record.setLzfpc( sacSmaState.getLzfpc() );
        record.setAdimc( sacSmaState.getAdimc() );

        try
        {       
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }   
    }

//  -------------------------------------------------------     
    public boolean saveFcstTimeSeries(FcstTsDescriptor descriptor,
                                      RegularTimeSeries timeSeries, 
                                      int hoursToSave )
    {
        
        boolean result = false;
        
        if (descriptor.getTableName().equalsIgnoreCase("FcstHeight"))
        {
            result = saveFcstHeightTs(descriptor, timeSeries, hoursToSave);    
        }
        else if (descriptor.getTableName().equalsIgnoreCase("FcstOther"))
        {
            result = saveFcstEvapTs(descriptor, timeSeries, hoursToSave);   
        }
        else if (descriptor.getTableName().equalsIgnoreCase("FcstDischarge"))
        {
            result = saveFcstDischargeTs(descriptor, timeSeries, hoursToSave);   
        }
        
        return result;
    }

//  -------------------------------------------------------     


    private boolean saveFcstEvapTs( FcstTsDescriptor descriptor, 
                                    RegularTimeSeries timeSeries,
                                    int hoursToSave)
    {
        MeasuringUnit dbUnit = MeasuringUnit.inches;
        boolean success = false;
       
        saveToIngestFilterIfNeeded(descriptor);
       
        FcstOtherTable table = new FcstOtherTable( _db );
        FcstOtherRecord record = new FcstOtherRecord();
       
        long startTime, endTime, interval;
        final long HOUR_IN_MILLIS = 60 * 60 * 1000;

        startTime = timeSeries.getStartTime();
        endTime = timeSeries.getEndTime();
        
        //adjust the endtime
        long possibleEndTime = startTime + ((hoursToSave-1)*HOUR_IN_MILLIS);
        if (possibleEndTime <= endTime)
        {
            endTime = possibleEndTime;
        }
        
        interval = timeSeries.getIntervalInHours() * HOUR_IN_MILLIS;
        
        record.setLid( descriptor.getLid() );
        record.setPe( descriptor.getPe() );
        record.setTs( descriptor.getTs() );
        record.setExtremum( descriptor.getExtremum() );
        record.setShef_qual_code( descriptor.getShef_qual_code() );
        record.setProduct_id( descriptor.getProduct_id() );
        record.setProbability( descriptor.getProbability() );
        record.setQuality_code( descriptor.getQuality_code() );
        record.setRevision( descriptor.getRevision() );
        record.setDur( descriptor.getDur() );
        record.setProducttime( descriptor.getProducttime() );
        record.setBasistime( descriptor.getBasistime() );
        record.setPostingtime( System.currentTimeMillis() );

        int recordCount = 0;
        int insertOrUpdateCount = 0;
        int failureCount = 0;
        int skippedCount = 0;

        for ( long time = startTime; time <= endTime; time += interval )
        {
            Measurement measurement = timeSeries.getMeasurementByTime( time );
        
            record.setValidtime( time );
            record.setValue( measurement.getValue(dbUnit));
   
            recordCount++;
   
            if ( record.getValue() >= 0.0)
            {   
        
                try
                {
                    table.insertOrUpdate( record );
                    insertOrUpdateCount++;
                  //System.out.println("count = " + count);
                }
                catch ( SQLException e )
                {
                    failureCount++;
                    logSQLException(e);
                    _logger.log("record # = " + recordCount);  
                }
             }
             else
             {
                 skippedCount++;
                 _logger.log("DataMgr.saveFcstHeight(): measurement = " + 
                                 measurement + 
                                 " record = " + record);     
             }     
        } //end for

      _logger.log("DataMgr.saveFcstHeight(): " + 
                      " recordCount = " + recordCount +
                      " insertOrUpdateCount = " + insertOrUpdateCount + 
                      " failureCount = " + failureCount +
                      " skippedCount = " + skippedCount);
     
      if ((insertOrUpdateCount + skippedCount) == recordCount)
      {
          success = true;
      }              
  
      return success; 
   
    }

//  ------------------------------------------------------- 
    
    /**
     * Publish data to the riverstatus table.
     */
    private void postMaxFcst(String locationId, String physicalElement,
            String typeSource) {
        String table = null;

        _logger.log("Updating RiverStatus for: " + locationId + " "
                + physicalElement);

        if (physicalElement.startsWith("H") || physicalElement.startsWith("h"))
            table = "FcstHeight";
        else
            table = "FcstDischarge";

        ForecastPoster fcstPoster = new ForecastPoster(_db, _logger);
        boolean status = fcstPoster.loadMaxFcstData(table, locationId,
                physicalElement);

        if (status)
            _logger.log("RiverStatus update complete !");
        else
            _logger.log("RiverStatus update FAILED !!!");
    }

    /*
    private void runLoadMaxFcst(String locationId, String physicalElement, String typeSource)
    {
//      get the database name and build the command line arguments for the
         // load max forecast program

         String cmdLineArgString = null;
         String dbName = _db.getDatabaseName();
         int returnCode = 0;

         // check if there is a value for db_name
         if (dbName != null)
         {
             if (dbName.length() != 0)
             {
                 cmdLineArgString = " -d" + dbName +
                           " -l" + locationId +
                           " -p" + physicalElement +
                           " -t" + typeSource;
             }

      
            String osSuffix = ".LX";
           
            
            EnvHelper envHelper = new EnvHelper();
            String dirString = envHelper.getProperty("WHFS_BIN_DIR"); 
            String programPath = dirString + "/load_maxfcst" + osSuffix;
    
            String commandString = programPath + " " + cmdLineArgString;
       

            try
            {
                _logger.log("executing " + commandString);
                Process process = Runtime.getRuntime().exec(commandString);
                process.waitFor();

                _logger.log("Completed execution of " + commandString);
    
                returnCode = process.exitValue();
            }
            catch(IOException e)
            {
                e.printStackTrace();
            }
        
            catch( InterruptedException e )
            {
                e.printStackTrace();
            }
   
            if (returnCode != 0)
            {
                System.out.println("returnCode from loadMaxFcst=" + returnCode);
            }

        }

    } //end runLoadMaxFcst
    */

 //  -------------------------------------------------------  
  

    private boolean saveFcstHeightTs( FcstTsDescriptor descriptor,
                                      RegularTimeSeries timeSeries,
                                      int hoursToSave )
    {
        boolean success = false;
        MeasuringUnit dbUnit = MeasuringUnit.feet;
        
        saveToIngestFilterIfNeeded(descriptor);
        
        FcstHeightTable table = new FcstHeightTable( _db );
        FcstHeightRecord record = new FcstHeightRecord();
        long startTime, endTime, interval;
        final long HOUR_IN_MILLIS = 60 * 60 * 1000;

        startTime = timeSeries.getStartTime();
        endTime = timeSeries.getEndTime();
        
        //adjust the endtime
        long possibleEndTime = startTime + ((hoursToSave-1)*HOUR_IN_MILLIS);
        if (possibleEndTime <= endTime)
        {
            endTime = possibleEndTime;
        }
        
        interval = timeSeries.getIntervalInHours() * HOUR_IN_MILLIS;
            

        record.setLid( descriptor.getLid() );
        record.setPe( descriptor.getPe() );
        record.setTs( descriptor.getTs() );
        record.setExtremum( descriptor.getExtremum() );
        record.setShef_qual_code( descriptor.getShef_qual_code() );
        record.setProduct_id( descriptor.getProduct_id() );
        record.setProbability( descriptor.getProbability() );
        record.setQuality_code( descriptor.getQuality_code() );
        record.setRevision( descriptor.getRevision() );
        record.setDur( descriptor.getDur() );
        record.setProducttime( descriptor.getProducttime() );
        record.setBasistime( descriptor.getBasistime() );
        record.setPostingtime( System.currentTimeMillis() );

        int recordCount = 0;
        int insertOrUpdateCount = 0;
        int failureCount = 0;
        int skippedCount = 0;
        
        

        for ( long time = startTime; time <= endTime; time += interval )
        {
            Measurement measurement = timeSeries.getMeasurementByTime( time );

            record.setValidtime( time );
            record.setValue( measurement.getValue(dbUnit) );
           
            recordCount++;
           
            if ( record.getValue() >= 0.0)
            {  
                
                try
                {
                    table.insertOrUpdate( record );
                    insertOrUpdateCount++;
                    //System.out.println("count = " + count);
                }
                catch ( SQLException e )
                {
                    failureCount++;
                    logSQLException(e);
                    _logger.log("failed record # = " + recordCount);  
                }
            }
            else
            {
                   skippedCount++;
                   //_logger.log("DataMgr.saveFcstHeight(): measurement = " + 
                  //                     measurement + 
                  //                     " record = " + record);     
            }     
        }
        
        _logger.log("DataMgr.saveFcstHeight(): " + 
                        " recordCount = " + recordCount +
                        " insertOrUpdateCount = " + insertOrUpdateCount + 
                        " failureCount = " + failureCount +
                        " skippedCount = " + skippedCount);
             
        if ((insertOrUpdateCount + skippedCount) == recordCount)
        {
            success = true;
        }              
        
        if (_shouldSaveCrestMeasurement)
        {
            saveHeightCrestMeasurement(table, record, startTime, endTime, timeSeries); 
        }

        postMaxFcst(descriptor.getLid(), descriptor.getPe(), descriptor.getTs());
          

        return success;
        
    }
    

//  -------------------------------------------------------    
 private boolean saveFcstDischargeTs( FcstTsDescriptor descriptor,
                                      RegularTimeSeries timeSeries,
                                      int hoursToSave)
    {
        boolean success = false;
        MeasuringUnit dbUnit = MeasuringUnit.cfs;

        
        saveToIngestFilterIfNeeded(descriptor);
        
        FcstDischargeTable table = new FcstDischargeTable( _db );
        FcstDischargeRecord record = new FcstDischargeRecord();
        long startTime, endTime, interval;
        final long HOUR_IN_MILLIS = 60 * 60 * 1000;

        startTime = timeSeries.getStartTime();
        endTime = timeSeries.getEndTime();
        
//      adjust the endtime
        long possibleEndTime = startTime + ((hoursToSave-1)*HOUR_IN_MILLIS);
        if (possibleEndTime <= endTime)
        {
            endTime = possibleEndTime;
        }
        
        interval = timeSeries.getIntervalInHours() * HOUR_IN_MILLIS;
            

        record.setLid( descriptor.getLid() );
        record.setPe( descriptor.getPe() );
        record.setTs( descriptor.getTs() );
        record.setExtremum( descriptor.getExtremum() );
        record.setShef_qual_code( descriptor.getShef_qual_code() );
        record.setProduct_id( descriptor.getProduct_id() );
        record.setProbability( descriptor.getProbability() );
        record.setQuality_code( descriptor.getQuality_code() );
        record.setRevision( descriptor.getRevision() );
        record.setDur( descriptor.getDur() );
        record.setProducttime( descriptor.getProducttime() );
        record.setBasistime( descriptor.getBasistime() );
        record.setPostingtime( System.currentTimeMillis() );

        int recordCount = 0;
        int insertOrUpdateCount = 0;
        int failureCount = 0;
        int skippedCount = 0;

        for ( long time = startTime; time <= endTime; time += interval )
        {
            Measurement measurement = timeSeries.getMeasurementByTime( time );

            record.setValidtime( time );
            record.setValue( measurement.getValue(dbUnit) );
           
            recordCount++;
           
            if ( record.getValue() >= 0.0)
            {  
                
                try
                {
                    table.insertOrUpdate( record );
                    insertOrUpdateCount++;
                    //System.out.println("count = " + count);
                }
                catch ( SQLException e )
                {
                    failureCount++;
                    logSQLException(e);
                    _logger.log("failed record # = " + recordCount);  
                }
            }
            else
            {
                   skippedCount++;
                  // _logger.log("DataMgr.saveFcstDischarge(): measurement = " + 
                 //                      measurement + 
                 //                      " record = " + record);     
            }     
        }
        
        _logger.log("DataMgr.saveFcstDischarge(): " + 
                        " recordCount = " + recordCount +
                        " insertOrUpdateCount = " + insertOrUpdateCount + 
                        " failureCount = " + failureCount +
                        " skippedCount = " + skippedCount);
             
        if ((insertOrUpdateCount + skippedCount) == recordCount)
        {
            success = true;
        }              

        if (_shouldSaveCrestMeasurement)
        {
            saveDischargeCrestMeasurement(table, record, startTime, endTime, timeSeries); 
        }

        postMaxFcst(descriptor.getLid(), descriptor.getPe(), descriptor.getTs());
    
          
        return success;
        
    }

//-------------------------------------------------------    
    private void saveHeightCrestMeasurement(FcstHeightTable table, FcstHeightRecord record, 
                                       long startTime,  long endTime,
                                       RegularTimeSeries timeSeries)
    {
        
        AbsTimeMeasurement crestMeasurement = timeSeries.getMaxMeasurement(startTime, endTime);
        record.setValue(crestMeasurement.getValue());
        record.setValidtime(crestMeasurement.getTime());
        record.setExtremum("X");
        
        FcstTsDescriptor descriptor = new FcstTsDescriptor("FcstHeight");
        descriptor.setLid(record.getLid());
        descriptor.setPe(record.getPe());
        descriptor.setTs(record.getTs());
        descriptor.setExtremum(record.getExtremum());
        descriptor.setDur(record.getDur());
        
        saveToIngestFilterIfNeeded(descriptor);
        
             
        try
        {
            table.insertOrUpdate(record);
        }
        catch(SQLException e)
        {
            logSQLException(e);
            _logger.log("failed to insert crest for FcstHeight record =  " + record);      
        }
        return;
    }

//  -------------------------------------------------------      
    private void saveDischargeCrestMeasurement(FcstDischargeTable table, FcstDischargeRecord record, 
                                            long startTime,  long endTime,
                                            RegularTimeSeries timeSeries)
    {

        AbsTimeMeasurement crestMeasurement = timeSeries.getMaxMeasurement(startTime, endTime);
        record.setValue(crestMeasurement.getValue());
        record.setValidtime(crestMeasurement.getTime());
        record.setExtremum("X");
        
        FcstTsDescriptor descriptor = new FcstTsDescriptor("FcstHeight");
        descriptor.setLid(record.getLid());
        descriptor.setPe(record.getPe());
        descriptor.setTs(record.getTs());
        descriptor.setExtremum(record.getExtremum());
        descriptor.setDur(record.getDur());
        
        saveToIngestFilterIfNeeded(descriptor);

        try
        {
            table.insertOrUpdate(record);
        }
        catch(SQLException e)
        {
            logSQLException(e);
            _logger.log("failed to insert crest for FcstDischarge record =  " + record);      
        }
        return;
    }

//-------------------------------------------------------      
    private void saveToIngestFilterIfNeeded(IngestFilterTsDescriptor descriptor)
    {
        String header = "DataMgr.saveToIngestFilterIfNeeded(): ";
       // System.out.println(header + "inside");
        
        IngestFilterTable table = new IngestFilterTable(_db);
    
        IngestFilterRecord record = new IngestFilterRecord();
     
        record.setLid( descriptor.getLid() );
        record.setPe( descriptor.getPe() );
        record.setTs( descriptor.getTs() );
        record.setExtremum( descriptor.getExtremum() );
        record.setDur( descriptor.getDur() );
 
        record.setTs_rank((short)1);
        record.setIngest("T");
        record.setOfs_input("F");
        record.setStg2_input("F");
    
        try
        {
            String whereString = record.getWhereString();
            int recordCount = table.selectCount(whereString);
        
             
            if (recordCount < 1)
            {
                System.out.println(header + " Attempting to insert" + record);
                
                table.insert(record);
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        
        }
    
        return;    
    }
    
//  -------------------------------------------------------    
    private void saveToIngestFilter(IngestFilterTsDescriptor descriptor)
    {
       // String header = "DataMgr.saveToIngestFilter(): ";
        
      //  System.out.println(header + "inside");
        
        IngestFilterTable table = new IngestFilterTable(_db);
    
        IngestFilterRecord record = new IngestFilterRecord();
     
        record.setLid( descriptor.getLid() );
        record.setPe( descriptor.getPe() );
        record.setTs( descriptor.getTs() );
        record.setExtremum( descriptor.getExtremum() );
        record.setDur( descriptor.getDur() );
 
        record.setTs_rank((short)1);
        record.setIngest("T");
        record.setOfs_input("F");
        record.setStg2_input("F");
    
        try
        {
            System.out.println("Attempting to insert" + record);
            table.insert(record);
            
        }
        catch(SQLException e)
        {
            logSQLException(e);
        
        }
    
        return;    
    }
    
//  -------------------------------------------------------     
 
   
//  -------------------------------------------------------     

    
    public void deleteState( SacSmaState sacSmaState )
    {
        SacSmaStateTable table = new SacSmaStateTable( _db );
        SacSmaStateRecord record = new SacSmaStateRecord();

        record.setBasin_id( sacSmaState.getBasinId() );
        record.setSource( sacSmaState.getSource() );
        record.setValidtime( sacSmaState.getValidTime() );
        record.setBasistime( sacSmaState.getBasisTime() );
        record.setPostingtime( sacSmaState.getPostingTime() );
        
        record.setUztwc( sacSmaState.getUztwc() );
        record.setUzfwc( sacSmaState.getUzfwc() );
        record.setLztwc( sacSmaState.getLztwc() );
        record.setLzfsc( sacSmaState.getLzfsc() );
        record.setLzfpc( sacSmaState.getLzfpc() );
        record.setAdimc( sacSmaState.getAdimc() );

        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }
    }

//  -------------------------------------------------------     

    
    public void deleteParams( SacSmaParameters sacSmaParams )
    {
        SacSmaParamsTable table = new SacSmaParamsTable( _db );
        SacSmaParamsRecord record = new SacSmaParamsRecord();
        
        record.setBasin_id( sacSmaParams.getBasinId() );
        record.setSource(sacSmaParams.getSource());
        record.setValidtime(sacSmaParams.getValidTime() );
        record.setPostingtime(sacSmaParams.getPostingTime());
        
        record.setUztwm(sacSmaParams.getUztwm());
        record.setUzfwm(sacSmaParams.getUzfwm());
        record.setUzk(sacSmaParams.getUzk());
        record.setPctim(sacSmaParams.getPctim());
        record.setAdimp(sacSmaParams.getAdimp());
        record.setRiva(sacSmaParams.getRiva());
        record.setZperc(sacSmaParams.getZperc());
        record.setRexp(sacSmaParams.getRexp());
        record.setLztwm(sacSmaParams.getLztwm());
        record.setLzfsm(sacSmaParams.getLzfsm());
        record.setLzfpm(sacSmaParams.getLzfpm());
        record.setLzsk(sacSmaParams.getLzsk());
        record.setLzpk(sacSmaParams.getLzpk());
        record.setPfree(sacSmaParams.getPfree());
        record.setRserv(sacSmaParams.getRserv());
        record.setSide(sacSmaParams.getSide());

        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            logSQLException(e);
        }
    }

//  -------------------------------------------------------     
    public boolean hasSacSmaParams(String basinId)
    {
        boolean result = false;
        
        SacSmaParamsTable table = new SacSmaParamsTable(_db);
        
        String whereClause = "where basin_id = '" + basinId + "'";
        int count = 0;
        
        
        try
        {
           count = table.selectCount(whereClause);    
        }
        catch (SQLException e)
        {
           logSQLException(e);
        }
        
        if (count > 0)
        {
            result = true;
        }
        
        return result;
    }
//  ------------------------------------------------------- 
    public MonthlyValues loadMonthlyValues(String locationId, boolean isAdjustment)
    {
//        String header = "loadMonthlyValues():";
        
        MonthlyValues monthlyValues = null;    
    
        String baseWhereClause =  "WHERE lid = '" + locationId + "' AND pe='EA'" ;
        String whereClause = null;
       
        String adjustmentClause = null;
        if (isAdjustment)
        {
            adjustmentClause = " AND adjustment = 'Y' AND dur = 1001 ";       
        }
        else
        {
            adjustmentClause = " AND adjustment != 'Y' AND dur = 2001 ";     
        }
        
        whereClause = baseWhereClause + adjustmentClause;
    
        List monthlyValuesList = loadMonthlyValuesList(whereClause);
        
  //      System.out.println(header + " whereClause = " + whereClause);
        
        if (monthlyValuesList.size() > 0)
        {
            monthlyValues = (MonthlyValues) monthlyValuesList.get(0);    
        }
        else
        {
            monthlyValues = new MonthlyValues();    
        }
        
        return monthlyValues;
          
    } //end getMonthlyValues

//  -------------------------------------------------------     
    
    public List loadMonthlyValuesList( String where )
    {
         List monthlyValuesRecordList = null;
         MonthlyValuesTable table = new MonthlyValuesTable( _db );
        
         try 
         {
             
             monthlyValuesRecordList = table.select( where );
             
         }
         catch( SQLException e )
         {
             _logger.log("Error code = " + e.getMessage());
             _logger.log("error retrieving SacSmaStateRecord: " + where );
         }

         return getMonthlyValuesListFromMonthlyValuesRecordList( monthlyValuesRecordList );   
    }
    
//     -------------------------------------------------------     

       private List getMonthlyValuesListFromMonthlyValuesRecordList( List monthlyValuesRecordList )
       {
           List monthlyValuesList = new ArrayList();
        
           for( int i = 0; i < monthlyValuesRecordList.size(); i++ )
           {
               MonthlyValues addMonthlyValues = new MonthlyValues();
               MonthlyValuesRecord monthlyValuesRecord = (MonthlyValuesRecord) monthlyValuesRecordList.get( i );
               double[] valueArray = new double[ 12 ];
            
               addMonthlyValues.setBasinId(monthlyValuesRecord.getLid());
               addMonthlyValues.setPe( monthlyValuesRecord.getPe() );
               addMonthlyValues.setDur( monthlyValuesRecord.getDur() );
               addMonthlyValues.setTs( monthlyValuesRecord.getTs() );
               addMonthlyValues.setExtremum( monthlyValuesRecord.getExtremum() );
               if ( monthlyValuesRecord.getAdjustment().equalsIgnoreCase( "Y" ) )
               {
                   addMonthlyValues.setAdjustment( true );
               }
               else
               {
                   addMonthlyValues.setAdjustment( false );
               }
               addMonthlyValues.setPostingTime(monthlyValuesRecord.getPostingtime());
            
               valueArray[ 0 ] = monthlyValuesRecord.getJan_value();
               valueArray[ 1 ] = monthlyValuesRecord.getFeb_value();
               valueArray[ 2 ] = monthlyValuesRecord.getMar_value();
               valueArray[ 3 ] = monthlyValuesRecord.getApr_value();
               valueArray[ 4 ] = monthlyValuesRecord.getMay_value();
               valueArray[ 5 ] = monthlyValuesRecord.getJun_value();
               valueArray[ 6 ] = monthlyValuesRecord.getJul_value();
               valueArray[ 7 ] = monthlyValuesRecord.getAug_value();
               valueArray[ 8 ] = monthlyValuesRecord.getSep_value();
               valueArray[ 9 ] = monthlyValuesRecord.getOct_value();
               valueArray[ 10 ] = monthlyValuesRecord.getNov_value();
               valueArray[ 11 ] = monthlyValuesRecord.getDec_value();

               addMonthlyValues.setValues( valueArray );
            
               monthlyValuesList.add( addMonthlyValues );
           }
    
       return monthlyValuesList;    
       }

//     -------------------------------------------------------     

       public void saveMonthlyValues( MonthlyValues monthlyValues )
       {
           MonthlyValuesTable table = new MonthlyValuesTable( _db );
           MonthlyValuesRecord record = new MonthlyValuesRecord();
           long currentTime = System.currentTimeMillis();
           double[] valueArray = monthlyValues.getValueArray();

           record.setLid( monthlyValues.getBasinId() );
           record.setPe( monthlyValues.getPe() );
           record.setDur( monthlyValues.getDur() );
           record.setTs( monthlyValues.getTs() );
           record.setExtremum( monthlyValues.getExtremum() );
           if ( monthlyValues.isAdjustment() )
           {
               record.setAdjustment( "Y" );
           }
           else
           {
               record.setAdjustment( "N" );
           }
           record.setPostingtime( currentTime );
           record.setJan_value( valueArray[ 0 ] );
           record.setFeb_value( valueArray[ 1 ] );
           record.setMar_value( valueArray[ 2 ] );
           record.setApr_value( valueArray[ 3 ] );
           record.setMay_value( valueArray[ 4 ] );
           record.setJun_value( valueArray[ 5 ] );
           record.setJul_value( valueArray[ 6 ] );
           record.setAug_value( valueArray[ 7 ] );
           record.setSep_value( valueArray[ 8 ] );
           record.setOct_value( valueArray[ 9 ] );
           record.setNov_value( valueArray[ 10 ] );
           record.setDec_value( valueArray[ 11 ] );
           try
           {       
               table.insertOrUpdate( record );
           }
           catch ( SQLException e )
           {
               logSQLException(e);
           }   
       }

//     -------------------------------------------------------     

       public void deleteMonthlyValues( MonthlyValues monthlyValues )
       {
           MonthlyValuesTable table = new MonthlyValuesTable( _db );
           MonthlyValuesRecord record = new MonthlyValuesRecord();
           double[] valueArray = monthlyValues.getValueArray();

           record.setLid( monthlyValues.getBasinId() );
           record.setPe( monthlyValues.getPe() );
           record.setDur( monthlyValues.getDur() );
           record.setTs( monthlyValues.getTs() );
           record.setExtremum( monthlyValues.getExtremum() );
           if ( monthlyValues.isAdjustment() )
           {
               record.setAdjustment( "Y" );
           }
           else
           {
               record.setAdjustment( "N" );
           }
           record.setPostingtime( monthlyValues.getPostingTime() );
           record.setJan_value( valueArray[ 0 ] );
           record.setFeb_value( valueArray[ 1 ] );
           record.setMar_value( valueArray[ 2 ] );
           record.setApr_value( valueArray[ 3 ] );
           record.setMay_value( valueArray[ 4 ] );
           record.setJun_value( valueArray[ 5 ] );
           record.setJul_value( valueArray[ 6 ] );
           record.setAug_value( valueArray[ 7 ] );
           record.setSep_value( valueArray[ 8 ] );
           record.setOct_value( valueArray[ 9 ] );
           record.setNov_value( valueArray[ 10 ] );
           record.setDec_value( valueArray[ 11 ] );

           try
           {       
               table.delete( record );
           }
           catch ( SQLException e )
           {
               logSQLException(e);
           }   
       }

//  -------------------------------------------------------     
 
    private SshpConfigRecord getSshpConfigRecord(SSHPConfig config)
    {
        SshpConfigRecord record = new SshpConfigRecord();
       
        record.setLid(config.getLid());
        record.setBasin_id(config.getBasinId());        
        record.setPostingtime(config.getPostingTime());
       
        record.setModel_pref(config.getModelPref());
        record.setSource_pref(config.getSourcePref());
          
        // convert and set boolean Y or N strings
        record.setUse_static_evap(getStringFromBoolean(config.useStaticEvap()));          
        record.setAuto_process(getStringFromBoolean(config.isAutoProcess()));  
        record.setUse_blend(getStringFromBoolean(config.useBlend()));
        
        record.setBlend_method(config.getBlendMethod().getName());
        record.setBlend_hours(config.getBlendHours());

     
       
        return record;
    }  

//  -------------------------------------------------------     
    public SSHPConfig getSSHPConfig(String locationId)
    {
        String header = "DataMgr.getSshpConfig(): ";
        SSHPConfig config = null;
         
        config = (SSHPConfig) _sshpConfigMap.get(locationId);
        if (config == null)
        {
            String whereClause = " "; 
            loadSSHPConfigList(whereClause); //throw out the returned list, we don't need it
            config = (SSHPConfig) _sshpConfigMap.get(locationId);
        }
      
        return config;
        
    }
    
//  -------------------------------------------------------     


   
    private SSHPConfig getSshpConfigFromRecord(SshpConfigRecord record)
    {
        SSHPConfig config = new SSHPConfig();
             
        config.setLid(record.getLid());

        config.setBasinId( record.getBasin_id());        
        config.setPostingTime( record.getPostingtime());
       
        config.setModelPref( record.getModel_pref());
        config.setSourcePref( record.getSource_pref());
        
        config.setUseStaticEvap(getBooleanFromString(record.getUse_static_evap()));
        config.setAutoProcess(getBooleanFromString(record.getAuto_process()));
       
        config.setUseBlend(getBooleanFromString(record.getUse_blend())); 
        
       
        ForecastInterpolationMethod method = ForecastInterpolationMethod.getMethodByName(record.getBlend_method());
        config.setBlendMethod(method);
        
        
        config.setBlendHours(record.getBlend_hours());

        return config;
    }  
  
//  -------------------------------------------------------     
    private boolean getBooleanFromString(String booleanString)
    {
        boolean result = false;
        if (booleanString.equalsIgnoreCase("Y"))
        {
            result = true;
        }
     
        return result;
    }
//  -------------------------------------------------------     
    private String getStringFromBoolean(boolean value)
    {
        String resultString = null; 
        if (value)
        {
            resultString = "Y";
        }
        else
        {
            resultString = "N";
        }
     
        return resultString;
    }
//  -------------------------------------------------------     

    public void saveSSHPConfig(SSHPConfig config)
    {
               
        SshpConfigTable table = new SshpConfigTable(_db);
        SshpConfigRecord record = getSshpConfigRecord(config);
        
        record.setPostingtime(System.currentTimeMillis());
        
        try
        {
             table.insertOrUpdate(record);
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }   


        return;
        
    }
//  -------------------------------------------------------   
 
    public void deleteSSHPConfig(SSHPConfig config)
    {
        SshpConfigTable table = new SshpConfigTable(_db);
        SshpConfigRecord record = getSshpConfigRecord(config);
        
        try
        {
            table.delete(record);
            _sshpConfigMap.remove(record.getLid());
        }
        catch (SQLException e)
        {
            logSQLException(e);
        } 

        return;  

    }

//  -------------------------------------------------------     
    public List getSSHPConfigList()
    {
        if (_sshpConfigList == null)
        {
            _sshpConfigList = loadSSHPConfigList(" order by lid ");
        }
        
        return _sshpConfigList;
        
    }
    
//  -------------------------------------------------------     
    
    public List loadSSHPConfigList(String whereClause)
    {
        SshpConfigTable table = new SshpConfigTable(_db);
        SshpConfigRecord record = null;
        List recordList = null;

        _sshpConfigList = new ArrayList();
   
       
        try
        {
            recordList = table.select(whereClause);
                      
            for (int i = 0; i < recordList.size(); i++)
            {
                record = (SshpConfigRecord) recordList.get(i);
                SSHPConfig config = getSshpConfigFromRecord(record);
                _sshpConfigList.add(config);
                _sshpConfigMap.put(config.getLid(), config);
            }           
            
        }
        catch (SQLException e)
        {
            logSQLException(e);
        } 

        return _sshpConfigList;

    }
//  ------------------------------------------------------- 
    
    public List loadAllPreferredSacSmaBasinIds()
    {
        List locationIdList = new ArrayList();
        List sshpConfigList = getSSHPConfigList();
        
        for (int i = 0 ;  i < sshpConfigList.size(); i++)
        {
            SSHPConfig config = (SSHPConfig) sshpConfigList.get(i);
            String basinId = config.getLid();
            
            if ( config.getModelPref().equals(RainfallRunoffModelType.SAC_SMA.getName())
               )      
            {
                locationIdList.add(basinId);    
            }
        }  
        
        return locationIdList;
    }
//  ------------------------------------------------------- 
    public List loadAllLocationIdsToAutoUpdateSacStates()
    {
        List locationIdList = new ArrayList();
        List sshpConfigList = getSSHPConfigList();
        
        for (int i = 0 ;  i < sshpConfigList.size(); i++)
        {
            SSHPConfig config = (SSHPConfig) sshpConfigList.get(i);
            String locationId = config.getLid();
            
            if ( (config.getModelPref().equals(RainfallRunoffModelType.SAC_SMA.getName())) && 
                 (config.isAutoProcess())
               )      
            {
                locationIdList.add(locationId);    
            }
        }  
        
        return locationIdList;
    }
//  ------------------------------------------------------- 
    
    public RainfallRunoffModelType getPreferredRainfallRunoffModelType(String locationId)
    {
        String header = "DataMgr.getPreferredRainfallRunoffModelType(): ";
        RainfallRunoffModelType type = RainfallRunoffModelType.API_MKC;
        
        SSHPConfig config = getSSHPConfig(locationId);
        
        if (config != null)
        {
            String modelPref = config.getModelPref();
        
            _logger.log(header + "modelPref = :" + modelPref + ":");

            type = RainfallRunoffModelType.getRainfallRunoffModelTypeByName(modelPref);
        }
        
        _logger.log(header + "type = " + type);
      
     
        return type;
    }

//  ---------------------------------------------------------------------------------------

    public BasinHrapHelper getBasinHrapHelper(String basinId)
    {     
    
        BasinHrapHelper basinHrapHelper = (BasinHrapHelper) _basinHelperMap.get(basinId);
        if (basinHrapHelper ==  null)
        {
            basinHrapHelper = loadBasinHrapHelper(basinId);
            if (basinHrapHelper != null)
            {
                _basinHelperMap.put(basinId, basinHrapHelper);
            }
            
        }
        
        return basinHrapHelper;
    }

    //  ---------------------------------------------------------------------------------------
   
    public BasinHrapHelper loadBasinHrapHelper(String basinId)
    {     
    
        BasinHrapHelper basinHrapHelper = initBasinHrapHelper(basinId);
        
        return basinHrapHelper;
    }
// ----------------------------------------------------------------------------------------
 
    private BasinHrapHelper initBasinHrapHelper(String basinId)
    {
        List recordList = null;
        String whereClause = new String();
        
        LineSegsTable  table = new LineSegsTable(this._db);
        LineSegsRecord record = null;
        
        BasinHrapHelper basinHrapSet = null;
        
        
        // Go to LineSegs table to get number of rows and HRAP rows and columns
        whereClause = " WHERE area_id='" + basinId + "' ORDER BY hrap_row ASC ";
        
        try
        {
            recordList = table.select(whereClause);
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
        
        for (int i = 0; i < recordList.size(); i++)
        {
            record = (LineSegsRecord) recordList.get(i);       
            
            if (i == 0)
            {
                basinHrapSet = new BasinHrapHelper(basinId, recordList.size());
                basinHrapSet.setIsValid(true);
                basinHrapSet.setAreaSize( record.getArea() );
            }
            
            basinHrapSet.setBasinHrapRow(i, record.getHrap_row());
            basinHrapSet.setBasinHrapBegColumn(i, record.getHrap_beg_col());
            basinHrapSet.setBasinHrapEndColumn(i,  record.getHrap_end_col());
        }
        
        if (basinHrapSet == null)
        {
            basinHrapSet = new BasinHrapHelper(basinId, 0);
            basinHrapSet.setIsValid(false);
        }
        
        return(basinHrapSet);
        
    } // end of getBasinRowsCols method
    
// -------------------------------------------------------------------------------------------------   
    public long getLatestVarSacSmaStatePostingTime(SSHPSource source)
    {
        long latestPostingTime = -1;
        List recordList = null;
        SacSmaStateTable table = new SacSmaStateTable(_db);
        
        String whereString = "WHERE source = '" + source.getSacSmaSource() + "' ORDER BY postingtime DESC ";
        
        try
        {
            recordList = table.selectNRecords(whereString, 1);

            if (recordList.size() > 0)
            {
                SacSmaStateRecord record = (SacSmaStateRecord) recordList.get(0);
                latestPostingTime = record.getPostingtime();
            }
        }
        catch (SQLException exception)
        {
            logSQLException(exception);
        }
        
        return latestPostingTime;
    }
//  -------------------------------------------------------------------------------------------------   

    /**
     *  This method is used by the SSHP MAP Preprocessor to determine what hours (if any) to rerun.
     *  It is also used by VAR to determine what hours (if any) to rerun. 
     */
    public List getNewerBestQpeXmrgFileTimeList(long lastPostingTime)
    {
        
        String header = "DataMgr.getNewerBestQpeXmrgFileTimeList(): ";
        
        RWResultTable table = new RWResultTable(_db);
        RWResultRecord record = null;
        List recordList = null;
        
        List timeList = new ArrayList();
        
        
        try
        {
            String lastPostingTimeString = DbTimeHelper.getDateTimeStringFromLongTime(lastPostingTime);
            String whereString = " WHERE last_save_time >= '" + lastPostingTimeString + "' ORDER BY obstime ";
            
            
            System.out.println(header + " whereString = " + whereString);
            recordList = table.select(whereString);    
            
            for (int i = 0 ; i < recordList.size(); i++)
            {
                record = (RWResultRecord) recordList.get(i);
                
                long time = record.getObstime();
                timeList.add(new Long(time));
            }
            
        }
        
        catch (SQLException e)
        {
           logSQLException(e);    
        }
        
        
        return timeList;
        
    }

//	------------------------------------------------------------------------------------------------- 
    public List getNewerRfcXmrgFileTimeList(long lastPostingTime)
    {
        String header = "DataMgr.getNewerRfcXmrgTimeList(1 arg): ";
        System.out.println(header);
        
        String rfcQpeGridDirName = _appsDefaults.getToken("gaq_xmrg_1hr_dir");
        
        System.out.println(header + " after _appsDefaults");
        
        // rfc mosaic file name format
        SimpleDateFormat rfcXmrgFileDateFormat = new SimpleDateFormat("'RFCMOSAIC01'yyyyMMddHH'z'");
        rfcXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
  
        return getNewerRfcXmrgFileTimeList(lastPostingTime, rfcQpeGridDirName, rfcXmrgFileDateFormat);
    }
    
//  ------------------------------------------------------------------------------------------------- 
  
    public List getNewerRfcXmrgFileTimeList(long lastPostingTime, 
                                            String gridDirectoryString,
                                            SimpleDateFormat dateFormat)
    {
        String header = "DataMgr.getNewerRfcXmrgTimeList(3 args): ";
        System.out.println(header);
        List timeList = new ArrayList(); 
        
        System.out.println(header + " lastPostingTime = " + lastPostingTime +
                                    " gridDirectoryString = " + gridDirectoryString + 
                                    "dateFormat = " + dateFormat);
        System.out.flush();
        
        File[] fileArray = null;
        File directory = new File(gridDirectoryString);   
        
        if (directory != null)
        {
            fileArray = directory.listFiles();
        }
        
        // for each file in the directory
        // examine the last update time,
        // if it is newer than the last update time,
        // then parse out its time and then add it to the list
        // If it fails to parse, then we don't want it anyway;
        // It is some other type of file that got in the directory somehow.
        if (fileArray != null)
        {
            for (int i = 0 ; i < fileArray.length; i++)
            {
                File file = fileArray[i];
                long lastModTime = 0;
                try 
                {   


                    lastModTime = file.lastModified();
                    if (lastModTime >= lastPostingTime)  //is it new?
                    {
                        _logger.log(header + 
                                "lastModTime = " + 
                                DbTimeHelper.getDateTimeStringFromLongTime(lastModTime) +
                                " fileName = " + file.getName() );

                        Date date = dateFormat.parse(file.getName());
                        long gridTime = date.getTime();

                        _logger.log(header + 
                                "gridTime = " + 
                                DbTimeHelper.getDateTimeStringFromLongTime(gridTime) +
                                " fileName = " + file.getName() );


                        timeList.add( (new Long(gridTime)) );    
                    }
                }
                catch (ParseException e)
                {
                    //some other kind of file was parsed,
                    //we don't want it
                }               
            }
        }

        return timeList;
        
    }

//  -------------------------------------------------------------------------------------------------   


	public List loadUnitHydrographEntryList( String lid, String orderClause )
	// Used by UnitHydrographEditor to retrieve data from the UnitGraph table
	{
		List unitHydrographRecordList = null;
		String where = " WHERE lid='" + lid + "' " + orderClause;
		UnitGraphTable table = new UnitGraphTable( _db );
		
		try
		{
			unitHydrographRecordList = table.select( where );
		}
        
		catch( SQLException e )
		{
			logSQLException(e);
		}

		return getUnitHydrographEntryListFromUnitGraphRecordList( unitHydrographRecordList );
	}

//	-------------------------------------------------------------------------------------------------   

	public List loadUnitHydrographDescriptorList(String lid)
	{
	    List descList = new ArrayList();
		UnitGraphTable table = new UnitGraphTable( _db );
		List recordList = null;
		
		String where = " WHERE lid='" + lid + "' ORDER BY area_id, model, dur ";
		
		String oldAreaId = null;
		String oldModel = null;
		int oldDur = 0;
		
		try
		{
			recordList = table.select( where );
			
			boolean addToDescriptorList = false;
			
			for (int i = 0; i < recordList.size(); i++)
			{	    
			    UnitGraphRecord record = (UnitGraphRecord) recordList.get(i);
			    
			    if (i > 0)
			    {
			        // if the record is not the same as the previous record then add it to the
			        //list
			        if ((! record.getArea_id().equals(oldAreaId)) ||
			           (! record.getModel().equals(oldModel)) ||
			           ( record.getDur() != oldDur))
			        {
			            addToDescriptorList = true;
			        }
			        
			    }
			    else // i == 0, there are no "old" variables
			    {
			        addToDescriptorList = true;
			
			    }
			    
			    if (addToDescriptorList)
			    {
			        UnitHydrographDescriptor desc = new UnitHydrographDescriptor(record.getLid(),
		                record.getArea_id(), record.getModel(), record.getDur());
			    
			        descList.add(desc);
			        
			    	addToDescriptorList = false;
			    }
			      
			    // set the "old" variables for next iteration 
				oldAreaId = record.getArea_id();
				oldModel = record.getModel();
				oldDur = record.getDur();
				
			
			}
		}
        
		catch( SQLException e )
		{
			logSQLException(e);
		}

		/*
		for (int i = 0; i < descList.size(); i++)
		{
		    UnitHydrographDescriptor desc = (UnitHydrographDescriptor) descList.get(i);
		    System.out.println(desc);
		}
		*/
		
		return descList;
	    
	} // loadUnitHydrographDescriptorList

//	-------------------------------------------------------------------------------------------------   

	private List getUnitHydrographEntryListFromUnitGraphRecordList( List unitGraphRecordList )
	{
		List unitHydrographEntryList = new ArrayList();

		for( int i = 0; i < unitGraphRecordList.size(); i++ )
		{
			UnitHydrographEntry newUnitHydrographEntry = new UnitHydrographEntry();
			UnitGraphRecord unitGraphRecord = (UnitGraphRecord) unitGraphRecordList.get( i );
			
			newUnitHydrographEntry.setLocationId( unitGraphRecord.getLid() );
			newUnitHydrographEntry.setAreaId( unitGraphRecord.getArea_id() );
			newUnitHydrographEntry.setModel(unitGraphRecord.getModel());
			newUnitHydrographEntry.setDur( unitGraphRecord.getDur() );
			newUnitHydrographEntry.setOrdinal( unitGraphRecord.getOrdinal() );
			
			newUnitHydrographEntry.setDischarge( unitGraphRecord.getDischarge() );
			
			
			unitHydrographEntryList.add( newUnitHydrographEntry );
		}
    
		return unitHydrographEntryList;
	}

//	-------------------------------------------------------------------------------------------------   

	
	public List getRatingShift( String lid )
	// Used by RatingCurveEditor to retrieve data from the RatingShift table
	{
		List ratingShiftRecordList = null;
		String where = " WHERE lid='" + lid + "' ORDER BY date desc";
		RatingShiftTable table = new RatingShiftTable( _db );
		
		try
		{
			ratingShiftRecordList = table.select( where );
		}
		catch( SQLException e )
		{
			_logger.log("Error code = " + e.getMessage());
			_logger.log("error retrieving SacSmaStateRecord: " + where );
		}
		
		List ratingShiftEntryList = null;
		
		if (ratingShiftRecordList != null)
		{
		    ratingShiftEntryList = getRatingShiftEntryListFromRatingShiftRecordList( ratingShiftRecordList );
		}
		
		return ratingShiftEntryList;
	}

//	-------------------------------------------------------------------------------------------------   

	
	private List getRatingShiftEntryListFromRatingShiftRecordList( List recordList )
	{
		List ratingShiftEntryList = new ArrayList();
		
		for( int i = 0; i < recordList.size(); i++ )
		{
			RatingShift addRatingShiftEntry = new RatingShift();
			RatingShiftRecord record = (RatingShiftRecord) recordList.get( i );
			
			addRatingShiftEntry.setLid( record.getLid() );
			addRatingShiftEntry.setDate( record.getDate() );
			addRatingShiftEntry.setShiftAmount( record.getShift_amount() );
			
			if ( record.getActive().trim().equalsIgnoreCase( "T" ) )
			{
				addRatingShiftEntry.setActive( true );
			}
			else
			{
				addRatingShiftEntry.setActive( false );
			}
			
			ratingShiftEntryList.add( addRatingShiftEntry );
		}
		return ratingShiftEntryList; 
	}
	

//	-------------------------------------------------------------------------------------------------   


	public void saveUnitHydrographEntry( UnitHydrographEntry entry )
	{
		UnitGraphRecord record = new UnitGraphRecord();
		UnitGraphTable table = new UnitGraphTable( _db );
		
		record = getUnitGraphRecordFromUnitHydrographEntry( entry );

		try
		{		
			table.insertOrUpdate( record );
		}
		catch( SQLException e)
		{
			logSQLException(e);
		}
	}


//	-------------------------------------------------------------------------------------------------   

	private UnitGraphRecord getUnitGraphRecordFromUnitHydrographEntry( UnitHydrographEntry entry )
	{
		UnitGraphRecord returnRecord = new UnitGraphRecord();
		
		returnRecord.setLid( entry.getLocationId() );
		returnRecord.setArea_id( entry.getAreaId() );
		returnRecord.setModel(entry.getModel());
		returnRecord.setDur( entry.getDur() );	
		returnRecord.setOrdinal( entry.getOrdinal() );
		
		returnRecord.setDischarge( entry.getDischarge() );
		
		
		return returnRecord;
	}

//	-------------------------------------------------------------------------------------------------   

	
	public void removeUnitHydrographEntry( UnitHydrographEntry entry )
	{
		UnitGraphRecord record = new UnitGraphRecord();
		UnitGraphTable table = new UnitGraphTable( _db );
		
		record = getUnitGraphRecordFromUnitHydrographEntry( entry );

		try
		{		
			table.delete( record );
		}
		catch( SQLException e)
		{
			logSQLException(e);
		}
	}

//	-------------------------------------------------------------------------------------------------   

	
	public void deleteUnitHydrograph( String lid, String basinId, String model, int duration )
	{
		UnitGraphTable table = new UnitGraphTable( _db );
		String whereString = " WHERE lid='" + lid + "' AND area_id='" + basinId + "' AND model = '" +
							model + "' AND dur = " + duration;
		
		try
		{
			table.delete( whereString );
		}
		catch ( SQLException e )
		{
			logSQLException( e );
		}
	}
	
//	-------------------------------------------------------------------------------------------------   

	public void deleteRatingShift( RatingShift shift )
	{
		RatingShiftRecord record = new RatingShiftRecord();
		RatingShiftTable table = new RatingShiftTable( _db );
		
		record = getRatingShiftRecordFromRatingShift( shift );
		try
		{
			table.delete( record );
		}
		catch( SQLException e)
		{
			logSQLException(e);
		}
	}
	
	public void saveRatingShift( RatingShift shift )
	{
		RatingShiftRecord record = new RatingShiftRecord();
		RatingShiftTable table = new RatingShiftTable( _db );
		
		record = getRatingShiftRecordFromRatingShift( shift );
		
		try
		{
			table.insertOrUpdate( record );
		}
		catch( SQLException e)
		{
			logSQLException(e);
		}
		
	}
	
	private RatingShiftRecord getRatingShiftRecordFromRatingShift( RatingShift shift )
	{
		RatingShiftRecord record = new RatingShiftRecord();
		
		record.setLid( shift.getLid() );
		record.setDate( shift.getDate() );
		record.setShift_amount( shift.getShiftAmount() );
		if ( shift.isActive() )
		{
			record.setActive( "T" );
		}
		else
		{
			record.setActive( "F" );		
		}
		return record;
	}
	
	/*	private void insertParams(String locationId)
	{
		SacSmaParamsTable table = new SacSmaParamsTable(_db);
		SacSmaParamsRecord record = new SacSmaParamsRecord();
		
		long currentTime = System.currentTimeMillis();
		
		
		record.setBasin_id(locationId);
		
		record.setValidtime(currentTime);
		record.setPostingtime(currentTime);
		
		record.setUztwm(65.0);
		record.setUzfwm(30.0);
		record.setUzk(0.180);
		record.setPctim(0.010);
		record.setAdimp(0.130);
		record.setRiva(0.03);
		record.setZperc(125.0);
		record.setRexp(3.5);
		record.setLztwm(175.0);
		record.setLzfsm(100.0);
		record.setLzfpm(260.0);
		record.setLzsk(0.0450);
		record.setLzpk(0.0030);
		record.setPfree(0.60);
		record.setRserv(0.30);
		record.setSide(0.00);
		
		try
		{	
			table.insert(record);	
		}
		catch (SQLException e)
		{
			 logSQLException(e);
		}	
	}
*/	
//	-------------------------------------------------------		
/*	private void insertState(String locationId)
	{
		SacSmaStateTable table = new SacSmaStateTable(_db);
		SacSmaStateRecord record = new SacSmaStateRecord();
		
		long currentTime = System.currentTimeMillis();
		
		record.setBasin_id(locationId);
		
		record.setValidtime(currentTime);
		record.setPostingtime(currentTime);
		
		record.setUztwc(63.33);
		record.setUzfwc(0.090);
		record.setLztwc(135.71);
		record.setLzfsc(12.187);
		record.setLzfpc(165.26);
		record.setAdimc(215.75);
		
		try
		{	
			table.insert(record);	
		}
		catch (SQLException e)
		{
			 logSQLException(e);
		}	
	}	
*/
	
} //end class DataMgr
