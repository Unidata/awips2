/*
 * Created on Jun 13, 2005
 *
 * 
 * 
 */
package ohd.hseb.pdc_pp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;

/**
 * @author Gautam Sood
 *         Chip Gobs
 */
public class PDCPreprocessor
{
    private static final String _versionString = "OB8.1";
    private static final String _dateString = "March 27, 2007";
    
    private static final long MILLIS_PER_HOUR = 60 * 60 * 1000;
    private static final Runtime _runtime = Runtime.getRuntime();
    
    private PDCPreprocessorDataMgr _dataMgr = null;
    private boolean _debug = false;
 
    private String _preprocessedFilePath = null;  //directory where the processed files are placed in
    private String _logDirName = null;   //directory where the log will be placed in
    private Logger _logger = null;  
    private Set _completeLidSet = null;
    private Set _completeRiverStationLidSet = null;
    
    private long _endTime = 0;
    private long _endTimeLastHour = 0;
   // private long _endTimeNextHour = 0;
    
    private List _heightTimeSeriesList = null;
    
    private List _stagePoolTimeSeriesList = new ArrayList();
    private List _percentFloodFlowTimeSeriesList = new ArrayList();
    
    
    private List _lakeTimeSeriesList = null;
    private List _dischargeTimeSeriesList = null;
    private List _heightDerivedDischargeTimeSeriesList = new ArrayList();
    private List _flowStorageTimeSeriesList = new ArrayList();

    
    private List _temperatureTimeSeriesList = null;
    private List _tempTimeSeriesList = new ArrayList();
    private List _maxTempTimeSeriesList = new ArrayList();
    private List _minTempTimeSeriesList = new ArrayList();
    private List _dewPointTimeSeriesList = new ArrayList();
    private List _deltaDewPointTimeSeriesList = new ArrayList();
    private List _deltaTempTimeSeriesList = new ArrayList();
    
    private List _windTimeSeriesList = null;
    private List _windSpeedTimeSeriesList = new ArrayList();
    private List _windDirectionTimeSeriesList = new ArrayList();
    
    private List _snowTimeSeriesList = null;
    private List _snowWaterTimeSeriesList = new ArrayList();
    private List _depthAboveFloodStageTimeSeriesList = new ArrayList();
    private List _deltaSnowWaterTimeSeriesList = new ArrayList();

    private List _weatherTimeSeriesList = null;
    private List _relativeHumidityTimeSeriesList = new ArrayList();
    

    private List _instantPrecipTimeSeriesList = new ArrayList();
    private List _hourlyPrecipTimeSeriesList = new ArrayList();
    private List _threeHourPrecipTimeSeriesList = new ArrayList();
    private List _sixHourPrecipTimeSeriesList = new ArrayList();
    private List _twentyFourHourPrecipTimeSeriesList = new ArrayList();
    
    private int _heightHours = 0;
    private int _temperatureHours = 0;
    private int _snowHours = 0;
    private int _windHours = 0;
    private int _weatherHours = 0;
    private int _precipHours = 0;
    private int _lowerWindow = 0;
    private int _upperWindow = 0;
  
    public PDCPreprocessor()
    {
    }

/**
 * initialize( String[] args ) - Used to process the command line arguments and set appropriate variables.
 * @param args - Command line arguments
 */
    private void initialize( String[] args )
    {
        String header = "PDCPreprocessor.initialize(): ";
        StringBuffer baseConnectionStringBuffer = new StringBuffer();  //gotten from the command line
        String logFileName = null;  

        
        getAppsDefaults();

        
        for (int i = 0; i < args.length; i++)
        {
            System.out.println("args[" + i + "] = " + args[i]);     
        }
        
        processCommandLineArguments( baseConnectionStringBuffer, args );

        logFileName = _logDirName + "/PDCPreprocessor.log";

        _logger = new FileLogger(logFileName, true, true); 

        
        _logger.log("----------------------------------------------------------------------------------------- ");
        _logger.log("version = " + _versionString + " date = " + _dateString);
        
        _logger.log( "Starting PDC Preprocessor - " +
                DbTimeHelper.getDateTimeStringFromLongTime( System.currentTimeMillis() ) );

        
        _logger.log(header + "JDBC connection string = " + 
                            baseConnectionStringBuffer.toString() );
        
        _dataMgr = new PDCPreprocessorDataMgr(
                    baseConnectionStringBuffer.toString(),
                    _logger,
                    _preprocessedFilePath,
                    _lowerWindow,
                    _upperWindow,
                    _debug);
        
        _endTime = System.currentTimeMillis();
        _endTimeLastHour = _endTime;
        _endTimeLastHour /= MILLIS_PER_HOUR;
        _endTimeLastHour *= MILLIS_PER_HOUR;
        
 //       _endTimeNextHour = _endTimeLastHour + MILLIS_PER_HOUR;
        
        
    }
    
    // -----------------------------------------------------------------------------------------
    
    private void disconnect()
    {
       _dataMgr.disconnect(); 
       return;
    }

    // -----------------------------------------------------------------------------------------
    
    private long getNonPrecipEndTime()
    {
        
        return _endTimeLastHour;
    }
    
    private long getNonPrecipDailyEndTime()
    {
        String header = "PDCPreprocessor.getNonPrecipDailyEndTime(): ";
        
        long dailyEndTime = _endTimeLastHour;
        dailyEndTime /= PDCPreprocessorDataMgr.MILLIS_PER_DAY ;
        dailyEndTime *= PDCPreprocessorDataMgr.MILLIS_PER_DAY ;
        
        dailyEndTime += PDCPreprocessorDataMgr.MILLIS_PER_12_HOURS;
        
        System.out.println(header + "dailyEndTime = " +
                           DbTimeHelper.getDateTimeStringFromLongTime(dailyEndTime));
        
        return  dailyEndTime;
        
    }
    
    // -----------------------------------------------------------------------------------------
    
    private long getPrecipEndTime()
    {
        
        return _endTime;
    }
    
    // -----------------------------------------------------------------------------------------
    
/**
 * processObsData() - Processes the Observed data and calls appropriate methods from the Data Manager to write the data 
 *                    out to the .dat files.
 */    
    public void processObsData()
    {
        String header = "PDCPreprocessor.processObsData(): ";
            
        CodeTimer precipTimer = new CodeTimer();
         
        _dataMgr.createLidMap();
        _dataMgr.createShefDurMap();
        _completeLidSet = _dataMgr.getCompleteLidSet();
       
        _completeRiverStationLidSet = _dataMgr.getRiverStationLidSet();
            
        _dataMgr.handleCacheAging();
        
        printFreeMemory("Before handleHeightData");
        handleHeightData();
        printFreeMemory("After handleHeightData");
        
        handleFlowAndStorageData();
        printFreeMemory("After handleFlowAndStorageData");
  
        handleTemperatureData();
        printFreeMemory("After handleTemperatureData");
        
        
        handleWindData();
        printFreeMemory("After handleWindData");
        
        
        handleSnowData();
        printFreeMemory("After handleSnowData");
        
        
        handleWeatherData();
        printFreeMemory("After handleWeatherData");
        
        
      
        // handle precip data
        precipTimer.start();
       // processPrecipData();
        processPrecipDataWithCaching(_precipHours);
        precipTimer.stop("Reading and processing the Precip Data took ");

        CodeTimer writePrecipDataFileTimer = new CodeTimer();
        writePrecipDataFileTimer.start();
        _dataMgr.writeIrregularObsDataToFile(_instantPrecipTimeSeriesList, "PrecipInstant");

        _dataMgr.writeRegularObsDataToFile(_hourlyPrecipTimeSeriesList, "Precip1Hour");
        _dataMgr.writeRegularObsDataToFile(_threeHourPrecipTimeSeriesList, "Precip3Hour");
        _dataMgr.writeRegularObsDataToFile(_sixHourPrecipTimeSeriesList, "Precip6Hour");
        _dataMgr.writeRegularObsDataToFile(_twentyFourHourPrecipTimeSeriesList, "Precip24Hour");

        writePrecipDataFileTimer.stop("Writing all of the precip data files took");
    }
    
    static public void printFreeMemory()
    {
        printFreeMemory(null);
    }
    
    static public void printFreeMemory(String message)
    {
        long freeMemory = _runtime.freeMemory();
        long maxMemory = _runtime.maxMemory();
        if (message == null)
        {
            message = "";
        }
        
        System.out.println(message + " Free memory = " + freeMemory + " Max memory = " + maxMemory);
       
    }
     
    private void handleHeightData()
    {
        String header = "PDCPreprocessor.handleHeightData(): ";
        
        CodeTimer heightDbTimer = new CodeTimer();
        CodeTimer heightCacheTimer = new CodeTimer();
       
        heightCacheTimer.start();
        _heightTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Height",
                                        getNonPrecipEndTime(), _heightHours );
        heightCacheTimer.stop(header + "height cache usage took");
        
        addMissingRiverTimeSeries( _heightTimeSeriesList, 
                                    getRegularObsTimeSeriesDescriptor( "HG", "RZ", "Z",
                                   (short) 0,  getNonPrecipEndTime(), _heightHours, true), true );   
        
        processHeightTimeSeriesList();
        
         _heightTimeSeriesList = null;
        System.gc();
        
        _dataMgr.writeRegularObsDataToFile( _stagePoolTimeSeriesList, "Height" );
        _dataMgr.writeRegularObsDataToFile( _depthAboveFloodStageTimeSeriesList, "HeightAboveFloodStage" );
        
        
        
        _stagePoolTimeSeriesList = null;
        _depthAboveFloodStageTimeSeriesList = null;
        
        System.gc();
       
    }
    // --------------------------------------------------------------------------------------------
    private void handleFlowAndStorageData()
    {
        _lakeTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Lake", getNonPrecipEndTime(), _heightHours );
        _dischargeTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Discharge", getNonPrecipEndTime(), _heightHours );
 //       addMissingTimeSeries( _lakeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "LS", "RZ", "Z", (short) 0,  getNonPrecipEndTime(), _heightHours ), true );
 
        //adds discharge data that was derived from height data, as needed (if there was no data for it)
        incorporateConvertedHeightData(_heightDerivedDischargeTimeSeriesList,
                                       _dischargeTimeSeriesList);
       
        addMissingTimeSeries( _dischargeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "QR", "RZ", "Z", (short) 0,  getNonPrecipEndTime(), _heightHours, true ), true );
 //       addMissingTimeSeries( _dischargeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "QT", "RZ", "Z", (short) 0,  getNonPrecipEndTime(), _heightHours, true ), true );
       
        
        printFreeMemory("handleFlowAndStorageData() before processFlowStorageTimeSeriesList()");
        
        processFlowStorageTimeSeriesList();
        
        printFreeMemory("handleFlowAndStorageData() after processFlowStorageTimeSeriesList()");
                
     
        _lakeTimeSeriesList = null;
        _dischargeTimeSeriesList = null;
        _heightDerivedDischargeTimeSeriesList = null;
        System.gc();
        
        printFreeMemory("handleFlowAndStorageData() before processFloodFlowPercentageTimeSeriesList()");
        
        processFloodFlowPercentageTimeSeriesList();
        printFreeMemory("handleFlowAndStorageData() after processFloodFlowPercentageTimeSeriesList()");
        
        
        printFreeMemory("handleFlowAndStorageData() before writeRegularObsDataToFile() #1");
        _dataMgr.writeRegularObsDataToFile( _flowStorageTimeSeriesList, "FlowStorage" );
        printFreeMemory("handleFlowAndStorageData() after writeRegularObsDataToFile() #1");
         
        printFreeMemory("handleFlowAndStorageData() before writeRegularObsDataToFile() #2");
        _dataMgr.writeRegularObsDataToFile( _percentFloodFlowTimeSeriesList, "PercentFloodFlow" );
        printFreeMemory("handleFlowAndStorageData() after writeRegularObsDataToFile() #2");
        
        
    }
    // --------------------------------------------------------------------------------------------
       
    private void incorporateConvertedHeightData(List derivedDischargeTimeSeriesList,
                                                List dischargeTimeSeriesList)
    {
        
        // if there are any derived time series that 
        // are not present in the regular discharge time series, then add them
        
        List timeSeriesToAddList = new ArrayList();
        
        int lastFoundIndex = 0;
        
        for (int i = 0; i < derivedDischargeTimeSeriesList.size(); i++)
        {
            RegularObsTimeSeries derivedDischargeTimeSeries =
                    (RegularObsTimeSeries) derivedDischargeTimeSeriesList.get(i);
            
            RegularObsTimeSeriesDescriptor derivedDescriptor =
                                           derivedDischargeTimeSeries.getDescriptor();
            
            String derivedLid = derivedDescriptor.getLid();
            
            boolean foundMatch = false;
            for (int j = lastFoundIndex; !foundMatch && j < dischargeTimeSeriesList.size(); j++)
            {
                RegularObsTimeSeries dischargeTimeSeries = 
                            (RegularObsTimeSeries) dischargeTimeSeriesList.get(j);
                
                RegularObsTimeSeriesDescriptor descriptor = dischargeTimeSeries.getDescriptor();
                
                if (derivedLid.equalsIgnoreCase(descriptor.getLid()) )
                {
                    foundMatch = true;
                    lastFoundIndex = j;
                }
           
                
            }
            
            if (! foundMatch) //it wasn't in the list, so add it to the list to add later
                        // we don't want to screw up the iteration through the dischargeTimeSeriesList
            {    
                timeSeriesToAddList.add(derivedDischargeTimeSeries);
            }
            
        }
        
        // add all of the needed converted time series
        for (int i = 0; i < timeSeriesToAddList.size(); i++)
        {
               RegularObsTimeSeries timeSeries = 
                   (RegularObsTimeSeries) timeSeriesToAddList.get(i);
               
               addTimeSeriesToOrderedList(dischargeTimeSeriesList, timeSeries);
        }
         
    }
    // --------------------------------------------------------------------------------------------
      
    private void handleTemperatureData()
    {
        
        // Note: Temperature data does NOT use caching.
        // It was found to be too complicated.
        // Your mileage may vary.
        String header = "PDCPreprocessor.handleTemperatureData(): ";
        CodeTimer timer = new CodeTimer();
        
        CodeTimer dbTimer = new CodeTimer();
        CodeTimer addMissingTimer = new CodeTimer();
        CodeTimer processTimer = new CodeTimer();
        CodeTimer fileWriterTimer = new CodeTimer();
        timer.start();
        
        dbTimer.start();
        _temperatureTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListFromDb( "Temperature", getNonPrecipEndTime(), _temperatureHours );
        dbTimer.stop(header + " reading from the db took ");
  
        
        addMissingTimer.start();
        
        addMissingTimeSeries( _temperatureTimeSeriesList, 
                               getRegularObsTimeSeriesDescriptor( "TA", "RZ", "Z", (short) 0,
                                    getNonPrecipEndTime(), _temperatureHours, true ), true  );
     
        addMissingTimeSeries( _temperatureTimeSeriesList,
                               getRegularObsTimeSeriesDescriptor( "TA", "RZ", "X", (short) 0,
                                    getNonPrecipDailyEndTime(), _temperatureHours, false ), false  );
        
        addMissingTimeSeries( _temperatureTimeSeriesList,
                               getRegularObsTimeSeriesDescriptor( "TA", "RZ", "N", (short) 0,
                                    getNonPrecipDailyEndTime(), _temperatureHours, false ), false );
        
        addMissingTimeSeries( _temperatureTimeSeriesList,
                               getRegularObsTimeSeriesDescriptor( "TD", "RZ", "Z", (short) 0,
                                    getNonPrecipEndTime(), _temperatureHours, true ), true );
        
        addMissingTimer.stop(header + "4 addMissingTimeSeries() calls took ");
       
        
        processTimer.start();
        
        printFreeMemory(header + "before processTemperatureTimeSeriesList");
        
        processTemperatureTimeSeriesList();
        processTimer.stop(header + " processing Temperature data took ");
        printFreeMemory(header + "after processTemperatureTimeSeriesList");
        
        _temperatureTimeSeriesList = null;
        System.gc();    
        printFreeMemory(header + "after  _temperatureTimeSeriesList = null");
        
        
        fileWriterTimer.start();
        _dataMgr.writeRegularObsDataToFile( _tempTimeSeriesList, "Temperature" );
        _tempTimeSeriesList = null;
        System.gc();    
        printFreeMemory(header + "after  _tempTimeSeriesList = null");
        
        _dataMgr.writeRegularObsDataToFile( _minTempTimeSeriesList, "TemperatureMin" );
        _minTempTimeSeriesList = null;
        System.gc();    
        printFreeMemory(header + "after  _minTempTimeSeriesList = null");
        
        
        _dataMgr.writeRegularObsDataToFile( _maxTempTimeSeriesList, "TemperatureMax" );
        _maxTempTimeSeriesList = null;
        System.gc();    
        printFreeMemory(header + "after  _maxTempTimeSeriesList = null");
        
        _dataMgr.writeRegularObsDataToFile( _dewPointTimeSeriesList, "DewPoint" );
        _dataMgr.writeRegularObsDataToFile( _deltaTempTimeSeriesList, "TemperatureDelta" );
        _dataMgr.writeRegularObsDataToFile( _deltaDewPointTimeSeriesList, "DewPointDelta" );
        fileWriterTimer.stop(header + " all the 6 file writes took ");
        
        
        _tempTimeSeriesList = null;
        _minTempTimeSeriesList = null;
        _maxTempTimeSeriesList = null;
        _dewPointTimeSeriesList = null;
        _deltaTempTimeSeriesList = null;
        _deltaDewPointTimeSeriesList = null;
        
        System.gc();
        
        timer.stop(header + " took ");
    }
    //  --------------------------------------------------------------------------------------------
    private void handleWindData()
    {
        
        String header = "PDCPreprocessor.handleWindData(): ";
        
        CodeTimer windTimer = new CodeTimer();
        windTimer.start();
        _windTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Wind",
                            getNonPrecipEndTime(), _windHours );
        
        windTimer.stop(header + "wind cache usage took ");   
        addMissingTimeSeries( _windTimeSeriesList, 
                            getRegularObsTimeSeriesDescriptor( "UD", "RZ", "Z", (short) 0,
                                    getNonPrecipEndTime(), _windHours, true ), true );
        addMissingTimeSeries( _windTimeSeriesList,
                            getRegularObsTimeSeriesDescriptor( "US", "RZ", "Z", (short) 0,
                                    getNonPrecipEndTime(), _windHours, true ), true );
        
        processWindTimeSeriesList();
        _windTimeSeriesList = null;
        System.gc();   
        
        _dataMgr.writeRegularObsDataToFile( _windSpeedTimeSeriesList, "WindSpeed" );
        _dataMgr.writeRegularObsDataToFile( _windDirectionTimeSeriesList, "WindDirection" );

        _windSpeedTimeSeriesList = null;
        _windDirectionTimeSeriesList = null;
        
        System.gc();    
    }
    
    // ----------------------------------------------------------------------------------------------
    
    private void handleSnowData()
    {
        String header = "PDCPreprocessor.handleSnowData(): ";
             
        CodeTimer snowTimer = new CodeTimer();
        snowTimer.start();
        _snowTimeSeriesList = _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Snow", getNonPrecipEndTime(), _snowHours );
        snowTimer.stop(header + "snow cache usage took ");   
          
        addMissingTimeSeries( _snowTimeSeriesList,
                getRegularObsTimeSeriesDescriptor( "SW", "RZ", "Z", (short) 0,
                        getNonPrecipEndTime(), _snowHours, true ), true );
        processSnowTimeSeriesList();
        _snowTimeSeriesList = null;
        System.gc();
           
        
        _dataMgr.writeRegularObsDataToFile( _snowWaterTimeSeriesList, "SnowWaterEquivalent" );   
        _dataMgr.writeRegularObsDataToFile( _deltaSnowWaterTimeSeriesList, "SnowWaterEquivalentDelta" );
 
        
    }
    
    private void handleWeatherData()
    {
        _weatherTimeSeriesList = 
             _dataMgr.getNonPrecipObsTimeSeriesListUsingCache( "Weather", 
                       getNonPrecipEndTime(), _weatherHours );
        
        addMissingTimeSeries( _weatherTimeSeriesList, 
                getRegularObsTimeSeriesDescriptor( "XR", "RZ", "Z", (short) 0,
                        getNonPrecipEndTime(), _weatherHours, true ), true );
        processWeatherTimeSeriesList();
        _weatherTimeSeriesList = null;
        System.gc();
        
        
        _dataMgr.writeRegularObsDataToFile( _relativeHumidityTimeSeriesList, "RelativeHumidity" );
        _relativeHumidityTimeSeriesList = null;
        System.gc();
     
    }
    
    // --------------------------------------------------------------------------------------------
    private void addAllMissingTimeSeries()
    {
        addMissingTimeSeries( _heightTimeSeriesList, getRegularObsTimeSeriesDescriptor( "HG", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _heightHours, true ), true );
        addMissingTimeSeries( _temperatureTimeSeriesList, getRegularObsTimeSeriesDescriptor( "TA", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(),  _temperatureHours, true ), true  );
        addMissingTimeSeries( _temperatureTimeSeriesList, getRegularObsTimeSeriesDescriptor( "TA", "RZ", "X", Short.parseShort( "0" ), getNonPrecipEndTime(), _temperatureHours, false ), false  );
        addMissingTimeSeries( _temperatureTimeSeriesList, getRegularObsTimeSeriesDescriptor( "TA", "RZ", "N", Short.parseShort( "0" ), getNonPrecipEndTime(), _temperatureHours, false ), false );
        addMissingTimeSeries( _temperatureTimeSeriesList, getRegularObsTimeSeriesDescriptor( "TD", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _temperatureHours, true ), true );
        addMissingTimeSeries( _windTimeSeriesList, getRegularObsTimeSeriesDescriptor( "UD", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _windHours, true ), true );
        addMissingTimeSeries( _windTimeSeriesList, getRegularObsTimeSeriesDescriptor( "US", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _windHours, true ), true );
        addMissingTimeSeries( _snowTimeSeriesList, getRegularObsTimeSeriesDescriptor( "SW", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _snowHours, true ), true );
        addMissingTimeSeries( _weatherTimeSeriesList, getRegularObsTimeSeriesDescriptor( "XR", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _weatherHours, true ), true );
        addMissingTimeSeries( _lakeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "LS", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _heightHours, true ), true );
        addMissingTimeSeries( _dischargeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "QR", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _heightHours, true ), true );
        addMissingTimeSeries( _dischargeTimeSeriesList, getRegularObsTimeSeriesDescriptor( "QT", "RZ", "Z", Short.parseShort( "0" ), getNonPrecipEndTime(), _heightHours, true ), true );
    }
    // --------------------------------------------------------------------------------------------
    
    private void addMissingTimeSeries( List obsTimeSeriesList,
                                       RegularObsTimeSeriesDescriptor descriptor,
                                       boolean isHourly, Set lidSet )
    {
        String header = "PDCPreprocessor.addMissingTimeSeries(): ";
        CodeTimer methodTimer = new CodeTimer();
        methodTimer.start();
        
        CodeTimer getLidSetTimer = new CodeTimer();
        CodeTimer createMissingTimer = new CodeTimer();
        CodeTimer addToOrderListTimer = new CodeTimer();
        
        Set partialLidSet = null;
        Set missingLidSet = new HashSet( lidSet );
        List missingLidList = null;
        String lid = null;
        
        descriptor.setMissing(true);
        
        if (
                 (descriptor.isMissing() == true) &&
                 (descriptor.getExtremum().equals("X") )
           )
        {
            System.out.println(header + "descriptor =  " + descriptor );
        } 
        
        getLidSetTimer.start();
        partialLidSet = getLidSetFromObsTimeSeriesList( 
                            obsTimeSeriesList,
                            descriptor.getPe(), 
                            descriptor.getExtremum() );
        
        getLidSetTimer.stop(header + " getLidSetFromObsTimeSeriesList() took");
        
        missingLidSet.removeAll( partialLidSet );
        
        missingLidList = new ArrayList( missingLidSet );
        
        
        
        for ( int i = 0; i < missingLidList.size(); i++ )
        {
            lid = (String) missingLidList.get( i );
            createMissingTimer.restart();
            RegularObsTimeSeries obsTimeSeries = 
                createMissingTimeSeries( lid, 
                                        new RegularObsTimeSeriesDescriptor( descriptor ), 
                                        isHourly );
            createMissingTimer.stop();
            
            addToOrderListTimer.restart();
            addTimeSeriesToOrderedList( obsTimeSeriesList, obsTimeSeries );
            addToOrderListTimer.stop();
        }   
        
        System.out.println(header + 
                "total elapsed time for createMissingTimeSeries calls = " +
                createMissingTimer.getElapsedTime() + " millis.");
        
        System.out.println(header + 
                "total elapsed time for addToOrderListTimer calls = " +
                addToOrderListTimer.getElapsedTime() + " millis.");
        
        methodTimer.stop(header + "took");
    }
    
    
    private void addMissingTimeSeries( List obsTimeSeriesList, 
                                       RegularObsTimeSeriesDescriptor descriptor,
                                       boolean isHourly )
    {
         addMissingTimeSeries(obsTimeSeriesList, descriptor, isHourly, _completeLidSet);
         
         return;
    }
    
    private void addMissingRiverTimeSeries( List obsTimeSeriesList,
                                            RegularObsTimeSeriesDescriptor descriptor,
                                            boolean isHourly )
    {
        addMissingTimeSeries(obsTimeSeriesList, descriptor, isHourly, _completeRiverStationLidSet);  
        
        return;
    }
    
    // -------------------------------------------------------------------------------------------- 
    private RegularObsTimeSeries createMissingTimeSeries( String lid,
                                                          RegularObsTimeSeriesDescriptor descriptor,
                                                          boolean isHourly )
    {
        RegularObsTimeSeries obsTimeSeries = null;
        
        descriptor.setLid( lid );
        
        descriptor.setMissing(true);
        
        if ( isHourly )
        {
            obsTimeSeries = new RegularObsTimeSeries( descriptor, 1, new HourlyTimeSlotPolicy() );
        }
        else
        {
            obsTimeSeries = new RegularObsTimeSeries( descriptor, 24, new DailyTimeSlotPolicy() );
        }
        
      
        
        /*
        
        for ( long indexTime = descriptor.getStartTime();
              indexTime < descriptor.getEndTime();
              indexTime += PDCPreprocessorDataMgr.MILLIS_PER_HOUR )
        {
            TimeValuePair timeValuePair = new TimeValuePair( indexTime, PDCPreprocessorDataMgr.MISSING );
            obsTimeSeries.addTimeValuePairIfBetterMatch( timeValuePair );
        }
        */
        
        return obsTimeSeries;
    }
    
   
    // --------------------------------------------------------------------------------------------
    private RegularObsTimeSeriesDescriptor getRegularObsTimeSeriesDescriptor ( String pe, 
                                                                               String ts, 
                                                                               String extremum,
                                                                               short duration,
                                                                               long endTime,
                                                                               int hoursToProcess,
                                                                               boolean adjustByOneHour)
    {
        
        int hoursBack = hoursToProcess;
        
        RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
        
        descriptor.setPe( pe );
        descriptor.setTs( ts );
        descriptor.setExtremum( extremum );
        descriptor.setDur( duration );
        descriptor.setEndTime( TimeHelper.roundTimeInMillisToNearestHour( endTime ) );
        
        if (adjustByOneHour)  //for normal descriptors, not for TA X or TA N
        {
            hoursBack -= 1;
        }
        
        descriptor.setStartTime( descriptor.getEndTime() - ( ( hoursBack ) *
                PDCPreprocessorDataMgr.MILLIS_PER_HOUR ) );
        
        return descriptor;
    }
    
    // --------------------------------------------------------------------------------------------
    public void addTimeSeriesToOrderedList(List obsTimeSeriesList, RegularObsTimeSeries newTimeSeries )
    {
        String header = "PDCPreprocessor.addTimeSeriesToOrderedList(): ";
        RegularObsTimeSeries obsTimeSeries = null;
        String newLid = newTimeSeries.getDescriptor().getLid();
        int index = -1;
        boolean found = false;
        
        Comparator comparator = new RegularObsTimeSeriesComparator();
        
        index = Collections.binarySearch(obsTimeSeriesList, newTimeSeries, comparator);      // -4
        
        // Add the non-existent item to the list
        if (index < 0) 
        {
            obsTimeSeriesList.add(-index-1, newTimeSeries);
        }

        else if (index == 0)
        {
            obsTimeSeriesList.add(index, newTimeSeries);
            System.out.println(header + "Item added at 0 index in the list");
        }

        else
        {
            System.out.println(header + "Item aleady in the list");
        }
    }

    // --------------------------------------------------------------------------------------------
    public void addTimeSeriesToOrderedList_orig( List obsTimeSeriesList, RegularObsTimeSeries newTimeSeries )
    {
        RegularObsTimeSeries obsTimeSeries = null;
        String newLid = newTimeSeries.getDescriptor().getLid();
        int index = -1;
        boolean found = false;
        
        for ( int i = 0; i < obsTimeSeriesList.size(); i++ )
        {
            obsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesList.get( i );
            
            if ( newLid.compareToIgnoreCase( obsTimeSeries.getDescriptor().getLid() ) < 0 )
            {
                index = i;
                found = true;
                break;
            }
        }
        
        if ( found )   //descriptor inserted at index 
        {
            obsTimeSeriesList.add( index, newTimeSeries );
        }
        else // descriptorlist is either empty or descriptor belongs at the end of the list
        {
            index = 0;
            obsTimeSeriesList.add( newTimeSeries );
        }
    }

/**
 * 
 * @param obsTimeSeriesList - list of ObsTimeSeries objects
 * @return Set of lids in the obsTimeSeriesList
 */    
    private Set getLidSetFromObsTimeSeriesList( List obsTimeSeriesList, String pe, String extremum )
    {
        Set lidSet = new HashSet();
        
        for ( int i = 0; i < obsTimeSeriesList.size(); i++ )
        {
            RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) obsTimeSeriesList.get( i );
            if ( ( obsTimeSeries.getDescriptor().getPe().equalsIgnoreCase( pe ) ) &&
               ( obsTimeSeries.getDescriptor().getExtremum().equalsIgnoreCase( extremum ) ) )
            {
                lidSet.add( obsTimeSeries.getDescriptor().getLid() );
            }
        }
        return lidSet;
    }
/**
 * Takes the temperature Timeseries List and extracts 24 hour min/max, Air temperature, and Dewpoint timeseries
 *
 */    
    private void processTemperatureTimeSeriesList()
    {
        String pe = null;
        RegularObsTimeSeriesDescriptor descriptor = null;
        
        if ( _temperatureTimeSeriesList != null )
        {
            for ( int i = 0; i < _temperatureTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _temperatureTimeSeriesList.get( i );

                descriptor = obsTimeSeries.getDescriptor();
                long startTime = descriptor.getStartTime();
                
                pe = descriptor.getPe();
                if ( ( pe.equalsIgnoreCase( "TA" ) ) && 
                        ( descriptor.getExtremum().equalsIgnoreCase( "X" ) ) )
                {
                   // descriptor.setStartTime( startTime + 24 * PDCPreprocessorDataMgr.MILLIS_PER_HOUR );
                    obsTimeSeries.setObsTimeSeriesDescriptor( descriptor );
                    _maxTempTimeSeriesList.add( obsTimeSeries );
                }
                else if ( ( pe.equalsIgnoreCase( "TA" ) ) &&
                        ( descriptor.getExtremum().equalsIgnoreCase( "N" ) ) )
                {
                  //  descriptor.setStartTime( startTime + 24 * PDCPreprocessorDataMgr.MILLIS_PER_HOUR );
                    obsTimeSeries.setObsTimeSeriesDescriptor( descriptor );

                    _minTempTimeSeriesList.add( obsTimeSeries );
                }
                else if ( ( pe.equalsIgnoreCase( "TA" ) ) &&
                        ( descriptor.getExtremum().equalsIgnoreCase( "Z" ) ) )
                {
                    _tempTimeSeriesList.add( obsTimeSeries );
                    _deltaTempTimeSeriesList.add( _dataMgr.getDeltaTimeSeries( obsTimeSeries ) );

                }
                else if ( pe.equalsIgnoreCase( "TD" ) )
                {
                    _deltaDewPointTimeSeriesList.add( _dataMgr.getDeltaTimeSeries( obsTimeSeries ) );

                   // descriptor.setStartTime( startTime + 24 * PDCPreprocessorDataMgr.MILLIS_PER_HOUR );
                    _dewPointTimeSeriesList.add( obsTimeSeries );
                    
                }
            }
        }
    }


/**
 * takes the Snow Timeseries list and extracts all Timeseries with a PE of "SW"
 *
 */
    private void processSnowTimeSeriesList()
    {
        String pe = null;
        RegularObsTimeSeriesDescriptor descriptor = null;
        
        if ( _snowTimeSeriesList != null )
        {
            for ( int i = 0; i < _snowTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _snowTimeSeriesList.get( i );
                
                descriptor = obsTimeSeries.getDescriptor();
                
                pe = descriptor.getPe();
                
                if ( pe.equalsIgnoreCase( "SW" ) )  // Snow Water equivalent
                {
                    _snowWaterTimeSeriesList.add( obsTimeSeries );
                    _deltaSnowWaterTimeSeriesList.add( _dataMgr.getDeltaTimeSeries( obsTimeSeries ) );
                }
            }
        }
    }

/**
 * processFlowStorageTimeSeriesList() - Processes the dischargeTimeSeriesList and retrieves the valid flowStorage 
 *                                      timeseries 
 *
 */
    private void processFlowStorageTimeSeriesList()
    {
        String pe = null;
        
        RegularObsTimeSeriesDescriptor descriptor = null;
        
        if ( _dischargeTimeSeriesList != null )
        {
            for ( int i = 0; i < _dischargeTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _dischargeTimeSeriesList.get( i );
                
                descriptor = obsTimeSeries.getDescriptor();
                
                pe = descriptor.getPe();
                
                if ( ( pe.equalsIgnoreCase( "QR" ) ) || ( pe.equalsIgnoreCase( "QT" ) ) )
                {
                    addTimeSeriesToOrderedList( _flowStorageTimeSeriesList, obsTimeSeries );
//                    _flowStorageTimeSeriesList.add ( obsTimeSeries );
                }
            }
        }
        
        if ( _lakeTimeSeriesList != null )
        {
            for ( int i = 0; i < _lakeTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _lakeTimeSeriesList.get( i );
                
                descriptor = obsTimeSeries.getDescriptor();
                
                pe = descriptor.getPe();
                
                if ( pe.equalsIgnoreCase( "LS" ) )
                {
                    addTimeSeriesToOrderedList( _flowStorageTimeSeriesList, obsTimeSeries );
//                    _flowStorageTimeSeriesList.add ( obsTimeSeries );
                }
            }
        }
    }
    
    
/**
* takes the Weather Timeseries list and extracts all Timeseries with a PE of "XR" (Relative Humidity)
*
*/
        private void processWeatherTimeSeriesList()
        {
            String pe = null;
            RegularObsTimeSeriesDescriptor descriptor = null;
            
            if ( _weatherTimeSeriesList != null )
            {
                for ( int i = 0; i < _weatherTimeSeriesList.size(); i++ )
                {
                    RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _weatherTimeSeriesList.get( i );
                    
                    descriptor = obsTimeSeries.getDescriptor();
                    
                    pe = descriptor.getPe();
                    
                    if ( pe.equalsIgnoreCase( "XR" ) )  // Relative Humidity
                    {
                        _relativeHumidityTimeSeriesList.add( obsTimeSeries );
                    }
                }
            }
        }

/**
 * Takes the Wind Timeseries and extracts the Wind Speed and Wind Direction Time Series
 *
 */
    private void processWindTimeSeriesList()
    {
        String pe = null;
        RegularObsTimeSeriesDescriptor descriptor = null;
        
        if ( _windTimeSeriesList != null )
        {
            for ( int i = 0; i < _windTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _windTimeSeriesList.get( i );
                
                descriptor = obsTimeSeries.getDescriptor();
                
                pe = descriptor.getPe();
                
                if ( pe.equalsIgnoreCase( "US" ) )  // Wind speed
                {
                    _windSpeedTimeSeriesList.add( obsTimeSeries );
                }
                else if ( pe.equalsIgnoreCase( "UD" ) )  // Wind direction
                {
                    _windDirectionTimeSeriesList.add( obsTimeSeries );
                }
            }
        }
        
    }

/**
 * Takes all of the Height Timeseries and divides it into the stage/pool and depth_above_flood_stage timeserieslist.
 * Also creates a flowstorage timeseries list
 *
 */    
    private void processHeightTimeSeriesList()
    {
        String header = "PDCPreprocessor.processHeightTimeSeriesList(): ";
        
        String pe = null;
        RegularObsTimeSeries derivedDischargeTimeSeries = null;
        
        if ( _heightTimeSeriesList != null )
        {
            for ( int i = 0; i < _heightTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _heightTimeSeriesList.get( i );
                pe = obsTimeSeries.getDescriptor().getPe();
                
                if (  
                        pe.equalsIgnoreCase( "HG" ) || 
                        pe.equalsIgnoreCase( "HP" ) ||
                        pe.equalsIgnoreCase( "HT" )
                   )
                {
                    _stagePoolTimeSeriesList.add( obsTimeSeries );
                    _depthAboveFloodStageTimeSeriesList.add( _dataMgr.getDepthAboveFSTimeSeries( obsTimeSeries ) );
                    derivedDischargeTimeSeries = _dataMgr.getDischargeTimeSeriesFromHeightTimeSeries( obsTimeSeries );
                    if ( derivedDischargeTimeSeries != null )
                    {
                        _heightDerivedDischargeTimeSeriesList.add( derivedDischargeTimeSeries );   
                    }
                }
            }
        }
    }

/**
 * Takes the floodStorageTimeSeriesList and creates the percentFloodFlowTimeSeriesList
 *
 */    
    private void processFloodFlowPercentageTimeSeriesList()
    {
        String pe = null;
        
        if ( _flowStorageTimeSeriesList != null )
        {
            for ( int i = 0; i < _flowStorageTimeSeriesList.size(); i++ )
            {
                RegularObsTimeSeries obsTimeSeries = (RegularObsTimeSeries) _flowStorageTimeSeriesList.get( i );
                if ( obsTimeSeries != null )
                {
                    //pe = obsTimeSeries.getDescriptor().getPe();
                       
                    RegularObsTimeSeries percentFloodFlowTimeSeries =
                        _dataMgr.getPercentFloodFlowTimeSeriesListFromFlowStorageTimeSeries( obsTimeSeries );
                    if ( percentFloodFlowTimeSeries != null )
                    {
                        _percentFloodFlowTimeSeriesList.add( percentFloodFlowTimeSeries );
                    }
                   
                }
            }
        }
    }
    
/**
 * Runs the PDC Precip Preprocessor (C code) and then takes the generated C code and creates the instant/six hour/24 hour
 * precip timeseries lists.
 *
 */    
    private void processPrecipData()
    {        
        if ( _precipHours > 0 )
        {
            _dataMgr.executePDCPrecipPP(getPrecipEndTime(), _precipHours);
            _instantPrecipTimeSeriesList = _dataMgr.getInstantaneousPrecipTimeSeriesList(getPrecipEndTime());
            
            _hourlyPrecipTimeSeriesList = _dataMgr.get1HourPrecipTimeSeriesListFromCFile();
            _threeHourPrecipTimeSeriesList = _dataMgr.get3HourPrecipTimeSeriesListFromCFile();
            _sixHourPrecipTimeSeriesList = _dataMgr.get6HourPrecipTimeSeriesListFromCFile();
            _twentyFourHourPrecipTimeSeriesList = _dataMgr.get24HourPrecipTimeSeriesListFromCFile();
        }
    }
    
    private void processPrecipDataWithCaching(int numHoursToPreprocess)
    {        
        PrecipListHolder holder = 
              _dataMgr.processPrecipDataWithCaching(getPrecipEndTime(), numHoursToPreprocess);
        
        _instantPrecipTimeSeriesList = holder.getInstantPrecipTimeSeriesList();
        _hourlyPrecipTimeSeriesList = holder.getOneHourPrecipTimeSeriesList();
        _threeHourPrecipTimeSeriesList = holder.getThreeHourPrecipTimeSeriesList();
        _sixHourPrecipTimeSeriesList = holder.getSixHourPrecipTimeSeriesList();
        _twentyFourHourPrecipTimeSeriesList = holder.getTwentyFourHourPrecipTimeSeriesList();
    }
    
    
  
/**
 * 
 * @param baseConnectionStringBuffer - Postgres database connection string
 * @param logDirName - Directory where the log file will be created
 * @param args - Command line arguments
 */    
    private void processCommandLineArguments( StringBuffer baseConnectionStringBuffer, 
                                              String[] args )
    {
        if ( args.length < 1 )
        {
            System.err.println("Usage: ohd.hseb.PDCPreprocessor.PDCPreprocessor connection_string logFileDir "); 
            System.exit(0);
        }    

        baseConnectionStringBuffer.append( args[0] );

        if ( args.length >= 2 )
        {
            if ( args[1].equalsIgnoreCase( "true" ) )
            {
                _debug = true;
            }
        }
    }
    
/**
 * Get's token values from .Apps_defaults
 *
 */    
    private void getAppsDefaults()
    {
        AppsDefaults ad = new AppsDefaults();
    //    String OS = System.getProperty("os.name").toLowerCase();

        _temperatureHours = ad.getInt( "pdc_temperature_hours", 168 ) + 24;
        _heightHours = ad.getInt( "pdc_height_hours", 168 );
        _snowHours = ad.getInt( "pdc_snow_hours", 168 ) + 24;
        _windHours = ad.getInt( "pdc_wind_hours", 168 );
        _weatherHours = ad.getInt( "pdc_weather_hours", 168 );
        _precipHours = ad.getInt( "pdc_precip_hours", 168 );
        _lowerWindow = ad.getInt( "pdc_lower_window", 2 );
        _upperWindow = ad.getInt( "pdc_upper_window", 2 );
        
       // if ( (OS.indexOf("nt") > -1)
       //         || (OS.indexOf("windows 2000") > -1)
       //         || (OS.indexOf("windows xp") > -1) )
       //  {
       //     _preprocessedFilePath = "D:\\data\\PDCPreprocessor\\data\\";
       //     _logDirName = "D:\\data\\PDCPreprocessor\\log\\";
      //   }
     //   else // assume it's unix
     //   {
            _preprocessedFilePath = ad.getToken( "pdc_pp_dir" ) + "/";
            _logDirName = ad.getToken( "pdc_pp_log_dir" ) + "/";
            
     //    }
    }
    
    public void testPDCFiles()
    {
       _dataMgr.testPDCFiles();    
        
    }
    
    public static void main( String[] args )
    {
        
        CodeTimer programTimer = new CodeTimer();
        programTimer.start();
        
        PDCPreprocessor pdcPreprocessor = new PDCPreprocessor();
        boolean debug = false;
        
        pdcPreprocessor.initialize( args );
        
        pdcPreprocessor.processObsData();
        
        pdcPreprocessor.disconnect();

        programTimer.stop("Finished PDCProcessor in");
    }
    
}
