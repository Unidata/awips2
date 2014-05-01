/*
 * Created on Mar 16, 2004
 *
 * 
 */
package ohd.hseb.sshp;

import java.util.List;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.TimeSeriesFileManager;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;

/**
 * @author GobsC
 *
 * The purpose of this class is to check the 
 * most recent saved Sacramento Model states and
 * run the model from the last saved state if the
 * states have aged too much. 
 * 
 * The class also is responsible for creating "background" forecasts for situational awareness.
 */
public class SacStateUpdater
{
 
    private FileLogger _logger = null;
    
    private static final long _millisPerHour = 1000 * 60 * 60;
    private long _timeDiffMax = 0;
    
    private int _hoursForward = 0;

    private DataMgr _dataMgr = null;
    private String _backgroundFcstOutputDirectoryNameString = null;
    private static final String SSHP_BACKGROUND_FORECAST_OUTPUT_DIR = "sshp_background_forecast_output_dir";
    
    private static final int _defaultBackgroundFcstLengthInHours = 48;
    private int _backgroundFcstLengthInHours = _defaultBackgroundFcstLengthInHours;
    private static final String SSHP_BACKGROUND_FORECAST_FCST_LENGTH = "sshp_background_forecast_length";
    
    //  -------------------------------------------------------------
    
    public SacStateUpdater(DataMgr dataMgr, int expirationHours, int hoursForward, String logFileName)
    {
        _dataMgr = dataMgr;
        _timeDiffMax = expirationHours * _millisPerHour;
        
        _hoursForward = hoursForward;
        
        boolean keepFileOpen = true;
        boolean appendDateTime = true;
         
        _logger = new FileLogger(logFileName, keepFileOpen, appendDateTime);
        
        AppsDefaults ad = new AppsDefaults();
        
        _backgroundFcstOutputDirectoryNameString = ad.getToken(SSHP_BACKGROUND_FORECAST_OUTPUT_DIR);
        _backgroundFcstLengthInHours = ad.getInt(SSHP_BACKGROUND_FORECAST_FCST_LENGTH,
                                                 _defaultBackgroundFcstLengthInHours);
        
        return;
    }
    
    //  -------------------------------------------------------------
    public void finalize()
    {
       _logger.close();    
    }
    
    //  -------------------------------------------------------------
    
    public void updateAllStates()
    {
         /* 
           get list of locations to maintain (from SSHPConfig) 
         
           for each locationId
               time = _dataMgr.getTimeOfBestRecentState()
               if more than expirationHours old, then run model
               for that locationId

               save the SacState, and the prior runoff
         */  

         
         List<String> locationIdList = _dataMgr.loadAllLocationIdsToAutoUpdateSacStates();
        
         for (String locationId : locationIdList)     
         {           
              updateStatesByLocation(locationId, true);                  
         }
         
         if (locationIdList.size() == 0)
         {
             _logger.log("There are no locations for which auto process has been set.");  
         }


    } //update states
  
    // -------------------------------------------------------------
    public void updateStatesByLocation(String locationId, boolean saveStates)
    {
        String header = "updateStatesByLocation(): ";
        _logger.log(header + "Checking states for " + locationId); 
        
        SacSmaState bestState = _dataMgr.loadBestRecentSacState(locationId);
        
        
        if (bestState != null)
        {
            long bestStateTime = bestState.getValidTime();


            //long bestStateTime = _dataMgr.loadTimeOfBestRecentSacState(locationId);

            long currentTime = System.currentTimeMillis();
            currentTime = TimeHelper.truncateTimeInMillisToNearestHour(currentTime, 1);

            long timeDiff = (currentTime - bestStateTime);
            if (timeDiff > _timeDiffMax)
            {
                _logger.log("Updating states for " + locationId + "."); 

                StreamModel model = new StreamModel(locationId, 
                        RainfallRunoffModelType.SAC_SMA,
                        _dataMgr);

                model.setSacSmaStateDescriptor(new SacSmaStateDescriptor(bestState));
                model.setModelStartTime(bestState.getValidTime());                         

                long modelEndTime = currentTime + (_hoursForward * _millisPerHour);
                model.setModelEndTime(modelEndTime);                           


                _logger.log("Last saved state time = " + DbTimeHelper.getDateTimeStringFromLongTime(bestStateTime));
                _logger.log("Model run end time = " + DbTimeHelper.getDateTimeStringFromLongTime(modelEndTime));

                model.runModel();


                if (saveStates)
                {

                    _logger.log("Saving model states for " + locationId + ".");            
                    model.saveModelStates(_logger);


                    _logger.log("Saving computed runoff time series for " + locationId + "."); 
                    model.saveRunoffTimeSeries(_logger);
                }

            }
            else
            {
                _logger.log("Model States for " + locationId + " are up to date.");   
            }
        } //end if bestState != null
        else
        {
            _logger.log("No original model States for " + locationId + " are available.  Unable to update."); 
        }
    }
    
    // -------------------------------------------------------------
    public void updateAllBackgroundForecasts()
    {
        /* 
           get list of locations for which to compute forecasts

           for each locationId
               run the model and update the latest forecast stored on disk as a flat file
         */  


        List<String> locationIdList = _dataMgr.loadLocationIdsForForecasts();

        for (String locationId : locationIdList)
        {     
            _logger.log("Attempting to update a background forecast for " + locationId );  
            updateForecastByLocation(locationId);                  
        }


        if (locationIdList.size() == 0)
        {
            _logger.log("There are no locations for which to update a background forecast.");  
        }


    } //update states
  
    // -------------------------------------------------------------
    
    public void updateForecastByLocation(String locationId)
    {
        String header = "updateForecastByLocation(): ";
       // _logger.log(header + " search for valid model states for " + locationId); 

        StreamModel model = null;
        SSHPConfig config = _dataMgr.getSSHPConfig(locationId);
        
        long currentTime = System.currentTimeMillis();
        currentTime = TimeHelper.truncateTimeInMillisToNearestHour(currentTime, 1);

        boolean isStateAvailable = false;
        
        String modelPreference = config.getModelPref();
        
        if (modelPreference.equals("SAC-SMA"))
        {
            SacSmaState bestState = _dataMgr.loadBestRecentSacStateForForecasts(locationId);
            _logger.log(header + "bestState for  " + locationId + " = " + bestState); 
           
            if (bestState != null)
            { 
                isStateAvailable = true;
                
                model = new StreamModel(locationId, 
                                        RainfallRunoffModelType.SAC_SMA,
                                        _dataMgr);

                model.setSacSmaStateDescriptor(new SacSmaStateDescriptor(bestState));
                model.setModelStartTime(bestState.getValidTime());                         

                long modelEndTime = currentTime + (_backgroundFcstLengthInHours * _millisPerHour);
                model.setModelEndTime(modelEndTime);  
            }
        }
        else //API_MKC
        {
            AbsTimeMeasurement ffhMeasurement = _dataMgr.loadMostCurrentFfhMeasurement(locationId, 
                                                                                    config.getBasinId(),
                                                                                    currentTime);
            _logger.log(header + "Using KC-API for  " + locationId); 
            
            if (ffhMeasurement != null)
            {
                isStateAvailable = true;
                
                model = new StreamModel(locationId, 
                        RainfallRunoffModelType.API_MKC,
                        _dataMgr);

                model.setModelStartTime(ffhMeasurement.getTime());                         

                long modelEndTime = currentTime + (_backgroundFcstLengthInHours * _millisPerHour);
                model.setModelEndTime(modelEndTime);  
            }
        }
        
      
        if (isStateAvailable)
        {
            _logger.log("Updating Forecasts for " + locationId + "."); 

      
         //   _logger.log("Last saved state time = " + DbTimeHelper.getDateTimeStringFromLongTime(bestStateTime));
        //    _logger.log("Model run end time = " + DbTimeHelper.getDateTimeStringFromLongTime(modelEndTime));

            model.runModel();

            //for situational awareness applications (e.g. RiverMonitor)
            TimeSeriesFileManager fileManager = new TimeSeriesFileManager();
            RegularTimeSeries fcstStageTimeSeries = model.getForecastStageTimeSeriesHolder().getTimeSeries();

            //This feature can be turned off by not setting the token
            if (_backgroundFcstOutputDirectoryNameString != null)
            {
                String filePath = _backgroundFcstOutputDirectoryNameString + "/"
                + model.getLocationId() + "_fcst_stage.dat";
             
                
                _logger.log(header + "saving forecasts for " + locationId + " to " + filePath); 

                fileManager.saveRegularTimeSeries(fcstStageTimeSeries, 
                        filePath, 
                        MeasuringUnit.feet);
            }
            else
            {
                _logger.log(header + "_backgroundFcstOutputDirectoryNameString is null," +
                            " unable to save forecast timeseries to a file."); 
            }

        } //end isStateAvailable
        else
        {
            _logger.log(header + "No valid " + modelPreference +  " model states for " + locationId +
                                " are available.  Unable to create and save a background forecast."); 
        }
    }
    
    // -------------------------------------------------------------
    
    
    public static void main(String[] args)
    {
    
        if (args.length < 4)
        {
            System.err.println("Usage: ohd.hseb.sshp.StateUpdater connection_string expirationHours hoursForward logFilePath"); 
            System.exit(0);
        }    
    
    
        String baseConnectionString = args[0];
        
        String expirationHoursString = args[1];
        String hoursForwardString = args[2];
        String logFilePath = args[3];
        
        int expirationHours = Integer.parseInt(expirationHoursString);

        if (expirationHours <= 0)
        {
            System.err.println("expirationHours must be a number > 0");
            System.exit(0); 
        }
        
        int hoursForward = Integer.parseInt(hoursForwardString);
        // zero is ok
     
        //String logFilePath = logFileDir + "/SacStateUpdater.log";
        Logger logger = new FileLogger(logFilePath);
 
     
        DataMgr dataMgr = new DataMgr(baseConnectionString, logger);
     
        SacStateUpdater updater = new SacStateUpdater(dataMgr, expirationHours, hoursForward, logFilePath);
        
        updater.updateAllStates();
        
        updater.updateAllBackgroundForecasts();
        
        dataMgr.disconnect();
        
    } //end main()
    
    // -------------------------------------------------------------
   

}
