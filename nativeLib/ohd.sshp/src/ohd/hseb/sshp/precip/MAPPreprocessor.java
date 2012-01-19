/*
 * Created on 5/24/04 
 * Last major change on 7/23/04
*/
 
package ohd.hseb.sshp.precip;


import ohd.hseb.sshp.DataMgr;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.measurement.*;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;


import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;


/**
 * @author Chip Gobs
 * This is the MAPPreprocessor for OB7.2.
 * It not only can process the traditional MPE best estimate files,
 * but can also process the RFC MOSAIC files.
 * 
 */
public class MAPPreprocessor
{
    private static final String _className = "MAPPreprocessor";
    private static final double _timeSeriesMissingValue = -9999.0;
  
    private DataMgr _dataMgr = null;
    
    private String _mpeQpeGridDirName = null;
    private String _rfcQpeGridDirName = null;
    private String _bestQpeDateFormatFromToken = null; 
    private String _sshpMapQpeToUseTokenValueString = null;
 
    SimpleDateFormat _localXmrgFileDateFormat = null;
    SimpleDateFormat _rfcXmrgFileDateFormat = null;
     
    
    // QPE usage
    private int _qpeUsage = MIXED_QPE;
    
    private static final int RFC_QPE = 0;
    private static final int MIXED_QPE = 1;
    private static final int LOCAL_BEST_QPE = 2;
    
    private static final String _rfcOnlyString = "RFC_ONLY";
    private static final String _mixedString = "MIXED";
    private static final String _localBestOnlyString = "LOCAL_BEST_ONLY";
  
    
    private List _areaIdList = null;
    private List _basinHrapHelperList  = null;
    private Logger _logger = null;
        
    // ----------------------------------------------------------------------------
    
    public MAPPreprocessor(DataMgr dataMgr, Logger logger)
    {
         _dataMgr = dataMgr;
        _logger = logger;
    
        readAppsDefaults();
 
        // load all the areas that are suitable for use as SSHP basins
        _areaIdList = _dataMgr.loadAllSshpBasinIds();
 
        //let the db manager add rows to ingest filter as needed
        _dataMgr.saveMapRowsToIngestFilterAsNeeded(_areaIdList);
            
        //load the basinHrapHelpers and return them in a List
        _basinHrapHelperList = getBasinHrapHelperList(_areaIdList);
   
          
         return;
    }
    
    // ----------------------------------------------------------------------------
    
    private void readAppsDefaults()
    {
        String header = "MAPPreprocessor.readAppsDefaults()";      
        AppsDefaults ad = new AppsDefaults();
   
        //determine grid directories
        _mpeQpeGridDirName = ad.getToken("rfcwide_xmrg_dir");
        _rfcQpeGridDirName = ad.getToken("gaq_xmrg_1hr_dir");
                     
        //create the appropriate SimpleDateFormat objects
        _bestQpeDateFormatFromToken = ad.getToken("st3_date_form");
        if (_bestQpeDateFormatFromToken != null )
        {
            if (_bestQpeDateFormatFromToken.equals("Ymd"))
            { 
                _localXmrgFileDateFormat = new SimpleDateFormat("'xmrg'yyyyMMddHH'z'");
                _localXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
               
            }
            else //mdY , this is the way .Apps_Defaults delivers it. Strange, huh?
            {
                _localXmrgFileDateFormat = new SimpleDateFormat("'xmrg'MMddyyyyHH'z'");
                _localXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            }
        }
        else //token was null
        {
            _localXmrgFileDateFormat = new SimpleDateFormat("'xmrg'MMddyyyyHH'z'");
            _localXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));   
        }

        // rfc mosaic file name format
        _rfcXmrgFileDateFormat = new SimpleDateFormat("'RFCMOSAIC01'yyyyMMddHH'z'");
        _rfcXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
   
          
        //default value to use both best local and rfc data,
        //with the rfc data favored
        _qpeUsage = MIXED_QPE;
        
        _sshpMapQpeToUseTokenValueString =  ad.getToken("sshp_map_qpe_to_use");
        
        if (_sshpMapQpeToUseTokenValueString != null)
        {
            if (_sshpMapQpeToUseTokenValueString.equalsIgnoreCase(_mixedString) )
            {
                _qpeUsage = MIXED_QPE;
                _logger.log(header + "_qpeUsage is MIXED_QPE");
            }
            else if (_sshpMapQpeToUseTokenValueString.equalsIgnoreCase(_localBestOnlyString) )
            {
                _qpeUsage = LOCAL_BEST_QPE;
                _logger.log(header + "_qpeUsage is LOCAL_BEST_QPE");
            }
            else if (_sshpMapQpeToUseTokenValueString.equalsIgnoreCase(_rfcOnlyString) )
            {
                _qpeUsage = RFC_QPE;
                _logger.log(header + "_qpeUsage is RFC_QPE");
            }
        }
    }
 
    // ----------------------------------------------------------------------------
    
    public void preprocessAll()
    {

        String header = "MAPPreprocessor.preprocessAll():";
        _logger.log("Beginning Preprocessing");
        CodeTimer timer = new CodeTimer(_logger);

        //find the latest posting time in the ArealObs table
        long latestPostingTime = _dataMgr.getLatestPostingTimeForMAPPreprocessor(); 
        
        
        String latestPostingTimeString = 
               DbTimeHelper.getDateTimeStringFromLongTime(latestPostingTime);
        
        _logger.log(header + "latestPostingTime = " + latestPostingTimeString);
        
        //handle the local best estimate qpe files
        if ((_qpeUsage == LOCAL_BEST_QPE) || (_qpeUsage == MIXED_QPE))
        {
     
            // based on that latestPostingTime, find the times of all the newer
            // Xmrg files from
            // the locally-run MPE
            List localFileTimeList = _dataMgr.getNewerBestQpeXmrgFileTimeList(latestPostingTime);

            preprocessQpeFiles(localFileTimeList, _localXmrgFileDateFormat, _mpeQpeGridDirName);
        }
        
        
        //handle the RFC files 
        if ((_qpeUsage == RFC_QPE) || (_qpeUsage == MIXED_QPE))
        {
           
            List rfcFileTimeList = _dataMgr.getNewerRfcXmrgFileTimeList(latestPostingTime,
                                                                        _rfcQpeGridDirName,
                                                                        _rfcXmrgFileDateFormat);
            //   // RFCMOSAIC012006060411
            preprocessQpeFiles(rfcFileTimeList,
                               _rfcXmrgFileDateFormat,
                               _rfcQpeGridDirName);

        }
        _logger.log("Preprocessing complete.");
        
        
        return;
    } // preprocessAll
    
    // ----------------------------------------------------------------------------
    private void preprocessQpeFiles(List timeList, DateFormat dateFormat, String baseDirectory)
    {
        String header = "preprocessQpeFiles(): ";
        // create an Xmrg file reader
        XmrgReader reader = new XmrgReader(_logger);
   
        _logger.log(header + "baseDirectory = " + baseDirectory );    
        
        
        // for each re-run hour of input
        for (int i = 0; i < timeList.size(); i++)
        {
            Long timeLong = (Long) timeList.get(i);
            long time = timeLong.longValue();
            
            String gridFileName = dateFormat.format(new Date(time));
            String fullGridPath = baseDirectory + "/" + gridFileName;
                
            String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);            
   
            XmrgGrid grid = reader.loadGrid(time, fullGridPath);
     
            if (grid.isValid())
            {
                _logger.log(header + "grid for time = " + timeString + " is VALID ");    
                
                // for each basin, process its data
                for (int j = 0; j < _basinHrapHelperList.size(); j++)
                {
                    BasinHrapHelper basinHrapHelper = (BasinHrapHelper) _basinHrapHelperList.get(j);
                
                    preprocess(basinHrapHelper, grid);
                }
            }
            else
            {
               // _logger.log(header + "grid for time = " + timeString + " is invalid");    
            }
        }        
        
    }
 
    // ----------------------------------------------------------------------------
    
    public void preprocess(BasinHrapHelper basinHrapHelper, XmrgGrid grid)
    {
       String areaId = basinHrapHelper.getBasinId(); 

       Measurement mapMeasurement = getMAPMeasurementFromGrid(basinHrapHelper, grid);
  
       _dataMgr.saveMapMeasurement(areaId, mapMeasurement, grid.getTime(), _logger);
  
       return;

    }
    
    // ----------------------------------------------------------------------------
    private Measurement getMAPMeasurementFromGrid(BasinHrapHelper basinHrapHelper, XmrgGrid grid)
    {
        String header = "MAPPreprocessor.getMAPMeasurementFromGrid(): ";
        MeasuringUnit precipUnit = MeasuringUnit.inches;
        double mapValue = -1.0;
        double binValue = 0;
        double totalValue = 0;
        int validValueCount = 0;
        
        // CHANGE 8/23/05    
        _logger.log(header + " for basinId =  " + basinHrapHelper.getBasinId());
        
        int rowCount = basinHrapHelper.getRowCount();

        boolean gridAccessProblem = false;

        //for each row
        for (int i = 0; (i < rowCount && !gridAccessProblem) ; i++)
        {
            int hrapRow = basinHrapHelper.getBasinHrapRow(i);
            
            // for each column
            for (int hrapCol = basinHrapHelper.getBasinHrapBegColumn(i); 
                       ( (hrapCol <= basinHrapHelper.getBasinHrapEndColumn(i)) && 
                        (!gridAccessProblem));
                    hrapCol++)
            {
                //yank out the bin value at that point
                binValue = grid.getValue(hrapRow, hrapCol);
                if (binValue >= 0.0)
                {
                    totalValue += binValue;
                    validValueCount++;       
                }
                else if (binValue == XmrgGrid.OFF_GRID_VALUE)
                {
                    _logger.log(header + "Problem with basinId = " + basinHrapHelper.getBasinId() +
                            " attempting to access out of bounds grid location row = " + 
                            hrapRow + " and column = " + hrapCol + ".");
                    gridAccessProblem = true;
            
                }
            }           
        }
        
        
        // if some kind of failure occured
        Measurement mapMeasurement = null;
        if  ( (validValueCount < 1) || (gridAccessProblem) )
        {
            mapValue = _timeSeriesMissingValue;  
            mapMeasurement = new Measurement(mapValue, precipUnit);
            mapMeasurement.setIsMissing(true);  
           // System.out.println(header + "for " + basinHrapHelper.getBasinId() + " totalValue = " + totalValue + " validValueCount = " + validValueCount);
        }
        else //success occured, so create a decent measurement
        {   
            //System.out.println(header + "for " + basinHrapHelper.getBasinId() + " totalValue = " + totalValue + " validValueCount = " + validValueCount);
            mapValue = totalValue / validValueCount; 
            if (mapValue < 0.005)
            {
                mapValue = 0.0;            
            }
            mapMeasurement = new Measurement(mapValue, precipUnit);   
        }
        
         
        
        return mapMeasurement;   
        
    }  
            
    // ----------------------------------------------------------------------------
    // ----------------------------------------------------------------------------
        
    // ----------------------------------------------------------------------------
    private List getBasinHrapHelperList(List areaIdList)
    {
        List basinHrapHelperList = new ArrayList();
        for (int i = 0; i < areaIdList.size(); i++)
        {
            String areaId = (String ) areaIdList.get(i);
            
            BasinHrapHelper helper = _dataMgr.loadBasinHrapHelper(areaId);
            
            if (helper != null)
            {
                //_logger.log(helper.toString());
 
                basinHrapHelperList.add(helper);
            }
        }    
        
        
        return basinHrapHelperList;
        
    } 
    // ----------------------------------------------------------------------------
 
    public void finalize()
    {
        try
        {
            _dataMgr.disconnect();
            _logger.close();    
        }
        catch (Throwable t)
        {
            System.err.println(t.getMessage());
        }
        
       
    }
    // ----------------------------------------------------------------------------
    
    public static void main(String[] argArray)
    {            
         MAPPreprocessor preprocessor = null;
         String baseConnectionString = argArray[0];
 
         //initialize the logging
         String logFileDir = argArray[1];
         String logFilePath = logFileDir + "/" + _className + ".log";
         Logger logger = new FileLogger(logFilePath, true, true);
         
         logger.log("Initializing " + _className);
  
         // start the clock on the timer
         CodeTimer timer = new CodeTimer(logger);
         timer.start();
         
         //initialize the database connection
         DataMgr dataMgr = new DataMgr(baseConnectionString, logger);
        
         //create a new MAPPreprocessor instance and preprocess all the unprocessed
         //XMRG files.
         preprocessor = new MAPPreprocessor(dataMgr, logger);
         preprocessor.preprocessAll();
        
         timer.stop("All of Processing for " + _className + " took ");    

         preprocessor.finalize();
         
         return;
    }            
    // ----------------------------------------------------------------------------
           
} //MAPPreprocessor
