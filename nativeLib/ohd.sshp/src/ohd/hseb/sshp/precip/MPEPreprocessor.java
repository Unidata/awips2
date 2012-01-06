package ohd.hseb.sshp.precip;


import java.text.SimpleDateFormat;

import java.util.List;
import java.util.TimeZone;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

public class MPEPreprocessor extends BaseArealPrecipPreprocessor implements ArealPrecipPreprocessor
{

    private static final String _className = "MPEPreprocessor";

    public MPEPreprocessor(DataMgr dataMgr, Logger logger, String typeSourceCode)
    {

        initialize(dataMgr, logger, typeSourceCode);

        return;
    }

    // ----------------------------------------------------------------------------

    protected void readAppsDefaults()
    {
        String header = "MPEPreprocessor.readAppsDefaults()";      
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

        String header = "MPEPreprocessor.preprocessAll(): ";
        _logger.log(header + "Beginning Preprocessing");
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
        _logger.log(header + "Preprocessing complete.");


        return;

    } // preprocessAll

    // ----------------------------------------------------------------------------

    public static void main(String[] argArray)
    {            
        MPEPreprocessor preprocessor = null;
        String baseConnectionString = argArray[0];
        String typeSourceCode = "PM";
        
        //initialize the logging
        String logFileDir = argArray[1];
        // For now have the log filename use MAP not MPE
//        String logFilePath = logFileDir + "/" + _className + ".log";
        String logFilePath = logFileDir + "/" + "MAPPreprocessor" + ".log";
        Logger logger = new FileLogger(logFilePath, true, true);

        logger.log("Initializing " + _className);

        // start the clock on the timer
        CodeTimer timer = new CodeTimer(logger);
        timer.start();

        //initialize the database connection
        DataMgr dataMgr = new DataMgr(baseConnectionString, logger);

        //create a new MPEPreprocessor instance and preprocess all the unprocessed
        //XMRG files.
        preprocessor = new MPEPreprocessor(dataMgr, logger, typeSourceCode);
        preprocessor.preprocessAll();

        timer.stop("All of Processing for " + _className + " took ");    

        preprocessor.finalize();

        return;
    } // End of psvm

    // ----------------------------------------------------------------------------

} // End of MPEPreprocessor class
