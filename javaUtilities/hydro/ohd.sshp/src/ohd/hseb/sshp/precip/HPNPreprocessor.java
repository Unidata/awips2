package ohd.hseb.sshp.precip;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.TimeHelper;


public class HPNPreprocessor extends BaseArealPrecipPreprocessor implements ArealPrecipPreprocessor
{

    private static final String _className = "HPNPreprocessor";
    private String _hpeGridDirName = null;
 //   private final long _tolerenceTime = 5 * TimeHelper.MILLIS_PER_MINUTE;
    private long _millisBeforeTOTH = 0;
    private long _millisAfterTOTH = 0;
    
    public HPNPreprocessor(DataMgr dataMgr, Logger logger, String typeSourceCode)
    {

        initialize(dataMgr, logger, typeSourceCode);

        return;
    }

    // ----------------------------------------------------------------------------

    protected void readAppsDefaults()
    {
        String header = "HPNPreprocessor.readAppsDefaults(): ";      

        final int MIN_MINUTES = 0;
        final int MAX_MINUTES = 25;
        final int DEFAULT_MINUTES = 5;
        int minutesBefore = MIN_MINUTES;
        int minutesAfter  = MIN_MINUTES;
        
        AppsDefaults ad = new AppsDefaults();
        
        //determine grid directories
        _hpeGridDirName = ad.getToken("hpe_nowcast_dir", ".");
        _logger.log(header + "Directory to search for HPN grid files = " + _hpeGridDirName);
        
        minutesBefore = ad.getInt("sshp_hpn_minutes_before", DEFAULT_MINUTES);
        if (minutesBefore < MIN_MINUTES || minutesBefore > MAX_MINUTES)
            minutesBefore = DEFAULT_MINUTES;
        _logger.log(header + "Minutes to check before Top of the Hour = " + minutesBefore);

        minutesAfter = ad.getInt("sshp_hpn_minutes_after", DEFAULT_MINUTES);
        if (minutesAfter < MIN_MINUTES || minutesAfter > MAX_MINUTES)
            minutesAfter = DEFAULT_MINUTES;
        _logger.log(header + "Minutes to check after  Top of the Hour = " + minutesAfter);
        
        _millisBeforeTOTH = minutesBefore *  TimeHelper.MILLIS_PER_MINUTE;
        _millisAfterTOTH = minutesAfter *  TimeHelper.MILLIS_PER_MINUTE;
        
        //create the appropriate SimpleDateFormat objects
        _hpeXmrgFileDateFormat = new SimpleDateFormat("'ACC4kmH'yyyyMMddHHmm'z'");
        _hpeXmrgFileDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

    }
    
    // ----------------------------------------------------------------------------

    protected long getValidTime(long origTime)
    {
        String header = "HPNPreprocessor.getValidTime(): ";      

        // Add the before TOTH window time (ie 5 minutes) plus another one hour.
        long newTime = origTime + _millisBeforeTOTH + TimeHelper.MILLIS_PER_HOUR;
        
        // now trim it down to the top of hour
        long validTime = TimeHelper.truncateTimeInMillisToNearestHour(newTime, 1);
        String validTimeString = DbTimeHelper.getDateTimeStringFromLongTime(validTime);
        _logger.log(header + "validTime = " + validTimeString);

        return validTime;
    }
  
    // ----------------------------------------------------------------------------

    public void preprocessAll()
    {

        String header = "HPNPreprocessor.preprocessAll(): ";
        _logger.log(header + "Beginning Preprocessing");
        CodeTimer timer = new CodeTimer(_logger);

        //get the time for right NOW to find the nearest hour
        long currentRunTime = System.currentTimeMillis();
        String currentRunTimeString = DbTimeHelper.getDateTimeStringFromLongTime(currentRunTime);
        _logger.log(header + "currentRunTime   = " + currentRunTimeString);

        long topOfTheHour = TimeHelper.truncateTimeInMillisToNearestHour(currentRunTime, 1);
        String tothTimeString = DbTimeHelper.getDateTimeStringFromLongTime(topOfTheHour);
        _logger.log(header + "TopOfTheHourTime = " + tothTimeString);

        //handle the HPE ACC files 
        List hpeFileList = getHpnXmrgFileTimeList(topOfTheHour, _hpeGridDirName, _hpeXmrgFileDateFormat);

        //ACC4KmHyyyymmddhhMMz
        preprocessQpeFiles(hpeFileList, _hpeXmrgFileDateFormat, _hpeGridDirName);

        _logger.log(header + "Preprocessing complete.");

        return;

    } // preprocessAll

    // ----------------------------------------------------------------------------

    private List getHpnXmrgFileTimeList(long currentHourTime, String gridDirectoryString, SimpleDateFormat dateFormat)
    {
        String header = "HPNPreprocessor.getHpnXmrgFileTimeList(): ";

        File directory = new File(gridDirectoryString);   
        File[] fileArray = directory.listFiles();
        List timeList = new ArrayList();

        // for each file in the directory
        // read the filename and parse out its time
        // if it falls within the time window then check if it is the "best time" found so far
        // If it fails to parse, then we don't want it because
        // it is some other type of file that got in the directory somehow.       
        final long beginingOfTimeWindow =  currentHourTime - _millisBeforeTOTH;
        final long endOfTimeWindow =  currentHourTime + _millisAfterTOTH;
        final long beyondTimeWindow =  endOfTimeWindow + _millisBeforeTOTH + 1;

        _logger.log(header +
                " Valid Time Window = " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(beginingOfTimeWindow) +
                " through " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(endOfTimeWindow) );

        if (fileArray != null)
        {
            long bestTimeFound = beyondTimeWindow;
            for (int i = 0 ; i < fileArray.length; i++)
            {
                File file = fileArray[i];
                try 
                {   
                    Date date = dateFormat.parse(file.getName());
                    long gridTime = date.getTime();

                    if ( (gridTime >= beginingOfTimeWindow) && (gridTime <= endOfTimeWindow) )
                    {
                        _logger.log(header + " fileName = " + file.getName() );
                        _logger.log(header + " fileTime = " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(gridTime) );

                        bestTimeFound = getClosestToHourFileTime(currentHourTime, bestTimeFound, gridTime);
                    }
                }
                catch (ParseException e)
                {
                    //some other filename format was found,
                    //we don't want it
                }

            } //for list of file names

            if (bestTimeFound != beyondTimeWindow)
            {
                timeList.add( bestTimeFound );
                _logger.log(header + " bestTimeFound = " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(bestTimeFound) );
            }
            else
            {
                _logger.log(header +
                        " No ACC4KmHyyyymmddhhMMz files were found within the time window " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(beginingOfTimeWindow) +
                        " - " + DbTimeHelper.getDateTimeToMinutesStringFromLongTime(endOfTimeWindow) );
            }

        } //if ANY files found in the directory

        return timeList;

    } // End of getHpeXmrgFileTimeList

    // ----------------------------------------------------------------------------

    private Long getClosestToHourFileTime(long TopOfTheHourTime, long Time1, long Time2)
    {
        long timeDiffernce1 =  Math.abs(TopOfTheHourTime - Time1);
        long timeDiffernce2 =  Math.abs(TopOfTheHourTime - Time2);

        if (timeDiffernce1 < timeDiffernce2)
            return Time1;
        else
            return Time2;
    }

    // ----------------------------------------------------------------------------

    public static void main(String[] argArray)
    {            
        HPNPreprocessor preprocessor = null;
        String baseConnectionString = argArray[0];
        String typeSourceCode = "FL";

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

        //create a new HPN Preprocessor instance and process all ACC* XMRG files.
        preprocessor = new HPNPreprocessor(dataMgr, logger, typeSourceCode);
        preprocessor.preprocessAll();

        timer.stop("All of Processing for " + _className + " took ");    

        preprocessor.finalize();

        return;
    } // End of psvm

    // ----------------------------------------------------------------------------

} // End of HPNPreprocessor class
