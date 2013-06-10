package ohd.hseb.util.test;

import ohd.hseb.db.DbTimeHelper;
import junit.framework.TestCase;

public class TimeTest extends TestCase
{

    
    public void testTime()
    {
        long timeLong = 0;
        String resultDateTimeString = null;
        
        String epochDateTimeString = "1970-01-01 00:00:00";
        String epochDateTimeToMinutesString = "1970-01-01 00:00";
        
        String epochDateTimePlusOneMinuteString = "1970-01-01 00:01:00";
        
        String epochDateTimeToMinutesPlusOneMinuteString = "1970-01-01 00:01";
        
        
        //first moment in epoch to seconds
        timeLong = DbTimeHelper.getLongTimeFromDateTimeString(epochDateTimeString);
        assertEquals(0, timeLong);
           
        resultDateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(0);
        assertEquals(epochDateTimeString, resultDateTimeString);
        
        
        //first moment in epoch to minutes
        timeLong = DbTimeHelper.getLongTimeFromDateTimeToMinutesString(epochDateTimeToMinutesString);
        assertEquals(0, timeLong);
        
        
        resultDateTimeString = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(timeLong);
        assertEquals(epochDateTimeToMinutesString, resultDateTimeString);
       
        //one minute after the epoch, to seconds
        timeLong = DbTimeHelper.getLongTimeFromDateTimeString(epochDateTimePlusOneMinuteString);
        assertEquals(60000, timeLong);
     
        
        //one minute after the epoch, in minutes
        timeLong = DbTimeHelper.getLongTimeFromDateTimeToMinutesString(epochDateTimeToMinutesPlusOneMinuteString);
        assertEquals(60000, timeLong);
     
        
        
        return;
        
        
    }
    
    
    
}
