package ohd.hseb.raxbase.util;

import java.util.Calendar;

public class DateManager
{
    public static String getDateTimeStringFromCalendar( Calendar calendar )
    {
        String dateTimeString = getDateStringFromCalendar( calendar );
        int hour = calendar.get( Calendar.HOUR_OF_DAY );
        String hourString = "" + hour;
        int minute = calendar.get( Calendar.MINUTE );
        String minuteString = "" + minute;
        int second = calendar.get( Calendar.SECOND );
        String secondString = "" + second;
        
        
        if ( hour < 10 )
        {
            hourString = "0" + hour;
        }
        if ( minute < 10 )
        {
            minuteString = "0" + minute;
        }
        if ( second < 10 ) 
        {
            secondString = "0" + second;
        }
        
        dateTimeString += " " + hourString + ":" + minuteString + ":" + secondString;
        
        return dateTimeString;
    }
    
    public static String getDateStringFromCalendar( Calendar calendar )
    {
        int year = calendar.get( Calendar.YEAR );
        int month = calendar.get( Calendar.MONTH ) + 1;
        int day = calendar.get( Calendar.DAY_OF_MONTH );
        String monthString = null;
        String dayString = null;
        
        if ( month < 10 )
        {
            monthString = "0" + month;
        }
        else
        {
            monthString = Integer.toString( month );
        }
        
        if ( day < 10 )
        {
            dayString = "0" + day;
        }
        else
        {
            dayString = Integer.toString( day );
        }
        
        String dateString = year + "-" + monthString + "-" + dayString;
        
        return dateString;
    }
}
