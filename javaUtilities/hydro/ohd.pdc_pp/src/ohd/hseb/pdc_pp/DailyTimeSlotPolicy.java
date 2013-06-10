package ohd.hseb.pdc_pp;

import java.util.Map;

public class DailyTimeSlotPolicy implements TimeSlotPolicy
{
    public Long getSlotDateTimeInMillis( long originalDateTimeInMillis )
    {
        
        originalDateTimeInMillis = truncateTimeInMillisToDay( originalDateTimeInMillis );  
        originalDateTimeInMillis += PDCPreprocessorDataMgr.MILLIS_PER_12_HOURS;
        
        return new Long( originalDateTimeInMillis );
    }

    public void insertTimeValuePairIfBetterMatch( Map timeValuePairHashMap, TimeValuePair newTimeValuePair )
    {
        long newTopOfTheHourDateTime = truncateTimeInMillisToDay( newTimeValuePair.getDateTime() );
        newTopOfTheHourDateTime += PDCPreprocessorDataMgr.MILLIS_PER_12_HOURS;
        
        long newObsDateTimeDiff = 0;
        long oldObsDateTimeDiff = 0;
        Long topOfHourKey = new Long( newTopOfTheHourDateTime );

        TimeValuePair timeValuePair = (TimeValuePair) timeValuePairHashMap.get( topOfHourKey );
        
        newObsDateTimeDiff = Math.abs( newTimeValuePair.getDateTime() - newTopOfTheHourDateTime );
        
        if ( timeValuePair == null )
        {
            timeValuePairHashMap.put( topOfHourKey, newTimeValuePair );
        }
        else
        {
            oldObsDateTimeDiff = Math.abs( timeValuePair.getDateTime() - newTopOfTheHourDateTime );
            
            if ( oldObsDateTimeDiff > newObsDateTimeDiff ) // if new valuepair is closer to top of the hour
            {
                timeValuePairHashMap.put( topOfHourKey, newTimeValuePair );
            }
        }
    }
    
    private long truncateTimeInMillisToDay( long dateTime )
    {
        long dateTimeReturnValue = dateTime;
        
        dateTimeReturnValue /= PDCPreprocessorDataMgr.MILLIS_PER_DAY;
        dateTimeReturnValue *= PDCPreprocessorDataMgr.MILLIS_PER_DAY;
        
        return dateTimeReturnValue;
    }

}
