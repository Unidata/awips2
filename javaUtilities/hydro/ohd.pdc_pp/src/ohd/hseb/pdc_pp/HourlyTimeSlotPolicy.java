package ohd.hseb.pdc_pp;

import java.util.Map;

import ohd.hseb.util.TimeHelper;

public class HourlyTimeSlotPolicy implements TimeSlotPolicy
{
    private long _minutesBeforeTopOfHour = PDCPreprocessorDataMgr._minutesBeforeTopOfHour;
    private long _minutesAfterTopOfHour = PDCPreprocessorDataMgr._minutesAfterTopOfHour;

    public Long getSlotDateTimeInMillis( long originalDateTimeInMillis )
    {
        return new Long( TimeHelper.roundTimeInMillisToNearestHour( originalDateTimeInMillis ) );
    }

    private boolean obsDateTimeInRange( long dateTime, long newTopOfTheHourDateTime )
    {
        long lowerWindowMinutes = newTopOfTheHourDateTime - ( _minutesBeforeTopOfHour * 60 * 1000 );
        long upperWindowMinutes = newTopOfTheHourDateTime + ( _minutesAfterTopOfHour * 60 * 1000 );
        boolean inRange = false;
        
        if ( ( dateTime >= lowerWindowMinutes ) && ( dateTime <= upperWindowMinutes ) )
        {
            inRange = true;
        }
        
        return inRange;
    }

    public void insertTimeValuePairIfBetterMatch( Map timeValuePairHashMap, TimeValuePair newTimeValuePair )
    {
        
        String header = "HourlyTimeSlotPolicy.insertTimeValuePairIfBetterMatch() ";
        
        long newTopOfTheHourDateTime = TimeHelper.roundTimeInMillisToNearestHour( newTimeValuePair.getDateTime() );
        long newObsDateTimeDiff = 0;
        long oldObsDateTimeDiff = 0;
        Long topOfHourKey = new Long( newTopOfTheHourDateTime );

        TimeValuePair timeValuePair = (TimeValuePair) timeValuePairHashMap.get( topOfHourKey );
        
        newObsDateTimeDiff = Math.abs( newTimeValuePair.getDateTime() - newTopOfTheHourDateTime );
        
        if ( timeValuePair == null )
        {
            if ( obsDateTimeInRange( newTimeValuePair.getDateTime(), newTopOfTheHourDateTime ) )
            {
                timeValuePairHashMap.put( topOfHourKey, newTimeValuePair );
          //       System.out.println(header + "null previous value, so put " + newTimeValuePair + " into the map");
            }
            else
            {
          //      System.out.println(header + " " + timeValuePair +  " is out of the time range, so won't insert into the map");
            }
        
        }
        else
        {   
            oldObsDateTimeDiff = Math.abs( timeValuePair.getDateTime() - newTopOfTheHourDateTime );
            
            if ( oldObsDateTimeDiff > newObsDateTimeDiff ) // if new valuepair is closer to top of the hour
            {
                timeValuePairHashMap.put( topOfHourKey, newTimeValuePair );
        //        System.out.println(header + "closer than previous value, so put " + newTimeValuePair + " into the map");
            }
            else
            {
                
       //         System.out.println(header + " " + newTimeValuePair + " is not closer in time than " + 
       //                          timeValuePair + " so won't insert into the map");
            }
        }
    }
}
