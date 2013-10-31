package ohd.hseb.pdc_pp;

import java.util.Map;

public interface TimeSlotPolicy
{
    
    public Long getSlotDateTimeInMillis( long originalDateTimeInMillis );
    
    public void insertTimeValuePairIfBetterMatch( Map timeValuePairHashMap, TimeValuePair timeValuePair );
   
}