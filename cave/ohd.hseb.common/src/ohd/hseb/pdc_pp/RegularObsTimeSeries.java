package ohd.hseb.pdc_pp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * 
 * @author Gautam Sood
 *
 */
public class RegularObsTimeSeries
{
    
    private String _mergerString = null;
    
    private TimeSlotPolicy _timeSlotPolicy = null;
    private RegularObsTimeSeriesDescriptor _obsTimeSeriesDescriptor;
    private Map _topOfTheHourTimeValuePairMap = new HashMap();
    private boolean _valuePairListNeedsRefreshing = true;
    private List _timeValuePairList = new ArrayList();
//    private List _dateTimeList = new ArrayList();
    private long _timeStepDurationInMillis = 0; //Time in millis between each value
    
    private static final int MILLIS_PER_HOUR = 60 * 60 * 1000;
    
/**
 * @param descriptor - RegularObsTimeSeriesDescriptor
 * @param timeStepDurationInHours - Time step duration in hours.  e.g. hourly timeseries, 6 hourly timeseries, etc
 * @param timeSlotPolicy - Hourly or Daily time slot policy
 */
    public RegularObsTimeSeries( RegularObsTimeSeriesDescriptor descriptor, long timeStepDurationInHours, TimeSlotPolicy timeSlotPolicy )
    {
        _obsTimeSeriesDescriptor = descriptor;
        _timeStepDurationInMillis = timeStepDurationInHours * PDCPreprocessorDataMgr.MILLIS_PER_HOUR;
        _timeSlotPolicy = timeSlotPolicy;
        
        setMergerString("UNTOUCHED");
    }
    
    public RegularObsTimeSeries( RegularObsTimeSeries origTimeSeries)
    {
        //Note: This is a deep copy, not a shallow one
        setObsTimeSeriesDescriptor(new RegularObsTimeSeriesDescriptor(origTimeSeries.getDescriptor()) );
        
        _timeSlotPolicy = origTimeSeries._timeSlotPolicy; //immutable object
        
        Map copiedMap = copyTimeOfTheHourTimeValuePairMap(origTimeSeries.getTopOfTheHourTimeValuePairMap());
        setTopOfTheHourTimeValuePairMap( copiedMap );
        
     //   setDateTimeList( origTimeSeries.getDateTimeList() );
        _timeStepDurationInMillis = origTimeSeries.getTimeStepIntervalInMillis();
        
        setMergerString(origTimeSeries.getMergerString());
    }
    
    private Map copyTimeOfTheHourTimeValuePairMap(Map origMap)
    {
        Map map = new HashMap();
       
        TimeValuePair origPair = null;
        TimeValuePair newPair = null;
        
        Collection valuesCollection = origMap.values();
        
        List valueList = new ArrayList(valuesCollection);
        
        for (int i = 0; i < valueList.size(); i++)
        {
            origPair = (TimeValuePair) valueList.get(i);    
            
            newPair = new TimeValuePair(origPair);
            map.put( new Long(newPair.getDateTime()), newPair);
            
        }
        return map;      
    }
    
    
    public String toString()
    {
        updateTimeValuePairList();

        StringBuffer stringBuffer = new StringBuffer();
        
        stringBuffer.append("Time step = " + _timeStepDurationInMillis/MILLIS_PER_HOUR + " hours: ");
        
        for ( int i = 0; i < _timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) _timeValuePairList.get( i );
            if (timeValuePair != null)
            {
                stringBuffer.append(timeValuePair.toString() + '|');
            }
        }
        return stringBuffer.toString();
    }
    
    public List getTimeValuePairList( boolean forceRefresh )
    {
        if ( ( _valuePairListNeedsRefreshing ) || ( forceRefresh ) )
        {
            updateTimeValuePairList();
        }
        
        return _timeValuePairList;
    }
    
    private void updateTimeValuePairList()
    {
        String header = "RegularObsTimeSeries.updateTimeValuePairList(): ";
        _timeValuePairList.clear();
        long endTime = _obsTimeSeriesDescriptor.getEndTime();
        long startTime = _obsTimeSeriesDescriptor.getStartTime();
        
       // System.out.println(header + "startTime = " + startTime + " endTime = " + endTime );

        for ( long time = startTime; time <= endTime; time+=_timeStepDurationInMillis )
        {
            Long dateTimeLong = new Long( time );
            TimeValuePair timeValuePair = (TimeValuePair) _topOfTheHourTimeValuePairMap.get( dateTimeLong );
            
            /*
            if (timeValuePair == null)
            {
                 System.out.println(header + " value being added is null for desc = " + _obsTimeSeriesDescriptor);  
            }
            */
            
            _timeValuePairList.add( timeValuePair );
            
        }

        _valuePairListNeedsRefreshing = false;
        return;
    }

    public void addTimeValuePairIfBetterMatch( TimeValuePair newTimeValuePair )
    {
        _valuePairListNeedsRefreshing = true;
        
        if (_timeSlotPolicy == null) //null is, in effect, a permissive policy, letting everything in
        {
            Long key = new Long(newTimeValuePair.getDateTime());
            _topOfTheHourTimeValuePairMap.put(key, newTimeValuePair);
        }

        else //when you have a real policy
        {
            Long topOfTheHourKey = _timeSlotPolicy.getSlotDateTimeInMillis(newTimeValuePair.getDateTime());

            if (topOfTheHourKey != null)
            {
                _timeSlotPolicy.insertTimeValuePairIfBetterMatch(_topOfTheHourTimeValuePairMap, newTimeValuePair);
            }
        }
    }
    
    /**
     * @param otherTimeSeries
     * This method takes the argument's time value pairs and uses them to 
     *  update the current object's _topOfTheHourTimeValuePairMap.
     * 
     */
    public void replaceMatchingTimeValuePairs(RegularObsTimeSeries otherTimeSeries)
    {
        String header = "RegularObsTimeSeries.replaceMatchingTimeValuePairs(): ";
        boolean done = false;
        
        this._valuePairListNeedsRefreshing = true;
        
        List otherTimeValuePairList = otherTimeSeries.getTimeValuePairList(true);
        if (otherTimeValuePairList == null)
        {
            System.err.println(header + " ERROR  - otherTimeValuePairList is null. ");
            done = true;          
        }
           
        for (int i = 0; ( !done && (i < otherTimeValuePairList.size()) ); i++)
        {
            TimeValuePair newTimeValuePair = (TimeValuePair) otherTimeValuePairList.get(i);
            Long key = null;
            
            if (newTimeValuePair != null)
            {
                key = new Long(newTimeValuePair.getDateTime());
                if (key != null)
                {
                    //if there is a value already here, then replace it
                    if (this._topOfTheHourTimeValuePairMap.get(key) != null)
                    {
                        this._topOfTheHourTimeValuePairMap.put(key, newTimeValuePair);   
                    }
                    else //value not already present, so leave it alone
                    {
                        
                    }
                }
                else
                {
                   throw new Error(header + "key is not allowed to be null");
                }
            }
       
        }
        
        return;
    }
   
    // ---------------------------------------------------------------------------------------
    public void replaceNonMatchingTimeValuePairs(RegularObsTimeSeries anotherTimeSeries)
    {
        String header = "RegularObsTimeSeries.replaceNonMatchingTimeValuePairs(): ";
        boolean done = false;
        
        this._valuePairListNeedsRefreshing = true;
        
        List otherTimeValuePairList = anotherTimeSeries.getTimeValuePairList(true);
        if (otherTimeValuePairList == null)
        {
            System.err.println(header + " ERROR  - otherTimeValuePairList is null. ");
            done = true;          
        }
           
        for (int i = 0; ( !done && (i < otherTimeValuePairList.size()) ); i++)
        {
            TimeValuePair newTimeValuePair = (TimeValuePair) otherTimeValuePairList.get(i);
            Long key = null;
            
            if (newTimeValuePair != null)
            {
                key = new Long(newTimeValuePair.getDateTime());
                if (key != null)
                {
                    //if there is no value already here, then put one in
                    if (this._topOfTheHourTimeValuePairMap.get(key) == null)
                    {
                        this._topOfTheHourTimeValuePairMap.put(key, newTimeValuePair);   
                    }
                    else //value already present, so leave it alone
                    {
                        
                    }
                }
                else
                {
                   throw new Error(header + "key is not allowed to be null");
                }
            }
       
        }
        
        return;
    }
    
    // ---------------------------------------------------------------------------------------
       
    public void removeTimeValuePairsBefore(long startTime)
    {
        List keyList  = new ArrayList(_topOfTheHourTimeValuePairMap.keySet());
            
        //remove any excess Hours from the front of the list.
        for (int i = 0; i < keyList.size() ; i++)
        {
           Object objectKey = keyList.get(i); 
           
           if (objectKey !=  null)
           {
               Long key = (Long) objectKey;
               
               if (key.longValue() < startTime)
               {
                   _topOfTheHourTimeValuePairMap.remove(key);          
               }   
           }
           
        }
        _valuePairListNeedsRefreshing = true;
    }
    
    // ---------------------------------------------------------------------------------------
    
    public void removeTimeValuePairsAfter(long endTime)
    {
        List keyList  = new ArrayList(_topOfTheHourTimeValuePairMap.keySet());
            
        //remove any excess Hours from the front of the list.
        for (int i = 0; i < keyList.size() ; i++)
        {
           Object objectKey = keyList.get(i); 
           
           if (objectKey !=  null)
           {
               Long key = (Long) objectKey;
               
               if (key.longValue() > endTime)
               {
                   _topOfTheHourTimeValuePairMap.remove(key);          
               }   
           }
           
        }
        _valuePairListNeedsRefreshing = true;
    }
   
    // ---------------------------------------------------------------------------------------
      
    public long getLatestTimeValue(long badValue)
    {
        String header = "RegularObsTimeSeries.getLatestTimeValue(): ";
        long latestTimeValue = badValue;
        List timeValuePairList  = getTimeValuePairList(false);
        
        if (timeValuePairList != null)
        {   
            
            if (timeValuePairList.size() > 0)
            {
                TimeValuePair pair = (TimeValuePair) timeValuePairList.get(timeValuePairList.size() - 1);
                if (pair != null)
                {
                    latestTimeValue = pair.getDateTime();
                   // System.out.println(header + "latestTimeValue = "
                   //         + latestTimeValue);
                }
                else
                {
                   // System.out.println(header + "latest TimeValuePair is null ");
                }
            }
            else
            {
               // System.out.println(header + "timeValuePairList.size() == 0 ");
            }
        }
        else
        {
          //  System.out.println(header + "timeValuePairList is null");
        }
        
        return latestTimeValue;
    }
 
    // ---------------------------------------------------------------------------------------
    
    public void setObsTimeSeriesDescriptor( RegularObsTimeSeriesDescriptor obsTimeSeriesDescriptor )
    {
        _obsTimeSeriesDescriptor = obsTimeSeriesDescriptor;
        _valuePairListNeedsRefreshing = true;
    }
    // ---------------------------------------------------------------------------------------
    
    public RegularObsTimeSeriesDescriptor getDescriptor()
    {
        return _obsTimeSeriesDescriptor;
    }
    // ---------------------------------------------------------------------------------------
    public void setStartTime(long startTime)
    {
        getDescriptor().setStartTime(startTime); 
        
        _valuePairListNeedsRefreshing = true;
        
    }
    // ---------------------------------------------------------------------------------------
    
    public void setEndTime(long endTime)
    {
        getDescriptor().setEndTime(endTime); 
        
        _valuePairListNeedsRefreshing = true;
        
    }
    // ---------------------------------------------------------------------------------------
    public void setTimeValuePairList( List timeValueList )
    {
        _timeValuePairList.addAll( timeValueList );
    }
    // ---------------------------------------------------------------------------------------
    
    public void addTimeValuePairList( List timeValueList )
    {
        String header = "RegularObsTimeSeries.addTimeValuePairList(): ";
        _valuePairListNeedsRefreshing = true;
        for ( int i = 0; i < timeValueList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValueList.get( i );
          //  System.out.println(header + " attempting to add timeValuePair = " + timeValuePair );
            addTimeValuePairIfBetterMatch( timeValuePair );
        }
    }
    // ---------------------------------------------------------------------------------------
    
    public void setTopOfTheHourTimeValuePairMap( Map topOfTheHourTimeValuePairMap )
    {
        _topOfTheHourTimeValuePairMap = topOfTheHourTimeValuePairMap;
    }
    // ---------------------------------------------------------------------------------------
    
    public Map getTopOfTheHourTimeValuePairMap()
    {
        return _topOfTheHourTimeValuePairMap;
    }
    // ---------------------------------------------------------------------------------------
    
 //   public void setDateTimeList( List dateTimeList )
 //   {
 //       _dateTimeList = dateTimeList;
 //   }

 //   public List getDateTimeList()
 //   {
 //       return _dateTimeList;
 //   }

     /**
     * @return Returns the timeStepDurationInMillis.
     */
    public long getTimeStepIntervalInMillis()
    {
        return _timeStepDurationInMillis;
    }

    /**
     * @param mergerString The mergerString to set.
     */
    public void setMergerString(String mergerString)
    {
        _mergerString = mergerString;
    }

    /**
     * @return Returns the mergerString.
     */
    public String getMergerString()
    {
        return _mergerString;
    }
}