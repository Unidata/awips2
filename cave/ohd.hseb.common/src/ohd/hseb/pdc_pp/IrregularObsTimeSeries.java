package ohd.hseb.pdc_pp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 
 * @author Gautam Sood
 *
 */
public class IrregularObsTimeSeries
{
    private RegularObsTimeSeriesDescriptor _obsTimeSeriesDescriptor;
    private Map _timeValuePairMap = new HashMap();
    private boolean _valuePairListNeedsRefreshing = true;
    private List _timeValuePairList = new ArrayList();
    private List _dateTimeList = new ArrayList();
    private long _timeStepIntervalInMillis = 0;
    
/**
 * @param descriptor - RegularObsTimeSeriesDescriptor
 * @param timeStepIntervalInHours - Time step duration in hours.  e.g. hourly timeseries, 6 hourly timeseries, etc
 * @param timeSlotPolicy - Hourly or Daily time slot policy
 */
    public IrregularObsTimeSeries( RegularObsTimeSeriesDescriptor descriptor, long timeStepIntervalInHours )
    {
        _obsTimeSeriesDescriptor = descriptor;
        _timeStepIntervalInMillis = timeStepIntervalInHours * PDCPreprocessorDataMgr.MILLIS_PER_HOUR;
    }
    
    public IrregularObsTimeSeries( IrregularObsTimeSeries obsTimeSeries, long timeStepDurationInHours )
    {
        setObsTimeSeriesDescriptor( obsTimeSeries.getDescriptor() );
        setTopOfTheHourTimeValuePairMap( obsTimeSeries.getTopOfTheHourTimeValuePairMap() );
        setDateTimeList( obsTimeSeries.getDateTimeList() );
        _timeStepIntervalInMillis = timeStepDurationInHours * PDCPreprocessorDataMgr.MILLIS_PER_HOUR;
    }
    
    public String toString()
    {
        updateTimeValuePairList();

        StringBuffer stringBuffer = new StringBuffer();
        
        for ( int i = 0; i < _timeValuePairList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) _timeValuePairList.get( i );
            stringBuffer.append(timeValuePair);
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
    
    private void setDateTimeList( List dateTimeList )
    {
        _dateTimeList = dateTimeList;
    }

    private List getDateTimeList()
    {
        return _dateTimeList;
    }
    
    
    private void updateTimeValuePairList()
    {
        _timeValuePairList.clear();
        
        for ( int i = 0; i < _dateTimeList.size(); i++ )
        {
            Long dateTimeLong = (Long) _dateTimeList.get( i );
            TimeValuePair timeValuePair = (TimeValuePair) _timeValuePairMap.get( dateTimeLong );
            _timeValuePairList.add( timeValuePair );
        }

        _valuePairListNeedsRefreshing = false;
        return;
    }

    public void addTimeValuePairIfBetterMatch( TimeValuePair newTimeValuePair )
    {
        _valuePairListNeedsRefreshing = true;
        
        Long dateTimeLong = new Long( newTimeValuePair.getDateTime() );
        
        _timeValuePairMap.put( dateTimeLong, newTimeValuePair );
        addDateTimeToDateTimeList( dateTimeLong );
    }
    
    private void addDateTimeToDateTimeList( Long dateTimeLong )
    {
        int index = -1;
        boolean found = false;
        
        for ( int i = 0; i < _dateTimeList.size(); i++ )
        {
            Long dateTime = (Long) _dateTimeList.get( i );
            
            
            if ( dateTimeLong.longValue() < dateTime.longValue() )
            {
                index = i;
                found = true;
                break;
            }
        }
        
        if ( found )   //descriptor inserted at index 
        {
            _dateTimeList.add( index, dateTimeLong );
        }
        else // descriptorlist is either empty or descriptor belongs at the end of the list
        {
            index = 0;
            _dateTimeList.add( dateTimeLong );
        }
    }


    public void setObsTimeSeriesDescriptor( RegularObsTimeSeriesDescriptor obsTimeSeriesDescriptor )
    {
        _obsTimeSeriesDescriptor = obsTimeSeriesDescriptor;
    }

    public RegularObsTimeSeriesDescriptor getDescriptor()
    {
        return _obsTimeSeriesDescriptor;
    }

    public void setTimeValuePairList( List timeValueList )
    {
        _timeValuePairList.addAll( timeValueList );
    }

    public void addTimeValuePairList( List timeValueList )
    {
        _valuePairListNeedsRefreshing = true;
        for ( int i = 0; i < timeValueList.size(); i++ )
        {
            TimeValuePair timeValuePair = (TimeValuePair) timeValueList.get( i );
            addTimeValuePairIfBetterMatch( timeValuePair );
        }
    }

    public void setTopOfTheHourTimeValuePairMap( Map topOfTheHourTimeValuePairMap )
    {
        _timeValuePairMap = topOfTheHourTimeValuePairMap;
    }

    public Map getTopOfTheHourTimeValuePairMap()
    {
        return _timeValuePairMap;
    }

  
    /**
     * @return Returns the timeStepDurationInMillis.
     */
    public long getTimeStepIntervalInMillis()
    {
        return _timeStepIntervalInMillis;
    }
}