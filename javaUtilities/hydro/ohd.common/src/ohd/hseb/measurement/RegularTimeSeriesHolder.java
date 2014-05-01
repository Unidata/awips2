/*
 * Created on Oct 14, 2003
 *
 * 
 * */
package ohd.hseb.measurement;

import java.util.*;

/**
 * @author Chip Gobs
 *
 * This is a simple Holder class for a TimeSeries object
 * It will forward TimeSeriesEvents to the listeners.
 * It won't notify the listener with the same Receiver as the Event's source.
 */
public class RegularTimeSeriesHolder 
{
    private RegularTimeSeries _timeSeries;
    private java.util.List _listenerList = new ArrayList();
    
    private long _eventCount = 0;
    

    public void setTimeSeries(RegularTimeSeries timeSeries)
    {
        _timeSeries = timeSeries;
    }
    
    public void forwardEvent(TimeSeriesEvent event)
    {
        event.setEventId(_eventCount);
   
        notifyListeners(event);  
        
        _eventCount++;      
    }


    public RegularTimeSeries getTimeSeries()
    {
        return _timeSeries;
    }
  
    
     
    public void addListener(TimeSeriesListener listener)
    {
        _listenerList.add(listener);    
    }
  
    
    public void removeListener(TimeSeriesListener listener)
    {
        _listenerList.remove(listener);    
    }
  
    
    private void notifyListeners(TimeSeriesEvent event)
    {
       for (int i = 0 ; i < _listenerList.size(); i++)
       {
            TimeSeriesListener listener = (TimeSeriesListener) _listenerList.get(i);
         
            if (listener.getReceiver() != event.getSource())
            {
                listener.handleTimeSeriesEvent(event);
            }           
       }
    }
    
  
    
    
}
