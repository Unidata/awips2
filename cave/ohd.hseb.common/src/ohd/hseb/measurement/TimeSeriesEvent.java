/*
 * Created on Apr 23, 2004
 *
 * 
 */
package ohd.hseb.measurement;

/**
 * @author GobsC
 *
 *
 */
public class TimeSeriesEvent
{

    private String _message = null;
    private Object _source = null;
    private long _eventId = 0;
    
    public TimeSeriesEvent(String message, Object source)
    {
        _message = message;
        _source = source;
        
    }

    public void setMessage(String message)
    {
        _message = message;
    }

    public String getMessage()
    {
        return _message;
    }

    public void setSource(Object source)
    {
        _source = source;
    }

    public Object getSource()
    {
        return _source;
    }

    public void setEventId(long eventId)
    {
        _eventId = eventId;
    }

    public long getEventId()
    {
        return _eventId;
    }
    
    public String toString()
    {
        String outString = "Event #" + this.getEventId() + ": " + this.getMessage() ;
        
        return outString;
             
    }
    
    
}
