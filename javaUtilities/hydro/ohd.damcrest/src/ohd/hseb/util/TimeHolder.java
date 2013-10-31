/*
 * Created on Oct 31, 2003
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author Chip Gobs
 *
 * This class is intended to be used as a way for
 * other objects to point to something that is always used
 * to maintain a time.  Multiple objects may wish to set
 * or get the time contained herein.  They can do this to
 * remain in synch about a particular time concept.
    Example:  various objects need to know what the model run time is
    without explicitly know who is maintaining it, so they just have a reference
    to the same timeholder, which they can access.
 **/
public class TimeHolder
{
    private long _time  = 0;

    public void setTime(long time)
    {
        _time = time;
    }

    public long getTime()
    {
        return _time;
    }
    
}
