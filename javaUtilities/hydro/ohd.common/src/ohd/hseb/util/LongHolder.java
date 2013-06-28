/*
 * Created on May 21, 2004
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 * Holds a LongValue.  This is needed because Long is immutable.
 */
public class LongHolder
{
    private long _value;
    
    public LongHolder(long value)
    {
        _value = value;    
    }

	public void setValue(long value)
	{
		_value = value;
	}

	public long getValue()
	{
		return _value;
	}
    

}
