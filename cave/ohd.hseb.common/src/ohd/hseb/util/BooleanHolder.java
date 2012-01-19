/*
 * Created on May 14, 2004
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 * 
 */
public class BooleanHolder
{
    private boolean _value;
    
    public BooleanHolder(boolean value)
    {
        setValue(value);
    }

	public void setValue(boolean value)
	{
		_value = value;
	}

	public boolean getValue()
	{
		return _value;
	}
}
