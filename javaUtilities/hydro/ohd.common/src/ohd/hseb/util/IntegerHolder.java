/*
 * Created on May 21, 2004
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 * Holds an int value.  This is needed because Integer is immutable and int is passed by value to
 * methods.
 */
public class IntegerHolder
{
    private int _value;
    
    public IntegerHolder(int value)
    {
        _value = value;    
    }

	public void setValue(int value)
	{
		_value = value;
	}

	public int getValue()
	{
		return _value;
	}
	
	public void change(int amount)
	{
	    _value += amount;
	}
	
	public void increment()
	{
	    change(1);
	}
    
	public void decrement()
	{
	    change(-1);
	}

}
