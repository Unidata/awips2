/*
 * Created on May 21, 2004
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 * Holds a double value  This is needed because Double is immutable and primitives are passed by
 * value to methods.
 */
public class DoubleHolder
{
    private double _value;
    
    public DoubleHolder(double value)
    {
        _value = value;    
    }

	public void setValue(double value)
	{
		_value = value;
	}

	public double getValue()
	{
		return _value;
	}
    

}
