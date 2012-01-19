/*
 * Created on Jul 15, 2004
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 *
 */
public class MaxMinData
{
    private double _maxValue;
    private double _minValue;
//  ---------------------------------------------------------------
    public MaxMinData(double maxValue, double minValue)
    {
        _maxValue = maxValue;
        _minValue = minValue;
    }
// ---------------------------------------------------------------
	public void setMaxValue(double maxValue)
	{
		_maxValue = maxValue;
	}
//  ---------------------------------------------------------------

	public double getMaxValue()
	{
		return _maxValue;
	}
//  ---------------------------------------------------------------

	public void setMinValue(double minValue)
	{
		_minValue = minValue;
	}
//  ---------------------------------------------------------------

	public double getMinValue()
	{
		return _minValue;
	}
//  ---------------------------------------------------------------

}
