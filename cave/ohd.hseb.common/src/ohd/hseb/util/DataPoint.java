/*
 * Created on Jul 16, 2003
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author gobsc
 *
 * 
 */
public class DataPoint
{
	private double _x;
	private double _y;
	
	public DataPoint()
	{
		
	}
	
	public DataPoint(double x, double y)
	{
	   setX(x);
	   setY(y);	
	}

	public void setX(double x)
	{
		this._x = x;
	}

	public double getX()
	{
		return _x;
	}

	public void setY(double y)
	{
		this._y = y;
	}

	public double getY()
	{
		return _y;
	}
	
	public String toString()
	{
	    String outString = "( " + _x + ", " + _y + ")";
	    return outString;	
	}

}
