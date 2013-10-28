/*
 * Created on Jul 13, 2004
 *
 * 
 */
package ohd.hseb.measurement;

/**
 * @author GobsC
 *
 * 
 */
public class MeasurementPoint
{
    private Measurement _xMeasurement;
    private Measurement _yMeasurement;
    
   
    public MeasurementPoint(Measurement measurement1, Measurement measurement2)
    {
        _xMeasurement = measurement1;
        _yMeasurement = measurement2;
    }
    
//  -------------------------------------------------------------------------------
   
    public MeasurementPoint(MeasurementPoint point, MeasuringUnit unit1, MeasuringUnit unit2)
    {
        _xMeasurement = point.getXMeasurement().getCopy(unit1);
        _yMeasurement = point.getYMeasurement().getCopy(unit2);
    }

// -------------------------------------------------------------------------------
	public void setXMeasurement(Measurement xMeasurement)
	{
		_xMeasurement = xMeasurement;
	}
//  -------------------------------------------------------------------------------

	public Measurement getXMeasurement()
	{
		return _xMeasurement;
	}
//  -------------------------------------------------------------------------------

	public void setYMeasurement(Measurement yMeasurement)
	{
		_yMeasurement = yMeasurement;
	}
//  -------------------------------------------------------------------------------

	public Measurement getYMeasurement()
	{
		return _yMeasurement;
	}
//  -------------------------------------------------------------------------------
   
    
}
