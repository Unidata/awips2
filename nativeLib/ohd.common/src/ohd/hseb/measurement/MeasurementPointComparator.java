/*
 * Created on Jul 15, 2004
 *
 * 
 */
package ohd.hseb.measurement;

import java.util.Comparator;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasurementPoint;
import ohd.hseb.measurement.MeasuringUnit;

/**
 * @author GobsC
 *
 * 
 */
public class MeasurementPointComparator implements Comparator
{

    private boolean _compareX = false;
    private boolean _ascendingOrder = true;

    public MeasurementPointComparator(boolean compareX, boolean ascendingOrder )
    {
        _compareX = compareX;
        _ascendingOrder = ascendingOrder;
    }
	
	public int compare(Object uhg1, Object uhg2)
	{
        int result = 0;

		MeasurementPoint point1 = (MeasurementPoint) uhg1;
        MeasurementPoint point2= (MeasurementPoint) uhg2;

        Measurement m1 = null;
        Measurement m2 = null;

        if (_compareX)
        {
            m1 = point1.getXMeasurement();
            m2 = point2.getXMeasurement();
        }
        else
        {
            m1 = point1.getYMeasurement();
            m2 = point2.getYMeasurement();
        }

        //pick a standard unit
        MeasuringUnit unit = m1.getUnit();


        double value1 = m1.getValue(unit);
        double value2 = m2.getValue(unit);

        if (value1 < value2)
        {
            result = -1;
        }
        else if (value1 == value2)
        {
            result = 0;
        }
        else // value1  > value2
        {
            result = 1;
        }

        if (! _ascendingOrder)
        {
            result *= -1;
        }

		return result;
	}

}
