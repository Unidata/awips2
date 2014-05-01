/*
 * Created on Feb 9, 2004
 *
 * 
 */
package ohd.hseb.measurement;


/**
 * @author GobsC
 *
 *  This is a holder class for Measurement objects.
 */
public class MeasurementHolder
{
    
    private Measurement _measurement;

   
    public Measurement getMeasurement()
    {
        return _measurement;
    }

    
    public void setMeasurement(Measurement measurement)
    {
        _measurement = measurement;
    }

}
