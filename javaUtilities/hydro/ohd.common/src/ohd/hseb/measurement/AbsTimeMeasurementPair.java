package ohd.hseb.measurement;


/**
 * @author Chip Gobs
 *
 * Contains a pair of Measurements
 */
public class AbsTimeMeasurementPair
{
    private AbsTimeMeasurement _measurement1;
    private AbsTimeMeasurement _measurement2;
    
   // ----------------------------------------------------------------------------------------------
    
    public AbsTimeMeasurementPair(AbsTimeMeasurement measurement1, AbsTimeMeasurement measurement2)
    {
        setMeasurement1(measurement1);
        setMeasurement1(measurement2);
  
        return;
    }

    /**
     * @param measurement1 The measurement1 to set.
     */
    public void setMeasurement1(AbsTimeMeasurement measurement1)
    {
        _measurement1 = new AbsTimeMeasurement(measurement1);
    }

    /**
     * @return Returns the measurement1.
     */
    public AbsTimeMeasurement getMeasurement1()
    {
        return _measurement1;
    }

    /**
     * @param measurement2 The measurement2 to set.
     */
    public void setMeasurement2(AbsTimeMeasurement measurement2)
    {
        _measurement2 = new AbsTimeMeasurement(measurement2);
    }

    /**
     * @return Returns the measurement2.
     */
    public AbsTimeMeasurement getMeasurement2()
    {
        return _measurement2;
    }
    
    // ---------------------------------------------------------------------------------------------- 
    
    
}
