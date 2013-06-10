package ohd.hseb.model;

import ohd.hseb.measurement.AbsTimeMeasurement;


/**
 * @author Chip Gobs
 *
 * Contains a pair of Measurements
 */
public class ObservedForecastMeasurementPair
{
    private AbsTimeMeasurement _observedMeasurement;
    private AbsTimeMeasurement _forecastMeasurement;
    
   // ----------------------------------------------------------------------------------------------
    
    public ObservedForecastMeasurementPair(AbsTimeMeasurement observedMeasurement,
                                           AbsTimeMeasurement forecastMeasurement)
    {
        setObservedMeasurement(observedMeasurement);
        setForecastMeasurement(forecastMeasurement);
  
        return;
    }

    public void setObservedMeasurement(AbsTimeMeasurement observedMeasurement)
    {
        _observedMeasurement = new AbsTimeMeasurement(observedMeasurement);
    }

    public AbsTimeMeasurement getObservedMeasurement()
    {
        return _observedMeasurement;
    }

    public void setForecastMeasurement(AbsTimeMeasurement forecastMeasurement)
    {
        _forecastMeasurement = new AbsTimeMeasurement(forecastMeasurement);
    }

    public AbsTimeMeasurement getForecastMeasurement()
    {
        return _forecastMeasurement;
    }
    
    public int hashCode()
    {
        return _observedMeasurement.hashCode() + _forecastMeasurement.hashCode();
    }
    
    public String toString()
    {
        String outString = "Observed: " + _observedMeasurement.toString() + 
        				   "  Forecast: "  + _forecastMeasurement.toString();
        
    
        return outString;
    }
    
    // ---------------------------------------------------------------------------------------------- 
    
    
}
