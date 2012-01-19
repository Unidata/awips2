/*
 * Created on Oct 23, 2003
 *
 * 
 */
package ohd.hseb.util;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;

/**
 * @author GobsC
 *
 * 
 */
public class UnitValueMapper implements ValueMapper
{

    private MeasuringUnit _inputMeasuringUnit = null;
    private MeasuringUnit _outputMeasuringUnit = null;

    public UnitValueMapper(MeasuringUnit fromUnit, MeasuringUnit toUnit)
    {
        _inputMeasuringUnit = fromUnit;
        _outputMeasuringUnit = toUnit;
    }
    
    public  Measurement getResultMeasurement(Measurement inputMeasurement)
    {
        Measurement convertedMeasurement = 
                         Measurement.getConvertedCopy(inputMeasurement,
                                                     _inputMeasuringUnit);
        double keyValue = convertedMeasurement.getValue();
        
        Measurement measurement = new Measurement(keyValue, _inputMeasuringUnit);
        Measurement convertedOutputMeasurement = Measurement.getConvertedCopy(measurement, _outputMeasuringUnit);
        
        return convertedOutputMeasurement;
        
    }
    
    //--------------------------------------------------------
    
  

     public MeasuringUnit getInputMeasuringUnit()
     {
         return _inputMeasuringUnit;
     }

     //--------------------------------------------------------
    


     public MeasuringUnit getOutputMeasuringUnit()
     {
         return _outputMeasuringUnit;
     }
     //--------------------------------------------------------


}
