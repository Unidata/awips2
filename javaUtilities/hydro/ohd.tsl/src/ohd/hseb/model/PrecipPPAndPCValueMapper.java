/*
 * Created on Oct 23, 2003
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.util.ValueMapper;

/**
 * @author GobsC
 *
 * 
 */
public class PrecipPPAndPCValueMapper implements ValueMapper
{

    private MeasuringUnit _inputMeasuringUnit = null;
    private MeasuringUnit _outputMeasuringUnit = null;
    private double _difference = 0;

    // --------------------------------------------------------------------------
    
    public PrecipPPAndPCValueMapper(double minValue1,
                                    double minValue2,
                                    MeasuringUnit unit)
    {
        _difference = minValue2 - minValue1;
        
        _inputMeasuringUnit = unit;
        _outputMeasuringUnit = unit;
     
        return;
    }
   
    // --------------------------------------------------------------------------
    
    public  Measurement getResultMeasurement(Measurement inputMeasurement)
    {
        Measurement convertedMeasurement = inputMeasurement.getCopy();
        double value = convertedMeasurement.getValue();
        value += _difference;
        
        convertedMeasurement.setValue(value);
        
        return convertedMeasurement;
        
    }
    
    // --------------------------------------------------------------------------
      
     public MeasuringUnit getInputMeasuringUnit()
     {
         return _inputMeasuringUnit;
     }

     // --------------------------------------------------------------------------
 
     public MeasuringUnit getOutputMeasuringUnit()
     {
         return _outputMeasuringUnit;
     }
     // --------------------------------------------------------------------------
     


}
