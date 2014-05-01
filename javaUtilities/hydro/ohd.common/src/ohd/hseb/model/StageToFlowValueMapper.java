/*
 * Created on Oct 22, 2003
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
 * This implementation of ValueMapper maps river stages to discharge.
 */
public class StageToFlowValueMapper implements ValueMapper
{
	private MeasuringUnit _inputMeasuringUnit = MeasuringUnit.feet;
    private MeasuringUnit _outputMeasuringUnit = MeasuringUnit.cfs;
    
    
	private RatingCurve _ratingCurve = null;

	//--------------------------------------------------------

	public StageToFlowValueMapper(RatingCurve ratingCurve)
	{
		_ratingCurve = ratingCurve;
	}
    //--------------------------------------------------------
	
    
    public Measurement getResultMeasurement(Measurement inputMeasurement)
    {
        Measurement resultMeasurement = null;
        
         Measurement convertedMeasurement = 
                     Measurement.getConvertedCopy(inputMeasurement,
                                                 _inputMeasuringUnit);
        double keyValue = convertedMeasurement.getValue();
       
        double resultValue = 0;
        if (_ratingCurve != null)
        {
            resultValue = _ratingCurve.getDischargeFromStage(keyValue);
            resultMeasurement = new Measurement(resultValue,
                                                _outputMeasuringUnit);
        }
        
        return resultMeasurement;
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

}  // StageToFlowValueMapper
