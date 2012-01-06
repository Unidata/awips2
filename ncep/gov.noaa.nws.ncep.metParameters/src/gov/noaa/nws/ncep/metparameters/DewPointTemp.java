package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class DewPointTemp extends AbstractMetParameter implements
					javax.measure.quantity.Temperature {
	
	public DewPointTemp() {
		super( UNIT );
	}
	
	@DeriveMethod
	public DewPointTemp derive( MixingRatio m, PressureLevel p ) throws InvalidRangeException, InvalidValueException, NullPointerException {
	     if ( m.hasValidValue() && p.hasValidValue()  ){
		        Amount theDewpointTemperatureAmount = PRLibrary.prDwpt(m , p );
		        this.setValue(theDewpointTemperatureAmount);
	     }else
	    	    setValueToMissing();
		return this;
	}

	@DeriveMethod
	public DewPointTemp derive( SurfaceMixingRatio m, SurfacePressure p ) throws InvalidRangeException, InvalidValueException, NullPointerException {
	     if ( m.hasValidValue() && p.hasValidValue()  ){
		        Amount theDewpointTemperatureAmount = PRLibrary.prDwpt(m , p );
		        this.setValue(theDewpointTemperatureAmount);
	     }else
	    	    setValueToMissing();
		return this;
	}

	@DeriveMethod
	public DewPointTemp derive ( AirTemperature t, DewPointDepression dp) throws InvalidValueException, NullPointerException{
		if ( t.hasValidValue() && dp.hasValidValue() ){
		           Amount theDewpointTemperatureAmount = PRLibrary.prDwdp(t, dp);
		           this.setValue(theDewpointTemperatureAmount);
		}else
			setValueToMissing();
		
		return this;
	}
	
	@DeriveMethod
     public DewPointTemp derive ( AirTemperature t, RelativeHumidity rh) throws InvalidValueException, NullPointerException, InvalidRangeException{
    	      if ( t.hasValidValue() && rh.hasValidValue() ){
		           Amount dewpointAmount = PRLibrary.prRhdp(t, rh);
    	           this.setValue(dewpointAmount);
    	      }else
    	    	  setValueToMissing();
    	 return this;
     }
}
 
















