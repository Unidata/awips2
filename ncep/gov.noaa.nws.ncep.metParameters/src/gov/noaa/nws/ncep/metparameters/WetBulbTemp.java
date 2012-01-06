package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 public class WetBulbTemp extends AbstractMetParameter implements 
		javax.measure.quantity.Temperature {

	public WetBulbTemp() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( AirTemperature t, MixingRatio m, PressureLevel p ) throws InvalidValueException, NullPointerException, InvalidRangeException{
	
		if ( t.hasValidValue() && m.hasValidValue() && p.hasValidValue() ){
		       Amount val = PRLibrary.prTmwb(t, m, p );                     	
		       setValue(val);
	    }else
		       setValueToMissing();
	
		return this;
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( AirTemperature t, SurfaceMixingRatio m, SurfacePressure p ) throws InvalidValueException, NullPointerException, InvalidRangeException{
	
		if ( t.hasValidValue() && m.hasValidValue() && p.hasValidValue() ){
		       Amount val = PRLibrary.prTmwb(t, m, p );                     	
		       setValue(val);
	    }else
		       setValueToMissing();
	
		return this;
	}
	
}

















