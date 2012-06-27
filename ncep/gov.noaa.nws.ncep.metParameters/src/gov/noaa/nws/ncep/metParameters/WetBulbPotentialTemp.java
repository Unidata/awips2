package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class WetBulbPotentialTemp extends AbstractMetParameter implements 
		javax.measure.quantity.Temperature {

	public WetBulbPotentialTemp() {
		 super( UNIT );
	}

	@DeriveMethod
	AbstractMetParameter derive (PressureLevel p,  AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException, InvalidRangeException{
	    if ( p.hasValidValue() && t.hasValidValue() && d.hasValidValue() ){
		     Amount val = PRLibrary.prThwc(p, t, d );                     	
		     setValue(val);
	     }
	    else
	       setValueToMissing();
	    
		return this;
	}

//	AbstractMetParameter derive (SurfacePressure p,  AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException, InvalidRangeException{
//	    Amount val = PRLibrary.prThwc(p, t, d );                     	
//		this.setValue(val);
//		return this;
//	}	
	
}











