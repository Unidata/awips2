package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 
public class DryAirDensity extends AbstractMetParameter implements javax.measure.quantity.VolumetricDensity {

	public DryAirDensity() {
		super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive(PressureLevel p, AirTemperature t) throws InvalidRangeException, InvalidValueException, NullPointerException{
		Amount val =  PRLibrary.prDden( p, t )	;
		setValue(val);
		return this;
	}
}












