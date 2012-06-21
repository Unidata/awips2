package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
 
public class MaxDailyWeatherMapTemp extends AbstractMetParameter implements javax.measure.quantity.Temperature {

	public MaxDailyWeatherMapTemp() {
		super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( Max6HrTemp t00x, Max6HrTemp t06x, MaxMidnightTemp tdxc ) throws InvalidValueException, NullPointerException{
	          Amount val = PRLibrary.prDmax(t00x, t06x, tdxc);
	          setValue(val);
		      return this;
	}
	
}












