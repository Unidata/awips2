package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
 
public class MinDailyWeatherMapTemp extends AbstractMetParameter implements javax.measure.quantity.Temperature {

	public MinDailyWeatherMapTemp() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( Min6HrTemp t00x, Min6HrTemp t06x ) throws InvalidValueException, NullPointerException{
		if ( t00x.hasValidValue() && t06x.hasValidValue() ){
		      Amount val = PRLibrary.prDmin(t00x, t06x);
		      setValue(val);
		}else
			setValueToMissing();
		
		return this;
	}
	
}












