package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 public class WindSpeedComp extends AbstractMetParameter implements javax.measure.quantity.Angle {

	public WindSpeedComp() {
		 super( UNIT );
	} 
	@DeriveMethod
	AbstractMetParameter derive ( WindDirection wd, WindSpeed ws, WindCompDirection d) throws InvalidValueException, NullPointerException{
		if ( wd.hasValidValue() && ws.hasValidValue() && d.hasValidValue() ){     
		     Amount windDrct = PRLibrary.prWcmp(wd , ws , d );
		     setValue(windDrct);
		}else
			setValueToMissing();
		
		     return this;
	}
}
