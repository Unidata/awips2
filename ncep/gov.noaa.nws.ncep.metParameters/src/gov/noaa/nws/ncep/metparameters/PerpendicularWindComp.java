package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class PerpendicularWindComp extends AbstractMetParameter implements javax.measure.quantity.Angle {

	public PerpendicularWindComp() {
		 super( UNIT );
	} 
	
	@DeriveMethod
	AbstractMetParameter derive ( WindDirection wd, WindSpeed ws,WindCompDirection d) throws InvalidValueException, NullPointerException, InvalidRangeException{
		Amount windDrct = PRLibrary.prWnml( wd , ws , d );
		this.setValue(windDrct);
		return this;
	}
}
