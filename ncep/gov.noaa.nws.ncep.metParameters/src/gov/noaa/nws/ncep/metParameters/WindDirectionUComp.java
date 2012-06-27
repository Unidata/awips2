package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class WindDirectionUComp extends AbstractMetParameter implements javax.measure.quantity.Velocity{

	public WindDirectionUComp() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive( WindSpeed w, WindDirection d ) throws InvalidValueException, NullPointerException{
		 if ( w.hasValidValue() && d.hasValidValue()){
		       Amount uWnd  = PRLibrary.prUwnd( w, d );
		       setValue( uWnd );
		 }else
			 setValueToMissing();
		       return this;
	}
}
