package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;

 public class StormMotionSpeed extends AbstractMetParameter implements
							javax.measure.quantity.Velocity {

	public StormMotionSpeed(){
		 super( UNIT );
	}
	
 	@DeriveMethod
	AbstractMetParameter derive ( EstStormDirectionUComp u, EstStormDirectionVComp v ) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
 		    Amount val = PRLibrary.prSped( u, v );
		    setValue ( val );
		}else
			setValueToMissing();
		return this;
	}
}



