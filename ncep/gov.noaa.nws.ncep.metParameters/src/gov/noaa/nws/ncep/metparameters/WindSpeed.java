package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;

 public class WindSpeed extends AbstractMetParameter implements
							javax.measure.quantity.Velocity {

	public WindSpeed(){
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( WindDirectionUComp u, WindDirectionVComp v ) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
		   Amount val = PRLibrary.prSped( u, v );
		   setValue ( val );
		}else
			setValueToMissing();
		
		return this;
	}
//	@DeriveMethod
//	public AbstractMetParameter getWindSpeedFromWindBarb( WindBarb wb ) {
//		return wb.getWindSpeed();
//	}
}