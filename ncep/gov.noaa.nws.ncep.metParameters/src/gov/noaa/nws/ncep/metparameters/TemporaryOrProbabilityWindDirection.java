package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

  public class TemporaryOrProbabilityWindDirection extends AbstractMetParameter implements javax.measure.quantity.Angle {

	public TemporaryOrProbabilityWindDirection() {
		 super( UNIT );
	} 
	
 	@DeriveMethod
	AbstractMetParameter derive ( WindDirectionUComp u, WindDirectionVComp v) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
 		     Amount windDrct = PRLibrary.prDrct( u , v );
		     setValue(windDrct);
		}else
			setValueToMissing();
		return this;
	}	
 }
