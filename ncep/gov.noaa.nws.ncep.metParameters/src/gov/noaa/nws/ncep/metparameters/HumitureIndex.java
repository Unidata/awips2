package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class HumitureIndex extends AbstractMetParameter implements javax.measure.quantity.Dimensionless {

	public HumitureIndex(){
		super( UNIT );
	}
	@DeriveMethod
	public HumitureIndex derive( AirTemperature t, DewPointTemp dt ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		if ( t.hasValidValue() && dt.hasValidValue() ){
		      Amount hmtrAmount = PRLibrary.prHmtr(t, dt);
		      setValue(hmtrAmount);
		}else
			setValueToMissing();
		return this;
	}
	
}
