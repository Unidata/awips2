package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import javax.measure.quantity.Dimensionless;

 public class FosbergFireWxIndex extends AbstractMetParameter implements Dimensionless {

	public FosbergFireWxIndex(){
		super( UNIT );
	}
		
	@DeriveMethod
     AbstractMetParameter derive ( AirTemperature t, RelativeHumidity r, WindSpeed w) throws InvalidValueException, NullPointerException{
		if ( t.hasValidValue() &&  r.hasValidValue() && w.hasValidValue() ) {
			Amount val = PRLibrary.prFosb( t, r, w );
			setValue( val );
		}
		else{
			     setValueToMissing();
		 }
		return this;
	}
 }












