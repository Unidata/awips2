package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class LCLParcelTemperature extends AbstractMetParameter implements javax.measure.quantity.Temperature {

	public LCLParcelTemperature() {
		super( UNIT );
	}

	@DeriveMethod		
	public LCLParcelTemperature derive(AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		if ( t.hasValidValue() && d.hasValidValue() ){
		     Amount val = PRLibrary.prTlcl(t, d );
		     setValue(val);
		}
		else
			setValueToMissing();
		
		return this;
	}

}
