package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class SatVaporPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure {
	public SatVaporPressure() {
		 super( UNIT );
	}

	@DeriveMethod		
	public SatVaporPressure derive(  AirTemperature t ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		if ( t.hasValidValue() ){
		                Amount vaporPresAmount = PRLibrary.prVapr( t );
		                this.setValue(vaporPresAmount);
		}else
			  setValueToMissing();
		return this;
	}		
}