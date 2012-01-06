package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class VaporPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure {
	public VaporPressure() {
		 super( UNIT );
	}

	@DeriveMethod		
	public VaporPressure derive(  DewPointTemp d ) throws InvalidValueException, NullPointerException, InvalidRangeException {

		if ( d.hasValidValue() ){
		        Amount vaporPresAmount = PRLibrary.prVapr( d );
		        setValue(vaporPresAmount);
		}else
		    setValueToMissing();
		
		return this;
	}		
}