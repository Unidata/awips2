package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class HeatIndex extends AbstractMetParameter implements 
				javax.measure.quantity.Temperature {

	public HeatIndex( ) {
		super( UNIT );
	}
	
	@DeriveMethod
	public HeatIndex derive( AirTemperature t, RelativeHumidity rh ) throws InvalidValueException, NullPointerException, InvalidRangeException {
     if ( t.hasValidValue() &&  rh.hasValidValue() ){
    	 Amount theRelhAmount = PRLibrary.prHeat(t, rh);
    	 this.setValue(theRelhAmount);
     }
     else 
    	 setValueToMissing();
     
     return this;
	}
}
