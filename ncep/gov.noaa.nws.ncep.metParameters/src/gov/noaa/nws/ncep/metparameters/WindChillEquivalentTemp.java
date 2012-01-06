package gov.noaa.nws.ncep.metparameters;


import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class WindChillEquivalentTemp extends AbstractMetParameter implements 
										javax.measure.quantity.Temperature {

	public WindChillEquivalentTemp( ) {
		 super( UNIT );
	}
	
	@DeriveMethod
	public WindChillEquivalentTemp derive( AirTemperature t, WindSpeed ws) throws InvalidRangeException, InvalidValueException, NullPointerException {
        if ( t.hasValidValue() && ws.hasValidValue() ){
		     Amount tempAmount =  PRLibrary.prWcht(t, ws);
             setValue(tempAmount);
        }
        else
           setValueToMissing();
         
        return this;
	}		

}
