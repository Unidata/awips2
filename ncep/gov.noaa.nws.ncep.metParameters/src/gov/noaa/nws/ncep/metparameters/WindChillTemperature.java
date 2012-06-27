package gov.noaa.nws.ncep.metparameters;


import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class WindChillTemperature extends AbstractMetParameter implements 
										javax.measure.quantity.Temperature {

	public WindChillTemperature( ) {
		 super( UNIT );
	}

	@DeriveMethod
	public WindChillTemperature derive( AirTemperature t, WindSpeed ws) throws InvalidRangeException, InvalidValueException, NullPointerException {
        if ( t.hasValidValue() && ws.hasValidValue() ){      
		      Amount tempAmount =  PRLibrary.prWceq( t, ws);
              setValue(tempAmount);
        }else
            setValueToMissing();  
        	
         return this;
	}		

}
