package gov.noaa.nws.ncep.metparameters;


import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 
public class AirTemperature extends AbstractMetParameter implements 
										javax.measure.quantity.Temperature {

	public AirTemperature( ) {
		 super( UNIT );
	}

 	@DeriveMethod
	public AirTemperature derive( PressureLevel p, PotentialTemp pt) throws InvalidRangeException, InvalidValueException, NullPointerException {
         if ( p.hasValidValue() && pt.hasValidValue() ){       
 		        Amount tempAmount =  PRLibrary.prTmpk(p, pt);
                setValue(tempAmount);
         }
         else
        	 setValueToMissing();
         
                return this;
	}		
}
