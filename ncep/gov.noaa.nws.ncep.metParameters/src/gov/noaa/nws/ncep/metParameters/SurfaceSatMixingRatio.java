/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
/**
 * @author archana
 *
 */
 public final class SurfaceSatMixingRatio extends AbstractMetParameter implements javax.measure.quantity.Dimensionless {
	
	public SurfaceSatMixingRatio(){
		 super( UNIT );
	}
	
 	@DeriveMethod
	public SurfaceSatMixingRatio derive ( AirTemperature t , SurfacePressure p ) throws InvalidValueException, NullPointerException, InvalidRangeException{
       if  ( t.hasValidValue() && p.hasValidValue() ){
 		     Amount mixingRatio = PRLibrary.prMixr( t, p );
		     setValue(mixingRatio);
       }
       else
    	   setValueToMissing();
       
		return this;
	}
}
