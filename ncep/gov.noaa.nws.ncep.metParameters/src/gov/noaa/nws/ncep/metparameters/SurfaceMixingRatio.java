/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;



import javax.measure.quantity.Dimensionless;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 /**
 * @author archana
 *
 */
public final class SurfaceMixingRatio extends AbstractMetParameter implements Dimensionless {
	
	public SurfaceMixingRatio(){
		 super( UNIT );
	}
	
 	@DeriveMethod
	public SurfaceMixingRatio derive ( DewPointTemp d , SurfacePressure p ) throws InvalidValueException, NullPointerException, InvalidRangeException{
		if ( d.hasValidValue() && p.hasValidValue() ){
 		         Amount mixingRatio = PRLibrary.prMixr( d, p );
		         setValue(mixingRatio);
		}else
			setValueToMissing();
		return this;
	} 	
}
