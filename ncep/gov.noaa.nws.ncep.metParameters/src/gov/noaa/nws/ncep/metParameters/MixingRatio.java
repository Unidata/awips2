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
public final class MixingRatio extends AbstractMetParameter implements javax.measure.quantity.Dimensionless {
	public MixingRatio(){
		 super( UNIT );
	}
	
	@DeriveMethod
	public MixingRatio derive ( DewPointTemp d , PressureLevel p ) throws InvalidValueException, NullPointerException, InvalidRangeException{
		if ( d.hasValidValue() && p.hasValidValue() ){
		                Amount mixingRatio = PRLibrary.prMixr( d, p );
		                this.setValue(mixingRatio);
		}else
			setValueToMissing();
		return this;
	}	
}
