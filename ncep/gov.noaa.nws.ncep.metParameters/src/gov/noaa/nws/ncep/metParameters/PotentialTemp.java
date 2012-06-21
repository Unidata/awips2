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
 public final class PotentialTemp extends AbstractMetParameter implements
						javax.measure.quantity.Temperature {
	public PotentialTemp() {
		 super( UNIT );
	}

	@DeriveMethod
	public PotentialTemp derive( AirTemperature t, PressureLevel p ) throws InvalidRangeException, InvalidValueException, NullPointerException {
		if ( t.hasValidValue() && p.hasValidValue() ){
		          Amount thePotentialTempAmount = PRLibrary.prThta(t, p);
		          this.setValue(thePotentialTempAmount);
		}else
	             setValueToMissing();
		return this;
	}
	
//@DeriveMethod
//	public PotentialTemp derive( AirTemperature t, SeaLevelPressure p ) throws InvalidRangeException, InvalidValueException, NullPointerException {
//		if ( t.hasValidValue() && p.hasValidValue() ){
//		          Amount thePotentialTempAmount = PRLibrary.prThta(t, p);
//		          this.setValue(thePotentialTempAmount);
//		}else
//	             setValueToMissing();
//		return this;
//	}
	
}
