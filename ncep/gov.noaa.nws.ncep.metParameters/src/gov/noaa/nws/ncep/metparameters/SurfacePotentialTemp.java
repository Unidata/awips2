/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;


import javax.measure.unit.SI;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
/**
 * @author archana
 *
 */
public final class SurfacePotentialTemp extends AbstractMetParameter implements
							javax.measure.quantity.Temperature {
	public SurfacePotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SurfacePotentialTemp derive( AirTemperature t, SurfacePressure p ) throws InvalidRangeException, InvalidValueException, NullPointerException {

 		
 		if ( t.hasValidValue() && p.hasValidValue() ){
 		     Amount thePotentialTempAmount = PRLibrary.prThta(t, p);
		     this.setValue(thePotentialTempAmount);
		}else
			setValueToMissing();
		return this;
	}

}