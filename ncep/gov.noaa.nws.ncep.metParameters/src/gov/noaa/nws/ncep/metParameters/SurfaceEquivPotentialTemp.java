package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.SI;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class SurfaceEquivPotentialTemp extends AbstractMetParameter implements
											javax.measure.quantity.Temperature {
	public SurfaceEquivPotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SurfaceEquivPotentialTemp derive( SurfacePressure p, AirTemperature t, DewPointTemp dpt ) throws InvalidValueException, NullPointerException, InvalidRangeException {

 		
 		if ( p.hasValidValue() && t.hasValidValue() && dpt.hasValidValue() ){
 		      Amount theEquivPotTempAmount = PRLibrary.prThte( p, t, dpt );
              this.setValue(theEquivPotTempAmount);
        }
        else
             setValueToMissing();	
        return this;
	}

}

