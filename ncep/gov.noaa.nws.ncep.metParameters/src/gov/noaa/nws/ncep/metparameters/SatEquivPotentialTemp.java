package gov.noaa.nws.ncep.metparameters;


import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class SatEquivPotentialTemp extends AbstractMetParameter implements
											javax.measure.quantity.Temperature {
	public SatEquivPotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SatEquivPotentialTemp derive( PressureLevel p, AirTemperature t, AirTemperature t2 ) throws InvalidValueException, NullPointerException, InvalidRangeException {
        if ( p.hasValidValue() && t.hasValidValue() && t2.hasValidValue() ){
 		     Amount theEquivPotTempAmount = PRLibrary.prThte(p, t, t2);
             setValue(theEquivPotTempAmount);
        }else
        	setValueToMissing();
        return this;
	}

}

