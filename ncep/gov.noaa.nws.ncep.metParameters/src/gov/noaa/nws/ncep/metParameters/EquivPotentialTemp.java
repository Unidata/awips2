package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class EquivPotentialTemp extends AbstractMetParameter implements
											javax.measure.quantity.Temperature {
	public EquivPotentialTemp() {
		super( UNIT );
	}

	@DeriveMethod
	public EquivPotentialTemp derive( PressureLevel p, AirTemperature t, DewPointTemp dpt ) throws InvalidValueException, NullPointerException, InvalidRangeException {
        if ( p.hasValidValue() && t.hasValidValue() && dpt.hasValidValue() ){
		         Amount theEquivPotTempAmount = PRLibrary.prThte(p, t, dpt);
                setValue(theEquivPotTempAmount);
        }else
        	setValueToMissing();
        return this;
	}

}

