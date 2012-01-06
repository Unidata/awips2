package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class LCLParcelPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure{
	public LCLParcelPressure() {
		super( UNIT );
	}

	@DeriveMethod		
	public LCLParcelPressure derive(AirTemperature t, PressureLevel p, LCLParcelTemperature parcelTemp ) throws InvalidValueException, NullPointerException {
		if ( t.hasValidValue() && p.hasValidValue() && parcelTemp.hasValidValue() ){
		    Amount val = PRLibrary.prPlcl(t, p, parcelTemp);
		    setValue(val);
		}else
			setValueToMissing();
		return this;
	}

}
