package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class AirParcelTemp extends AbstractMetParameter implements javax.measure.quantity.Temperature {
	public AirParcelTemp() {
		 super( UNIT );
	}

	@DeriveMethod		
	public AirParcelTemp derive(EquivPotentialTemp et, PressureLevel p, AirTemperature t ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		if ( et.hasValidValue() && p.hasValidValue() && t.hasValidValue() ){
		    Amount val = PRLibrary.prTmst(et, p, t);
		    setValue(val);
		}
		else
			setValueToMissing();
		
		return this;
	}
}
