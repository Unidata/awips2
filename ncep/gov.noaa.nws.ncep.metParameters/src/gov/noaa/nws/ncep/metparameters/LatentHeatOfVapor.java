package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class LatentHeatOfVapor extends AbstractMetParameter implements javax.measure.quantity.Energy{
	public LatentHeatOfVapor() {
		super( UNIT );
	}

	@DeriveMethod		
	public LatentHeatOfVapor derive(  AirTemperature t ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		if ( t.hasValidValue() ){
		   Amount val = PRLibrary.prLhvp( t );
		   setValue(val);
		}else
			setValueToMissing();
		return this;
	}	

}
