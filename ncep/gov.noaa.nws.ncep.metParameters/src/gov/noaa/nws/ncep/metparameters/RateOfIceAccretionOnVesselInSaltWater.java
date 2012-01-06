package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
 
public class RateOfIceAccretionOnVesselInSaltWater extends AbstractMetParameter implements javax.measure.quantity.Velocity {

    public RateOfIceAccretionOnVesselInSaltWater() {
		super( UNIT );
	}
    
	@DeriveMethod
	AbstractMetParameter derive( AirTemperature airTemp, SeaSurfaceTemp seaTemp, WindSpeed ws ) throws InvalidValueException, NullPointerException, InvalidRangeException{
		if ( airTemp.hasValidValue() && seaTemp.hasValidValue() && ws.hasValidValue() ){
		     Amount val = PRLibrary.prIgro(airTemp, seaTemp, ws );
		     setValue(val);
		}else
			setValueToMissing();
		
		return this;
	}
	
 }
