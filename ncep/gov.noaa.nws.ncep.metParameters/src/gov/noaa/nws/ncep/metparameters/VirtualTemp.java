package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
public class VirtualTemp extends AbstractMetParameter implements 
javax.measure.quantity.Temperature{

	public VirtualTemp(){
		 super( UNIT );
	}

	//TODO test this to see if the name of the derive method  can be anything other than 'derive'
	@DeriveMethod
	public VirtualTemp computeVirtualTemp(AirTemperature t, DewPointTemp d, PressureLevel p) throws InvalidValueException, NullPointerException, InvalidRangeException{
		if (t.hasValidValue() && d.hasValidValue() && p.hasValidValue() ){
		    Amount virtualTempAmount = PRLibrary.prTvrk(t, d, p);
		    setValue(virtualTempAmount);
		}else
			setValueToMissing();
		
		return this;
	}

//	@DeriveMethod
//	public VirtualTemp computeVirtualTemp(AirTemperature t, DewPointTemp d, SurfacePressure p) throws InvalidValueException, NullPointerException, InvalidRangeException{
//		if (t.hasValidValue() && d.hasValidValue() && p.hasValidValue() ){
//		    Amount virtualTempAmount = PRLibrary.prTvrk(t, d, p);
//		    setValue(virtualTempAmount);
//		}else
//			setValueToMissing();
//		
//		return this;
//	}


 }

