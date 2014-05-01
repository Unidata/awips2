package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;


import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
/**
 * Maps to any of the GEMPAK parameters TVRK/TVRC or TVRF depending on the units 
 * in which the virtual temperature is to be computed.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize 

public class VirtualTemp extends AbstractMetParameter implements 
javax.measure.quantity.Temperature, ISerializableObject{

	/**
	 * 
	 */
	private static final long serialVersionUID = 6705218807309667397L;

	public VirtualTemp(){
		 super( UNIT );
	}

	//TODO test this to see if the name of the derive method  can be anything other than 'derive'
	@DeriveMethod
	public VirtualTemp computeVirtualTemp(AirTemperature t, DewPointTemp d, PressureLevel p) throws InvalidValueException, NullPointerException {
		if (t.hasValidValue() && d.hasValidValue() && p.hasValidValue() ){
		    Amount virtualTempAmount = PRLibrary.prTvrk(t, d, p);
		    setValue(virtualTempAmount);
		}else
			setValueToMissing();
		
		return this;
	}

//	@DeriveMethod
//	public VirtualTemp computeVirtualTemp(AirTemperature t, DewPointTemp d, SurfacePressure p) throws InvalidValueException, NullPointerException {
//		if (t.hasValidValue() && d.hasValidValue() && p.hasValidValue() ){
//		    Amount virtualTempAmount = PRLibrary.prTvrk(t, d, p);
//		    setValue(virtualTempAmount);
//		}else
//			setValueToMissing();
//		
//		return this;
//	}


 }

