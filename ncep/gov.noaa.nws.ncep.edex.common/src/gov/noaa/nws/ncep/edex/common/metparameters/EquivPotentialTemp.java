package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter THTE
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class EquivPotentialTemp extends AbstractMetParameter implements
											javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8632870848396073629L;

	public EquivPotentialTemp() {
		super( UNIT );
	}

	@DeriveMethod
	public EquivPotentialTemp derive( PressureLevel p, AirTemperature t, DewPointTemp dpt ) throws InvalidValueException, NullPointerException  {
        if ( p.hasValidValue() && t.hasValidValue() && dpt.hasValidValue() ){
		         Amount theEquivPotTempAmount = PRLibrary.prThte(p, t, dpt);
                setValue(theEquivPotTempAmount);
        }else
        	setValueToMissing();
        return this;
	}

}

