package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter DDEN
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class DryAirDensity extends AbstractMetParameter implements javax.measure.quantity.VolumetricDensity,
ISerializableObject{

	/**
	 * 
	 */
	private static final long serialVersionUID = -5600672976643167843L;

	public DryAirDensity() {
		super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive(PressureLevel p, AirTemperature t) throws   InvalidValueException, NullPointerException{
		Amount val =  PRLibrary.prDden( p, t )	;
		setValue(val);
		return this;
	}
}












