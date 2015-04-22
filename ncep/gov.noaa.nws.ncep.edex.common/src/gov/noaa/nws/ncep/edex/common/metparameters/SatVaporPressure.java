package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter VAPS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize




public class SatVaporPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6945752189065771295L;

	public SatVaporPressure() {
		 super( UNIT );
	}

	@DeriveMethod		
	public SatVaporPressure derive(  AirTemperature t ) throws InvalidValueException, NullPointerException  {
		if ( t.hasValidValue() ){
		                Amount vaporPresAmount = PRLibrary.prVapr( t );
		                this.setValue(vaporPresAmount);
		}else
			  setValueToMissing();
		return this;
	}		
}