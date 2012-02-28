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
 * Maps to the GEMPAK parameter VAPR
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 

public class VaporPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7385036051859860024L;

	public VaporPressure() {
		 super( UNIT );
	}

	@DeriveMethod		
	public VaporPressure derive(  DewPointTemp d ) throws InvalidValueException, NullPointerException  {

		if ( d.hasValidValue() ){
		        Amount vaporPresAmount = PRLibrary.prVapr( d );
		        setValue(vaporPresAmount);
		}else
		    setValueToMissing();
		
		return this;
	}		
}