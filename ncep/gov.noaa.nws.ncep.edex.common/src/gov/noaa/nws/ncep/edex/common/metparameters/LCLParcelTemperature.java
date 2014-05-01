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
 * Maps to the GEMPAK parameter TLCL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class LCLParcelTemperature extends AbstractMetParameter implements
javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5018087414179576098L;

	public LCLParcelTemperature() {
		super( UNIT );
	}

	@DeriveMethod		
	public LCLParcelTemperature derive(AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException  {
		if ( t.hasValidValue() && d.hasValidValue() ){
		     Amount val = PRLibrary.prTlcl(t, d );
		     setValue(val);
		}
		else
			setValueToMissing();
		
		return this;
	}

}
