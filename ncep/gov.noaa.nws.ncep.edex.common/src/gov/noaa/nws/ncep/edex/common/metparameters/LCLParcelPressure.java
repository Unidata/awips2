package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to the GEMPAK parameter PLCL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class LCLParcelPressure extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject{
	/**
	 * 
	 */
	private static final long serialVersionUID = -1959728817791370835L;

	public LCLParcelPressure() {
		super( UNIT );
	}

	@DeriveMethod		
	public LCLParcelPressure derive(AirTemperature t, PressureLevel p, LCLParcelTemperature parcelTemp ) throws InvalidValueException, NullPointerException {
		if ( t.hasValidValue() && p.hasValidValue() && parcelTemp.hasValidValue() ){
		    Amount val = PRLibrary.prPlcl(t, p, parcelTemp);
		    setValue(val);
		}else
			setValueToMissing();
		return this;
	}

}
