package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.AirTemperature;
import gov.noaa.nws.ncep.edex.common.metparameters.EquivPotentialTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AirParcelTemp extends AbstractMetParameter implements javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8870456977205187179L;

	public AirParcelTemp() {
		 super( UNIT );
	}

	@DeriveMethod		
	public AirParcelTemp derive(EquivPotentialTemp et, PressureLevel p, AirTemperature t ) throws InvalidValueException, NullPointerException  {
		if ( et.hasValidValue() && p.hasValidValue() && t.hasValidValue() ){
		    Amount val = PRLibrary.prTmst(et, p, t);
		    setValue(val);
		}
		else
			setValueToMissing();
		
		return this;
	}
}
