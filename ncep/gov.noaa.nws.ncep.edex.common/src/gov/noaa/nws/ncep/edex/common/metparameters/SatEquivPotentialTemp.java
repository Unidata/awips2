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
 * Maps to the GEMPAK parameter THES
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



public class SatEquivPotentialTemp extends AbstractMetParameter implements
											javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = 3560480372372905066L;

	public SatEquivPotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SatEquivPotentialTemp derive( PressureLevel p, AirTemperature t, AirTemperature t2 ) throws InvalidValueException, NullPointerException  {
        if ( p.hasValidValue() && t.hasValidValue() && t2.hasValidValue() ){
 		     Amount theEquivPotTempAmount = PRLibrary.prThte(p, t, t2);
             setValue(theEquivPotTempAmount);
        }else
        	setValueToMissing();
        return this;
	}

}

