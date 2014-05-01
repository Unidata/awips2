package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to any of  the GEMPAK parameters THWK/THWC or THWF
 * depending on the units in which the wet bulb potential temperature needs to be derived.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 

public class WetBulbPotentialTemp extends AbstractMetParameter implements 
		javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 795571539832273959L;

	public WetBulbPotentialTemp() {
		 super( UNIT );
	}

	@DeriveMethod
	AbstractMetParameter derive (PressureLevel p,  AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException {
	    if ( p.hasValidValue() && t.hasValidValue() && d.hasValidValue() ){
		     Amount val = PRLibrary.prThwc(p, t, d );                     	
		     setValue(val);
	     }
	    else
	       setValueToMissing();
	    
		return this;
	}

//	AbstractMetParameter derive (SurfacePressure p,  AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException {
//	    Amount val = PRLibrary.prThwc(p, t, d );                     	
//		this.setValue(val);
//		return this;
//	}	
	
}











