package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import  gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to any of  the GEMPAK parameters TMWK/TMWC or TMWF
 * depending on the units in which the wet bulb temperature needs to be derived.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 

 public class WetBulbTemp extends AbstractMetParameter implements 
		javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5416052389195936616L;

	public WetBulbTemp() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( AirTemperature t, MixingRatio m, PressureLevel p ) throws InvalidValueException, NullPointerException {
	
		if ( t.hasValidValue() && m.hasValidValue() && p.hasValidValue() ){
		       Amount val = PRLibrary.prTmwb(t, m, p );                     	
		       setValue(val);
	    }else
		       setValueToMissing();
	
		return this;
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( AirTemperature t, SurfaceMixingRatio m, SurfacePressure p ) throws InvalidValueException, NullPointerException {
	
		if ( t.hasValidValue() && m.hasValidValue() && p.hasValidValue() ){
		       Amount val = PRLibrary.prTmwb(t, m, p ); 
		       if (val.getUnit().equals(SI.KELVIN)) {
		    	    // unit of val is Kelvin but value returned is a Celcius value,
		    	    // therefore, need to compute the Kelvin value as compared to legacy 
		    	    // (might have to modify PRLibrary.prTmwb() to return a Celcius Amount instead)
					double wbVal = (Double)val.getValue(); 
					wbVal = wbVal + 273.15;
				    val = new Amount (wbVal , SI.KELVIN); 					
				}
		       setValue(val);
	    }else
		       setValueToMissing();
	
		return this;
	}
	
}

















