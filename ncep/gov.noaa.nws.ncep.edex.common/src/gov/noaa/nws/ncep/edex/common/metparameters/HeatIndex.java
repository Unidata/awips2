package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.edex.common.metparameters.RelativeHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
/**
 * Maps to the GEMPAK parameter HEAT
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class HeatIndex extends AbstractMetParameter implements 
				javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2316672515150990983L;

	public HeatIndex( ) {
		super( UNIT );
	}
	
	@DeriveMethod
	public HeatIndex derive( AirTemperature t, RelativeHumidity rh ) throws InvalidValueException, NullPointerException  {
     if ( t.hasValidValue() &&  rh.hasValidValue() ){
    	 Amount theRelhAmount = PRLibrary.prHeat(t, rh);
    	 this.setValue(theRelhAmount);
     }
     else 
    	 setValueToMissing();
     
     return this;
	}
}
