package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;


import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 

/**
 * Maps to the GEMPAK parameter IGRO
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class RateOfIceAccretionOnVesselInSaltWater extends AbstractMetParameter implements
javax.measure.quantity.Velocity, ISerializableObject {

    /**
	 * 
	 */
	private static final long serialVersionUID = 4592641986498306172L;

	public RateOfIceAccretionOnVesselInSaltWater() {
		super( UNIT );
	}
    
	@DeriveMethod
	AbstractMetParameter derive( AirTemperature airTemp, SeaSurfaceTemp seaTemp, WindSpeed ws ) throws InvalidValueException, NullPointerException {
		if ( airTemp.hasValidValue() && seaTemp.hasValidValue() && ws.hasValidValue() ){
		     Amount val = PRLibrary.prIgro(airTemp, seaTemp, ws );
		     setValue(val);
		}else
			setValueToMissing();
		
		return this;
	}
	
 }
