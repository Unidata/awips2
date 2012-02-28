/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Temperature;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TNCF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 public class ClimNightTemp extends AbstractMetParameter implements Temperature,
 ISerializableObject{

	 /**
	 * 
	 */
	private static final long serialVersionUID = 7198420674894755646L;

	public ClimNightTemp() {
	      super( UNIT );
	 }
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological Night-time temperature.";
	 }
 }
