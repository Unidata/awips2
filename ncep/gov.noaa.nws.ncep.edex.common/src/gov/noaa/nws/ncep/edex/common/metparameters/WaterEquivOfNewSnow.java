package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter WEQS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class WaterEquivOfNewSnow extends AbstractMetParameter
 implements Length, ISerializableObject {

     /**
	 * 
	 */
	private static final long serialVersionUID = 1803236555737125814L;

	public WaterEquivOfNewSnow(){
		 super( UNIT );
     }

 }
