package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter CLCT
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class MaxCloudCover extends AbstractMetParameter implements
Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2925253923914118756L;

	public MaxCloudCover() {
		  super( UNIT );
	}
	
  }