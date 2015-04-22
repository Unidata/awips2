package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter HOWW
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class WindWaveHeight extends AbstractMetParameter implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5764540871910027620L;

	public WindWaveHeight() {
		super( UNIT );
	}
	
}
