package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to any of the GEMPAK parameters VSBK or VSBY or VSBN depending on the units 
 * in which the visibility is reported (kilometers, statue miles or nautical miles respectively).
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class Visibility extends AbstractMetParameter implements
							javax.measure.quantity.Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -838483615539473671L;

	public Visibility() {
		super( UNIT );
	}

}
