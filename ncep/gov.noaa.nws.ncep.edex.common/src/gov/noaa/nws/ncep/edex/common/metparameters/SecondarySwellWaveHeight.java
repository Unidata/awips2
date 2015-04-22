package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter HOS2
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class SecondarySwellWaveHeight extends AbstractMetParameter
implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3450300196292105594L;

	public SecondarySwellWaveHeight() {
		super( UNIT );
	}
	
}
