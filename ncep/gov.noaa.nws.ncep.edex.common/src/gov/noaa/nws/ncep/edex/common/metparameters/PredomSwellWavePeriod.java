package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Duration;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter POSW
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class PredomSwellWavePeriod extends AbstractMetParameter implements Duration, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7273625766532365623L;

	public PredomSwellWavePeriod() {
		 super( UNIT );
	}
	
}
