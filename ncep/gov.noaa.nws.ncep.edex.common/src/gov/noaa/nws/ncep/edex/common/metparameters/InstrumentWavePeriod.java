package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Duration;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class InstrumentWavePeriod extends AbstractMetParameter implements
 Duration, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8897120024853102693L;

	public InstrumentWavePeriod() {
		 super( UNIT );
	}
	
 }
