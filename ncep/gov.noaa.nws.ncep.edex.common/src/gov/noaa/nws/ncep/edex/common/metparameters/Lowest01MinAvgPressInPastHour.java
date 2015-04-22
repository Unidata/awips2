package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PMN1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class Lowest01MinAvgPressInPastHour extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = 9129617762010357872L;

	public Lowest01MinAvgPressInPastHour() {
		super( UNIT );
	}

}

