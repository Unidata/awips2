package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter POWW
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



public class WindWavePeriod extends AbstractMetParameter implements Duration, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 9068132951552388458L;

	public WindWavePeriod() {
		 super( UNIT );
	}
	
}
