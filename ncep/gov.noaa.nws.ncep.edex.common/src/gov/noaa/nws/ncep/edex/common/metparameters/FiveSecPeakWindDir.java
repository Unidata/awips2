package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PWDR
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class FiveSecPeakWindDir extends AbstractMetParameter implements
 javax.measure.quantity.Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -9021635625609421348L;

	public FiveSecPeakWindDir() {
		super( UNIT );
	} 
}
