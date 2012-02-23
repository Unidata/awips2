package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TGST
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 
 public class ProbableWindGust extends AbstractMetParameter implements
							javax.measure.quantity.Velocity, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -845022460206752829L;

	public ProbableWindGust() {
		 super( UNIT );
	 }

}
