package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class WindCompDirection extends AbstractMetParameter
implements javax.measure.quantity.Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3645429836806486033L;

	public WindCompDirection() {
		 super( UNIT );
	}
	
}
