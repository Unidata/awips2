package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Velocity;
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


public class VerticalVelocity extends AbstractMetParameter
implements Velocity, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2287974035743287368L;

	protected VerticalVelocity() {
		super(UNIT);
	}

}
