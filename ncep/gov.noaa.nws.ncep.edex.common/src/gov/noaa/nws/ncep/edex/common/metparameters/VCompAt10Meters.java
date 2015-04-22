package gov.noaa.nws.ncep.edex.common.metparameters;


import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class VCompAt10Meters extends AbstractMetParameter
implements javax.measure.quantity.Velocity, Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -6244870468588440392L;

	public VCompAt10Meters() {
		 super(  UNIT );
	}
	
}
