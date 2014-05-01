/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Velocity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SP20
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class EquivWindSpeed20min extends AbstractMetParameter implements
 Velocity, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8503686810160432294L;

	public EquivWindSpeed20min() {
		super(UNIT);
	}

 }

