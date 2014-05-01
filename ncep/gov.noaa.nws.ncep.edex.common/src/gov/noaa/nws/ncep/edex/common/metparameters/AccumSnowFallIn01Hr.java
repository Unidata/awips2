package gov.noaa.nws.ncep.edex.common.metparameters;

import java.io.Serializable;

import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 
/**
 * 
 * Maps to the modelsounding parameter snowWater
 *
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 public class AccumSnowFallIn01Hr extends AbstractMetParameter implements
 Length, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6822860327143776139L;

	public AccumSnowFallIn01Hr(){
             super(UNIT);
	}
}
