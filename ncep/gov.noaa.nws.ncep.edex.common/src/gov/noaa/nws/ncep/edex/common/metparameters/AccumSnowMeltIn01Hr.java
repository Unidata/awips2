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
 * Maps to the modelsounding parameter snowMelt
 *
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 public class AccumSnowMeltIn01Hr extends AbstractMetParameter implements
 Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 517046204174710703L;

	public AccumSnowMeltIn01Hr(){
             super(UNIT);
	}
}
