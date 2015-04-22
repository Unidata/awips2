/**
 * 
 */
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

public final class PotentialTempAt10Meters extends AbstractMetParameter implements
						javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -535215977337883178L;

	public PotentialTempAt10Meters()  {
		 super(  UNIT  ) ;
	}

	
}
