package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class DryBulbTemp extends AbstractMetParameter implements 
		javax.measure.quantity.Temperature, ISerializableObject {


	/**
	 * 
	 */
	private static final long serialVersionUID = 3026150096446118901L;

	public DryBulbTemp() {
		 super( UNIT );
	}
	
}

















