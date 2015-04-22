package gov.noaa.nws.ncep.edex.common.metparameters;



import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the modelsounding parameter prCloud
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class PressureAtCloudLevel extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	


	private static final long serialVersionUID = 5582361384028648335L;

	public PressureAtCloudLevel()  {
		super ( UNIT );
	}
}

