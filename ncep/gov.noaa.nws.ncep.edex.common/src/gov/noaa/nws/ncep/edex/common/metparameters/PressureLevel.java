package gov.noaa.nws.ncep.edex.common.metparameters;



import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;



/**
 * Maps to the GEMPAK parameter PRES
 * This parameter is intended to be used as a Vertical Coordinate.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class PressureLevel extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	

	/**
	 * 
	 */
	private static final long serialVersionUID = 167491198657595212L;

	public PressureLevel() throws Exception {
//		 super( new UnitAdapter().marshal(UNIT) );
		super ( UNIT );
	}
}

