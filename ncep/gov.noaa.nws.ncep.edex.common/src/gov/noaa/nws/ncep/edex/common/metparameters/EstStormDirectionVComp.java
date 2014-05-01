package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter VSTM
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class EstStormDirectionVComp extends AbstractMetParameter implements
javax.measure.quantity.Velocity, ISerializableObject{

	/**
	 * 
	 */
	private static final long serialVersionUID = -6158382200072441442L;

	public EstStormDirectionVComp(){
		 super( UNIT );
	}
	
}
