package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to either the GEMPAK parameter TDXC or TDXF depending on the
 * unit used to measure the temperature
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class Max24HrTemp extends AbstractMetParameter implements
 javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8225713333054145814L;

	public Max24HrTemp(){
		super( UNIT );
	}
	
}
