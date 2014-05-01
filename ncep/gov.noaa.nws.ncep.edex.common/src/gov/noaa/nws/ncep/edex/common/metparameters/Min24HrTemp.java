package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to either the GEMPAK parameter TDNF or TDNC depending on the unit used 
 * to measure the temperature
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class Min24HrTemp extends AbstractMetParameter implements
							javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -3934543961904215455L;

	public Min24HrTemp() throws Exception {
		super( new UnitAdapter().marshal(UNIT) );
	}
}
