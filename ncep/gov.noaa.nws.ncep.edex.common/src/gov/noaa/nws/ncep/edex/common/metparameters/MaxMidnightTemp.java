package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TDXC - maximum midnight temperature reported locally at 00 LST
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class MaxMidnightTemp extends AbstractMetParameter implements
 javax.measure.quantity.Temperature, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -5614173427532739513L;

	public MaxMidnightTemp() throws Exception {
		super( new UnitAdapter().marshal(UNIT) );
	}
	 
 }
