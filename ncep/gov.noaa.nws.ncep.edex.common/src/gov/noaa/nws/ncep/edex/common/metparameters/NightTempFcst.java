/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Temperature;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TNYF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class NightTempFcst extends AbstractMetParameter implements
		Temperature, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -6365011797040807386L;

	public NightTempFcst() throws Exception {
		super( new UnitAdapter().marshal(UNIT) );
	}
	 
 }
