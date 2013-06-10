package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CondProbOf12HrRain extends AbstractMetParameter implements
	Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 5188256528302697257L;

	public CondProbOf12HrRain()throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
	 
}