package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the bufrmos parameter csevere24hr (new GEMPAK alias used - TC24)
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class CondProbOf24HrSevereWeather extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8606034719943640699L;

	public CondProbOf24HrSevereWeather()throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
}