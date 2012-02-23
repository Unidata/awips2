package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to GEMPAK parameters STIM
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class ReportTimeInHourMins extends AbstractMetParameter implements
							javax.measure.quantity.Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -838483615539473683L;

	public ReportTimeInHourMins() {
		super( UNIT );
	}

}
