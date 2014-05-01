/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 *Maps to the GEMPAK parameter ACRT 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AircraftReportType extends AbstractMetParameter implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6027887696168926504L;

	public AircraftReportType() {
		super(UNIT);
//		setValueIsString();  
	}

}