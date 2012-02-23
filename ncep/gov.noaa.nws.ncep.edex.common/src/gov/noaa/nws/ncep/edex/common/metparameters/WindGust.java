package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter GUST
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



 public class WindGust extends AbstractMetParameter implements
 javax.measure.quantity.Velocity, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4318966667549027562L;

	public WindGust() {
		super( UNIT );		
	}
	
//	@DeriveMethod
//	public AbstractMetParameter getWindSpeedFromGustBarb( GustBarb gb ) {
//		return gb.getWindGust();
//	}
}
