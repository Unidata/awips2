package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;


/**
 * Maps to the GEMPAK parameter PWSP
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



public class SpeedOf05SecPeakWind extends AbstractMetParameter implements
 javax.measure.quantity.Velocity, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6216706738928142396L;

	public SpeedOf05SecPeakWind(){
		 super( UNIT );
	}
	
 }