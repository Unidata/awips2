package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Maps to any of the GEMPAK parameters TMPC, TMPK and TMPF (depending on the unit used) 
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SkinTemperature extends AbstractMetParameter implements 
										javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2955966767363683734L;

	public SkinTemperature( ) {
		 super( UNIT) ;
	}

 }