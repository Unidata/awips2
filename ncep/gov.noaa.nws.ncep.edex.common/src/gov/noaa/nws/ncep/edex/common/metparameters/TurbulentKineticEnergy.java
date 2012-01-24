 package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TKEL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 
 public class TurbulentKineticEnergy extends AbstractMetParameter
 implements javax.measure.quantity.Energy, ISerializableObject{
	/**
	 * 
	 */
	private static final long serialVersionUID = -4336082923017129663L;

	public TurbulentKineticEnergy() {
		super( UNIT );
	}
 }
