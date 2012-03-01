package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SEPA
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class IsentropesVerticalSeparation extends AbstractMetParameter implements 
javax.measure.quantity.Pressure, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -5980882473474381393L;

	public IsentropesVerticalSeparation() {
		 super( UNIT );
	}

}

