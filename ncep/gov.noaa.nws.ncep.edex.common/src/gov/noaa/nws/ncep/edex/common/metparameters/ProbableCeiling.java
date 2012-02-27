package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 
/**
 * Maps to the GEMPAK parameter TCEL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 
public class ProbableCeiling extends AbstractMetParameter
implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4508683917613735355L;

	public ProbableCeiling() {
		 super( UNIT );
	}
  
}
