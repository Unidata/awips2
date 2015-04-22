/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SI12
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class FcstSnowIcePelletAccumulation12Hrs extends AbstractMetParameter
		implements Length, ISerializableObject {

  /**
	 * 
	 */
	private static final long serialVersionUID = 1190330935212040850L;

public	FcstSnowIcePelletAccumulation12Hrs(){
		super( UNIT );
	}	
 }
