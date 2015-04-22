package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * Maps to the GEMPAK parameters CBS2 and CB2M depending
 * on whether the cloud base was measured in feet or meters respectively
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize   
  public class CloudBase2 extends AbstractMetParameter
		implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4796330470566529739L;

	public CloudBase2() {
		  super( UNIT );
	}
	
  }