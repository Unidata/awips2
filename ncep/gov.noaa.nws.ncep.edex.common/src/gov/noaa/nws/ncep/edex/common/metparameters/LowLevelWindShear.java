package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter LLWS (low level wind shear forecast flag)
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class LowLevelWindShear extends AbstractMetParameter implements
    javax.measure.quantity.Dimensionless, ISerializableObject {
      
	private static final long serialVersionUID = -702246286811850500L;
	
	public LowLevelWindShear() {
		  super( UNIT );
	}
}

  
  
  
  
  
  
  
  
  