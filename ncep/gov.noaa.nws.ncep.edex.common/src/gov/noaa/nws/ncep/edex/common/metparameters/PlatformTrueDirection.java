/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Angle;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SHPD
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class PlatformTrueDirection extends AbstractMetParameter implements Angle, ISerializableObject {
           /**
	 * 
	 */
	private static final long serialVersionUID = 4241526169911161569L;

		public PlatformTrueDirection(){
        	   super( UNIT );
           }
 }
