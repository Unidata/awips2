package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Temperature;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to either of the GEMPAK parameters SSTC/SSTF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



 public class SeaSurfaceTemp extends AbstractMetParameter implements
		Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -734515503056112921L;

	public SeaSurfaceTemp() {
		 super( UNIT );
	}  
	
 }
