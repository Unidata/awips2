package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Angle;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PKWD
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class OneMinPeakWindDir extends AbstractMetParameter
		implements Angle, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 256479496067713436L;

	public OneMinPeakWindDir (){
			super( UNIT );
	 }
	 
}
