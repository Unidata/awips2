package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter QPX2 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class Max12HrPrecipFcst extends
		AbstractMetParameter implements Length, ISerializableObject {

    /**
	 * 
	 */
	private static final long serialVersionUID = 3367483164300511042L;

	public Max12HrPrecipFcst(){
		 super( UNIT );
    }
	  
 }
