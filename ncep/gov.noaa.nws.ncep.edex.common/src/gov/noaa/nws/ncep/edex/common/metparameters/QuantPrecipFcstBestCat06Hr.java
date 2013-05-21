package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the Bufrmos parameter QPF06hr_bestCat (new GEMPAK alias used - QP06)
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class QuantPrecipFcstBestCat06Hr extends AbstractMetParameter implements
 Dimensionless, ISerializableObject {

     /**
	 * 
	 */
	

	public QuantPrecipFcstBestCat06Hr() {
		 super( UNIT );
		 //setValueIsString();
     }	

	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 