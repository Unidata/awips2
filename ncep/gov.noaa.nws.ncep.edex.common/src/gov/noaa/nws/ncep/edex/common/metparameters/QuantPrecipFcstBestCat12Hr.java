package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the Bufrmos parameter QPF12hr_bestCat
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class QuantPrecipFcstBestCat12Hr extends AbstractMetParameter implements
 Dimensionless, ISerializableObject {

     /**
	 * 
	 */
	

	public QuantPrecipFcstBestCat12Hr() {
		 super( UNIT );
		 //setValueIsString();
     }	

	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 