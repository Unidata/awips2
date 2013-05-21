package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the Bufrmos parameter QPF24hr_bestCat
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class QuantPrecipFcstBestCat24Hr extends AbstractMetParameter implements
 Dimensionless, ISerializableObject {

     /**
	 * 
	 */
	

	public QuantPrecipFcstBestCat24Hr() {
		 super( UNIT );
		 //setValueIsString();
     }	

	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 