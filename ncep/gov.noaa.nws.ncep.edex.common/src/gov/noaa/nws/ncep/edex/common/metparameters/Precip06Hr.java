package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter P06I or P06M - based on whether the precipitation
 * is measured in inches or mm
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class Precip06Hr extends AbstractMetParameter implements
 javax.measure.quantity.Length, ISerializableObject{
	 /**
	 * 
	 */
	private static final long serialVersionUID = -4801129276467749485L;

	public  Precip06Hr ( ){
		 super( UNIT );
	 }

  }