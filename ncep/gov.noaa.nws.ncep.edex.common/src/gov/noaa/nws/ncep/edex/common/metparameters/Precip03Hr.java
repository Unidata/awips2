package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter P03I or P03M - based on whether the precipitation
 * is measured in inches or mm
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class Precip03Hr extends AbstractMetParameter implements
 javax.measure.quantity.Length, ISerializableObject{
	 /**
	 * 
	 */
	private static final long serialVersionUID = -207901412863781931L;

	public  Precip03Hr ( ){
     		 super( UNIT );
          }

  }