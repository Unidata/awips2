package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter P01I or P01M - based on whether the precipitation
 * is measured in inches or mm
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class Precip01Hr extends AbstractMetParameter implements
 javax.measure.quantity.Length, ISerializableObject{
         /**
	 * 
	 */
	private static final long serialVersionUID = 621090290855727073L;

		public Precip01Hr ( ){
     		 super( UNIT );
          }

  }
