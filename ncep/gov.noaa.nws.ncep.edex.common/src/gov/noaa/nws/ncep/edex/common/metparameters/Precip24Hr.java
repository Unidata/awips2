package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter P24I or P24M - based on whether the precipitation
 * is measured in inches or mm
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class Precip24Hr extends AbstractMetParameter implements
 javax.measure.quantity.Length, ISerializableObject{
        /**
	 * 
	 */
	private static final long serialVersionUID = 8411794382636632766L;

		public Precip24Hr() {
   		 super( UNIT );
		}

  }
