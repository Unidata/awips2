package gov.noaa.nws.ncep.edex.common.metparameters;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;


@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class TotalPrecip extends AbstractMetParameter implements
 javax.measure.quantity.Length, ISerializableObject{

		/**
	 * 
	 */
	private static final long serialVersionUID = 5122003873558060930L;

		public TotalPrecip ( ){
     		 super( UNIT );
          }

  }
