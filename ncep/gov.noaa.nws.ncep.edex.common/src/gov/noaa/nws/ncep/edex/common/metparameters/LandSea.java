/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import java.io.Serializable;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the modelsounding parameter landSea
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


  public class LandSea extends AbstractMetParameter implements
  Dimensionless, ISerializableObject {


		/**
	 * 
	 */
	private static final long serialVersionUID = 6559113832610632797L;

		public LandSea(){
	           super( UNIT );
          }
  }