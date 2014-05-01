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
 * Maps to the modelsounding parameter frzgRainTyp
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


  public class FreezingRainType extends AbstractMetParameter implements
  Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6911996277316067187L;

	public FreezingRainType(){
	           super( UNIT );
          }
  }
