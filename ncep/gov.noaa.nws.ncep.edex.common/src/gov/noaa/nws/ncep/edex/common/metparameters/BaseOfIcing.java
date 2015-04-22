package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * Maps to GEMPAK parameter IBSE
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

  public class BaseOfIcing extends AbstractMetParameter
		implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5530237003460096006L;

	public BaseOfIcing() {
		super( UNIT );
	}	
  }