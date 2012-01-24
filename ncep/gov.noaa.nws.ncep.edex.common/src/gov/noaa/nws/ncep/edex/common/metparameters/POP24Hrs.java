/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 
public class POP24Hrs extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -323517251895490537L;

	public POP24Hrs() {
		 super( UNIT );
	}
	 
  }