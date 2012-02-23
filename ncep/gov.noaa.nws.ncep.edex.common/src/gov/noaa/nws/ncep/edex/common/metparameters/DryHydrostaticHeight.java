package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter DHGT
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class DryHydrostaticHeight extends AbstractMetParameter
		implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2584501102976729370L;

	public DryHydrostaticHeight(){
		super( UNIT );
	}

}
