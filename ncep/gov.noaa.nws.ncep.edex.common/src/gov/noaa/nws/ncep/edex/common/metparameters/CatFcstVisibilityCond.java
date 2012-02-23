package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * TODO: Cross-check if it maps to both the GEMPAK parameters FVIS and FVSA
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CatFcstVisibilityCond extends AbstractMetParameter implements 
Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 376519018066379860L;

	public CatFcstVisibilityCond() {
		super( UNIT );
	}

}