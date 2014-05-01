package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.quantity.PotentialForCyclonicUpdraftRotation;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Maps to the modelsounding parameter srHel
 *
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StormRelativeHelicity extends AbstractMetParameter implements
	PotentialForCyclonicUpdraftRotation, ISerializableObject {

/**
	 * 
	 */
	private static final long serialVersionUID = 2352306262526992878L;

public StormRelativeHelicity() {
		super( UNIT );
	}	
}


