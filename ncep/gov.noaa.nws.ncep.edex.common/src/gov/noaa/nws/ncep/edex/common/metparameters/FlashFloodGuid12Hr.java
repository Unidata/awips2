package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter FF12
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class FlashFloodGuid12Hr extends AbstractMetParameter
		implements Length, ISerializableObject {

    /**
	 * 
	 */
	private static final long serialVersionUID = -8994410230393003638L;

	public FlashFloodGuid12Hr(){
    	super( UNIT );
    }
}
