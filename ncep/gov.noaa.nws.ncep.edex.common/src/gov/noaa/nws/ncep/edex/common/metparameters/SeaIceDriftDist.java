package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter DTNM
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize




public class SeaIceDriftDist extends AbstractMetParameter
		implements Length, ISerializableObject {

    /**
	 * 
	 */
	private static final long serialVersionUID = -6810136439885658980L;

	public SeaIceDriftDist(){
		 super( UNIT );
    }
	
}

