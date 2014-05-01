package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Angle;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter DOSW
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class PredomSwellWaveDir extends AbstractMetParameter
		implements Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8593433134786232633L;

	public PredomSwellWaveDir() {
		super( UNIT );
	}

}
