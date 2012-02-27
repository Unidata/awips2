package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * Maps to the GEMPAK parameter FCIG
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CatFcstCeilingHeightCond extends AbstractMetParameter implements Dimensionless,ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1658711340119638316L;

	public CatFcstCeilingHeightCond() {
		super( UNIT );
	}

}