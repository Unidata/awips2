package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Duration;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class BruntVaisalaPeriod extends AbstractMetParameter implements Duration, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8554062919999416630L;

	public BruntVaisalaPeriod(){
		super( UNIT );
	}
}
