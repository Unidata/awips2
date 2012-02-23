package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.quantity.HeatFlux;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Avg1HrHeatFlux extends AbstractMetParameter implements HeatFlux, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5727639156619032638L;

	public Avg1HrHeatFlux(){ 
	     super( UNIT );
	}
}
