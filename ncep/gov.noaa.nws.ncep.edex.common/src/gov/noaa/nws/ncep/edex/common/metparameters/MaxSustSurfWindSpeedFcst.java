package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SK12
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class MaxSustSurfWindSpeedFcst extends AbstractMetParameter implements
 javax.measure.quantity.Velocity, ISerializableObject {

	/*
	 * This maps to legacy GFSXMOS parameter SK12, which is observed and not derived.
	 * On NMAP, SK12 is displayed as specific wind-speeds at specific stations. The categorical range 
	 * values defined for this parameter, in sfparm.hl2 are not used in the GUI...
	 * TODO :  Clarify whether or not these ranges need to be implemented at all... 
	 */
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -6413683612711961442L;

	public MaxSustSurfWindSpeedFcst(){
		 super( UNIT );
	}	
}