package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Velocity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PMPH
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class PeakWindSpeed extends AbstractMetParameter implements Velocity, ISerializableObject {
                 /**
	 * 
	 */
	private static final long serialVersionUID = 1574677024600899549L;

				public  PeakWindSpeed ( ){
                	    super ( UNIT );
                 }
 }
