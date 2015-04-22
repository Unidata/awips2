package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameters FELV or SELV depending on the
 * unit used to measure the flight level
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


  public class FlightLevel extends AbstractMetParameter implements Length {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7636793669183105894L;

	public FlightLevel() {
	  super( UNIT );
	}	
  }