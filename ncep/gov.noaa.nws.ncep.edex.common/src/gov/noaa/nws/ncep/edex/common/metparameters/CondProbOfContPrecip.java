/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the Bufrmos parameter POP_rain (new GEMPAK alias used - PORA)
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class CondProbOfContPrecip extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -828875553945419033L;

	public CondProbOfContPrecip()throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}

 }