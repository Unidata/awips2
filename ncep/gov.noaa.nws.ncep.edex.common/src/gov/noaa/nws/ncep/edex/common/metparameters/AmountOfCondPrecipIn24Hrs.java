package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.quantity.AmountOfPrecipitation;

import java.io.Serializable;

import javax.measure.quantity.AmountOfSubstance;
import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Maps to the bufrmos parameter condPrecipAmt_12hr
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AmountOfCondPrecipIn24Hrs extends AbstractMetParameter implements
		AmountOfPrecipitation, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7386803479446558071L;
public AmountOfCondPrecipIn24Hrs() throws Exception {
	super( new UnitAdapter().marshal(UNIT) );
	}	
}
