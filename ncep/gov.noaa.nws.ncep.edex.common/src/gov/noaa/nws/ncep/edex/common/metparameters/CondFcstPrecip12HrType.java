package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CondFcstPrecip12HrType extends AbstractMetParameter implements
Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -7746863199784977880L;

	public CondFcstPrecip12HrType() {
		  super( UNIT );
	}
	 
}