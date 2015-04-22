package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 
/**
 * Maps to the GEMPAK parameter MOBS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class MountainObscThreshMetIndicator extends
 AbstractMetParameter implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2376979287466609756L;

	public MountainObscThreshMetIndicator() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( CeilingFromSeaLevel tcms, MountainObscThresh motv) throws InvalidValueException, NullPointerException{
		Amount val = PRLibrary.prMobs(tcms, motv );
		setValue ( val );
		return this;
	}
}
