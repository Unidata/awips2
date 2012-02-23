package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.ProbableCeilingAsMeanSeaLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter WCMS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize 
public class CeilingFromSeaLevelWorstCase extends AbstractMetParameter
implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5379275105368701383L;

	public CeilingFromSeaLevelWorstCase(){
		 super( UNIT );
	}

	@DeriveMethod
	 AbstractMetParameter derive  ( CeilingFromSeaLevel  cmsl, ProbableCeilingAsMeanSeaLevel tcms ){
		if ( cmsl.hasValidValue() ){
		   Amount val = ( !tcms.hasValidValue()   ? cmsl : tcms  ); //prWcms
		   setValue( val );
		}
		else
			setValueToMissing();
		return this;
	}
}
