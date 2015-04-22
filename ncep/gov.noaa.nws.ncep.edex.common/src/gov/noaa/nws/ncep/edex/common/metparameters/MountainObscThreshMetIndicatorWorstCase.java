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
 * Maps to the GEMPAK parameter WMOB
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class MountainObscThreshMetIndicatorWorstCase extends
		AbstractMetParameter implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7188732050781835300L;

	public MountainObscThreshMetIndicatorWorstCase() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( CeilingFromSeaLevelWorstCase wcms, MountainObscThresh motv) throws InvalidValueException, NullPointerException{
		if ( wcms.hasValidValue() && motv.hasValidValue() ){
		        Amount val = PRLibrary.prMobs(wcms, motv );
		        setValue ( val );
		}else
			setValueToMissing();
		
		return this;
	}
}










