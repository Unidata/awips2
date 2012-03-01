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
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;


/**
 * Maps to the GEMPAK parameter SMXR
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public final class SurfaceMixingRatio extends AbstractMetParameter implements Dimensionless, ISerializableObject {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -8085056178927715836L;

	public SurfaceMixingRatio() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
	
 	@DeriveMethod
	public SurfaceMixingRatio derive ( DewPointTemp d , SurfacePressure p ) throws InvalidValueException, NullPointerException {
		if ( d.hasValidValue() && p.hasValidValue() ){
 		         Amount mixingRatio = PRLibrary.prMixr( d, p );
		         setValue(mixingRatio);
		}else
			setValueToMissing();
		return this;
	} 	
}
