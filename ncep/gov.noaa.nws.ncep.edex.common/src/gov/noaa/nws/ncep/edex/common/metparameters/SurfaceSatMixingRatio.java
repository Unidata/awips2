/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import 
gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to the GEMPAK parameter SMXS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public final class SurfaceSatMixingRatio extends AbstractMetParameter
 implements javax.measure.quantity.Dimensionless, ISerializableObject {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -8450901636891230762L;

	public SurfaceSatMixingRatio(){
		 super( UNIT );
	}
	
 	@DeriveMethod
	public SurfaceSatMixingRatio derive ( AirTemperature t , SurfacePressure p ) throws InvalidValueException, NullPointerException {
       if  ( t.hasValidValue() && p.hasValidValue() ){
 		     Amount mixingRatio = PRLibrary.prMixr( t, p );
		     setValue(mixingRatio);
       }
       else
    	   setValueToMissing();
       
		return this;
	}
}
