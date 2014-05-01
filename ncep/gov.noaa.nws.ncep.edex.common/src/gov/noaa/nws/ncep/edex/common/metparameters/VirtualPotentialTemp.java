/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;


import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Temperature;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter THTV
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class VirtualPotentialTemp extends AbstractMetParameter implements
		Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7559368640781326821L;


	public VirtualPotentialTemp() {
		 super( UNIT );
	}
	

//	@DeriveMethod
//      AbstractMetParameter derive(VirtualTemp v, SurfacePressure s ) throws   InvalidValueException, NullPointerException{
//    	   if ( v.hasValidValue() && s.hasValidValue() ){ 
//		        Amount val = PRLibrary.prThta( v, s ) ;  
//    	        setValue ( val );
//    	   }else
//    		   setValueToMissing();
//    	        return this;
//      }

	@DeriveMethod
    AbstractMetParameter derive(VirtualTemp v, PressureLevel p ) throws   InvalidValueException, NullPointerException{
  	        Amount val = PRLibrary.prThta( v , p ) ;  
  	        setValue ( val );
		        return this;
    }
	
}




