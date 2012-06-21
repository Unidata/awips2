/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

/**
 * @author archana
 *
 */
public class VirtualPotentialTemp extends AbstractMetParameter implements
		Temperature {

	public VirtualPotentialTemp() {
		 super( UNIT );
	}
	

//	@DeriveMethod
//      AbstractMetParameter derive(VirtualTemp v, SurfacePressure s ) throws InvalidRangeException, InvalidValueException, NullPointerException{
//    	   if ( v.hasValidValue() && s.hasValidValue() ){ 
//		        Amount val = PRLibrary.prThta( v, s ) ;  
//    	        setValue ( val );
//    	   }else
//    		   setValueToMissing();
//    	        return this;
//      }

	@DeriveMethod
    AbstractMetParameter derive(VirtualTemp v, PressureLevel p ) throws InvalidRangeException, InvalidValueException, NullPointerException{
  	        Amount val = PRLibrary.prThta( v , p ) ;  
  	        setValue ( val );
		        return this;
    }
	
}




