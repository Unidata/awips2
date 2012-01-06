package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

// This can also be used to represent a StationPressure.
//
public class SurfacePressure extends AbstractMetParameter implements javax.measure.quantity.Pressure {
     public SurfacePressure(){
		 super( UNIT );
     }
	     
     // TODO : check if this is correct.
// 	@DeriveMethod
//	AbstractMetParameter derive( AirTemperature tmpc, PotentialTemp thta) 
// 					throws InvalidValueException, NullPointerException, InvalidRangeException{
//		  if ( tmpc.hasValidValue() && thta.hasValidValue() ){
// 		           Amount val = PRLibrary.prPres(tmpc, thta );
// 		           setValue ( val );
//		  }else
//			  setValueToMissing();
// 		return this;
// 	}

 	@DeriveMethod
 	AbstractMetParameter derive( SeaLevelPressure altm, StationElevation selv )  
 					throws InvalidValueException, NullPointerException, InvalidRangeException {
 		
 		if( altm.hasValidValue() && selv.hasValidValue() ) {
 			
 			Amount val = PRLibrary.prPalt ( altm, selv );
 			this.setValue( val );
 		}
 		else {
 			this.setValueToMissing();
 		}
 		return this;
 	}
}
