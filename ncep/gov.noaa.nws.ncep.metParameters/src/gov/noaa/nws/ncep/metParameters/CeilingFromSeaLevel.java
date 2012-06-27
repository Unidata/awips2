package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;

 public class CeilingFromSeaLevel extends AbstractMetParameter implements Length {

	public CeilingFromSeaLevel() {
	      super( UNIT );
	}
	
	 @Override
	 public String getParameterDescription( ) {
		 return "The Ceiling as measured from sea level.";
	 }

     @DeriveMethod //TODO cross check the validity of this equation
	  AbstractMetParameter derive ( CeilingFromSurface c, StationElevation se) throws InvalidValueException, NullPointerException{
    	if ( c.hasValidValue() && se.hasValidValue() ){
    	     Amount val = PRLibrary.prCmsl( c, se );
    	     setValue( val );
    	}else
    		setValueToMissing();
    	
    	 return this; 
     }
 }
