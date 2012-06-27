package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
 
public class TemporaryCeilingAsMeanSeaLevel extends AbstractMetParameter implements Length {

	public TemporaryCeilingAsMeanSeaLevel() {
		 super( UNIT );
	}
	
      @DeriveMethod
	  AbstractMetParameter derive ( TemporaryOrProbabilityCeiling c, StationElevation se) throws InvalidValueException, NullPointerException{
    	 if ( c.hasValidValue() && se.hasValidValue() ){
    	     Amount val = PRLibrary.prCmsl( c, se );
    	     setValue ( val );
    	 }else
    		 setValueToMissing();
    	 return this;
     }
}















