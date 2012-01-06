/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;

/**
 * @author archana
 *
 */
public class CeilingFromSurface extends AbstractMetParameter implements Length {

	public CeilingFromSurface() {
	      super( UNIT );
	}
	
	 @Override
	 public String getParameterDescription( ) {
		 return "The Ceiling as measured from the station or surface.";
	 }

	 // This was not in the PRLibrary but adding it here since it makes sense. 
	 // (TODO : not tested.)
	 @DeriveMethod 
	 AbstractMetParameter derive( CeilingFromSeaLevel csl, StationElevation se) throws InvalidValueException, NullPointerException{
		 if( csl.hasValidValue() && se.hasValidValue() ) {
			 // subtract the surface elevation from the ceiling from sealevel. 
			 Double ceil = csl.getValueAs( getUnit() ).doubleValue() -
			 			    se.getValueAs( getUnit() ).doubleValue();

			 setValue( ceil, getUnit() );    				
		 }
		 else {
			 setValueToMissing();
		 }
		 return this;
	 }

}
