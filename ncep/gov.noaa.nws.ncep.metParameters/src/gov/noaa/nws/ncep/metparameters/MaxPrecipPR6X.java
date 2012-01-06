package gov.noaa.nws.ncep.metparameters;

import java.util.List;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
 
 public class MaxPrecipPR6X extends AbstractMetParameter implements Length {
         
	public MaxPrecipPR6X() {
		 super( UNIT );
	}
	 
    @DeriveMethod
    AbstractMetParameter derive( Precipitation p00z,Precipitation p06z,Precipitation p12z,Precipitation p18z ) throws InvalidValueException, NullPointerException{
    	Amount val = PRLibrary.prPr6x( p00z, p06z, p12z, p18z );
    	setValue(val);
    	return this;

    }


 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 