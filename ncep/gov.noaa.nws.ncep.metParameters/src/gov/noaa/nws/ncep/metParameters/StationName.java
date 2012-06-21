package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class StationName extends AbstractMetParameter implements Dimensionless {

	 // StationName is a String and so the Amount value does not apply	 
     public StationName() {
		 super( UNIT );
		 setValueIsString();
	}	
	
 	// NOT Implemented. Just enough to execute and return something
	@DeriveMethod
    public AbstractMetParameter getStationNameFromID( StationID stnId ) throws InvalidValueException, NullPointerException, InvalidRangeException{
    	 this.setStringValue(stnId.getStringValue() );
    	 return this;
     }
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 