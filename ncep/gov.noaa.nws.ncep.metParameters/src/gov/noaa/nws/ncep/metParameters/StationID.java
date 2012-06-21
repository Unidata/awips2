package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class StationID extends AbstractMetParameter implements Dimensionless {

	 // StationName is a String and so the Amount value does not apply
//	 @Override
//	 public Boolean hasStringValue() {
//		 return true;
//	 }
	 
     public StationID() {
		 super( UNIT );
		 setValueIsString();
	}	
	
// 	// NOT Implemented. Just enough to execute and return something
//	@DeriveMethod
//    public AbstractMetParameter getStationIdFromName( StationName stnName ) throws InvalidValueException, NullPointerException, InvalidRangeException{
//    	 this.setStringValue(stnName.getStringValue() );
//    	 return this;
//     }
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 