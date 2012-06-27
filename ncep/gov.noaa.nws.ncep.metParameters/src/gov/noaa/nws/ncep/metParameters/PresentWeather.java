package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class PresentWeather extends AbstractMetParameter implements Dimensionless {

	 // The SkyCoverage enum value is stored as a String. 
//	 @Override
//	 public Boolean hasStringValue() {
//		 return true;
//	 }
//	 
     public PresentWeather() {
		 super( UNIT );
		 setValueIsString();
	}	
	
  }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 