package gov.noaa.nws.ncep.metparameters;

import java.util.HashMap;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class SkyCoverage extends AbstractMetParameter implements Dimensionless {

	 // The SkyCoverage enum value is stored as a String. 
//	 @Override
//	 public Boolean hasStringValue() {
//		 return true;
//	 }
//	 
     public SkyCoverage() {
		 super( UNIT );
		 setValueIsString();
     }	
	
	
//	@DeriveMethod
//	public AbstractMetParameter determineFromCloudCover( CloudCover[] cldCoverList ) {
//		// TODO : what is the default "", "CLR"
//		if( cldCoverList == null || cldCoverList.length == 0 ) {
//			setStringValue("BLNK");
//			return this;
//		}
//		
//		// TODO : Raytheon reads the cloud_select.txt file to determine the 'rankedField'
//		// but here we'll just encode the rules for determining the skyCoverage from the
//		// various cloud coverages.
//		//
//		for( CloudCover cldCov : cldCoverList ) {
//			
//		}
//		
//		setStringValue("CLR");
//		
//		return this;
//	}
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 