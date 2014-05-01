package gov.noaa.nws.ncep.edex.common.metparameters;


import gov.noaa.nws.ncep.edex.common.metparameters.StationElevation;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * TODO: Cross-check if it maps to the GEMPAK parameter CEIL or CMSL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 public class CeilingFromSeaLevel extends AbstractMetParameter implements
 Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1967627991712235906L;

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
