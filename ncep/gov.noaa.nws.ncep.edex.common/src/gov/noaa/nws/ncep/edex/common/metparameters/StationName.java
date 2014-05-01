package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter STNM
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize




 public class StationName extends AbstractMetParameter implements
 Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -5094080499590194267L;

	// StationName is a String and so the Amount value does not apply	 
     public StationName() {
		 super( UNIT );
		 setValueIsString();
	}	
	
 	// NOT Implemented. Just enough to execute and return something
	@DeriveMethod
    public AbstractMetParameter getStationNameFromID( StationID stnId ) throws InvalidValueException, NullPointerException {
    	 this.setStringValue(stnId.getStringValue() );
    	 return this;
     }
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 