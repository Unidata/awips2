/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.StationElevation;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter ??
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CeilingFromSurface extends AbstractMetParameter implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6058298343743033424L;

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
