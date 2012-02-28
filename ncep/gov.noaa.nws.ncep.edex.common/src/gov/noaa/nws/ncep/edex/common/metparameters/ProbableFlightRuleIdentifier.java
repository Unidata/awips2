package gov.noaa.nws.ncep.edex.common.metparameters;


import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * Maps to the GEMPAK parameter TXVF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 

public class ProbableFlightRuleIdentifier extends AbstractMetParameter
		implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2175227267072536120L;

	public ProbableFlightRuleIdentifier() {
		 super( UNIT );
		 flightRuleCategory = FlightRuleCategory.UNKNOWN;
	}
	
	private FlightRuleCategory flightRuleCategory;
	  
    public enum FlightRuleCategory{
		  LIFR( 0 ),  IFR( 1 ),  MVFR( 2 ), VFR( 3 ), UNKNOWN( -9999 );
		  private int code; 
		  private FlightRuleCategory ( int i){
			  code = i;
		  }
		  
		  public int getCode(){
			  return code;
		  }
	  }
	  
	@DeriveMethod
	    AbstractMetParameter derive( ProbableCeiling c, ProbableVisibility v) throws InvalidValueException, NullPointerException{
		      if ( c.hasValidValue() && v.hasValidValue() ){
		           Amount val = PRLibrary.prXvfr( c, v); 
		           setValue( val );
		      }else
		    	   setValueToMissing();
		      return this; 
	  }
	
	/**
	 * @return the flightRuleCategory
	 */
	public final FlightRuleCategory getFlightRuleCategory() {
		return flightRuleCategory;
	}
 
}
