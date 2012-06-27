package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;


  public class FlightRulesID extends AbstractMetParameter implements javax.measure.quantity.Dimensionless {
      
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
	  
	  public FlightRulesID() {
		  super( UNIT );
	      flightRuleCategory = FlightRuleCategory.UNKNOWN;
	}
	  
	@DeriveMethod
	    AbstractMetParameter derive( CeilingFromSurface c, Visibility v) throws InvalidValueException, NullPointerException{
		      if ( c.hasValidValue() && v.hasValidValue() ){
		           Amount val = PRLibrary.prXvfr( c, v); 
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

  
  
  
  
  
  
  
  
  