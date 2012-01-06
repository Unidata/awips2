/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
 
/**
 * @author archana
 *
 */
public class FcstSnowIcePelletAccumToWatchThresh extends AbstractMetParameter
		implements Dimensionless {

	public FcstSnowIcePelletAccumToWatchThresh(){
    	super( UNIT );
	}
	
	@DeriveMethod
       AbstractMetParameter derive ( FcstSnowIcePelletAccumulation12Hrs si12, SnowIcePelletWatchThresh snip){
		if ( si12.hasValidValue() && snip.hasValidValue() ){
		     Amount val = new Amount ( si12.doubleValue() / snip.doubleValue() , Unit.ONE );
		     setValue( val );
		}else
			setValueToMissing();
		return this;
	  }
}