/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;
 
/**
 * @author archana
 *
 */
public class FcstFZRainAccumulationToWatchThresh extends AbstractMetParameter
		implements Dimensionless {

	public FcstFZRainAccumulationToWatchThresh(){
		super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( FcstFZRainAccumulationIn12Hours fz12, FZRainWatchThresh fzrt){
		if ( fz12.hasValidValue() && fzrt.hasValidValue() ){      
		      Amount val = new Amount ( fz12.doubleValue() / fzrt.doubleValue() , Unit.ONE );
        	  setValue( val );
		}else
			setValueToMissing();
        	  return this;
	  }
}





