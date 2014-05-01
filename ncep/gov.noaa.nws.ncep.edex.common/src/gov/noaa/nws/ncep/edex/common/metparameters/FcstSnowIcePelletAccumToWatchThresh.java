/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.SnowIcePelletWatchThresh;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 
/**
 * Maps to the GEMPAK parameter SNRT
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class FcstSnowIcePelletAccumToWatchThresh extends AbstractMetParameter
		implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4266261492936535275L;

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