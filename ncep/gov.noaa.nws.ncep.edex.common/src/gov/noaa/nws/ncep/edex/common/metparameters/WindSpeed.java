package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;



import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Maps to any of the GEMPAK parameters SPED (m/sec), SKNT (knots) 
 * or SMPH (miles/hour) based on the units in which the wind speed needs to be computed
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



 public class WindSpeed extends AbstractMetParameter implements
							javax.measure.quantity.Velocity, ISerializableObject {

	/**
	 * 
	 */
	@DynamicSerializeElement
	private static final long serialVersionUID = -4498547565649728275L;

	public WindSpeed() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
	
	@DeriveMethod
	public WindSpeed derive ( WindDirectionUComp u, WindDirectionVComp v ) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
		   Amount val = PRLibrary.prSped( u, v );
		   setValue ( val );
		}else
			setValueToMissing();
		
		return this;
	}
//	@DeriveMethod
//	public AbstractMetParameter getWindSpeedFromWindBarb( WindBarb wb ) {
//		return wb.getWindSpeed();
//	}
}