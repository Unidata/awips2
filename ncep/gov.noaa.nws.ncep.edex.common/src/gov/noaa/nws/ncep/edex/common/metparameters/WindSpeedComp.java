package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter WCMP
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



 public class WindSpeedComp extends AbstractMetParameter
  implements javax.measure.quantity.Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7155251791270662526L;
	public WindSpeedComp() {
		 super( UNIT );
	} 
	@DeriveMethod
	AbstractMetParameter derive ( WindDirection wd, WindSpeed ws, WindCompDirection d) throws InvalidValueException, NullPointerException{
		if ( wd.hasValidValue() && ws.hasValidValue() && d.hasValidValue() ){     
		     Amount windDrct = PRLibrary.prWcmp(wd , ws , d );
		     setValue(windDrct);
		}else
			setValueToMissing();
		
		     return this;
	}
}
