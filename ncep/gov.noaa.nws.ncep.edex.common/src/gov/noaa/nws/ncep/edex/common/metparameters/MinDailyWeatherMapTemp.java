package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter DMIN
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



public class MinDailyWeatherMapTemp extends AbstractMetParameter implements
javax.measure.quantity.Temperature, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2749065884345131481L;

	public MinDailyWeatherMapTemp() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( Min6HrTemp t00x, Min6HrTemp t06x ) throws InvalidValueException, NullPointerException{
		if ( t00x.hasValidValue() && t06x.hasValidValue() ){
		      Amount val = PRLibrary.prDmin(t00x, t06x);
		      setValue(val);
		}else
			setValueToMissing();
		
		return this;
	}
	
}












