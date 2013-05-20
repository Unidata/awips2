/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TNAF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class NightTempAnomaly extends AbstractMetParameter implements
		Temperature, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -8221554987555370951L;

	public NightTempAnomaly() throws Exception {
		super( new UnitAdapter().marshal(UNIT) );
	}
	
	@DeriveMethod
	public NightTempAnomaly derive( MinNightTemp minTemp, ClimNightTemp climNightTemp ) throws InvalidValueException, NullPointerException{
		
		if ( minTemp == null 
				|| climNightTemp == null 
				|| !minTemp.hasValidValue()
				||!climNightTemp.hasValidValue()){
			
			setUnit(NonSI.FAHRENHEIT);
			return this;
		}
			
			double tmax = minTemp.getValueAs("°F").doubleValue();
			double tclim = climNightTemp.getValueAs("°F").doubleValue();
			Double anomalyTemp = new Double (tmax - tclim);
			setValueAs(anomalyTemp, "°F");
	        return this;
	}

	@DeriveMethod
	public NightTempAnomaly derive( Min24HrTemp minTemp, ClimNightTemp climNightTemp ) throws InvalidValueException, NullPointerException{
		
		if ( minTemp == null 
				|| climNightTemp == null 
				|| !minTemp.hasValidValue()
				||!climNightTemp.hasValidValue()){
			
			setUnit(NonSI.FAHRENHEIT);
			return this;
		}
			
			double tmax = minTemp.getValueAs("°F").doubleValue();
			double tclim = climNightTemp.getValueAs("°F").doubleValue();
			Double anomalyTemp = new Double (tmax - tclim);
			setValueAs(anomalyTemp, "°F");
	        return this;
	}	
	
 }
