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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TDAF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class DayTempAnomaly extends AbstractMetParameter implements
		Temperature, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 6489210440084721750L;

	public DayTempAnomaly()throws Exception{
	    super(UNIT);     
	}
	
	@DeriveMethod
	public DayTempAnomaly derive( MaxDayTemp maxTemp, ClimDayTemp climDayTemp ) throws InvalidValueException, NullPointerException{
		
		if ( maxTemp == null 
				|| climDayTemp == null 
				|| !maxTemp.hasValidValue()
				||!climDayTemp.hasValidValue()){
			
			setUnit(NonSI.FAHRENHEIT);
			return this;
		}
			
			double tmax = maxTemp.getValueAs("°F").doubleValue();
			double tclim = climDayTemp.getValueAs("°F").doubleValue();
			Double anomalyTemp = new Double (tmax - tclim);
			setValueAs(anomalyTemp, "°F");
	        return this;
	}

	@DeriveMethod
	public DayTempAnomaly derive( Max24HrTemp maxTemp, ClimDayTemp climDayTemp ) throws InvalidValueException, NullPointerException{
		
		if ( maxTemp == null 
				|| climDayTemp == null 
				|| !maxTemp.hasValidValue()
				||!climDayTemp.hasValidValue()){
			
			setUnit(NonSI.FAHRENHEIT);
			return this;
		}
			
			double tmax = maxTemp.getValueAs("°F").doubleValue();
			double tclim = climDayTemp.getValueAs("°F").doubleValue();
			Double anomalyTemp = new Double (tmax - tclim);
			setValueAs(anomalyTemp, "°F");
	        return this;
	}
	
	
 }
