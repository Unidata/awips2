/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
//import gov.noaa.nws.ncep.metparameters.Amount;
//import gov.noaa.nws.ncep.metparameters.ClimateDataDbAccess;
//import gov.noaa.nws.ncep.metparameters.DayTempAnomaly;
//import gov.noaa.nws.ncep.metparameters.Max24HrTemp;
//import gov.noaa.nws.ncep.metparameters.StationID;
//import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

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

	public DayTempAnomaly() {
			super( UNIT );
	}
	 
//	@DeriveMethod
//	public DayTempAnomaly derive( Max24HrTemp max24HrTemp, StationID stationId ) throws InvalidValueException, NullPointerException  {
//		if ( max24HrTemp.hasValidValue() && stationId.hasValidValue() ){
//			String month = SelectedFrameTimeUtil.getFrameTimeMonthStringValue(); 
//			String dayOfMonth = SelectedFrameTimeUtil.getFrameTimeDayOfMonthStringValue(); 
//			double climateTDYFInF = getClimateTDYF(stationId.valueString, month, dayOfMonth); 
////			double max24HrTempInF = TemperatureConverter.convertToFahrenheitFromKelvin(max24HrTemp.doubleValue()); 
//			double max24HrTempInF = max24HrTemp.getValueAs(NonSI.FAHRENHEIT).doubleValue();  
//			double finalTDYFInF = max24HrTempInF - climateTDYFInF; 
////			System.out.println("=======, max24HrTempInF= "+max24HrTempInF); 
////			System.out.println("=======, climateTDYFInF= "+climateTDYFInF); 
////			System.out.println("=======, finalTDYFInF= "+finalTDYFInF); 
//			/*
//			 * tdaf: Day Temp anomaly in F
//			 */
//		      Amount tdafAmount = new Amount(finalTDYFInF, NonSI.FAHRENHEIT); 
//		      setValue(tdafAmount);
//		}else
//			setValueToMissing();
//		return this;
//	}
//	
//	private double getClimateTDYF(String stationId, String month, String day) {
//		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
//		double tdyfClimateValue = climateDataDbAccess.getTDYF(stationId, month, day); 
//		return tdyfClimateValue;  
//	}
	
 }
