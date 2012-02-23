/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;

//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
//import gov.noaa.nws.ncep.metparameters.Amount;
//import gov.noaa.nws.ncep.metparameters.ClimateDataDbAccess;
//import gov.noaa.nws.ncep.metparameters.Min24HrTemp;
//import gov.noaa.nws.ncep.metparameters.NightTempAnomaly;
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

	public NightTempAnomaly() {
		 super( UNIT );
	}
	
	 
//	@DeriveMethod
//	public NightTempAnomaly derive( Min24HrTemp min24HrTemp, StationID stationId ) throws InvalidValueException, NullPointerException  {
//		if ( min24HrTemp.hasValidValue() && stationId.hasValidValue() ){
//			String month = SelectedFrameTimeUtil.getFrameTimeMonthStringValue(); 
//			String dayOfMonth = SelectedFrameTimeUtil.getFrameTimeDayOfMonthStringValue(); 
//			double climateTNTFInF = getClimateTNTFInF(stationId.valueString, month, dayOfMonth); 
//			double min24HrTempInF = min24HrTemp.getValueAs(NonSI.FAHRENHEIT).doubleValue(); 
//			double finalTNAFInF = min24HrTempInF - climateTNTFInF; 
////			System.out.println("=======, min24HrTempInF= "+min24HrTempInF); 
////			System.out.println("=======, climateTNTFInF= "+climateTNTFInF); 
////			System.out.println("=======, finalTNAFInF= "+finalTNAFInF); 
//			
//			/*
//			 * tnaf: Night Temp anomaly in F
//			 */
//		      Amount tnafAmount = new Amount(finalTNAFInF, NonSI.FAHRENHEIT); 
//		      setValue(tnafAmount);
//		}else
//			setValueToMissing();
//		return this;
//	}
//	
//	private double getClimateTNTFInF(String stationId, String month, String day) {
//		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
//		double tntfClimateValue = climateDataDbAccess.getTNTF(stationId, month, day); 
//		return tntfClimateValue;  
//	}
	
 }
