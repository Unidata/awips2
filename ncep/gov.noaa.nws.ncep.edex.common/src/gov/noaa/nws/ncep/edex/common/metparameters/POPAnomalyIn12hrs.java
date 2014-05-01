/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
//import gov.noaa.nws.ncep.metparameters.Amount;
//import gov.noaa.nws.ncep.metparameters.ClimateDataDbAccess;
//import gov.noaa.nws.ncep.metparameters.POPAnomalyIn12hrs;
//import gov.noaa.nws.ncep.metparameters.POPFcst12Hrs;
//import gov.noaa.nws.ncep.metparameters.StationID;
//import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.NonSI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PP1A
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class POPAnomalyIn12hrs extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 6717100189995101363L;

	public POPAnomalyIn12hrs() {
		 super( UNIT );
	}
	 
//	@DeriveMethod
//	public POPAnomalyIn12hrs derive( POPFcst12Hrs popFcst12Hrs, StationID stationId ) throws InvalidValueException, NullPointerException  {
//		if ( popFcst12Hrs.hasValidValue() && stationId.hasValidValue() ){
//			String month = SelectedFrameTimeUtil.getFrameTimeMonthStringValue(); 
//			String dayOfMonth = SelectedFrameTimeUtil.getFrameTimeDayOfMonthStringValue(); 
//			double climatePPNT = getClimatePPNT(stationId.valueString, month, dayOfMonth); 
//			double finalPP1A = popFcst12Hrs.doubleValue() - climatePPNT; 
////			System.out.println("=======, popFcst12Hrs.doubleValue()= "+popFcst12Hrs.doubleValue()); 
////			System.out.println("=======, climatePPNT= "+climatePPNT); 
////			System.out.println("=======, finalPP1A= "+finalPP1A); 
//			
//			/*
//			 * pp1a: Probability of precipitation anomaly in a 12-hr period in percentage
//			 * pp1a = pp12 - ppnt(climate)
//			 */
//		      Amount pp1aAmount = new Amount(finalPP1A, NonSI.PERCENT); 
//		      setValue(pp1aAmount);
//		}else
//			setValueToMissing();
//		return this;
//	}
//	
//	private double getClimatePPNT(String stationId, String month, String day) {
//		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
//		double ppntClimateValue = climateDataDbAccess.getPPNT(stationId, month, day); 
//		return ppntClimateValue;  
//	}
	
  }

