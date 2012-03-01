/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
//import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
//import gov.noaa.nws.ncep.metparameters.Amount;
//import gov.noaa.nws.ncep.metparameters.ClimateDataDbAccess;
//import gov.noaa.nws.ncep.metparameters.POPAnomalyIn24hrs;
//import gov.noaa.nws.ncep.metparameters.POPFcst24Hrs;
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
 * Maps to the GEMPAK parameter PP2A
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class POPAnomalyIn24hrs extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 8690261205610416205L;

	public POPAnomalyIn24hrs() {
			super( UNIT );
	}	 
//	
//	
//	@DeriveMethod
//	public POPAnomalyIn24hrs derive( POPFcst24Hrs popFcst24Hrs, StationID stationId ) throws InvalidValueException, NullPointerException  {
//		if ( popFcst24Hrs.hasValidValue() && stationId.hasValidValue() ){
//			String month = SelectedFrameTimeUtil.getFrameTimeMonthStringValue(); 
//			String dayOfMonth = SelectedFrameTimeUtil.getFrameTimeDayOfMonthStringValue(); 
//			double climatePP24 = getClimatePP24(stationId.valueString, month, dayOfMonth); 
//			double finalPP2A = popFcst24Hrs.doubleValue() - climatePP24; 
////			System.out.println("=======, popFcst24Hrs.doubleValue()= "+popFcst24Hrs.doubleValue()); 
////			System.out.println("=======, climatePP24= "+climatePP24); 
////			System.out.println("=======, finalPP2A= "+finalPP2A); 
//			
//			/*
//			 * pp2a: Probability of precipitation anomaly in a 24-hr period in percentage
//			 * pp2a = pp24 - pp24(climate)
//			 */
//		      Amount pp2aAmount = new Amount(finalPP2A, NonSI.PERCENT); 
//		      setValue(pp2aAmount);
//		}else
//			setValueToMissing();
//		return this;
//	}
//	
//	private double getClimatePP24(String stationId, String month, String day) {
//		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
//		double pp24ClimateValue = climateDataDbAccess.getPP24(stationId, month, day); 
//		return pp24ClimateValue;  
//	}
	
 }

