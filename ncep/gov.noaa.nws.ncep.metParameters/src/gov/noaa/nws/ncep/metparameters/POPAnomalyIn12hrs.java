package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccess;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccessManager;

import javax.measure.unit.NonSI;

public class POPAnomalyIn12hrs extends AbstractMetParameter implements javax.measure.quantity.Dimensionless {

	public POPAnomalyIn12hrs(){
		super( UNIT );
	}
	
	@DeriveMethod
	public POPAnomalyIn12hrs derive( POPFcst12Hrs popFcst12Hrs, StationID stationId ) throws InvalidValueException, NullPointerException, InvalidRangeException {
		// SEE ClimDayTemp
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
		return this;
	}
	
	private double getClimatePPNT(String stationId, String month, String day) {
		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
		double ppntClimateValue = climateDataDbAccess.getPPNT(stationId, month, day); 
		return ppntClimateValue;  
	}
}
