/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccess;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccessManager;

import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;

/**
 * @author archana
 *
 */
 public class NightTempAnomaly extends AbstractMetParameter implements
		Temperature {

	 public NightTempAnomaly() {
		 super( UNIT );
	}
	 
		@DeriveMethod
		public NightTempAnomaly derive( Min24HrTemp min24HrTemp, StationID stationId ) throws InvalidValueException, NullPointerException, InvalidRangeException {
			if ( min24HrTemp.hasValidValue() && stationId.hasValidValue() ){

				// See DayTempAnomaly
				//				String month = SelectedFrameTimeUtil.getFrameTimeMonthStringValue(); 
//				String dayOfMonth = SelectedFrameTimeUtil.getFrameTimeDayOfMonthStringValue(); 
//				double climateTNTFInF = getClimateTNTFInF(stationId.valueString, month, dayOfMonth); 
//				double min24HrTempInF = min24HrTemp.getValueAs(NonSI.FAHRENHEIT).doubleValue(); 
//				double finalTNAFInF = min24HrTempInF - climateTNTFInF; 
//				System.out.println("=======, min24HrTempInF= "+min24HrTempInF); 
//				System.out.println("=======, climateTNTFInF= "+climateTNTFInF); 
//				System.out.println("=======, finalTNAFInF= "+finalTNAFInF); 
				
				/*
				 * tnaf: Night Temp anomaly in F
				 */
//			      Amount tnafAmount = new Amount(finalTNAFInF, NonSI.FAHRENHEIT); 
//			      setValue(tnafAmount);
			}else
				setValueToMissing();
			return this;
		}
		
		private double getClimateTNTFInF(String stationId, String month, String day) {
			ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
			double tntfClimateValue = climateDataDbAccess.getTNTF(stationId, month, day); 
			return tntfClimateValue;  
		}
 }
