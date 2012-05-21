/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import java.util.Calendar;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccess;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccessManager;

import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.time.DataTime;

 /**
 * @author archana
 *
 */
 public class ClimDayTemp extends AbstractMetParameter implements
		Temperature {

	 private static Calendar calendar = Calendar.getInstance(); 

	 public ClimDayTemp() {
	      super( UNIT );
	}	 
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological Day-time temperature.";
	 }
	 
	 @DeriveMethod
	 public ClimDayTemp derive( StationID stnid ) throws InvalidValueException, NullPointerException {

		DataTime dt = stnid.getValidTime();

		if( dt == null ) {
			throw new InvalidValueException("StationID doesn't have a valid time needed");
		}
		
		calendar.setTime( dt.getRefTime() ); 
		Integer month = calendar.get(Calendar.MONTH) + 1; 
		Integer dayOfMonth = calendar.get(Calendar.DAY_OF_MONTH); 
		
		double climateTDYFInF = getClimateTDYF(stnid.valueString, 
				month.toString(), dayOfMonth.toString() ); 
//		System.out.println("=======, climateTDYFInF= "+climateTDYFInF); 

		setValue( new Amount(climateTDYFInF, NonSI.FAHRENHEIT) );
	
		// TODO : if station is not in the db then set the value to missing;
		//setValueToMissing();
		
		return this;
	 }	
 
	 private double getClimateTDYF(String stationId, String month, String day) {
		ClimateDataDbAccess climateDataDbAccess = ClimateDataDbAccessManager.getInstance().getClimateDataDbAccess(); 
		double tdyfClimateValue = climateDataDbAccess.getTDYF(stationId, month, day); 
		return tdyfClimateValue;  
	}


 }
