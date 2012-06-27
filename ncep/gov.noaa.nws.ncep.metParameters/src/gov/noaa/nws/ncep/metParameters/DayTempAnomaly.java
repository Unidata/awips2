/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import java.util.Calendar;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccess;
import gov.noaa.nws.ncep.metparameters.dbquery.util.ClimateDataDbAccessManager;

import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;

import com.raytheon.uf.common.time.DataTime;

/**
 * @author archana
 *
 */
 public class DayTempAnomaly extends AbstractMetParameter implements
		Temperature {
	 	 
	 public DayTempAnomaly() {
			super( UNIT );
	}
	 
		@DeriveMethod
		public DayTempAnomaly derive( Max24HrTemp max24HrTemp, ClimDayTemp climDayTemp ) throws InvalidValueException, NullPointerException, InvalidRangeException {

			if ( max24HrTemp.hasValidValue() && climDayTemp.hasValidValue() ){
				 
//				if( max24HrTemp.hasValidTime() && climDayTemp.hasValidTime() ) {
//					
//				}
				double climateTDYFInF = climDayTemp.getValueAs( NonSI.FAHRENHEIT ).doubleValue(); 
				double max24HrTempInF = max24HrTemp.getValueAs(NonSI.FAHRENHEIT).doubleValue();  
				double finalTDYFInF = max24HrTempInF - climateTDYFInF; 
//				System.out.println("=======, max24HrTempInF= "+max24HrTempInF); 
//				System.out.println("=======, climateTDYFInF= "+climateTDYFInF); 
//				System.out.println("=======, finalTDYFInF= "+finalTDYFInF); 
				/*
				 * tdaf: Day Temp anomaly in F
				 */
			      Amount tdafAmount = new Amount(finalTDYFInF, NonSI.FAHRENHEIT); 
			      setValue(tdafAmount);
			      setValidTime( max24HrTemp.getValidTime() );
			}else
				setValueToMissing();
			return this;
		}		
 }
