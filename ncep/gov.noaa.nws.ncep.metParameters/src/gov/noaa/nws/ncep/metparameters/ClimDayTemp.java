/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

 /**
 * @author archana
 *
 */
 public class ClimDayTemp extends AbstractMetParameter implements
		Temperature {

	 public ClimDayTemp() {
	      super( UNIT );
	}	 
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological Day-time temperature.";
	 }
 }
