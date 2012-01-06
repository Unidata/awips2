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
 public class ClimNightTemp extends AbstractMetParameter implements Temperature {

	 public ClimNightTemp() {
	      super( UNIT );
	 }
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological Night-time temperature.";
	 }
 }
