/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;
/**
 * @author archana
 *
 */
 public class Clim12HrPOP extends AbstractMetParameter implements
		Dimensionless {

	 public Clim12HrPOP() {
	      super( UNIT );
	}	 

	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological 12 Hour Probability of Precipitation.";
	 }

 }