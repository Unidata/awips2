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
 public class Clim24HrPOP extends AbstractMetParameter implements
		Dimensionless {

	 public Clim24HrPOP() {
	      super( UNIT );
	}
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological 24 Hour Probability of Precipitation.";
	 }


 }