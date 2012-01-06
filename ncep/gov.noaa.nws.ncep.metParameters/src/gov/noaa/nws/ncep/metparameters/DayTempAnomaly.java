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
 public class DayTempAnomaly extends AbstractMetParameter implements
		Temperature {

	 public DayTempAnomaly() {
			super( UNIT );
	}
	 
 }
