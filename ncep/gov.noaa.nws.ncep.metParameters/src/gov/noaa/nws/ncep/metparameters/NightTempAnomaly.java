/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

/**
 * @author archana
 *
 */
 public class NightTempAnomaly extends AbstractMetParameter implements
		Temperature {

	 public NightTempAnomaly() {
		 super( UNIT );
	}
	 
 }
