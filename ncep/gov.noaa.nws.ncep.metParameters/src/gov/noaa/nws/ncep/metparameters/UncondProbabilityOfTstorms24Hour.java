/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

/**
 * @author archana
 *
 */
 public class UncondProbabilityOfTstorms24Hour extends AbstractMetParameter implements
		Dimensionless {

	 public UncondProbabilityOfTstorms24Hour() {
		 super( UNIT );
	}
	 
 }