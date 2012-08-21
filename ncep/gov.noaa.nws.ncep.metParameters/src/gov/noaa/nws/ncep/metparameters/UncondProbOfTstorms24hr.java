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
 public class UncondProbOfTstorms24hr extends AbstractMetParameter implements
		Dimensionless {

	 public UncondProbOfTstorms24hr() {
		 super( UNIT );
	}
	 
 }