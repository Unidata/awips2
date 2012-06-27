/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

/**
 * @author archana
 *
 */
public class SnowIcePelletWatchThresh extends AbstractMetParameter
		implements Length {

	public SnowIcePelletWatchThresh(){
		 super( UNIT );
	}
	
 }