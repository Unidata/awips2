/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;


/**
 * @author archana
 *
 */
public class PressTendency extends AbstractMetParameter implements
		Dimensionless {

	public PressTendency()  {
		 super( UNIT );
		 setValueIsString();
	}

}
