/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;

/**
 * @author archana
 *
 */
public class IcingTypeSymbol extends AbstractMetParameter implements
		Dimensionless {

	public IcingTypeSymbol() {
		super(UNIT);
		setValueIsString();
	}

}
