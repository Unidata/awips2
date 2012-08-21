/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;

/**
 * @author archana
 *
 */
public class IcingIntensitySymbol extends AbstractMetParameter implements
		Dimensionless {

	public IcingIntensitySymbol() {
		super(UNIT);
		setValueIsString();
	}

}
