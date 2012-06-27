/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;

/**
 * @author archana
 *
 */
public class TurbulenceFrequencySymbol extends AbstractMetParameter implements
		Dimensionless {

	public TurbulenceFrequencySymbol() {
		super(UNIT);
		setValueIsString();
	}

}