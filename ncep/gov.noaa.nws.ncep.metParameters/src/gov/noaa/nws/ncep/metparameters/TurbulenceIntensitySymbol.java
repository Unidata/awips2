/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;

/**
 * @author archana
 *
 */
public class TurbulenceIntensitySymbol extends AbstractMetParameter implements
		Dimensionless {

	public TurbulenceIntensitySymbol() {
		super(UNIT);
		setValueIsString();
	}

}
