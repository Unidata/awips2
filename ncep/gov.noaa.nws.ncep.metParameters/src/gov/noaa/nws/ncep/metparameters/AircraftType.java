/**
 * 
 */
package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;

/**
 * @author archana
 *
 */
public class AircraftType extends AbstractMetParameter implements Dimensionless {

	public AircraftType() {
		super(UNIT);
		setValueIsString();  
	}

}
