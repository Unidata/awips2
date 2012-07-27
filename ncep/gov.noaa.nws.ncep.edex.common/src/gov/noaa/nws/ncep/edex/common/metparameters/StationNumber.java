/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;

/**
 * @author archana
 *
 */
public class StationNumber extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2038229143862266928L;

	public StationNumber()  throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
		 setValueIsString();
	}	
	
}
