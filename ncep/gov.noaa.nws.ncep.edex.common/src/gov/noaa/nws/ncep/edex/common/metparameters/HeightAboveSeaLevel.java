package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Length;
import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/29/2011              qzhou       Added STDZ format
 * 
 * </pre>
 * 
 * @author 
 * @version 1.0
 */

/**
 * Maps to either of the GEMPAK parameters  HGHT or STDZ
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class HeightAboveSeaLevel extends AbstractMetParameter
		implements Length, ISerializableObject {

	/**
	 * 
	 */
@DynamicSerializeElement
	private static final long serialVersionUID = -3831219033024527337L;

@Override
public String getFormattedString( String formatStr ) {
	if( formatStr == null || formatStr.isEmpty() ||
		formatStr.startsWith("%" ) ) {
		return super.getFormattedString( formatStr );
	}
	else if( !formatStr.equals("STDZ") ) {
		return super.getFormattedString( formatStr );
	}
	
	Number valInMeters = getValueAs( SI.METER );
	
	// TODO : add code to create the abbreviated string.

	String valueStr = "";
	int value = valInMeters.intValue();
	
	if (!hasValidValue()) {
		valueStr = "";
	}
	else if (value < 10) {
		valueStr = "00" + Integer.toString(value);
	}
	else if (value < 100) {
		valueStr = "0" + formatStr;
	}		
	else if (value < 1000) {
		valueStr = Integer.toString(value);
	}
	else if (value < 10000) {
		if (value %10 < 5)
			valueStr = Integer.toString((int) (value/10));
		else
			valueStr = Integer.toString((int) (value/10 +1));
	}
	else if (value >= 10000) {
		valueStr = Integer.toString(value);
		valueStr = valueStr.substring(1, 4);
	}
	
	return valueStr; 		
}
	 
	public HeightAboveSeaLevel() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
}
