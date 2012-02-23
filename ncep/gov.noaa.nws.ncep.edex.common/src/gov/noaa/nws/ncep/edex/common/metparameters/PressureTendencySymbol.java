/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;


/**
 * Maps to the GEMPAK parameter PTND
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PressureTendencySymbol extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	/**
	 * The parameter values are indexes into the press_change_char_lookup.txt file which
	 * references symbols defined in the WxSymbols.svg file.
	 */
	private static final long serialVersionUID = -4616340047592131749L;

	public PressureTendencySymbol()  {
		 super( UNIT );
		 //setValueIsString(); it has both int and string
	}
}
