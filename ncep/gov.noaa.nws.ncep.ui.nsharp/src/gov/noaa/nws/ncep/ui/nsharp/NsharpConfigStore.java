/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding
 * 										
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import java.util.HashMap;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "NsharpConfigStore")
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpConfigStore implements ISerializableObject{

	//@XmlElement(name = "NsharpLineProperty", required = true)
	//private NsharpLineProperty lp;
	
	
	//public NsharpLineProperty getLp() {
	//	return lp;
	//}

	//public void setLp(NsharpLineProperty lp) {
	//	this.lp = lp;
	//}
	
	@XmlElement
	private NsharpGraphProperty graphProperty = new NsharpGraphProperty();
	
	@XmlElement
	//@XmlJavaTypeAdapter(NsharpConfigHashMapAdaptor.class)
	@XmlJavaTypeAdapter(value = NsharpConfigHashMapAdaptor.class)
	private HashMap<String, NsharpLineProperty> linePropertyMap = new HashMap<String, NsharpLineProperty>();
	

	
	public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
		return linePropertyMap;
	}

	public void setLinePropertyMap(
			HashMap<String, NsharpLineProperty> linePropertyMap) {
		this.linePropertyMap = linePropertyMap;
				
	}

	public NsharpGraphProperty getGraphProperty() {
		return graphProperty;
	}

	public void setGraphProperty(NsharpGraphProperty graphProperty) {
		this.graphProperty = graphProperty;
	}
	
	
}
