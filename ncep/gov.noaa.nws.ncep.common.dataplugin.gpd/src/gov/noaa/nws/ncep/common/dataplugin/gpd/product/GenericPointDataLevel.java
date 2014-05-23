/**
 * 
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.gpd.product;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class GenericPointDataLevel {
	@DynamicSerializeElement
	@XmlAttribute
	private float levelValue;
	
	@DynamicSerializeElement
	@XmlElement(name = "GPD-Parameter")
	private List<GenericPointDataParameter> gpdParameters = new ArrayList<GenericPointDataParameter>();

	

	public GenericPointDataLevel() {
		super();
		// TODO Auto-generated constructor stub
	}

	public GenericPointDataLevel(float levelValue, 
			List<GenericPointDataParameter> gpdParameters) {
		super();
		this.levelValue = levelValue;
		this.gpdParameters = gpdParameters;
	}

	

	public GenericPointDataLevel(float levelValue) {
		super();
		this.levelValue = levelValue;
	}

	public float getLevelValue() {
		return levelValue;
	}

	public void setLevelValue(float levelValue) {
		this.levelValue = levelValue;
	}

	public List<GenericPointDataParameter> getGpdParameters() {
		return gpdParameters;
	}

	public void setGpdParameters(List<GenericPointDataParameter> gpdParameters) {
		this.gpdParameters = gpdParameters;
	}

}
