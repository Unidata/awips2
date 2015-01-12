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
import java.util.Date;
import java.util.EnumSet;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime.FLAG;

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class GenericPointDataStationProduct {

	@DynamicSerializeElement
    @XmlElement(name="GPD-Station-Def")
	//private GenericPointDataLocation location;
	private ObStation location = new ObStation();
	
	@DynamicSerializeElement
	@XmlAttribute
	private float slat=-9999;

	@DynamicSerializeElement
	@XmlAttribute
	private float slon=-9999;
	
	@XmlAttribute
	@DynamicSerializeElement
	private int numLevel;
	
	@DynamicSerializeElement
	@XmlAttribute
	private int productVersion;
	
	@DynamicSerializeElement
	@XmlAttribute
	String productName;
	
	@DynamicSerializeElement
	@XmlAttribute
	protected Date refTime;
	
	@DynamicSerializeElement
	@XmlAttribute
	//set to -1, if forecast time is not used, foe example, for observed surface, observed snd type of data.
	//set forecast time in second.Its value is computed starting from reference time. So,
	//if forecastTime =0, means its forecast time is the same time as reference time
	//if forecastTime =3600, means its forecast time is one hour after the reference time
	//When saving data, this value is actually saved to DataTime in PluginDataObject Object. And 
	//utilityFlags is set to "FCST_USED" to indicate forecast time is used or not.
	protected int forecastTime;
	
	@DynamicSerializeElement
	@XmlAttribute
	private String utilityFlag = null;
	
	//list of master level values
	@DynamicSerializeElement
    @XmlElement(name="GPD-Level-Parameters")
	List<GenericPointDataLevel> levelLst = new ArrayList<GenericPointDataLevel>();


	public GenericPointDataStationProduct() {
		super();
		// TODO Auto-generated constructor stub
	}


	public GenericPointDataStationProduct(int productVersion, String productName) {
		super();
		this.productVersion = productVersion;
		this.productName = productName;
	}


	public void cleanUp(){
		//this.refTime = null;
		this.slat=-9999;
		this.slon=-9999;
		//this.location.setStationId("*");
	}
	public ObStation getLocation() {
		return location;
	}


	public void setLocation(ObStation location) {
		this.location = location;
	}


	public float getSlat() {
		return slat;
	}

	
	public void setSlat(float slat) {
		this.slat = slat;
	}


	public float getSlon() {
		return slon;
	}


	public void setSlon(float slon) {
		this.slon = slon;
	}


	public int getNumLevel() {
		return numLevel;
	}


	public void setNumLevel(int numLevel) {
		this.numLevel = numLevel;
	}


	public List<GenericPointDataLevel> getLevelLst() {
		return levelLst;
	}


	public void setLevelLst(List<GenericPointDataLevel> levelLst) {
		this.levelLst = levelLst;
	}


	@Override
	public Object clone() throws CloneNotSupportedException {
		GenericPointDataStationProduct prod = new GenericPointDataStationProduct();
		return prod;
	}


	public int getProductVersion() {
		return productVersion;
	}


	public void setProductVersion(int productVersion) {
		this.productVersion = productVersion;
	}


	public String getProductName() {
		return productName;
	}


	public void setProductName(String productName) {
		this.productName = productName;
	}


	public Date getRefTime() {
		return refTime;
	}


	public void setRefTime(Date refTime) {
		this.refTime = refTime;
	}


	public int getForecastTime() {
		return forecastTime;
	}
	

	public void setForecastTime(int forecastTime) {
		this.forecastTime = forecastTime;
	}


	public String getUtilityFlag() {
		return utilityFlag;
	}


	public void setUtilityFlag(String utilityFlag) {
		this.utilityFlag = utilityFlag;
	}



}
