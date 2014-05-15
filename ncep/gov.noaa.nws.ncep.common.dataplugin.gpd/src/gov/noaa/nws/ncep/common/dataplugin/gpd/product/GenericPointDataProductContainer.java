/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
* 
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

//import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataLocation;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "GenericPointDataProduct")
public class GenericPointDataProductContainer  {

	//private static final long serialVersionUID = 1L;
	
	// The data time for this product 
    @XmlElement(name="ProductTime")
    @DynamicSerializeElement
    protected Date refTime;
    
	@DynamicSerializeElement
	@XmlElement(name="GPD-ProdInfo-Def")
	private GenericPointDataProductInfo productInfo;
	
	//list of station product
	@DynamicSerializeElement
    @XmlElement(name="GPD-Station-Product")
	private List<GenericPointDataStationProduct> stnProdLst = new ArrayList<GenericPointDataStationProduct>();
		
	//when a same product (same report type and reftime) is issued for correction, a newer version number 
	//should be encoded. Otherwise, it will be dropped.
	@DynamicSerializeElement
	@XmlAttribute
	private int productCorrectionVersion=0; 
	
	public GenericPointDataProductContainer() {
		super();
		// TODO Auto-generated constructor stub
	}


	public GenericPointDataProductInfo getProductInfo() {
		return productInfo;
	}

	public void setProductInfo(GenericPointDataProductInfo prodInfo) {
		this.productInfo = prodInfo;
	}


	public Date getRefTime() {
		return refTime;
	}


	public void setRefTime(Date refTime) {
		this.refTime = refTime;
	}


	public List<GenericPointDataStationProduct> getStnProdLst() {
		return stnProdLst;
	}


	public void setStnProdLst(List<GenericPointDataStationProduct> stnProdLst) {
		this.stnProdLst = stnProdLst;
	}


	public int getProductCorrectionVersion() {
		return productCorrectionVersion;
	}


	public void setProductCorrectionVersion(int productCorrectionVersion) {
		this.productCorrectionVersion = productCorrectionVersion;
	}

}
