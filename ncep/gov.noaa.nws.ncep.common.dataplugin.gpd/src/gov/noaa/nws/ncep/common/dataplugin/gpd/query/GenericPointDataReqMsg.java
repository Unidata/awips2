package gov.noaa.nws.ncep.common.dataplugin.gpd.query;
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
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

@DynamicSerialize
@XmlRootElement(name = "GenericPointDataReqMsg")
@XmlAccessorType(XmlAccessType.NONE)
public class GenericPointDataReqMsg implements IServerRequest {
	public static enum GenericPointDataReqType{
		// get GPD data in Java Object format
		GET_GPD_PRODUCT_OBJECT,
		GET_GPD_STATION_PRODUCT_OBJECT,
		GET_GPD_MOVING_PRODUCT_OBJECT,
		GET_GPD_STATION_PRODUCT_OBJECT_LIST,
		GET_GPD_MOVING_PRODUCT_OBJECT_LIST,
		GET_GPD_STATION_MDL_SND_PRODUCT_OBJECT_LIST,
		GET_GPD_MOVING_MDL_SND_PRODUCT_OBJECT_LIST,
		GET_GPD_PRODUCT_INFO_OBJECT,
		GET_GPD_PRODUCT_TIMELINE_OBJECT,
		GET_GPD_PRODUCT_RANGESTART_TIME_OBJECT,
		GET_GPD_STATION_INFO_COLLECTION_OBJECT,
		GET_GPD_ALL_AVAILABLE_PRODUCTS,
		GET_GPD_AVAILABLE_MODEL_SOUNDING_PRODUCTS, //GPD pfc sounding
		GET_GPD_AVAILABLE_OBSERVED_SOUNDING_PRODUCTS,
		GET_GPD_AVAILABLE_SURFACE_PRODUCTS,
		
		//The following request types should not be used by CAVA JAVA applications
		//get/save product in XML format
		GET_GPD_PRODUCT_XML,
		GET_GPD_STATION_PRODUCT_XML,
		GET_GPD_MOVING_PRODUCT_XML,
		GET_GPD_PRODUCT_INFO_XML,
		STORE_GPD_PRODUCT_FROM_XML,
		//get/save product in GEMPAK table format
		GET_GPD_PRODUCT_GEMPAK_TBL,
		GET_GPD_STATION_PRODUCT_GEMPAK_TBL,
		GET_GPD_MOVING_PRODUCT_GEMPAK_TBL,
		GET_GPD_PRODUCT_INFO_GEMPAK_TBL,
		STORE_GPD_PRODUCT_FROM_GEMPAK_TBL,
		STORE_GPD_MDL_SND_PRODUCT_FROM_GEMPAK_TBL,
		STORE_GPD_OBS_SND_PRODUCT_FROM_GEMPAK_TBL,
		STORE_GPD_OBS_SFC_PRODUCT_FROM_GEMPAK_TBL,
		//Purge GPD DB
		PURGE_GPD_PRODUCT_ONETIME,
		PURGE_GPD_PRODUCT_ALLTIME,
		PURGE_GPD_EXPIRED_PRODUCT,
		PURGE_GPD_ALL_PRODUCTS
	}
	public static enum GenericPointDataQueryKey{
		BY_STN_ID,
		BY_SLAT_SLON,
		BY_PRODUCT_NAME
	} 
	
	//required for all
	@DynamicSerializeElement
	@XmlAttribute(required = true)
	private GenericPointDataReqType reqType;
	
	//required for GPD_PRODUCT, and GPD_STATION_PRODUCT
	@DynamicSerializeElement
    @XmlAttribute
    private Date refTime=null;
	
	//required for GPD_PRODUCT, GPD_REPORT_INFO, GPD_STATION_PRODUCT
	//optional for 
	@DynamicSerializeElement
    @XmlAttribute
    private String productName=null;
	
	//required for GPD_STATION_PRODUCT
	@DynamicSerializeElement
    @XmlAttribute
    private String stnId=null;
	
	//required for GPD_MOVING_PRODUCT
	@DynamicSerializeElement
    @XmlAttribute
    private float slat;
	
	//required for GPD_MOVING_PRODUCT
	@DynamicSerializeElement
    @XmlAttribute
    private float slon;
	
	//optional for GPD_STATION_PRODUCT, GPD_PRODUCT
	@DynamicSerializeElement
    @XmlAttribute
    private int productVersion=0;
	
	//optional for GPD_STATION_PRODUCT, GPD_PRODUCT
	//By default, this flag is false, and we will always return the "latest" version of product
	//If this flag is set, we will return based on the version number set in "productVersion" field
	@DynamicSerializeElement
    @XmlAttribute
    private boolean querySpecifiedProductVersion= false;
	
	@DynamicSerializeElement
    @XmlAttribute
    private GenericPointDataQueryKey queryKey;
	
	@DynamicSerializeElement
    private int maxNumLevel=0;
	
	@DynamicSerializeElement
    private String gpdDataString;
	
	// used for query a list of time line's sounding data for one station or moving product 
	//con be a list of reference time (for observed data) or a list of range start time (for pfc data)
	// used for CAVE Java query only...for now
	@DynamicSerializeElement
    private List<Date> queryTimeList;
	
	//used for query station collection, forecast time, or pfc sounding data
	// used for CAVE Java query only...for now
	@DynamicSerializeElement
    private String refTimeStr;
	//used for query station collection for model/pfc sounding
	// used for CAVE Java query only...for now
	@DynamicSerializeElement
    private String rangeStartTimeStr;
	
	public GenericPointDataReqMsg() {
		super();
	}
	/*
	public GenericPointDataProductContainer getGpdProduct(String productName, Date refTime, boolean useSpecifiedProdVersion, int productVersion){
		GenericPointDataProductContainer prodCon=null;
		this.reqType = GenericPointDataReqType.GET_GPD_PRODUCT_OBJECT;
		this.setProductName(productName);
		this.setProductVersion(productVersion);
		this.setQuerySpecifiedProductVersion(useSpecifiedProdVersion);
		this.setRefTime(refTime);
		try {
			Object rslts = ThriftClient.sendRequest( this );
			if( (rslts instanceof GenericPointDataProductContainer) ){
				prodCon= (GenericPointDataProductContainer)rslts;
			}		
			else { 
				System.out.println("Request Failed: ");
				
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return prodCon;
	}
	@SuppressWarnings("unchecked")
	public List<GenericPointDataStationProduct> getGpdStationProductList(String productName, GenericPointDataQueryKey queryKey, String stnId,float slat, float slon, List<Date> refTimeLst, boolean useSpecifiedProdVersion, int productVersion){
		if(GenericPointDataQueryKey.BY_STN_ID == queryKey){
			this.reqType = GenericPointDataReqType.GET_GPD_STATION_PRODUCT_OBJECT_LIST;
			this.setStnId(stnId);
		}
		else{
			this.reqType = GenericPointDataReqType.GET_GPD_MOVING_PRODUCT_OBJECT_LIST;
			this.setSlat(slat);
			this.setSlon(slon);
		}
		this.setProductName(productName);
		this.setProductVersion(productVersion);
		this.setQuerySpecifiedProductVersion(useSpecifiedProdVersion);
		this.setQueryKey(queryKey);
		this.setRefTimeList(refTimeLst);
		try {
			Object rslts = ThriftClient.sendRequest( this );
			if( (rslts instanceof List<?>) ){
				return ((List<GenericPointDataStationProduct>)rslts);
				//System.out.println(" stnProd stnId= "+stnProd.getLocation().getStationId()+ " slat="+stnProd.getSlat()+ " slon="+stnProd.getSlon());
			}		
			else { 
				System.out.println("Request Failed: ");

			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return null;
	}

	public GenericPointDataProductContainer getGpdStationProduct(String productName, GenericPointDataQueryKey queryKey, String stnId,float slat, float slon, Date refTime, boolean useSpecifiedProdVersion, int productVersion){
		GenericPointDataProductContainer stnProd=null;
		if(GenericPointDataQueryKey.BY_STN_ID == queryKey){
			this.reqType = GenericPointDataReqType.GET_GPD_STATION_PRODUCT_OBJECT;
			this.setStnId(stnId);
		}
		else{
			this.reqType = GenericPointDataReqType.GET_GPD_MOVING_PRODUCT_OBJECT;
			this.setSlat(slat);
			this.setSlon(slon);
		}
		this.setProductName(productName);
		this.setProductVersion(productVersion);
		this.setQuerySpecifiedProductVersion(useSpecifiedProdVersion);
		this.setQueryKey(queryKey);
		//Date refTime = new Date();
		//refTime.setTime(0);
		//refTime.setDate(8);
		//refTime.setHours(15);
		//refTime.setYear(113);
		//refTime.setMonth(4);
		//refTime.setMinutes(30);
		//"2013-05-08 14:30:00"
		//refTime.setTime(1368023400000L);
		this.setRefTime(refTime);
		try {
			Object rslts = ThriftClient.sendRequest( this );
			if( (rslts instanceof GenericPointDataProductContainer) ){
				stnProd= (GenericPointDataProductContainer)rslts;
				//System.out.println(" stnProd stnId= "+stnProd.getLocation().getStationId()+ " slat="+stnProd.getSlat()+ " slon="+stnProd.getSlon());
				
			}		
			else { 
				System.out.println("Request Failed: ");
				
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return stnProd;
	}
	
	public GenericPointDataProductInfo getGpdProductInfo(String productName){
		GenericPointDataProductInfo prodInfo=null;
		this.reqType = GenericPointDataReqType.GET_GPD_PRODUCT_INFO_OBJECT;
		this.setProductName(productName);
		try {
			Object rslts = ThriftClient.sendRequest( this );
			if( (rslts instanceof GenericPointDataProductInfo) ) {
				//			 			
				prodInfo = (GenericPointDataProductInfo)rslts;
				System.out.println(" report name= "+prodInfo.getName()+ " master="+prodInfo.getMasterLevel().getName()+ " maxLevel="+prodInfo.getMaxNumberOfLevel());
				for(Parameter pam: prodInfo.getParameterLst() ){
					System.out.println("parm ="+pam.getAbbreviation());
				}
			}
			else { 
				System.out.println("Request Failed: ");
				
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return prodInfo;
	}
	*/
	public GenericPointDataReqType getReqType() {
		return reqType;
	}
	public void setReqType(GenericPointDataReqType reqType) {
		this.reqType = reqType;
	}
	public Date getRefTime() {
		return refTime;
	}
	public void setRefTime(Date refTime) {
		this.refTime = refTime;
	}
	public String getProductName() {
		return productName;
	}
	public void setProductName (String productName) {
		this.productName = productName;
	}
	public String getStnId() {
		return stnId;
	}
	public void setStnId(String stnId) {
		this.stnId = stnId;
	}
	public int getProductVersion() {
		return productVersion;
	}
	public void setProductVersion(int productVersion) {
		this.productVersion = productVersion;
	}
	public boolean isQuerySpecifiedProductVersion() {
		return querySpecifiedProductVersion;
	}
	public void setQuerySpecifiedProductVersion(boolean querySpecifiedProductVersion) {
		this.querySpecifiedProductVersion = querySpecifiedProductVersion;
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
	public GenericPointDataQueryKey getQueryKey() {
		return queryKey;
	}
	public void setQueryKey(GenericPointDataQueryKey queryKey) {
		this.queryKey = queryKey;
	}

	public String getGpdDataString() {
		return gpdDataString;
	}

	public void setGpdDataString(String gpdDataString) {
		this.gpdDataString = gpdDataString;
	}

	public int getMaxNumLevel() {
		return maxNumLevel;
	}

	public void setMaxNumLevel(int maxNumLevel) {
		this.maxNumLevel = maxNumLevel;
	}

	public List<Date> getQueryTimeList() {
		return queryTimeList;
	}

	public void setQueryTimeList(List<Date> queryTimeList) {
		this.queryTimeList = queryTimeList;
	}
	public String getRefTimeStr() {
		return refTimeStr;
	}
	public void setRefTimeStr(String refTimeStr) {
		this.refTimeStr = refTimeStr;
	}
	public String getRangeStartTimeStr() {
		return rangeStartTimeStr;
	}
	public void setRangeStartTimeStr(String rangeStartTimeStr) {
		this.rangeStartTimeStr = rangeStartTimeStr;
	}

	
}
