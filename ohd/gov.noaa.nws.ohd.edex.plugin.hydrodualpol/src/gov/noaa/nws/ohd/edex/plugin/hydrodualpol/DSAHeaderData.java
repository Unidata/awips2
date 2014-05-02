package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

/**
 * Class to encapsulate DSA radar product header data. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * July 2013 DCS 167    P. Tilles   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Paul Tilles
 * 
 */


public class DSAHeaderData
{
	private String uri;
	private String mnemonic;
	
	private String radarId;	  
	private String obsTime, beginTimeString, endTimeString;
	private String fileName;
	
	private boolean isProductNull ;
	
	float maxVal, scale, offset;
	short stormTotalStartDate, stormTotalStartTime, stormTotalEndDate, stormTotalEndTime;
	short biasValue, volumeCoveragePattern, operationalMode, precipDetectedFlag, nullProductFlag;
	
	private int prodDate;
	private int prodHour;
	private int prodMin;
	private int productObsTimeMinutes; // minutes after the hour for the product obs time;
	
	public void setMnemonic(String mnemonic) {
		this.mnemonic = mnemonic;
	}

	public String getMnemonic() {
		return mnemonic;
	}

	public void setRadarId(String radarId) {
		this.radarId = radarId;
	}

	public String getRadarId() {
		return radarId;
	}
	
	public void setUri(String uri) {
		this.uri = uri;
	}

	public String getUri() {
		return uri;
	}
	public void setMaxVal(float maxVal) {
		this.maxVal = maxVal;
	}

	public float getMaxVal() {
		return maxVal;
	}
	public void setBiasValue(short biasValue) {
		this.biasValue = biasValue;
	}

	public short getBiasValue() {
		return biasValue;
	}
	
	public void setObsTime(String obsTime) {
		this.obsTime = obsTime;
	}

	public String getObsTime() {
		return obsTime;
	}
	
	public void setBeginTimeString(String beginTimeString) {
		this.beginTimeString = beginTimeString;
	}

	public String getBeginTimeString() {
		return beginTimeString;
	}
	
	public void setEndTimeString(String endTimeString) {
		this.endTimeString = endTimeString;
	}

	public String getEndTimeString() {
		return endTimeString;
	}
	
	public void setProdDate(int prodDate) {
		this.prodDate = prodDate;
	}

	public int getProdDate() {
		return prodDate;
	}

	public void setProdHour(int prodHour) {
		this.prodHour = prodHour;
	}

	public int getProdHour() {
		return prodHour;
	}

	public void setProdMin(int prodMin) {
		this.prodMin = prodMin;
	}

	public int getProdMin() {
		return prodMin;
	}
	
	public void setProductObsTimeMinutes(int productObsTimeMinutes) {
		this.productObsTimeMinutes = productObsTimeMinutes;
	}

	public int getProductObsTimeMinutes() {
		return productObsTimeMinutes;
	}
	
	public void setScale(float scale) {
		this.scale = scale;
	}

	public float getScale() {
		return scale;
	}
	
	public void setOffSet(float offset) {
		this.offset = offset;
	}

	public float getOffSet() {
		return offset;
	}


	public void setVolumeCoveragePattern(short volumeCoveragePattern) {
		this.volumeCoveragePattern = volumeCoveragePattern;
	}

	public short getVolumeCoveragePattern() {
		 return volumeCoveragePattern;
	}
	

	public void setOperationalMode(short operationalMode) {
		this.operationalMode = operationalMode;
	}

	public short getOperationalMode() {
		 return operationalMode;
	}
	

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileName() {
		return fileName;
	}
	
	public void setIsProductNull(boolean isProductNull) {
		this.isProductNull = isProductNull;
	}

	public boolean isProductNull() {
		return isProductNull;
	}
	
	public void setNullProductFlag(short nullProductFlag) {
		this.nullProductFlag = nullProductFlag;
	}

	public short getNullProductFlag() {
		return nullProductFlag;
	}
	
	public void setStormTotalStartTime(short stormTotalStartTime) {
		this.stormTotalStartTime = stormTotalStartTime;
	}

	public short getStormTotalStartTime() {
		return stormTotalStartTime;
	}
	
	public void setStormTotalStartDate(short stormTotalStartDate) {
		this.stormTotalStartDate = stormTotalStartDate;
	}

	public short getStormTotalStartDate() {
		return stormTotalStartDate;
	}
	
	public void setStormTotalEndTime(short stormTotalEndTime) {
		this.stormTotalEndTime = stormTotalEndTime;
	}

	public short getStormTotalEndTime() {
		return stormTotalEndTime;
	}
	
	public void setStormTotalEndDate(short stormTotalEndDate) {
		this.stormTotalEndDate = stormTotalEndDate;
	}

	public short getStormTotalEndDate() {
		return stormTotalEndDate;
	}
	
}
