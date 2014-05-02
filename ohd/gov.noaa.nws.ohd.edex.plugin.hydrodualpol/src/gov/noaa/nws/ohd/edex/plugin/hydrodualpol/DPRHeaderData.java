package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

/**
 * Class to ecapsulate DPR radar product header data. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 2013 DCS 167    P. Tilles   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Paul Tilles
 * 
 */



public class DPRHeaderData
{
	private String uri;
	private String mnemonic;
	
	private String radarId;	  
	private String obsTime;
	private String fileName;
	
	float maxVal, scale, offset;
	int volumeScanDate, volumeScanTime;
	short volumeCoveragePattern, operationalMode, biasValue, precipDetectedFlag;
	
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
	
	public void setVolumeScanTime(int volumeScanTime) {
		this.volumeScanTime = volumeScanTime;
	}

	public int getVolumeScanTime() {
		return volumeScanTime;
	}
	
	public void setVolumeScanDate(int volumeScanDate) {
		this.volumeScanDate = volumeScanDate;
	}

	public int getVolumeScanDate() {
		return volumeScanDate;
	}

	public void setPrecipDetectedFlag(short precipDetectedFlag) {
		this.precipDetectedFlag = precipDetectedFlag;
	}

	public int getPrecipDetectedFlag() {
		return precipDetectedFlag;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileName() {
		return fileName;
	}
}