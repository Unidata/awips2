package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

/**
 * Class to encapsulate DAA radar product header information. 
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

public class DAAHeaderData
{
	/*
	 * prodDate, prodHour, ProdMin
	 * 
	 */
	
	private String uri;
	private boolean isProductNull ;
	private int coverageDur;	 
	private float maxValh;
	private float biasValue;
	private String mnemonic;
	private String radarId;	  
	private short nullProductFlag;
	
	private int prodDate;
	private int prodHour;
	private int prodMin;
	private String obsTime;
	
	
	private String productGenerationTime;
	//private String fileName;
	
	private int productObsTimeMinutes; // minutes after the hour for the product obs time;
	private short minutesOffTopOfHour; // can be positive or negative
	
	private float scale;
	private float offset;
	
	public void setProductObsTimeMinutes(int productObsTimeMinutes) {
		this.productObsTimeMinutes = productObsTimeMinutes;
	}

	public int getProductObsTimeMinutes() {
		return productObsTimeMinutes;
	}

	public void setCoverageDur(int coverageDur) {
		this.coverageDur = coverageDur;
	}

	public int getCoverageDur() {
		return coverageDur;
	}

	public void setMaxValh(float maxValh) {
		this.maxValh = maxValh;
	}

	public float getMaxValh() {
		return maxValh;
	}

	public void setBiasValue(float biasValue) {
		this.biasValue = biasValue;
	}

	public float getBiasValue() {
		return biasValue;
	}

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

	public void setObsTime(String obsTime) {
		this.obsTime = obsTime;
	}

	public String getObsTime() {
		return obsTime;
	}

	public void setProductGenerationTime(String productGenerationTime) {
		this.productGenerationTime = productGenerationTime;
	}

	public String getProductGenerationTime() {
		return productGenerationTime;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public String getUri() {
		return uri;
	}

	public void setMinutesOffTopOfHour(short minutesOffTopOfHour) {
		this.minutesOffTopOfHour = minutesOffTopOfHour;
	}

	public short getMinutesOffTopOfHour() {
		return minutesOffTopOfHour;
	}

	public void setScale(float scale) {
		this.scale = scale;
	}

	public float getScale() {
		return scale;
	}

	public void setOffset(float offset) {
		this.offset = offset;
	}

	public float getOffset() {
		return offset;
	}

}
