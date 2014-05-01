package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;


/**
 * Defines a magnetometer station.
 * 
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 03/29/2013   975        sgurung     Initial Creation 
 * 07/17/2013   975        qzhou       Changed source type for reading source attributes
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "geoMagStation")
@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagStation {

	/**
	 * Station Code
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String stationCode;
	
	/**
	 * Provider 
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String provider;	

	/**
	 * Data sources for the station
	 */
	@XmlElement
    protected ArrayList<GeoMagSource> source;
    
	/**
	 * Indicates whether the station should be included
	 * in the K-index processing (if set to true).
	 */
    @XmlElement
    protected Boolean processKQDC;

	/**
	 * Indicates whether the Ks value (station estimate of kp) 
	 * should be calculated for the station (if set to true).
	 */
    @XmlElement
    protected Boolean kpStation;   

	/**
	 * Indicates whether the station has a header.
	 * Same station data may have or have no header.
	 */
    @XmlElement
    protected String hasHeader;   

	/**
	 * Data order (e.g. HDZF or XYZF)
	 */
    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String dataOrder;
  
    /**
	 * Location (latitude/longitude) of the station
	 */
	@XmlElement
	protected Location location;
	
	/**
	 * Raw data format of the input data for the station
	 */
	@XmlElement
	protected RawDataFormat rawDataFormat;
		
		
	
	public GeoMagStation() {
		source = new ArrayList<GeoMagSource>();
	}
	
	/**
	 * Gets the stationCode of this station
	 * @return the stationCode
	 */
	public String getStationCode() {
		return stationCode;
	}

	/**
	 * Sets the stationCode of this station
	 * @param stationCode the stationCode to set
	 */
	public void setStationCode(String stationCode) {
		this.stationCode = stationCode;
	}

	/**
	 * Gets the provider of this station
	 * @return the provider
	 */
	public String getProvider() {
		return provider;
	}

	/**
	 * Sets the provider of this station
	 * @param provider the provider to set
	 */
	public void setProvider(String provider) {
		this.provider = provider;
	}

	/**
	 * Gets the stationCode of this station
	 * @return the stationCode
	 */
	public ArrayList<GeoMagSource> getSource() {
		return source;
	}

	/**
	 * Sets the source of this station
	 * @param source the source to set
	 */
	public void setSource(ArrayList<GeoMagSource> source) {
		this.source = source;
	}

	
	/**
	 * Gets the processKQDC value of this station
	 * @return the processKQDC
	 */
	public boolean getProcessKQDC() {
		return processKQDC;
	}

	/**
	 * Sets the processKQDC value of this station
	 * @param processKQDC the processKQDC to set
	 */
	public void setProcessKQDC(boolean processKQDC) {
		this.processKQDC = processKQDC;
	}

	
	/**
	 * Gets the kpStation value of this station
	 * @return the kpStation
	 */
	public boolean getKpStation() {
		return kpStation;
	}

	/**
	 * Sets the kpStation value of this station
	 * @param kpStation the kpStation to set
	 */
	public void setKpStation(boolean kpStation) {
		this.kpStation = kpStation;
	}

	/**
	 * Gets the header true or false
	 * @return the hasHeader
	 */
	public String getHasHeader() {
		return hasHeader;
	}

	/**
	 * Sets the hasHeader value of this station
	 * @param hasHeader the hasHeader to set
	 */
	public void setHasHeader(String hasHeader) {
		this.hasHeader = hasHeader;
	}
	
	/**
	 * Gets the dataOrder of this station
	 * @return the dataOrder
	 */
	public String getDataOrder() {
		return dataOrder;
	}

	/**
	 * Sets the dataOrder of this station
	 * @param dataOrder the dataOrder to set
	 */
	public void setDataOrder(String dataOrder) {
		this.dataOrder = dataOrder;
	}

	/**
	 * Gets the longitude of this station
	 * @return the longitude
	 */
	public Location getLocation() {
		return location;
	}

	/**
	 * Sets the longitude of this station
	 * @param longitude the longitude to set
	 */
	public void setLocation(Location location) {
		this.location = location;
	}
	
	/**
	 * Gets the rawDataFormat of this station
	 * @return the rawDataFormat
	 */
	public RawDataFormat getRawDataFormat() {
		return rawDataFormat;
	}

	/**
	 * Sets the rawDataFormat of this station
	 * @param rawDataFormat the rawDataFormat to set
	 */
	public void setRawDataFormat(RawDataFormat rawDataFormat) {
		this.rawDataFormat = rawDataFormat;
	}

}
