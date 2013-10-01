package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/*
 * The KStationCoefficient.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

@XmlRootElement(name = "kStationCoefficient")
@XmlAccessorType(XmlAccessType.NONE)
//@XmlType(name = "")
public class KStationCoefficient {

	/**
	 * Station Code
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String stationCode;
	/**
	 * provider
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String provider;
	/**
	 * k9Limit
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String k9Limit;
	/**
	 * longitude
	 */
	@XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	private String longitude;
	/**
	 * fitTime
	 */
	@XmlElement
    protected ArrayList<KFitTime> fitTime;
	/**
	 * @XmlElement
	 */
	@XmlElement
	protected ArrayList<KsThree> ksThree;
	
	/**
	 * Default constructor.
	 */
	public KStationCoefficient() {
		fitTime = new ArrayList<KFitTime>();
		ksThree = new ArrayList<KsThree>();
	}
	
    /**
	 * @return the stationCode
	 */
	public String getStationCode() {
		return stationCode;
	}

	/**
	 * @param stationCode: the stationCode to set
	 */
	public void setStationCode(String stationCode) {
		this.stationCode = stationCode;
	}
	
	/**
	 * @return the provider
	 */
	public String getProvider() {
		return provider;
	}

	/**
	 * @param provider: the provider to set
	 */
	public void setProvider(String provider) {
		this.provider = provider;
	}

    /**
	 * @return the k9Limit
	 */
	public String getK9Limit() {
		return k9Limit;
	}

	/**
	 * @param k9Limit: the k9Limit to set
	 */
	public void setK9Limit(String k9Limit) {
		this.k9Limit = k9Limit;
	}

    /**
	 * @return the longitude
	 */
	public String getLongitude() {
		return longitude;
	}

	/**
	 * @param longitude: the longitude to set
	 */
	public void setLongitude(String longitude) {
		this.longitude = longitude;
	}
	
    /**
	 * @return the fitTime
	 */
	
	public ArrayList<KFitTime> getKFitTime() {
		return fitTime;
	}
	
    /**
	 * @return the fitTime
	 */
	
	public ArrayList<KsThree> getKsThree() {
		return ksThree;
	}
	public void setKsThree(ArrayList<KsThree> ksThree) {
		this.ksThree = ksThree;
	}
}

