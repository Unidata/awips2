
package gov.noaa.nws.ncep.common.dataplugin.uair;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decoder implementation for uair plugin
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial creation
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

@Entity
@Table(name = "uair", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UairRecord extends PluginDataObject {

	private static final long serialVersionUID = 1L;
    
    // Time of the UTC
    @DataURI(position = 4)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int UTC;

    // The observation report type.
    @DataURI(position = 1)
    @Column(length=32)
    @XmlAttribute
    @DynamicSerializeElement
    private String reportType;

    // Station ID
    @Column(length=16)
    @XmlAttribute
    @DynamicSerializeElement
    private String stationId;
    
    // Issue time for the bulletin
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar issueTime;

    // Observation time for the bulletin
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar observationTime;
    
    // Synoptic time for the bulletin
    // Time of the observation to the nearest hour.
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar synopticTime;
    
    // Type of data such as TTAA/BB/CC/DD or PP...
    @DataURI(position = 3)
    @Column(length=8)
    @XmlElement
    @DynamicSerializeElement
    private String dataType;
    
    // Correction indicator from wmo header
    @DataURI(position = 5)
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String corIndicator;
   
    // Text of the WMO header
    @Column(length=32)
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoHeader;

    // Station number
    @DataURI(position = 2)
    @Column(length=16)
    @XmlAttribute
    @DynamicSerializeElement
    private String stationNumber;

    // Latitude of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double slat;

    // Longitude of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double slon;

    // Elevation of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double selv;

    // Yes if report is a NIL.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Boolean nil;

    // bulletin message
    @Column(length=4000)
    @XmlAttribute
    @DynamicSerializeElement
    private String bullMessage;
    
    /** 
	 * Uair observation levels  
	 */
	@DynamicSerializeElement
	@XmlElement
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<ObsLevels> obsLevels = new HashSet<ObsLevels>();

	/** 
	 * Uair tropopause data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<Tropopause> tropopause = new HashSet<Tropopause>();

	/** 
	 * Uair maxwind data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<MaxWind> maxwind = new HashSet<MaxWind>();

	/** 
	 * Uair lifted index data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<LiftedIndex> liftedindex = new HashSet<LiftedIndex>();

    /**
     * Empty constructor.
     */
    public UairRecord() {
    	this.nil=false;
    	this.stationNumber="";
    	this.wmoHeader="";
    	this.stationId="";
    	this.corIndicator="";
    	this.dataType="";
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     * @param tableDef
     *            The table definitions for this class.
     */
    public UairRecord(String uri) {
        super(uri);
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * Get the report correction indicator.
     * 
     * @return The corIndicator
     */
    public String getCorIndicator() {
        return corIndicator;
    }

    /**
     * Set the report correction indicator.
     * 
     * @param corIndicator
     *            The corIndicator.
     */
    public void setCorIndicator(String corIndicator) {
        this.corIndicator = corIndicator;
    }

    /**
     * Get the observation report type.
     * 
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * Set the observation report type.
     * 
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public String getStationNumber() {
		return stationNumber;
	}

	public void setStationNumber(String stationNumber) {
		this.stationNumber = stationNumber;
	}

	public double getSlat() {
		return slat;
	}

	public void setSlat(double slat) {
		this.slat = slat;
	}

	public double getSlon() {
		return slon;
	}

	public void setSlon(double slon) {
		this.slon = slon;
	}

	public double getSelv() {
		return selv;
	}

	public void setSelv(double selv) {
		this.selv = selv;
	}

	public Calendar getObservationTime() {
		return observationTime;
	}

	public void setObservationTime(Calendar observationTime) {
		this.observationTime = observationTime;
	}

	public Calendar getSynopticTime() {
		return synopticTime;
	}

	public void setSynopticTime(Calendar synopticTime) {
		this.synopticTime = synopticTime;
	}

	public String getBullMessage() {
		return bullMessage;
	}

	public void setBullMessage(String bullMessage) {
		this.bullMessage = bullMessage;
	}

	public Calendar getIssueTime() {
		return issueTime;
	}

	public void setIssueTime(Calendar issueTime) {
		this.issueTime = issueTime;
	}

	public String getDataType() {
		return dataType;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public int getUTC() {
		return UTC;
	}

	public void setUTC(int utc) {
		UTC = utc;
	}

	public Boolean getNil() {
		return nil;
	}

	public void setNil(Boolean nil) {
		this.nil = nil;
	}

	public String getStationId() {
		return stationId;
	}

	public void setStationId(String stationId) {
		this.stationId = stationId;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}
    
	/**
	 * @return the set of uair observation levels
	 */
	public Set<ObsLevels> getObsLevels() {
		return obsLevels;
	}

	/**
	 * @param uair observation levels to set
	 */
	public void setObsLevels(Set<ObsLevels> uairLevel) {
		this.obsLevels = uairLevel;
	}

	/**
	 * @param add uair observation levels to set
	 */
	public void addObsLevels(ObsLevels plevel){
		obsLevels.add(plevel);
		plevel.setParentID(this);
	}

	/**
	 * @return the set of uair observation levels
	 */
	public Set<Tropopause> getTropopause() {
		return tropopause;
	}

	/**
	 * @param uair observation levels to set
	 */
	public void setTropopause(Set<Tropopause> trop) {
		this.tropopause = trop;
	}

	/**
	 * @param add uair observation levels to set
	 */
	public void addTropopause(Tropopause trop){
		tropopause.add(trop);
		trop.setParentID(this);
	}

	/**
	 * @return the set of uair maximum wind
	 */
	public Set<MaxWind> getMaxWind() {
		return maxwind;
	}

	/**
	 * @param uair maximum wind to set
	 */
	public void setMaxWind(Set<MaxWind> mwind) {
		this.maxwind = mwind;
	}

	/**
	 * @param add uair maximum wind to set
	 */
	public void addMaxWind(MaxWind mwind){
		maxwind.add(mwind);
		mwind.setParentID(this);
	}

	/**
	 * @return the set of uair lifted index
	 */
	public Set<LiftedIndex> getLiftedIndex() {
		return liftedindex;
	}

	/**
	 * @param uair lifted index to set
	 */
	public void setLiftedIndex(Set<LiftedIndex> li) {
		this.liftedindex = li;
	}

	/**
	 * @param add uair lifted index to set
	 */
	public void addLiftedIndex(LiftedIndex li){
		liftedindex.add(li);
		li.setParentID(this);
	}

	/**
	 * Override existing set method to modify any
	 * classes that use the dataURI as a foreign key
	 */
	@Override
	public void setIdentifier(Object dataURI)
	{

		this.identifier = dataURI;

		if(this.getObsLevels() != null && this.getObsLevels().size() > 0)
		{
			for (Iterator<ObsLevels> iter = this.getObsLevels().iterator(); iter.hasNext();) {
				ObsLevels level = iter.next();
				level.setParentID(this);
			}
		}

		if(this.getTropopause() != null && this.getTropopause().size() > 0)
		{
			for (Iterator<Tropopause> iter = this.getTropopause().iterator(); iter.hasNext();) {
				Tropopause trop = iter.next();
				trop.setParentID(this);
			}
		}

		if(this.getMaxWind() != null && this.getMaxWind().size() > 0)
		{
			for (Iterator<MaxWind> iter = this.getMaxWind().iterator(); iter.hasNext();) {
				MaxWind mwind = iter.next();
				mwind.setParentID(this);
			}
		}

		if(this.getLiftedIndex() != null && this.getLiftedIndex().size() > 0)
		{
			for (Iterator<LiftedIndex> iter = this.getLiftedIndex().iterator(); iter.hasNext();) {
				LiftedIndex li = iter.next();
				li.setParentID(this);
			}
		}

	}

}