/**
 * H5UairRecord
 * 
 * The java class defines the parameters in the postgres table and
 * the for HDF5 dataset for the upper air data.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial creation
 * 4/2011						T. Lee		Persist to HDF5
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.h5uair;

import java.util.Date;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name = "h5uair", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class H5UairRecord extends PluginDataObject implements ISpatialEnabled, 
		IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;
	public static final String PLUGIN_NAME = "h5uair";
    
    // Time of the UTC
    @DataURI(position = 4)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int UTC;

    // The observation report type.
    @DataURI(position = 1)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String reportType;

    // Station ID
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String stid;
    
    // Issue time for the bulletin
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar issueTime;

    // Observation time for the bulletin
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar obsTime;
    
    // Synoptic time for the bulletin
    // Time of the observation to the nearest hour.
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar synopticTime;
    
    // Type of data such as TTAA/BB/CC/DD or PP...
    @DataURI(position = 3)
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String dataType;
    
    // Correction indicator from wmo header
    @DataURI(position = 5)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String corr;
   
    // Text of the WMO header
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoHeader;

    // Station number
    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String stnum;

    // Latitude of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private float slat;

    // Longitude of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private float slon;

    // Elevation of the station
    @Column
    @XmlElement
    @DynamicSerializeElement
    private float selv;

    // Yes if report is a NIL.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Boolean nil;

    // bulletin message
    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private String bullMessage;
    
    /** 
	 * Uair observation levels  
	 */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Set<H5ObsLevels> obsLevels = new HashSet<H5ObsLevels>();

	/** 
	 * Uair tropopause data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Set<H5Tropopause> tropopause = new HashSet<H5Tropopause>();

	/** 
	 * Uair maxwind data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Set<H5MaxWind> maxwind = new HashSet<H5MaxWind>();

	/** 
	 * Uair lifted index data 
	 */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Set<H5LiftedIndex> liftedindex = new HashSet<H5LiftedIndex>();
	
	@Embedded 
	private PointDataView pdv;
	private Integer hdfFileId;

    /**
     * Empty constructor.
     */
    public H5UairRecord() {
    	this.nil=false;
    	this.stnum="";
    	this.wmoHeader="";
    	this.stid="";
    	this.corr="";
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
    public H5UairRecord(String uri) {
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
     * @return The corr
     */
    public String getCorr() {
        return corr;
    }

    /**
     * Set the report correction indicator.
     * 
     * @param corr
     *            The corr.
     */
    public void setCorr(String corr) {
        this.corr = corr;
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

    public String getStnum() {
		return stnum;
	}

	public void setStnum(String stnum) {
		this.stnum = stnum;
	}

	public double getSlat() {
		return slat;
	}

	public void setSlat(float slat) {
		this.slat = slat;
	}

	public double getSlon() {
		return slon;
	}

	public void setSlon(float slon) {
		this.slon = slon;
	}

	public double getSelv() {
		return selv;
	}

	public void setSelv(float selv) {
		this.selv = selv;
	}

	public Calendar getObsTime() {
		return obsTime;
	}

	public void setObsTime(Calendar obsTime) {
		this.obsTime = obsTime;
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

	public String getStid() {
		return stid;
	}

	public void setStid(String stid) {
		this.stid = stid;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}
    
	/**
	 * @return the set of uair observation levels
	 */
	public Set<H5ObsLevels> getObsLevels() {
		return obsLevels;
	}

	/**
	 * @param uair observation levels to set
	 */
	public void setObsLevels(Set<H5ObsLevels> uairLevel) {
		this.obsLevels = uairLevel;
	}

	/**
	 * @param add uair observation levels to set
	 */
	public void addObsLevels(H5ObsLevels plevel){
		obsLevels.add(plevel);
		plevel.setParentID(this);
	}

	/**
	 * @return the set of uair observation levels
	 */
	public Set<H5Tropopause> getTropopause() {
		return tropopause;
	}

	/**
	 * @param uair observation levels to set
	 */
	public void setTropopause(Set<H5Tropopause> trop) {
		this.tropopause = trop;
	}

	/**
	 * @param add uair observation levels to set
	 */
	public void addTropopause(H5Tropopause trop){
		tropopause.add(trop);
		trop.setParentID(this);
	}

	/**
	 * @return the set of uair maximum wind
	 */
	public Set<H5MaxWind> getMaxWind() {
		return maxwind;
	}

	/**
	 * @param uair maximum wind to set
	 */
	public void setMaxWind(Set<H5MaxWind> mwind) {
		this.maxwind = mwind;
	}

	/**
	 * @param add uair maximum wind to set
	 */
	public void addMaxWind(H5MaxWind mwind){
		maxwind.add(mwind);
		mwind.setParentID(this);
	}

	/**
	 * @return the set of uair lifted index
	 */
	public Set<H5LiftedIndex> getLiftedIndex() {
		return liftedindex;
	}

	/**
	 * @param uair lifted index to set
	 */
	public void setLiftedIndex(Set<H5LiftedIndex> li) {
		this.liftedindex = li;
	}

	/**
	 * @param add uair lifted index to set
	 */
	public void addLiftedIndex(H5LiftedIndex li){
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
			for (Iterator<H5ObsLevels> iter = this.getObsLevels().iterator(); iter.hasNext();) {
				H5ObsLevels level = iter.next();
				level.setParentID(this);
			}
		}

		if(this.getTropopause() != null && this.getTropopause().size() > 0)
		{
			for (Iterator<H5Tropopause> iter = this.getTropopause().iterator(); iter.hasNext();) {
				H5Tropopause trop = iter.next();
				trop.setParentID(this);
			}
		}

		if(this.getMaxWind() != null && this.getMaxWind().size() > 0)
		{
			for (Iterator<H5MaxWind> iter = this.getMaxWind().iterator(); iter.hasNext();) {
				H5MaxWind mwind = iter.next();
				mwind.setParentID(this);
			}
		}

		if(this.getLiftedIndex() != null && this.getLiftedIndex().size() > 0)
		{
			for (Iterator<H5LiftedIndex> iter = this.getLiftedIndex().iterator(); iter.hasNext();) {
				H5LiftedIndex li = iter.next();
				li.setParentID(this);
			}
		}

	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
     */
    @Override
    public PointDataView getPointDataView() {
        return this.pdv;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
     * .uf.common.pointdata.PointDataView)
     */
    @Override
    public void setPointDataView(PointDataView pdv) {
        this.pdv = pdv;
    }

    @Override
    public Integer getHdfFileId() {
        return null;
    }

    @Override
    public Date getPersistenceTime() {
//        return this.dataTime.getRefTime();
    	return null;
    }

    @Override

    public void setHdfFileId(Integer hdfFileId) {
    	this.hdfFileId = hdfFileId;
    }

	@Override
	public void setPersistenceTime(Date persistTime) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Amount getValue(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<Amount> getValues(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getString(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getStrings(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ISpatialObject getSpatialObject() {
		// TODO Auto-generated method stub
		return null;
	}
}