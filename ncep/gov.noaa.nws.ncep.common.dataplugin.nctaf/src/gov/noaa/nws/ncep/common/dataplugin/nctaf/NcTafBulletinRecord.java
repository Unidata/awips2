/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * Record implementation for a taf message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/22/2011   458			sgurung	    Initial Creation
 * 09/29/2011               sgurung     Added reportType
 * 10/26/2011               sgurung     Added tafValidPeriod
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class NcTafBulletinRecord implements ISpatialEnabled, IDecoderGettable,
		IPointData {

	private static final long serialVersionUID = 1L;

	/** The data time for this record */
	@Embedded
	@XmlElement
	@DynamicSerializeElement
	@DataURI(position = 0)
	protected DataTime dataTime;

	/** The raw data from the message */
	@Transient
	@DynamicSerializeElement
	protected Object messageData;

	@DynamicSerializeElement
	@XmlElement
	@Column
	private String wmoHeader;

	@DynamicSerializeElement
	@XmlElement
	@Column(length = 1024)
	private String tafText;

	// The observation report type
	@DataURI(position = 1)
	@XmlElement
	@Column
	@DynamicSerializeElement
	private String reportType;

	// Station Identifier for the data
	@DynamicSerializeElement
	@XmlElement
	@Column
	@Index(name = "nctaf_stationIndex")
	@DataURI(position = 2)
	private String stationId;

	@DynamicSerializeElement
	@XmlElement
	@Column
	@DataURI(position = 3)
	private String corIndicator;

	@DynamicSerializeElement
	@XmlElement
	@Column
	@DataURI(position = 4)
	private String amdIndicator;

	/** Issue date */
	@DynamicSerializeElement
	@XmlElement
	@Column
	// @DataURI(position = 4)
	private Date issue_time;

	/** Issue date string */
	@DynamicSerializeElement
	@XmlElement
	@Column
	@DataURI(position = 5)
	private String issue_timeString;

	/** Bulletin issuance time */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Date bulletin_time;

	/** Any remarks contained in the TAF record */
	@DynamicSerializeElement
	@XmlElement
	@Column
	private String remarks;

	/**
	 * The valid period for the TAF record.
	 */
	@DynamicSerializeElement
	@XmlElement
	@Embedded
	@Transient
	private NcTafPeriod tafValidPeriod;

	/** List of change groups (FM, BECMG, etc.) */
	@DynamicSerializeElement
	@XmlElement
	@Transient
	private Set<NcTafChangeGroup> changeGroups = new LinkedHashSet<NcTafChangeGroup>();

	@Embedded
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	public NcTafBulletinRecord() {
	}

	public DataTime getDataTime() {
		return dataTime;
	}

	public void setDataTime(DataTime dataTime) {
		this.dataTime = dataTime;
	}

	public Object getMessageData() {
		return messageData;
	}

	public void setMessageData(Object messageData) {
		this.messageData = messageData;
	}

	/**
	 * Get the WMO header for the enclosing WMO message.
	 * 
	 * @return The wmoHeader.
	 */
	public String getWmoHeader() {
		return wmoHeader;
	}

	/**
	 * Set the WMO header for the enclosing WMO message.
	 * 
	 * @param wmoHeader
	 *            The WMOHeader to set.
	 */
	public void setWmoHeader(String wmoHeader) {
		this.wmoHeader = wmoHeader;
	}

	/**
	 * Get the text of this terminal forecast.
	 * 
	 * @return The terminal forecast text.
	 */
	public String getTafText() {
		return tafText;
	}

	/**
	 * Set the text of this terminal forecast.
	 * 
	 * @param tafText
	 *            The terminal forecast text.
	 */
	public void setTafText(String tafText) {
		this.tafText = tafText;
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

	/**
	 * 
	 * @return
	 */
	public String getStationId() {
		return stationId;
	}

	/**
	 * 
	 * @param stationID
	 */
	public void setStationId(String stationID) {
		stationId = stationID;
	}

	/**
	 * 
	 * @return the corIndicator
	 */
	public String getCorIndicator() {
		return corIndicator;
	}

	/**
	 * 
	 * @param corIndicator
	 *            the corIndicator to set
	 */
	public void setCorIndicator(String corIndicator) {
		this.corIndicator = corIndicator;
	}

	/**
	 * 
	 * @return the amdIndicator
	 */
	public String getAmdIndicator() {
		return amdIndicator;
	}

	/**
	 * 
	 * @param amdIndicator
	 *            the amdIndicator to set
	 */
	public void setAmdIndicator(String amdIndicator) {
		this.amdIndicator = amdIndicator;
	}

	/**
	 * 
	 * @return the bulletin_time
	 */
	public Date getBulletin_time() {
		return bulletin_time;
	}

	/**
	 * 
	 * @param bulletin_time
	 *            the bulletin_time to set
	 */
	public void setBulletin_time(Date bulletin_time) {
		this.bulletin_time = bulletin_time;
	}

	/**
	 * @return the changeGroups
	 */
	public Set<NcTafChangeGroup> getChangeGroups() {
		return changeGroups;
	}

	/**
	 * @param changeGroups
	 *            the changeGroups to set
	 */
	public void setChangeGroups(Set<NcTafChangeGroup> changeGroups) {
		this.changeGroups = changeGroups;
	}

	/**
	 * @return the issue_time
	 */
	public Date getIssue_time() {
		return issue_time;
	}

	/**
	 * @param issue_time
	 *            the issue_time to set
	 */
	public void setIssue_time(Date issue_time) {
		this.issue_time = issue_time;
	}

	/**
	 * @return the issue_timeString
	 */
	public String getIssue_timeString() {
		return issue_timeString;
	}

	/**
	 * @param issue_timeString
	 *            the issue_time to set
	 */
	public void setIssue_timeString(String issue_timeString) {
		this.issue_timeString = issue_timeString;
	}

	/**
	 * @return the remarks
	 */
	public String getRemarks() {
		return remarks;
	}

	/**
	 * @param remarks
	 *            the remarks to set
	 */
	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	@Override
	public SurfaceObsLocation getSpatialObject() {
		return location;
	}

	public SurfaceObsLocation getLocation() {
		return location;
	}

	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	public double getLatitude() {
		return location.getLatitude();
	}

	public double getLongitude() {
		return location.getLongitude();
	}

	public Integer getElevation() {
		return location.getElevation();
	}

	public NcTafPeriod getTafValidPeriod() {
		return tafValidPeriod;
	}

	public void setTafValidPeriod(NcTafPeriod tafValidPeriod) {
		this.tafValidPeriod = tafValidPeriod;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
	 */
	@Override
	public PointDataView getPointDataView() {
		return this.pointDataView;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
	 * .uf.common.pointdata.PointDataView)
	 */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
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

}
