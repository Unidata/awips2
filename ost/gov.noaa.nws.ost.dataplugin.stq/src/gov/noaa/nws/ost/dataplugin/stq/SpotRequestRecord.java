package gov.noaa.nws.ost.dataplugin.stq;

import java.util.Calendar;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * SpotRequestRecord is the Data Access component for Spot Forecast Request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date           Ticket#    Engineer    Description
 * -------------- ---------- ----------- --------------------------
 * July 17, 2015             Pwang       Initial creation for STQ: Spot Request
 *                                       Data plugin
 * Jan  21, 2016  18524      pwang       Added the setter to avoid Archiver error
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "stqseq")
@Table(name = "stq", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@DynamicSerialize
public class SpotRequestRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    private static final String pluginName = "stq";

    private static final String stqDispSymbol = "S";

    /* 
     * Project key, such as FGOWB, which can be used for identify the STQ requests
     * Extract from ofile, 2nd segment, such as
     * FGOWB from 20150530.FGOWV.01
     */
    @DataURI(position = 1)
    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private String ofileKey;

    @DataURI(position = 2)
    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private String ofileVersion;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Site received the request, such as BYZ
    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private String site;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Project Name
    @Column(length = 32)
    @DynamicSerializeElement
    @XmlAttribute
    private String projectName;

    // Text of the WMO header
    @Transient
    @DynamicSerializeElement
    private String wmoHeader = "";

    @Transient
    @DynamicSerializeElement
    private String reqOfficial;

    @Transient
    @DynamicSerializeElement
    private String emPhone;

    //Time Zone
    @Transient
    @DynamicSerializeElement
    private String timeZone;

    // State
    @Transient
    @DynamicSerializeElement
    private String state;

    // bottom elevation
    @Transient
    @DynamicSerializeElement
    private int bottomElevation;

    // Top elevation
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int topElevation;

    // size (acres)
    @Transient
    @DynamicSerializeElement
    private int sizeAcres;

    @Transient
    @DynamicSerializeElement
    private String stqSymbol = stqDispSymbol;

    /**
     * 
     */
    public SpotRequestRecord() {
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
    public SpotRequestRecord(String dataUri) {
        super(dataUri);
    }

    /**
     * Override method getPluginMNmae
     */
    @Override
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Get OFile Key
     * @return
     */
    public String getOfileKey() {
        return ofileKey;
    }

    /**
     * Set OFile Key
     * @param ofileKey
     */
    public void setOfileKey(String ofileKey) {
        this.ofileKey = ofileKey;
    }

    /**
     * Get OFile Version
     * @return
     */
    public String getOfileVersion() {
        return ofileVersion;
    }

    /**
     * Set OFile Version
     * @param ofileVersion
     */
    public void setOfileVersion(String ofileVersion) {
        this.ofileVersion = ofileVersion;
    }

    /**
     * Get Site
     * @return
     */
    public String getSite() {
        return site;
    }

    /**
     * Set Site
     * @param site
     */
    public void setSite(String site) {
        this.site = site;
    }

    /**
     * Get SpatialObject: SurfaceObsLocation
     * @return
     */
    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    /**
     * Get location
     * @return
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * Set Location
     * @param location: SurfaceObsLocation
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * Get this observation's geometry.
     * 
     * @return The geometry for this observation.
     */
    public Geometry getGeometry() {
        return location.getGeometry();
    }

    /**
     * Get the geometry latitude.
     * 
     * @return The geometry latitude.
     */
    public double getLatitude() {
        return location.getLatitude();
    }

    /**
     * Get the geometry longitude.
     * 
     * @return The geometry longitude.
     */
    public double getLongitude() {
        return location.getLongitude();
    }

    /**
     * Set latitude
     * @param latitude
     */
    public void setLatitude(float latitude) {
        location.setLatitude(latitude);
    }

    /**
     * Set longitude
     * @param longitude
     */
    public void setLongitude(float longitude) {
        location.setLongitude(longitude);
    }

    /**
     * Get Project Name 
     * @return
     */
    public String getProjectName() {
        return projectName;
    }

    /**
     * Set Project name
     * @param projectName
     */
    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    /**
     * Get PointDataView
     * @return PointDataView
     */
    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    /**
     * Get WMO Header
     * @return
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Set WMO Header
     * @param wmoHeader
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * Get TimeZone
     * @return
     */
    public String getTimeZone() {
        return timeZone;
    }

    /**
     * Set TimeZone
     * @param timeZone
     */
    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    public String getState() {
        return state;
    }

    /**
     * Set State
     * @param state
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * Get Bottom Elevation
     * @return
     */
    public int getBottomElevation() {
        return bottomElevation;
    }

    /**
     * Set Bottom Elevation
     * @param bottomElevation
     */
    public void setBottomElevation(int bottomElevation) {
        this.bottomElevation = bottomElevation;
    }

    /**
     * Get Top Elevation
     * @return
     */
    public int getTopElevation() {
        return topElevation;
    }

    /**
     * Set Top Elevation
     * @param topElevation
     */
    public void setTopElevation(int topElevation) {
        this.topElevation = topElevation;
    }

    /**
     * Get Size of Spot
     * @return
     */
    public int getSizeAcres() {
        return sizeAcres;
    }

    public void setSizeAcres(int sizeAcres) {
        this.sizeAcres = sizeAcres;
    }

    /**
     * Get the station identifier for this observation.
     * 
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Integer getElevation() {
        return location.getElevation();
    }

    /**
     * Get whether the location for this observation is defined.
     * 
     * @return Is this location defined.
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }

    /**
     * get DataURI
     */
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    /**
     * Set the data uri for this observation.
     * 
     * @param dataURI
     */
    @Override
    public void setDataURI(String dataURI) {
        super.setDataURI(dataURI);
        identifier = dataURI;
    }

    /**
     * Get Request Official Name
     * @return
     */
    public String getReqOfficial() {
        return reqOfficial;
    }

    /**
     * Set Request Official Name
     * @param reqOfficial
     */
    public void setReqOfficial(String reqOfficial) {
        this.reqOfficial = reqOfficial;
    }

    /**
     * Get Requester's Emergency Phone Number
     * @return
     */
    public String getEmPhone() {
        return emPhone;
    }

    /**
     * Set Requester's Emergency Phone Number
     * @param emPhone
     */
    public void setEmPhone(String emPhone) {
        this.emPhone = emPhone;
    }

    /**
     * Get Symbol
     * @return
     */
    public String getStqSymbol() {
        return stqSymbol;
    }
    
    /**
     * setStqSymbol (only for serialization / archive)
     * 
     * @param stqSymbol
     */
    public void setStqSymbol(String stqSymbol) {
        this.stqSymbol = stqDispSymbol;
    }

    /**
     * toString
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Calendar c = getDataTime().getRefTimeAsCalendar();
        if (c != null) {
            sb.append(String.format("STQ:%1$tY%1$tm%1$td%1$tH%1$tM",
                    getDataTime().getRefTimeAsCalendar()));
        } else {
            sb.append("STQ:YYYYMMDDHHmm");
        }
        sb.append(String.format("%6.2f %7.2f:", getLatitude(), getLongitude()));
        return sb.toString();
    }

}
