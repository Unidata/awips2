/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.bufrua;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * UAObs represents a single vertical upper air observation at a single point in
 * time and space. The observation serves as a container for an indefinite
 * number of levels of data. This class may represent a fixed or mobile
 * location. For mobile stations, the station's movement information may be
 * captured as well as it's location.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 27, 2007  382      jkorman     Initial Coding.
 * Jan 04, 2008  712      jkorman     Lat/Lon were set incorrectly.
 * Jan 07, 2008  720      jkorman     remove default assignments from
 *                                    attributes.
 * Jan 08, 2008  382      jkorman     Added IVerticalSoundingCreator impl.
 * Jan 14, 2008  763      jkorman     Added &quot;below&quot; ground level
 *                                    exclusion to  getValue.
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from
 *                                    PluginDataObject.
 * Jun 20, 2013  2128     bsteffen    Ensure setDataURI sets the dataURI.
 * Jul 19, 2013  1992     bsteffen    Remove redundant time columns from
 *                                    bufrua.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufruaseq")
@Table(name = UAObs.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = UAObs.PLUGIN_NAME, indexes = { @Index(name = "bufrua_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UAObs extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    private static final Comparator<UAObs> corComparator = new Comparator<UAObs>() {
        @Override
        public int compare(UAObs a, UAObs b) {
            int compValue = 0;
            String wmoA = a.getWmoHeader();
            String wmoB = b.getWmoHeader();

            if (wmoA != null) {
                if (wmoB != null) {
                    compValue = wmoA.compareTo(wmoB);
                }
            }
            if (compValue != 0) {
                compValue *= -1;
            }
            return compValue;
        }
    };

    public static final String PLUGIN_NAME = "bufrua";

    // The observation report type.
    @DataURI(position = 1)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer reportType;

    @Embedded
    @DataURI(position = 4, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Correction indicator from wmo header
    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String corIndicator;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @DataURI(position = 3)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoHeader;

    // Station pressure in Pascals.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer pressure_station;

    // The total cloud cover in 1/8s coverage.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer totalCloudCover;

    // The platform directio in angular degrees.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer platformDirection;

    // The platform movement in meters per second.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double platformMovement;

    // ICAO of station if known.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String stationName;

    // The level data for this observation.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<UAObsLevel> levels;

    @Column(insertable = false, updatable = false)
    @XmlAttribute
    @DynamicSerializeElement
    private Integer idx;

    public void setIdx(Integer idx) {
        this.idx = idx;
    }

    public Integer getIdx() {
        return idx;
    }

    /**
     * Empty constructor.
     */
    public UAObs() {
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
    public UAObs(String uri) {
        super(uri);
        corIndicator = "null".equals(corIndicator) ? null : corIndicator;
        if (location != null) {
            String staId = location.getStationId();
            location.setStationId("null".equals(staId) ? null : staId);
        }
    }

    /**
     * Get the set of levels for this observation.
     * 
     * @return The level data.
     */
    public List<UAObsLevel> getLevels() {
        return levels;
    }

    /**
     * Set the set of levels for this observation.
     * 
     * @param levels
     *            the levels to set
     */
    public void setLevels(List<UAObsLevel> levels) {
        this.levels = levels;
    }

    /**
     * 
     * @param cloud
     */
    public void addLevel(UAObsLevel level) {
        if (levels == null) {
            levels = new ArrayList<UAObsLevel>();
        }
        levels.add(level);
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
     * Get the report data for this observation.
     * 
     * @return The Report data.
     */
    public String getReportData() {
        String s = null;
        if (messageData instanceof String) {
            s = (String) messageData;
        }
        return s;
    }

    /**
     * Set the report data for this observation.
     * 
     * @param reportData
     *            The Report data.
     */
    public void setReportData(String reportData) {
        messageData = reportData;
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
     * Get the observation report type.
     * 
     * @return the reportType
     */
    public Integer getReportType() {
        return reportType;
    }

    /**
     * Set the observation report type.
     * 
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(Integer reportType) {
        this.reportType = reportType;
    }

    /**
    /**
     * Get the station pressure at the observation site.
     * 
     * @return the pressure_station
     */
    public Integer getPressure_station() {
        return pressure_station;
    }

    /**
     * Set the station pressure at the observation site.
     * 
     * @param pressure_station
     *            the pressure_station to set
     */
    public void setPressure_station(Integer pressure_station) {
        this.pressure_station = pressure_station;
    }

    /**
     * Get the total clould cover (n/8s).
     * 
     * @return the totalCloudCover
     */
    public Integer getTotalCloudCover() {
        return totalCloudCover;
    }

    /**
     * Get the direction the platform is moving. (Valid only for mobile
     * observations i.e. TEMPSHIP.
     * 
     * @return the platformDirection
     */
    public Integer getPlatformDirection() {
        return platformDirection;
    }

    /**
     * Set the direction the platform is moving. (Valid only for mobile
     * observations i.e. TEMPSHIP.
     * 
     * @param platformDirection
     *            the platformDirection to set
     */
    public void setPlatformDirection(Integer platformDirection) {
        this.platformDirection = platformDirection;
    }

    /**
     * Get the movement of the platform in meters per second.
     * 
     * @return The platform movement in meters per second.
     */
    public Double getPlatformMovement() {
        return platformMovement;
    }

    /**
     * Set the movement of the platform in meters per second.
     * 
     * @param shipMovement
     *            The platform movement in meters per second.
     */
    public void setPlatformMovement(Double platformMovement) {
        this.platformMovement = platformMovement;
    }

    /**
     * Set the total clould cover (n/8s).
     * 
     * @param totalCloudCover
     *            the totalCloudCover to set
     */
    public void setTotalCloudCover(Integer totalCloudCover) {
        this.totalCloudCover = totalCloudCover;
    }

    /**
     * @return the stationName
     */
    public String getStationName() {
        return stationName;
    }

    /**
     * @param stationName
     *            the stationName to set
     */
    public void setStationName(String stationName) {
        this.stationName = stationName;
    }

    /**
     * 
     */
    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    /**
     * 
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    public SurfaceObsLocation getLocation() {
        if (location == null) {
            location = new SurfaceObsLocation();
        }
        return location;
    }

    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * Returns the hashCode for this object. This implementation returns the
     * hashCode of the generated dataURI.
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((getDataURI() == null) ? 0 : getDataURI().hashCode());
        return result;
    }

    /**
     * Checks if this record is equal to another by checking the generated
     * dataURI.
     * 
     * @param obj
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        UAObs other = (UAObs) obj;
        if (getDataURI() == null) {
            if (other.getDataURI() != null) {
                return false;
            }
        } else if (!getDataURI().equals(other.getDataURI())) {
            return false;
        }
        return true;
    }

    /**
     * Returns a
     * 
     * @param obsList
     * @return
     */
    public static final List<UAObs> sortByCorrection(List<UAObs> obsList) {

        // No need to sort for null, empty, or one item.
        if ((obsList != null) && (obsList.size() > 1)) {
            Collections.sort(obsList, getCorComparator());
        }
        return obsList;
    }

    public static Comparator<UAObs> getCorComparator() {
        return corComparator;
    }

    @Override
    public String toString() {
        return wmoHeader;
    }


    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }
}