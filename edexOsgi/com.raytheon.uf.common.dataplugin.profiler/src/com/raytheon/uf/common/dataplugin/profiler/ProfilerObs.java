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
package com.raytheon.uf.common.dataplugin.profiler;

import java.util.ArrayList;
import java.util.Calendar;
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
 * The ProfilerObs class encapsulates the location and time information for a
 * profiler observation as well as providing a container for the vertical level
 * data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * Apr 13, 2009  2251     jsanchez    Implemented IDecoderGettable methods  and
 *                                    plotted Profiler plots.
 * Jun 10, 2009  2489     jsanchez    Updated the windSpeeed & windDirection.
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from 
 *                                    PluginDataObject.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Dec 03, 2013  2537     bsteffen    Remove IDecoderGettable
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "profilerseq")
@Table(name = ProfilerObs.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = ProfilerObs.PLUGIN_NAME, indexes = { @Index(name = "profiler_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProfilerObs extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable,
        Comparable<ProfilerObs> {

    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "profiler";

    @Transient
    private String parameterName = null;

    @Transient
    private String unit = null;

    @Transient
    private ProfilerLevel profLevel = null;

    @Transient
    private Double windSpeed = null;

    @Transient
    private Double windDirection = null;

    @Transient
    private Integer levelId;

    @DataURI(position = 1)
    @XmlAttribute
    @DynamicSerializeElement
    private Integer reportType;

    // The profiler observation time.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar timeObs;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String profilerId;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoHeader;

    // the level data
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String profilerName;

    // the level data
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private List<ProfilerLevel> levels;

    @XmlAttribute
    @DynamicSerializeElement
    @Transient
    private Double sfcWindSpeed;

    @XmlAttribute
    @DynamicSerializeElement
    @Transient
    private Double sfcWindDir;

    /**
     * Create an empty ProfilerObs object.
     */
    public ProfilerObs() {
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
    public ProfilerObs(String uri) {
        super(uri);
    }

    /**
     * Get the observation time for this data.
     * 
     * @return The data observation time.
     */
    public Calendar getTimeObs() {
        return timeObs;
    }

    /**
     * Set the observation time for this data.
     * 
     * @param timeObs
     *            The data observation time.
     */
    public void setTimeObs(Calendar timeObs) {
        this.timeObs = timeObs;
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
     * @return the profilerId
     */
    public String getProfilerId() {
        return profilerId;
    }

    /**
     * @param profilerId
     *            the profilerId to set
     */
    public void setProfilerId(String profilerId) {
        this.profilerId = profilerId;
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
     * Was this location defined from the station catalog? False if not.
     * 
     * @return Was this location defined from the station catalog?
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }

    /**
     * Get the report type of this data.
     * 
     * @return the reportType
     */
    public Integer getReportType() {
        return reportType;
    }

    /**
     * Set the report type of this data.
     * 
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(Integer reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the sfcWindSpeed
     */
    public Double getSfcWindSpeed() {
        return sfcWindSpeed;
    }

    /**
     * @param sfcWindSpeed
     *            the sfcWindSpeed to set
     */
    public void setSfcWindSpeed(Double sfcWindSpeed) {
        this.sfcWindSpeed = sfcWindSpeed;
    }

    /**
     * @return the sfcWindDir
     */
    public Double getSfcWindDir() {
        return sfcWindDir;
    }

    /**
     * @param sfcWindDir
     *            the sfcWindDir to set
     */
    public void setSfcWindDir(Double sfcWindDir) {
        this.sfcWindDir = sfcWindDir;
    }

    /**
     * Set the WMOHeader of the file that contained this data.
     * 
     * @return The wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Get the WMOHeader of the file that contained this data.
     * 
     * @param wmoHeader
     *            The WMOHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the profilerName
     */
    public String getProfilerName() {
        return profilerName;
    }

    /**
     * @param profilerName
     *            the profilerName to set
     */
    public void setProfilerName(String profilerName) {
        this.profilerName = profilerName;
    }

    /**
     * 
     * @param level
     *            A profiler data level to add.
     */
    public void addLevel(ProfilerLevel level) {
        if (levels == null) {
            levels = new ArrayList<ProfilerLevel>();
        }
        levels.add(level);
    }

    /**
     * Get all levels contained by this object.
     * 
     * @return the levels
     */
    public List<ProfilerLevel> getLevels() {
        return levels;
    }

    /**
     * Set the level data into this object.
     * 
     * @param levels
     *            the levels to set
     */
    public void setLevels(List<ProfilerLevel> levels) {
        this.levels = levels;
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
     */
    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
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

    /**
     * 
     */
    @Override
    public String getMessageData() {
        return (profilerId != null) ? profilerId : "UNKN";
    }

    @Override
    public int compareTo(ProfilerObs other) {

        int result = 0;
        if (this == other) {
            result = 0;
        } else {
            result = timeObs.compareTo(other.timeObs);
        }
        return result;
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
