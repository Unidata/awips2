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
package com.raytheon.uf.common.dataplugin.bufrascat;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import org.locationtech.jts.geom.Geometry;

/**
 * PluginDataObject for ascat data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 2624       jkorman     Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Jul 16, 2015 4360       rferrel     Convert satId to int, wndSpd to float
 *                                      and give name to unique constraint.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrascatseq")
@Table(name = "bufrascat", uniqueConstraints = { @UniqueConstraint(name = "uk_bufrascat_datauri_fields", columnNames = {
        "stationid", "refTime", "satId", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "bufrascat", indexes = { @Index(name = "bufrascat_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class AScatObs extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    @DataURI(position = 1)
    @NullString
    @DynamicSerializeElement
    @Column(nullable = false)
    private int satId;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Text of the WMO header
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoHeader;

    @DynamicSerializeElement
    @Transient
    private Integer orbitNumber;

    // The observation time.
    @DynamicSerializeElement
    @Transient
    private Calendar validTime;

    @DynamicSerializeElement
    @Transient
    private Double windDir;

    @DataURI(position = 3)
    @NullString
    @Column(nullable = false)
    @DynamicSerializeElement
    private float windSpd;

    @DynamicSerializeElement
    @Transient
    private Double probRain;

    @DynamicSerializeElement
    @Transient
    private Integer rainIndex;

    /**
     * Empty constructor.
     */
    public AScatObs() {
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
    public AScatObs(String uri) {
        super(uri);
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
     * @return the location
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * @return the satId
     */
    public int getSatId() {
        return satId;
    }

    /**
     * @param satId
     *            the satId to set
     */
    public void setSatId(int satId) {
        this.satId = satId;
    }

    /**
     * @return the orbitNumber
     */
    public Integer getOrbitNumber() {
        return orbitNumber;
    }

    /**
     * @param orbitNumber
     *            the orbitNumber to set
     */
    public void setOrbitNumber(Integer orbitNumber) {
        this.orbitNumber = orbitNumber;
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
     * @return the windDir
     */
    public Double getWindDir() {
        return windDir;
    }

    /**
     * @param windDir
     *            the windDir to set
     */
    public void setWindDir(Double windDir) {
        this.windDir = windDir;
    }

    /**
     * @return the windSpd
     */
    public Float getWindSpd() {
        return windSpd;
    }

    /**
     * @param windSpd
     *            the windSpd to set
     */
    public void setWindSpd(Float windSpd) {
        this.windSpd = windSpd;
    }

    /**
     * @return the probRain
     */
    public Double getProbRain() {
        return probRain;
    }

    /**
     * @param probRain
     *            the probRain to set
     */
    public void setProbRain(Double probRain) {
        this.probRain = probRain;
    }

    /**
     * @return the rainIndex
     */
    public Integer getRainIndex() {
        return rainIndex;
    }

    /**
     * @param rainIndex
     *            the rainIndex to set
     */
    public void setRainIndex(Integer rainIndex) {
        this.rainIndex = rainIndex;
    }

    /**
     * Get the observation time for this data.
     * 
     * @return The data observation time.
     */
    public Calendar getValidTime() {
        return validTime;
    }

    /**
     * Set the observation time for this data.
     * 
     * @param timeObs
     *            The data observation time.
     */
    public void setValidTime(Calendar time) {
        validTime = time;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return location;
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
    public String getPluginName() {
        return "bufrascat";
    }
}
