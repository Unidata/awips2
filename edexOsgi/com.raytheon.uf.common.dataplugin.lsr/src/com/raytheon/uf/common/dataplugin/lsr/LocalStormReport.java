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
package com.raytheon.uf.common.dataplugin.lsr;

import java.util.Date;

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
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import org.locationtech.jts.geom.Geometry;

/**
 * Record implementation for Local Storm Reports
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 01, 2009           jkorman   Initial creation
 * Apr 04, 2013  1846     bkowal    Added an index on refTime and forecastTime
 * Apr 12, 2013  1857     bgonzale  Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen  Remove dataURI column from PluginDataObject.
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Oct 14, 2013  2361     njensen   Removed XML annotations and IDecoderGettable
 * Dec 10, 2013  2581     njensen   Removed dataURI column
 * Jan 15, 2014  2581     njensen   Changed constraint to use officeId instead
 *                                  of stationId
 * Jan 30, 2014  2581     njensen   Added dataURI column back in
 * Sep 16, 2014  2707     bclement  removed dataURI column, event type now
 *                                  string, added event units
 * Jan 06, 2014  2707     bclement  changed unique constraint from officeId to
 *                                  stationId
 * Jul 21, 2016  4360     rferrel   Named unique constraint. Made eventType not
 *                                  nullable.
 * Mar 12, 2018  6824     randerso  Improved readability of toString() result.
 *                                  Code cleanup.
 *
 * </pre>
 *
 * @author jkorman
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "lsrseq")
@Table(name = "lsr", uniqueConstraints = {
        @UniqueConstraint(name = "uk_lsr_datauri_fields", columnNames = {
                "latitude", "longitude", "stationId", "refTime", "forecastTime",
                "eventType" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "lsr", indexes = {
        @Index(name = "lsr_refTimeIndex", columnNames = { "refTime",
                "forecastTime" }) })
@DynamicSerialize
public class LocalStormReport extends PersistablePluginDataObject
        implements ISpatialEnabled, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    private static final int MISSING = -9999;

    @DataURI(position = 1)
    @NullString
    @Column(nullable = false)
    @DynamicSerializeElement
    private String eventType;

    @Transient
    @DynamicSerializeElement
    private String eventUnits;

    // Correction indicator from wmo header
    @Column
    @DynamicSerializeElement
    private String corIndicator;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @Transient
    @DynamicSerializeElement
    private String wmoHeader = "";

    // Text of the office
    @DynamicSerializeElement
    private String officeid = "";

    //
    @Transient
    @DynamicSerializeElement
    private String cityLoc = "";

    //
    @Transient
    @DynamicSerializeElement
    private String source = "";

    //
    @Transient
    @DynamicSerializeElement
    private String countyLoc = "";

    //
    @Transient
    @DynamicSerializeElement
    private String stateLoc = "";

    //
    @Transient
    @DynamicSerializeElement
    private String remarks = "";

    //
    @Transient
    @DynamicSerializeElement
    private Float magnitude = -9999.0f;

    // 0 = unknown
    // 1 = estimated
    // 2 = measured
    // 3 =
    // 4 =
    @Transient
    @DynamicSerializeElement
    private Integer magQual = MISSING;

    //
    @Transient
    @DynamicSerializeElement
    private Integer injuries = MISSING;

    //
    @Transient
    @DynamicSerializeElement
    private Integer fatalities = MISSING;

    /**
     * Empty default constructor
     */
    public LocalStormReport() {
    }

    /**
     * Construct an instance of this class using the supplied datauri.
     *
     * @param dataUri
     */
    public LocalStormReport(String dataUri) {
        super(dataUri);
    }

    /**
     * @return the eventType
     */
    public String getEventType() {
        return eventType;
    }

    /**
     * @param eventType
     *            the eventType to set
     */
    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    /**
     * @return the eventUnits
     */
    public String getEventUnits() {
        return eventUnits;
    }

    /**
     * @param eventUnits
     *            the eventUnits to set
     */
    public void setEventUnits(String eventUnits) {
        this.eventUnits = eventUnits;
    }

    /**
     * @return the corIndicator
     */
    public String getCorIndicator() {
        return corIndicator;
    }

    /**
     * @param corIndicator
     *            the corIndicator to set
     */
    public void setCorIndicator(String corIndicator) {
        this.corIndicator = corIndicator;
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
     * @return the officeid
     */
    public String getOfficeid() {
        return officeid;
    }

    /**
     * @param officeid
     *            the officeid to set
     */
    public void setOfficeid(String officeid) {
        this.officeid = officeid;
    }

    /**
     * @return the cityLoc
     */
    public String getCityLoc() {
        return cityLoc;
    }

    /**
     * @param cityLoc
     *            the cityLoc to set
     */
    public void setCityLoc(String cityLoc) {
        this.cityLoc = cityLoc;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    /**
     * @return the countyLoc
     */
    public String getCountyLoc() {
        return countyLoc;
    }

    /**
     * @param countyLoc
     *            the countyLoc to set
     */
    public void setCountyLoc(String countyLoc) {
        this.countyLoc = countyLoc;
    }

    /**
     * @return the stateLoc
     */
    public String getStateLoc() {
        return stateLoc;
    }

    /**
     * @param stateLoc
     *            the stateLoc to set
     */
    public void setStateLoc(String stateLoc) {
        this.stateLoc = stateLoc;
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

    /**
     * @return the magnitude
     */
    public Float getMagnitude() {
        return magnitude;
    }

    /**
     * @param magnitude
     *            the magnitude to set
     */
    public void setMagnitude(Float magnitude) {
        this.magnitude = magnitude;
    }

    /**
     * @return the magQual
     */
    public Integer getMagQual() {
        return magQual;
    }

    /**
     * @param magQual
     *            the magQual to set
     */
    public void setMagQual(Integer magQual) {
        this.magQual = magQual;
    }

    /**
     * @return the injuries
     */
    public Integer getInjuries() {
        return injuries;
    }

    /**
     * @param injuries
     *            the injuries to set
     */
    public void setInjuries(Integer injuries) {
        this.injuries = injuries;
    }

    /**
     * @return the fatalities
     */
    public Integer getFatalities() {
        return fatalities;
    }

    /**
     * @param fatalities
     *            the fatalities to set
     */
    public void setFatalities(Integer fatalities) {
        this.fatalities = fatalities;
    }

    /**
     * Set the data uri for this observation.
     *
     * @param dataURI
     */
    @SuppressWarnings("unchecked")
    @Override
    public void setDataURI(String dataURI) {
        super.setDataURI(dataURI);
        identifier = dataURI;
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
        if (location == null) {
            return Double.NaN;
        } else {
            return location.getLatitude();
        }
    }

    /**
     * Get the geometry longitude.
     *
     * @return The geometry longitude.
     */
    public double getLongitude() {
        if (location == null) {
            return Double.NaN;
        } else {
            return location.getLongitude();
        }
    }

    /**
     * Get the station identifier for this observation.
     *
     * @return the stationId
     */
    public String getStationId() {
        if (location == null) {
            return null;
        } else {
            return location.getStationId();
        }
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

    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Date date = getDataTime().getRefTime();
        if (date != null) {
            sb.append(
                    String.format("LSR:%1$tY-%1$tm-%1$td %1$tH%1$tMZ ", date));
        } else {
            sb.append("LSR:YYYY-MM-DD HHmmZ ");
        }
        sb.append(String.format("%6.2f %7.2f:", getLatitude(), getLongitude()));
        sb.append(String.format("%s:", cityLoc));
        sb.append(String.format("%s:", eventType));
        sb.append(String.format("%5.2f:%s", getMagnitude(), eventUnits));
        return sb.toString();
    }

    @Override
    public String getPluginName() {
        return "lsr";
    }

}
