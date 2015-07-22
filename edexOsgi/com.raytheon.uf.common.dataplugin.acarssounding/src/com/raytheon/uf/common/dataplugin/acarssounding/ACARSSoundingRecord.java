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
package com.raytheon.uf.common.dataplugin.acarssounding;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.measure.quantity.Angle;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 03, 2009 1939       jkorman     Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Oct 22, 2013 2361       njensen     Remove XML annotations and IDecoderGettable
 * Feb 27, 2014 2638       njensen     Remove bad dataURI annotation
 * Jul 21, 2015 4360       rferrel     Add name to unique constraint.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "acarssoundingseq")
@Table(name = "acarssounding", uniqueConstraints = { @UniqueConstraint(name = "uk_acarssounding_datauri_fields", columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "acarssounding", indexes = { @Index(name = "acarssounding_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class ACARSSoundingRecord extends PluginDataObject implements
        ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    // Time of the observation.
    @Column
    @DynamicSerializeElement
    private Calendar timeObs;

    // TODO Update once SurfaceObsLocation DataURI's are corrected.
    @Embedded
    @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Column(length = 32)
    @DynamicSerializeElement
    private String tailNumber;

    // Flight phase (A[scending] D[escending])
    @Column(length = 1)
    @DynamicSerializeElement
    private String phase = null;

    // oldest observation time in this sounding
    @Column
    @DynamicSerializeElement
    private Long oldestTime = Long.MAX_VALUE;

    // newest observation time in this sounding
    @Column
    @DynamicSerializeElement
    private Long newestTime = Long.MIN_VALUE;

    // The level data for this observation.
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parent", fetch = FetchType.EAGER)
    @DynamicSerializeElement
    private Set<ACARSSoundingLayer> levels;

    /**
     * 
     */
    public ACARSSoundingRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public ACARSSoundingRecord(String uri) {
        super(uri);
    }

    /**
     * @return the timeObs
     */
    public Calendar getTimeObs() {
        return timeObs;
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setTimeObs(Calendar timeObs) {
        this.timeObs = timeObs;
    }

    /**
     * 
     */
    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    /**
     * 
     * @return
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * 
     * @param location
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * Get the airport identifier for this sounding
     * 
     * @return The airport identifier.
     */
    public String getStationId() {
        return location.getStationId();
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
     * Is the location for this profile defined?
     * 
     * @return Is the location for this profile defined?
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }

    /**
     * @return the tailNumber
     */
    public String getTailNumber() {
        return tailNumber;
    }

    /**
     * @param tailNumber
     *            the tailNumber to set
     */
    public void setTailNumber(String tailNumber) {
        this.tailNumber = tailNumber;
    }

    /**
     * @return the phase
     */
    public String getPhase() {
        return phase;
    }

    /**
     * @param phase
     *            the phase to set
     */
    public void setPhase(String phase) {
        this.phase = phase;
    }

    /**
     * @return the oldestTime
     */
    public Long getOldestTime() {
        return oldestTime;
    }

    /**
     * @param oldestTime
     *            the oldestTime to set
     */
    public void setOldestTime(Long oldestTime) {
        this.oldestTime = oldestTime;
    }

    /**
     * @return the newestTime
     */
    public Long getNewestTime() {
        return newestTime;
    }

    /**
     * @param newestTime
     *            the newestTime to set
     */
    public void setNewestTime(Long newestTime) {
        this.newestTime = newestTime;
    }

    /**
     * Get the set of levels for this observation.
     * 
     * @return The level data.
     */
    public Set<ACARSSoundingLayer> getLevels() {
        return levels;
    }

    /**
     * Set the set of levels for this observation.
     * 
     * @param levels
     *            the levels to set
     */
    public void setLevels(Set<ACARSSoundingLayer> levels) {
        this.levels = levels;
    }

    /**
     * 
     * @param cloud
     */
    public void addLevel(ACARSSoundingLayer level) {
        if (level != null) {
            level.setParent(this);
            if (levels == null) {
                levels = new HashSet<ACARSSoundingLayer>();
            }
            levels.add(level);
            long cTime = level.getTimeObs().getTimeInMillis();
            if (cTime < oldestTime) {
                oldestTime = cTime;
            }
            if (cTime > newestTime) {
                newestTime = cTime;
            }
        }
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "acarssounding";
    }
}
