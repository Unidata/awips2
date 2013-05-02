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
import java.util.Collection;
import java.util.HashMap;
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
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
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
 * 20090403           1939 jkorman     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "acarssoundingseq")
@Table(name = "acarssounding", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "acarssounding",
		indexes = {
				@Index(name = "acarssounding_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ACARSSoundingRecord extends PluginDataObject implements
        ISpatialEnabled, IDecoderGettable {

    private static final long serialVersionUID = 1L;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
    static {
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
    }

    // Time of the observation.
    @Column
    @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar timeObs;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Column(length = 32)
    @DynamicSerializeElement
    @XmlElement
    private String tailNumber;

    // Flight phase (A[scending]  D[escending])
    @Column(length = 1)
    @DynamicSerializeElement
    @XmlElement
    private String phase = null;
    
    // oldest observation time in this sounding
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Long oldestTime = Long.MAX_VALUE;

    // newest observation time in this sounding
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Long newestTime = Long.MIN_VALUE;
    
    // The level data for this observation.
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parent", fetch = FetchType.EAGER)
    @XmlElement
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
     * @param phase the phase to set
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
     * @param oldestTime the oldestTime to set
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
     * @param newestTime the newestTime to set
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
        if(level != null) {
            level.setParent(this);
            if (levels == null) {
                levels = new HashSet<ACARSSoundingLayer>();
            }
            levels.add(level);
            long cTime = level.getTimeObs().getTimeInMillis();
            if(cTime < oldestTime) {
                oldestTime = cTime;
            }
            if(cTime > newestTime) {
                newestTime = cTime;
            }
        }
    }

    /**
     * This class does not implement IDecoderGettable so return null.
     * 
     * @return Always null for this class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    public Amount getValue(String pName) {
        Amount a = null;
        if ("NLAT".equals(pName)) {
            a = new Amount(getLatitude(), LOCATION_UNIT);
        } else if ("NLON".equals(pName)) {
            a = new Amount(getLongitude(), LOCATION_UNIT);
        }
        return a;
    }

    @Override
    public Collection<Amount> getValues(String paramName) {
        return null;
    }

    @Override
    public String getString(String paramName) {
        return null;
    }

    @Override
    public String[] getStrings(String paramName) {
        return null;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
