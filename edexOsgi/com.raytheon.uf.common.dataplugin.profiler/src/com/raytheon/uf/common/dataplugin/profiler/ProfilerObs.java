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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
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

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080303            969 jkorman     Initial implementation.
 * 20090413           2251 jsanchez    Implemented IDecoderGettable methods
 *                                      and plotted Profiler plots.
 * 20090610           2489 jsanchez    Updated the windSpeeed & windDirection.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "profilerseq")
@Table(name = "profiler", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "profiler",
		indexes = {
				@Index(name = "profiler_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProfilerObs extends PersistablePluginDataObject implements
        ISpatialEnabled, IDecoderGettable, IPointData, IPersistable,
        Comparable<ProfilerObs> {

    private static final long serialVersionUID = 1L;

    private static final HashMap<Integer, Integer> HGT_MAP = new HashMap<Integer, Integer>();
    static {
        HGT_MAP.put(100, 16180);
        HGT_MAP.put(150, 13608);
        HGT_MAP.put(200, 11784);
        HGT_MAP.put(250, 10363);
        HGT_MAP.put(300, 9164);
        HGT_MAP.put(400, 7185);
        HGT_MAP.put(500, 5574);
        HGT_MAP.put(700, 3012);
        HGT_MAP.put(850, 1457);
        HGT_MAP.put(925, 766);
    };

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Velocity> WIND_SPEED_UNIT = SI.METERS_PER_SECOND;

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();

    private static final String PROF_ID = "profid";

    static {
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
        PARM_MAP.put("WS", SFC_WNDSPD);
        PARM_MAP.put("WD", SFC_WNDDIR);
        PARM_MAP.put("PROF_ID", PROF_ID);
    }

    private static final String PRESS = "PRESS";

    private static final String AGL = "AGL";

    public static final String PRESS_PARAM_PTRN = ".*:" + PRESS + "=\\d{2,4}";

    public static final String AGL_PARAM_PTRN = ".*:" + AGL + "=\\d{2,4}";

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

    @Override
    public String getString(String paramName) {
        String value = null;
        String pName = PARM_MAP.get(paramName);
        if (PROF_ID.equals(pName)) {
            value = profilerId;
        }
        return value;
    }

    @Override
    public String[] getStrings(String paramName) {
        return null;
    }

    @Override
    public Amount getValue(String paramName) {
        Amount a = null;

        if (parseParameter(paramName)) {
            String pName = PARM_MAP.get(parameterName);
            if (unit.equals(AGL) && (levelId == 0)) {
                Double dValue = null;
                if (SFC_WNDSPD.equals(pName)) {
                    dValue = getSfcWindSpeed();
                    if (dValue != null) {
                        a = new Amount(dValue, WIND_SPEED_UNIT);
                    }
                } else if (SFC_WNDDIR.equals(pName)) {
                    dValue = getSfcWindDir();
                    if (dValue != null) {
                        a = new Amount(dValue, WIND_DIR_UNIT);
                    }
                }
            } else {
                if ((pName != null) && (levels != null) && (levels.size() > 0)) {
                    profLevel = getLevel(levelId);
                    if (profLevel != null) {
                        Double dValue = null;
                        if (SFC_WNDSPD.equals(pName)) {
                            dValue = getWindSpeed();
                            if (dValue != null) {
                                a = new Amount(dValue, WIND_SPEED_UNIT);
                            }
                        } else if (SFC_WNDDIR.equals(pName)) {
                            dValue = getWindDirection();
                            if (dValue != null) {
                                a = new Amount(dValue, WIND_DIR_UNIT);
                            }
                        }
                    }
                }
            }
        } else {
            String pName = PARM_MAP.get(paramName);
            if (STA_LAT.equals(pName)) {
                a = new Amount(getLatitude(), LOCATION_UNIT);
            } else if (STA_LON.equals(pName)) {
                a = new Amount(getLongitude(), LOCATION_UNIT);
            }
        }
        return a;
    }

    @Override
    public Collection<Amount> getValues(String paramName) {
        return null;
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

    /**
     * Get the IDecoderGettable interface implementation. This class does not
     * currently support this interface.
     * 
     * @return Returns null.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return this;
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

    private Double getWindSpeed() {
        if (windSpeed == null) {
            double u = profLevel.getUcWind().doubleValue();
            double v = profLevel.getVcWind().doubleValue();

            // equation from ProfilerUtils.getWindSpeed
            windSpeed = Math.sqrt((u * u) + (v * v));
        }

        return windSpeed;
    }

    /**
     * 
     * @return
     */
    private Double getWindDirection() {
        if (windDirection == null) {
            double ucw = profLevel.getUcWind().doubleValue();
            double vcw = profLevel.getVcWind().doubleValue();
            windDirection = Math.toDegrees(Math.atan2(-ucw, -vcw));
            if (windDirection < 0) {
                windDirection += 360.0;
            }
        }

        return windDirection;
    }

    private ProfilerLevel getLevel(Integer level) {
        ProfilerLevel retValue = null;
        if (level != null) {
            Integer stdHgt = HGT_MAP.get(level);
            if ((unit.equals(PRESS)) && (stdHgt != null)) {
                int diff = 99999;
                for (ProfilerLevel l : levels) {
                    Integer height = l.getLevelHeight();
                    if (height != null) {
                        int d = Math.abs(stdHgt - height);
                        if (d < diff) {
                            // Track of the closest level
                            diff = d;
                            retValue = l;
                        }
                    }
                } // for
            } else if (unit.equals(AGL)) {
                for (ProfilerLevel l : levels) {
                    if ((l.getLevelHeight() != null)
                            && (getElevation() != null)) {
                        // Adjust to agl heights!
                        Integer height = l.getLevelHeight() - getElevation();

                        if (level == 1500 && height <= 1500 && height >= 1250) {
                            retValue = l;
                        } else if (level == 1250 && height <= 1500
                                && height > 1125) {
                            retValue = l;
                        } else if (level == 1000 && height <= 1125
                                && height > 875) {
                            retValue = l;
                        } else if (level == 750 && height <= 875
                                && height > 625) {
                            retValue = l;
                        } else if (level == 500 && height <= 625 && height > 0) {
                            retValue = l;
                        }
                        // No need to go higher than this.
                        if (height > 2000) {
                            break;
                        }
                    }
                }
            }
        }
        return retValue;
    }

    /**
     * 
     */
    @Override
    public String getMessageData() {
        return (profilerId != null) ? profilerId : "UNKN";
    }

    /**
     * Determine if the parameter is a level request, and parse out the pressure
     * level or the AGL and parameter name if so.
     * 
     * @param parameter
     *            The parameter string to parse.
     * @return This is a level parameter.
     */
    private boolean parseParameter(String parameter) {
        boolean goodParse = false;
        int start = 0;
        Pattern p = Pattern.compile(PRESS_PARAM_PTRN);
        Pattern h = Pattern.compile(AGL_PARAM_PTRN);
        Matcher m = p.matcher(parameter);
        if (m.find()) {
            start = parameter.indexOf(":PRESS=");
            if (start > 0) {
                unit = PRESS;
                parameterName = parameter.substring(0, start);
                start += ":PRESS=".length();
                levelId = Integer.parseInt(parameter.substring(start));
            }
            goodParse = true;
        } else if ((m = h.matcher(parameter)).find()) {
            start = parameter.indexOf(":AGL=");
            if (start > 0) {
                unit = AGL;
                parameterName = parameter.substring(0, start);
                start += ":AGL=".length();
                levelId = Integer.parseInt(parameter.substring(start));
            }
            goodParse = true;
        }
        return goodParse;
    }

    @Override
    public int compareTo(ProfilerObs other) {
        final int BEFORE = -1;
        final int EQUAL = 0;
        final int AFTER = 1;

        int result = 0;
        if (this == other) {
            result = EQUAL;
        } else {
            result = timeObs.compareTo(other.timeObs);
        }
        return result;
    }

}
