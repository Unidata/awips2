/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import gov.noaa.nws.ncep.common.dataplugin.nctaf.util.VisibilityParser;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.time.DateUtils;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Record implementation for taf plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 20, 2011 458         sgurung     Main pdo class
 * Sep 23, 2011 458         sgurung     Converted to HDF5
 * Sep 29, 2011             sgurung     Added reportType
 * Oct 19, 2011             sgurung     Set dataTime with correct valid period
 * Oct 26, 2011             sgurung     Added probable parameters (for
 *                                      TEMPO/PROB groups).  Split change groups
 *                                      into hourly records.
 * Nov 03, 2011             sgurung     Added probable weather and method to
 *                                      calculate ceiling.
 * Nov 04, 2011             sgurung     Sort sky_cover before calculating
 *                                      ceiling.  Change startRefTime to nearest
 *                                      hour to get hourly refTimes
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 08, 2013 1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract.
 * Dec 03, 2013 2551        rjpeter     Extend PersistablePluginDataObject.
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "nctafseq")
@Table(name = "nctaf", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "nctaf", indexes = { @Index(name = "nctaf_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class NcTafRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    @XmlElement
    @Column
    private String wmoHeader;

    @DynamicSerializeElement
    @XmlElement
    // @Column(length = 1024)
    @Transient
    private String tafText;

    // The observation report type.
    @DataURI(position = 1)
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String reportType;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Station Identifier for the data
    @DynamicSerializeElement
    @XmlElement
    @Transient
    // @Index(name = "nctaf_stationIndex")
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
    @Transient
    private String remarks;

    /** A String containing the change group */
    @DynamicSerializeElement
    @XmlElement
    // @Column
    @Transient
    private String changeGroup;

    /** The period for which the TAF is valid */
    @DynamicSerializeElement
    @XmlElement
    @Embedded
    @Transient
    private NcTafPeriod tafChangePeriod;

    /**
     * The forecast valid starting date
     */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Calendar startDate;

    // This time is only used for BECMG groups. It marks the end time of the
    // BECMG transition period.
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Calendar transitionEndDate;

    /**
     * The forecast valid ending date
     */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Calendar endDate;

    /**
     * The sequence id is used to physically order a collection of ChangeGroups.
     * This is required because the start times may be ambiguous i.e. A BECMG
     * and TEMPO change group could share the same start time.
     */
    @DynamicSerializeElement
    @XmlElement
    @Column
    @DataURI(position = 6)
    private Integer sequenceId;

    /**
     * The change group indicator i.e. BECMG, FM, TEMPO, etc
     */
    @DynamicSerializeElement
    @XmlElement
    @Column
    @DataURI(position = 7)
    private String change_indicator;

    /**
     * The probability percentage for PROB and PROB TEMPO change groups.
     */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probability;

    /** Wind direction in degrees */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_dir_degrees;

    /** Wind speed in knots */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_speed_kt;

    /** Wind gust in knots */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_gust_kt;

    /** Wind shear height above ground level */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_shear_hgt_ft_agl;

    /** Wind shear direction in degrees */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_shear_dir_degrees;

    /** Wind shear speed in knots */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float wind_shear_speed_kt;

    /** Visibility (horizontal) in miles */
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Float visibility_mi;

    /** Altimeter reading in inches of mercury */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float altim_in_hg;

    /** Vertical visibility */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float vert_vis_ft;

    /** Maximum temperature */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer max_temp_c;

    /** Minimum temperature */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer min_temp_c;

    /** Wind direction in degrees (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_dir_degrees;

    /** Wind speed in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_speed_kt;

    /** Wind gust in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_gust_kt;

    /** Wind shear height above ground level (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_shear_hgt_ft_agl;

    /** Wind shear direction in degrees (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_shear_dir_degrees;

    /** Wind shear speed in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_wind_shear_speed_kt;

    /** Visibility (horizontal) in miles (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_visibility_mi;

    /** Vertical visibility (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Float probable_vert_vis_ft;

    /**
     * The valid period for the TAF record.
     */
    @DynamicSerializeElement
    @XmlElement
    private NcTafPeriod tafValidPeriod;

    /**
     * Weather and obscurations
     */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafWeatherCondition> weather;

    /**
     * (TEMPO/PROB) Weather and obscurations
     */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafWeatherCondition> probable_weather;

    /** Sky coverage */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafSkyCover> sky_cover;

    /** (TEMPO/PROB) Sky coverage */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafSkyCover> probable_sky_cover;

    /** The turbulence layers */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafTurbulenceLayer> turbulence_layers;

    /** The icing layers */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafIcingLayer> icing_layers;

    /** The temperature forecasts */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Set<NcTafTemperatureForecast> temp_forecasts;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // private Integer hdfFileId;

    public NcTafRecord() {
        wmoHeader = " ";
        tafText = " ";
        reportType = "TAF";
        stationId = " ";
        bulletin_time = null;
        corIndicator = " ";
        amdIndicator = " ";
        issue_timeString = " ";
        remarks = " ";
        changeGroup = " ";
        altim_in_hg = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        change_indicator = " ";
        max_temp_c = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
        min_temp_c = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
        probability = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
        remarks = " ";
        sequenceId = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
        vert_vis_ft = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        visibility_mi = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_dir_degrees = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_gust_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_speed_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_shear_hgt_ft_agl = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_shear_dir_degrees = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        wind_shear_speed_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        startDate = null;
        endDate = null;
        transitionEndDate = null;
        location = null;
        probable_vert_vis_ft = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_visibility_mi = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_dir_degrees = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_gust_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_speed_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_shear_hgt_ft_agl = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_shear_dir_degrees = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
        probable_wind_shear_speed_kt = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
    }

    /**
     * Constructs a taf record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public NcTafRecord(String uri) {
        super(uri);
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

    @Override
    public void setIdentifier(Object dataURI) {

        this.identifier = dataURI;

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
        NcTafRecord other = (NcTafRecord) obj;
        if (getDataURI() == null) {
            if (other.getDataURI() != null) {
                return false;
            }
        } else if (!getDataURI().equals(other.getDataURI())) {
            return false;
        }
        return true;
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

    /**
     * 
     * @return The string containing the change group text
     */
    public String getChangeGroup() {
        return this.changeGroup;
    }

    public void setChangeGroup(String changeGroup) {
        this.changeGroup = changeGroup;
    }

    public NcTafPeriod getTafChangePeriod() {
        return tafChangePeriod;
    }

    public void setTafChangePeriod(NcTafPeriod tafChangePeriod) {
        this.tafChangePeriod = tafChangePeriod;
    }

    public Float getWind_dir_degrees() {
        return wind_dir_degrees;
    }

    public void setWind_dir_degrees(Float wind_dir_degrees) {
        this.wind_dir_degrees = wind_dir_degrees;
    }

    public Float getWind_speed_kt() {
        return wind_speed_kt;
    }

    public void setWind_speed_kt(Float wind_speed_kt) {
        this.wind_speed_kt = wind_speed_kt;
    }

    public Float getWind_gust_kt() {
        return wind_gust_kt;
    }

    public void setWind_gust_kt(Float wind_gust_kt) {
        this.wind_gust_kt = wind_gust_kt;
    }

    public Float getWind_shear_hgt_ft_agl() {
        return wind_shear_hgt_ft_agl;
    }

    public void setWind_shear_hgt_ft_agl(Float wind_shear_hgt_ft_agl) {
        this.wind_shear_hgt_ft_agl = wind_shear_hgt_ft_agl;
    }

    public Float getWind_shear_dir_degrees() {
        return wind_shear_dir_degrees;
    }

    public void setWind_shear_dir_degrees(Float wind_shear_dir_degrees) {
        this.wind_shear_dir_degrees = wind_shear_dir_degrees;
    }

    public Float getWind_shear_speed_kt() {
        return wind_shear_speed_kt;
    }

    public void setWind_shear_speed_kt(Float wind_shear_speed_kt) {
        this.wind_shear_speed_kt = wind_shear_speed_kt;
    }

    public Float getVisibility_mi() {
        return visibility_mi;
    }

    public void setVisibility_mi(Float visibility_mi) {
        this.visibility_mi = visibility_mi;
    }

    public float getAltim_in_hg() {
        return altim_in_hg;
    }

    public void setAltim_in_hg(float altim_in_hg) {
        this.altim_in_hg = altim_in_hg;
    }

    public float getVert_vis_ft() {
        return probable_vert_vis_ft;
    }

    public void setVert_vis_ft(float vert_vis_ft) {
        this.probable_vert_vis_ft = vert_vis_ft;
    }

    public float getProbable_wind_dir_degrees() {
        return probable_wind_dir_degrees;
    }

    public void setProbable_wind_dir_degrees(float wind_dir_degrees) {
        this.probable_wind_dir_degrees = wind_dir_degrees;
    }

    public float getProbable_wind_speed_kt() {
        return probable_wind_speed_kt;
    }

    public void setProbable_wind_speed_kt(float wind_speed_kt) {
        this.probable_wind_speed_kt = wind_speed_kt;
    }

    public float getProbable_wind_gust_kt() {
        return probable_wind_gust_kt;
    }

    public void setProbable_wind_gust_kt(float wind_gust_kt) {
        this.probable_wind_gust_kt = wind_gust_kt;
    }

    public float getProbable_wind_shear_hgt_ft_agl() {
        return probable_wind_shear_hgt_ft_agl;
    }

    public void setProbable_wind_shear_hgt_ft_agl(float wind_shear_hgt_ft_agl) {
        this.probable_wind_shear_hgt_ft_agl = wind_shear_hgt_ft_agl;
    }

    public float getProbable_wind_shear_dir_degrees() {
        return probable_wind_shear_dir_degrees;
    }

    public void setProbable_wind_shear_dir_degrees(float wind_shear_dir_degrees) {
        this.probable_wind_shear_dir_degrees = wind_shear_dir_degrees;
    }

    public float getProbable_wind_shear_speed_kt() {
        return probable_wind_shear_speed_kt;
    }

    public void setProbable_wind_shear_speed_kt(float wind_shear_speed_kt) {
        this.probable_wind_shear_speed_kt = wind_shear_speed_kt;
    }

    public float getProbable_visibility_mi() {
        return probable_visibility_mi;
    }

    public void setProbable_visibility_mi(float visibility_mi) {
        this.probable_visibility_mi = visibility_mi;
    }

    public float getProbable_vert_vis_ft() {
        return probable_vert_vis_ft;
    }

    public void setProbable_vert_vis_ft(float vert_vis_ft) {
        this.probable_vert_vis_ft = vert_vis_ft;
    }

    public Integer getMax_temp_c() {
        return max_temp_c;
    }

    public void setMax_temp_c(Integer max_temp_c) {
        this.max_temp_c = max_temp_c;
    }

    public Integer getMin_temp_c() {
        return min_temp_c;
    }

    public void setMin_temp_c(Integer min_temp_c) {
        this.min_temp_c = min_temp_c;
    }

    public Integer getProbability() {
        return probability;
    }

    public void setProbability(Integer probability) {
        this.probability = probability;
    }

    /**
     * @return the sequenceId
     */
    public Integer getSequenceId() {
        return sequenceId;
    }

    /**
     * @param sequenceId
     *            the sequenceId to set
     */
    public void setSequenceId(Integer sequenceId) {
        this.sequenceId = sequenceId;
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

    public String getChange_indicator() {
        return change_indicator;
    }

    public void setChange_indicator(String change_indicator) {
        this.change_indicator = change_indicator;
    }

    public NcTafPeriod getTafValidPeriod() {
        return tafValidPeriod;
    }

    public void setTafValidPeriod(NcTafPeriod tafValidPeriod) {
        this.tafValidPeriod = tafValidPeriod;
    }

    public Set<NcTafTurbulenceLayer> getTurbulence_layers() {
        return turbulence_layers;
    }

    public void setTurbulence_layers(Set<NcTafTurbulenceLayer> turbulence_layers) {
        this.turbulence_layers = turbulence_layers;
    }

    /**
     * @param add
     *            taf turbulence layer to set
     */
    public void addTurbulence_layer(NcTafTurbulenceLayer turb) {
        turbulence_layers.add(turb);
    }

    public Set<NcTafIcingLayer> getIcing_layers() {
        return icing_layers;
    }

    public void setIcing_layers(Set<NcTafIcingLayer> icing_layers) {
        this.icing_layers = icing_layers;
    }

    /**
     * @param add
     *            taf icing layer to set
     */
    public void addIcing_layer(NcTafIcingLayer icng) {
        icing_layers.add(icng);
    }

    public Set<NcTafTemperatureForecast> getTemp_forecasts() {
        return temp_forecasts;
    }

    public void setTemp_forecasts(Set<NcTafTemperatureForecast> temp_forecasts) {
        this.temp_forecasts = temp_forecasts;
    }

    /**
     * @param add
     *            taf temp forecast to set
     */
    public void addTemp_forecast(NcTafTemperatureForecast tempFcst) {
        temp_forecasts.add(tempFcst);
    }

    public Set<NcTafWeatherCondition> getWeather() {
        return weather;
    }

    public void setWeather(Set<NcTafWeatherCondition> weather) {
        this.weather = weather;
    }

    /**
     * @param add
     *            taf weather condition to set
     */
    public void addWeather(NcTafWeatherCondition wthrCond) {
        weather.add(wthrCond);
    }

    public Set<NcTafWeatherCondition> getProbable_weather() {
        return probable_weather;
    }

    public void setProbable_weather(Set<NcTafWeatherCondition> weather) {
        this.probable_weather = weather;
    }

    /**
     * @param add
     *            probable taf weather condition to set
     */
    public void addProbable_weather(NcTafWeatherCondition wthrCond) {
        probable_weather.add(wthrCond);
    }

    public Set<NcTafSkyCover> getSky_cover() {
        return sky_cover;
    }

    public void setSky_cover(Set<NcTafSkyCover> sky_cover) {
        this.sky_cover = sky_cover;
    }

    /**
     * @param add
     *            taf sky cover to set
     */
    public void addSky_cover(NcTafSkyCover skyCvr) {
        sky_cover.add(skyCvr);
    }

    /**
     * @param add
     *            probable taf sky cover to set
     */
    public void addProbable_sky_cover(NcTafSkyCover skyCvr) {
        probable_sky_cover.add(skyCvr);
    }

    public Set<NcTafSkyCover> getProbable_sky_cover() {
        return probable_sky_cover;
    }

    public void setProbable_sky_cover(Set<NcTafSkyCover> sky_cover) {
        this.probable_sky_cover = sky_cover;
    }

    /**
     * @return the theStartDate
     */
    public Calendar getStartDate() {
        return startDate;
    }

    /**
     * @param theStartDate
     *            the theStartDate to set
     */
    public void setStartDate(Calendar start) {
        startDate = start;
    }

    /**
     * @return the transitionEndDate
     */
    public Calendar getTransitionEndDate() {
        return transitionEndDate;
    }

    /**
     * @param transitionEndDate
     *            the transitionEndDate to set
     */
    public void setTransitionEndDate(Calendar transitionEndDate) {
        this.transitionEndDate = transitionEndDate;
    }

    /**
     * @return the theEndDate
     */
    public Calendar getEndDate() {
        return endDate;
    }

    /**
     * @param theEndDate
     *            the theEndDate to set
     */
    public void setEndDate(Calendar end) {
        endDate = end;
    }

    public static String formatDate(Calendar dateTime) {
        return String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", dateTime);
    }

    /**
     * This method determines the ceiling value of a TAF.
     */
    public static float getCeiling(Set<NcTafSkyCover> skyCov, float vertVis) {

        // If a vertical visibility has been reported, the sky is obscured
        // and the vertical visibility becomes the ceiling.
        if ((vertVis >= 0) && (vertVis < 1e20f)) {
            return vertVis;
        } else {
            // Otherwise, determine the ceiling value.
            return findTafCeilingFromLayers(sortSkyCover(skyCov));
        }
    }

    public static Set<NcTafSkyCover> sortSkyCover(Set<NcTafSkyCover> skySet) {
        if (skySet != null) {
            SortedSet<NcTafSkyCover> skSet = new TreeSet<NcTafSkyCover>();
            for (NcTafSkyCover sc : skySet) {
                skSet.add(sc);
            }
            return skSet;
        } else {
            return new HashSet<NcTafSkyCover>();
        }
    }

    /**
     * This method calculates a floating point number representing the ceiling.
     * By definition, the ceiling is the lowest overcast or broken cloud layer,
     * so the method looks for the lowest layer that matches a BKN or OVC
     * condition, and returns that layer.
     * 
     * @param skyCov
     *            -- the set of sky coverage data
     * @return -- the ceiling
     */
    public static float findTafCeilingFromLayers(Set<NcTafSkyCover> skyCov) {
        float ceiling = 1e20f;
        // Find a ceiling in a TAF report.
        try {
            for (NcTafSkyCover sc : skyCov) {
                if (sc.getType().equals("CLR") || sc.getType().equals("SKC")) {
                    ceiling = IDecoderConstantsN.NEGATIVE_FLOAT_MISSING;
                    break;
                } else if ((sc.getType().equals("BKN"))
                        || (sc.getType().equals("OVC"))) {
                    if (sc.getHeight() != null) {
                        ceiling = sc.getHeight();
                        break;
                    }
                }

            }
        } catch (RuntimeException e) {
            // ignore cloud cover that is null
        }
        return ceiling >= 1e20f ? IDecoderConstantsN.NEGATIVE_FLOAT_MISSING
                : ceiling;
    }

    /**
     * Constructs an array of NcTafRecord PDOs given one or more bulletin
     * records. (The bulletin records each contain data from a whole bulletin,
     * while each NcTafRecord is more fine-grained -- representing the values
     * for a single NcTafChangeGroup of the entire bulletin [NcTafBulletin]). We
     * do this "reshuffle" because the
     * NcTafBulletin/NcTafChangeGroup/NcTafSkyCover (same for
     * NcTafTemperatureForecase, NcTafTurbulenceLayer, NcTafSkyCover,
     * NcTafWeatherCondition) hierarchy efficiently represents the original
     * structure as parsed from the text, but we want the persistent PDO
     * (NcTafRecord) to be optimized for efficient access.
     * 
     * @param bulletin
     *            the bulletin record to split
     * @return NcTafRecord[] the array of NcTafRecord PDOs
     */
    public static NcTafRecord[] split(NcTafBulletinRecord bulletin) {
        List<NcTafRecord> records = new ArrayList<NcTafRecord>();
        if (bulletin.getChangeGroups() != null) {
            Iterator<NcTafChangeGroup> chgGrps = bulletin.getChangeGroups()
                    .iterator();
            if (chgGrps != null) {
                while (chgGrps.hasNext()) {

                    NcTafRecord tfr = new NcTafRecord();

                    NcTafChangeGroup chgGrp = chgGrps.next();

                    tfr.setPersistenceTime(new Date());

                    NcTafPeriod validPeriod = chgGrp.getTafChangePeriod();
                    Calendar cStart = TimeTools
                            .copy(validPeriod.getStartDate());

                    TimeRange range = new TimeRange(cStart,
                            validPeriod.getEndDate());
                    DataTime tafPeriod = new DataTime(cStart, range);

                    tfr.setDataTime(tafPeriod);

                    if (bulletin.getTafValidPeriod() != null) {
                        tfr.setTafValidPeriod(bulletin.getTafValidPeriod());
                    }

                    if (chgGrp.getTafChangePeriod() != null) {
                        tfr.setTafChangePeriod(chgGrp.getTafChangePeriod());
                    }

                    if (chgGrp.getTafChangePeriod().getEndDate() != null) {
                        tfr.setEndDate(chgGrp.getTafChangePeriod().getEndDate());
                    }

                    if (chgGrp.getTafChangePeriod().getStartDate() != null) {
                        tfr.setStartDate(chgGrp.getTafChangePeriod()
                                .getStartDate());
                    }

                    if (chgGrp.getTafChangePeriod().getTransitionEndDate() != null) {
                        tfr.setTransitionEndDate(chgGrp.getTafChangePeriod()
                                .getTransitionEndDate());
                    }

                    if (bulletin.getWmoHeader() != null) {
                        tfr.setWmoHeader(bulletin.getWmoHeader());
                    }

                    if (bulletin.getTafText() != null) {
                        tfr.setTafText(bulletin.getTafText());
                    }

                    if (bulletin.getReportType() != null) {
                        tfr.setReportType(bulletin.getReportType());
                    }

                    if (bulletin.getStationId() != null) {
                        tfr.setStationId(bulletin.getStationId());
                    }

                    if (bulletin.getCorIndicator() != null) {
                        tfr.setCorIndicator(bulletin.getCorIndicator());
                    } else {
                        tfr.setCorIndicator("");
                    }

                    if (bulletin.getAmdIndicator() != null) {
                        tfr.setAmdIndicator(bulletin.getAmdIndicator());
                    } else {
                        tfr.setAmdIndicator("");
                    }

                    if (bulletin.getIssue_time() != null) {
                        tfr.setIssue_time(bulletin.getIssue_time());
                    }

                    if (bulletin.getIssue_timeString() != null) {
                        tfr.setIssue_timeString(bulletin.getIssue_timeString());
                    }

                    if (bulletin.getBulletin_time() != null) {
                        tfr.setBulletin_time(bulletin.getBulletin_time());
                    }

                    if (bulletin.getLocation() != null) {
                        tfr.setLocation(bulletin.getLocation());
                    }

                    if (chgGrp.getChange_indicator() != null) {
                        tfr.setChange_indicator(chgGrp.getChange_indicator());
                    }

                    if (chgGrp.getChangeGroup() != null) {
                        tfr.setChangeGroup(chgGrp.getChangeGroup());
                    }

                    if (chgGrp.getProbability() != null) {
                        tfr.setProbability(chgGrp.getProbability());
                    }

                    if (chgGrp.getMax_temp_c() != null) {
                        tfr.setMax_temp_c(chgGrp.getMax_temp_c());
                    }

                    if (chgGrp.getMin_temp_c() != null) {
                        tfr.setMin_temp_c(chgGrp.getMin_temp_c());
                    }

                    if ((chgGrp.getAltim_in_hg() != null)
                            && (chgGrp.getAltim_in_hg().trim().length() > 0)) {
                        tfr.setAltim_in_hg(Float.parseFloat(chgGrp
                                .getAltim_in_hg()));
                    } else {
                        tfr.setAltim_in_hg(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getRemarks() != null) {
                        tfr.setRemarks(chgGrp.getRemarks());
                    }

                    if (chgGrp.getSequenceId() != null) {
                        tfr.setSequenceId(chgGrp.getSequenceId());
                    }

                    VisibilityParser parser = new VisibilityParser();
                    if ((chgGrp.getVisibility_mi() != null)
                            && parser.decode(chgGrp.getVisibility_mi() + "SM")) {
                        float val = (float) parser.getPrevail_vsbySM();
                        tfr.setVisibility_mi(val);
                    } else {
                        tfr.setVisibility_mi(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if ((chgGrp.getVert_vis_ft() != null)
                            && (chgGrp.getVert_vis_ft().trim().length() > 0)) {
                        tfr.setVert_vis_ft(Float.parseFloat(chgGrp
                                .getVert_vis_ft()));
                    } else {
                        tfr.setVert_vis_ft(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if ((chgGrp.getWind_dir_degrees() != null)
                            && !chgGrp.getWind_dir_degrees().equalsIgnoreCase(
                                    "VRB")) {
                        tfr.setWind_dir_degrees(Float.parseFloat(chgGrp
                                .getWind_dir_degrees()));
                    } else {
                        tfr.setWind_dir_degrees(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getWind_gust_kt() != null) {
                        tfr.setWind_gust_kt(new Float(chgGrp.getWind_gust_kt()));
                    } else {
                        tfr.setWind_gust_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getWind_speed_kt() != null) {
                        tfr.setWind_speed_kt(new Float(chgGrp
                                .getWind_speed_kt()));
                    } else {
                        tfr.setWind_speed_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getWind_shear_dir_degrees() != null) {
                        tfr.setWind_shear_dir_degrees(new Float(chgGrp
                                .getWind_shear_dir_degrees()));
                    } else {
                        tfr.setWind_shear_dir_degrees(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getWind_shear_speed_kt() != null) {
                        tfr.setWind_shear_speed_kt(new Float(chgGrp
                                .getWind_shear_speed_kt()));
                    } else {
                        tfr.setWind_shear_speed_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getWind_shear_hgt_ft_agl() != null) {
                        tfr.setWind_shear_hgt_ft_agl(new Float(chgGrp
                                .getWind_shear_hgt_ft_agl()));
                    } else {
                        tfr.setWind_shear_hgt_ft_agl(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if ((chgGrp.getProbable_visibility_mi() != null)
                            && parser.decode(chgGrp.getProbable_visibility_mi()
                                    + "SM")) {
                        float val = (float) parser.getPrevail_vsbySM();
                        tfr.setProbable_visibility_mi(val);
                    } else {
                        tfr.setProbable_visibility_mi(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if ((chgGrp.getProbable_vert_vis_ft() != null)
                            && (chgGrp.getProbable_vert_vis_ft().trim()
                                    .length() > 0)) {
                        tfr.setProbable_vert_vis_ft(Float.parseFloat(chgGrp
                                .getProbable_vert_vis_ft()));
                    } else {
                        tfr.setProbable_vert_vis_ft(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if ((chgGrp.getProbable_wind_dir_degrees() != null)
                            && !chgGrp.getProbable_wind_dir_degrees()
                                    .equalsIgnoreCase("VRB")) {
                        tfr.setProbable_wind_dir_degrees(Float
                                .parseFloat(chgGrp
                                        .getProbable_wind_dir_degrees()));
                    } else {
                        tfr.setProbable_wind_dir_degrees(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getProbable_wind_gust_kt() != null) {
                        tfr.setProbable_wind_gust_kt(new Float(chgGrp
                                .getProbable_wind_gust_kt()));
                    } else {
                        tfr.setProbable_wind_gust_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getProbable_wind_speed_kt() != null) {
                        tfr.setProbable_wind_speed_kt(new Float(chgGrp
                                .getProbable_wind_speed_kt()));
                    } else {
                        tfr.setProbable_wind_speed_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getProbable_wind_shear_dir_degrees() != null) {
                        tfr.setProbable_wind_speed_kt(new Float(chgGrp
                                .getProbable_wind_shear_dir_degrees()));
                    } else {
                        tfr.setWind_shear_dir_degrees(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getProbable_wind_shear_speed_kt() != null) {
                        tfr.setWind_shear_speed_kt(new Float(chgGrp
                                .getProbable_wind_shear_speed_kt()));
                    } else {
                        tfr.setWind_shear_speed_kt(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getProbable_wind_shear_hgt_ft_agl() != null) {
                        tfr.setProbable_wind_shear_hgt_ft_agl(new Float(chgGrp
                                .getProbable_wind_shear_hgt_ft_agl()));
                    } else {
                        tfr.setProbable_wind_shear_hgt_ft_agl(IDecoderConstantsN.NEGATIVE_FLOAT_MISSING);
                    }

                    if (chgGrp.getIcing_layers() != null) {
                        tfr.setIcing_layers(chgGrp.getIcing_layers());
                    }

                    if (chgGrp.getSky_cover() != null) {
                        tfr.setSky_cover(chgGrp.getSky_cover());
                    }

                    if (chgGrp.getProbable_sky_cover() != null) {
                        tfr.setProbable_sky_cover(chgGrp
                                .getProbable_sky_cover());
                    }

                    if (chgGrp.getTemp_forecasts() != null) {
                        tfr.setTemp_forecasts(chgGrp.getTemp_forecasts());
                    }

                    if (chgGrp.getTurbulence_layers() != null) {
                        tfr.setTurbulence_layers(chgGrp.getTurbulence_layers());
                    }

                    if (chgGrp.getWeather() != null) {
                        tfr.setWeather(chgGrp.getWeather());
                    }

                    records.add(tfr);
                }

            }
        }

        List<NcTafRecord> hourlyRecords = splitToHourlyRecords(records);

        return hourlyRecords.toArray(new NcTafRecord[0]);
    }

    public static List<NcTafRecord> splitToHourlyRecords(
            List<NcTafRecord> records) {
        List<NcTafRecord> newRecords = new ArrayList<NcTafRecord>();
        int recordsSize = (records != null) ? records.size() : 0;

        if (recordsSize > 0) {

            for (int i = 0; i < recordsSize; i++) {

                NcTafRecord record = records.get(i);

                Calendar startRefTime = record.getStartDate();
                Calendar endRefTime = record.getEndDate();

                // add 1 minute so that 12:30 becomes 12:31 and hence is rounded
                // to 13:00 and not 12:00
                startRefTime.add(Calendar.MINUTE, 1);
                Date nearestHour = DateUtils.round(startRefTime.getTime(),
                        Calendar.HOUR);
                startRefTime.setTime(nearestHour);

                nearestHour = DateUtils.round(endRefTime.getTime(),
                        Calendar.HOUR);
                endRefTime.setTime(nearestHour);

                long milliseconds1 = startRefTime.getTimeInMillis();
                long milliseconds2 = endRefTime.getTimeInMillis();
                long diff = milliseconds2 - milliseconds1;
                long diffHours = diff / (60 * 60 * 1000);

                for (int hour = 0; hour < diffHours; hour++) {

                    NcTafRecord tfr = new NcTafRecord();

                    tfr.setPersistenceTime(new Date());
                    tfr.setDataTime(new DataTime(startRefTime));
                    tfr.setTafValidPeriod(record.getTafValidPeriod());
                    tfr.setTafChangePeriod(record.getTafChangePeriod());
                    tfr.setEndDate(record.getTafChangePeriod().getEndDate());
                    tfr.setStartDate(record.getTafChangePeriod().getStartDate());
                    tfr.setTransitionEndDate(record.getTafChangePeriod()
                            .getTransitionEndDate());
                    tfr.setWmoHeader(record.getWmoHeader());
                    if ((record.getTafText() != null)
                            && (record.getTafText().length() > 1024)) {
                        tfr.setTafText(record.getTafText().substring(0, 1024));
                    } else {
                        tfr.setTafText(record.getTafText());
                    }
                    tfr.setTafText(record.getTafText());
                    tfr.setReportType(record.getReportType());
                    tfr.setStationId(record.getStationId());
                    tfr.setCorIndicator(record.getCorIndicator());
                    tfr.setAmdIndicator(record.getAmdIndicator());
                    tfr.setIssue_time(record.getIssue_time());
                    tfr.setIssue_timeString(record.getIssue_timeString());
                    tfr.setBulletin_time(record.getBulletin_time());
                    tfr.setLocation(record.getLocation());
                    tfr.setChange_indicator(record.getChange_indicator());
                    if ((record.getChangeGroup() != null)
                            && (record.getChangeGroup().length() > 128)) {
                        tfr.setChangeGroup(record.getChangeGroup().substring(0,
                                128));
                    } else {
                        tfr.setChangeGroup(record.getChangeGroup());
                    }
                    tfr.setChangeGroup(record.getChangeGroup());
                    tfr.setProbability(record.getProbability());
                    tfr.setMax_temp_c(record.getMax_temp_c());
                    tfr.setMin_temp_c(record.getMin_temp_c());
                    tfr.setAltim_in_hg(record.getAltim_in_hg());
                    tfr.setRemarks(record.getRemarks());
                    tfr.setSequenceId(record.getSequenceId());
                    tfr.setVisibility_mi(record.getVisibility_mi());
                    tfr.setVert_vis_ft(record.getVert_vis_ft());
                    tfr.setWind_dir_degrees(record.getWind_dir_degrees());
                    tfr.setWind_gust_kt(record.getWind_gust_kt());
                    tfr.setWind_speed_kt(record.getWind_speed_kt());
                    tfr.setWind_shear_dir_degrees(record
                            .getWind_shear_dir_degrees());
                    tfr.setWind_shear_speed_kt(record.getWind_shear_speed_kt());
                    tfr.setWind_shear_hgt_ft_agl(record
                            .getWind_shear_hgt_ft_agl());
                    tfr.setProbable_visibility_mi(record
                            .getProbable_visibility_mi());
                    tfr.setProbable_vert_vis_ft(record
                            .getProbable_vert_vis_ft());
                    tfr.setProbable_wind_dir_degrees(record
                            .getProbable_wind_dir_degrees());
                    tfr.setProbable_wind_gust_kt(record
                            .getProbable_wind_gust_kt());
                    tfr.setProbable_wind_speed_kt(record
                            .getProbable_wind_speed_kt());
                    tfr.setWind_shear_dir_degrees(record
                            .getProbable_wind_shear_dir_degrees());
                    tfr.setWind_shear_speed_kt(record
                            .getProbable_wind_shear_speed_kt());
                    tfr.setProbable_wind_shear_hgt_ft_agl(record
                            .getProbable_wind_shear_hgt_ft_agl());
                    tfr.setIcing_layers(record.getIcing_layers());
                    tfr.setSky_cover(record.getSky_cover());
                    tfr.setProbable_sky_cover(record.getProbable_sky_cover());
                    tfr.setTemp_forecasts(record.getTemp_forecasts());
                    tfr.setTurbulence_layers(record.getTurbulence_layers());
                    tfr.setWeather(record.getWeather());
                    tfr.setProbable_weather(record.getWeather());
                    newRecords.add(tfr);
                    startRefTime.add(Calendar.HOUR_OF_DAY, 1);
                }
            }
        }

        return newRecords;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    /**
     * 
     * @param args
     */
    public static final void main(String[] args) {

        Set<NcTafSkyCover> skyCovers = new HashSet<NcTafSkyCover>();

        NcTafSkyCover cover = new NcTafSkyCover();
        cover.setGenus("");
        cover.setType("OVC");
        cover.setHeight(1800);
        skyCovers.add(cover);

        cover = new NcTafSkyCover();
        cover.setGenus("");
        cover.setType("BKN");
        cover.setHeight(1000);
        skyCovers.add(cover);

        cover = new NcTafSkyCover();
        cover.setGenus("");
        cover.setType("SCT");
        cover.setHeight(800);
        skyCovers.add(cover);

        for (NcTafSkyCover s : skyCovers) {
            System.out.println(s);
        }

        System.out.println(getCeiling(skyCovers, 8));

        System.out.println(getCeiling(skyCovers, -1));

        Calendar startRefTime = Calendar.getInstance();
        startRefTime.set(2011, 11, 04, 12, 30);
        startRefTime.add(Calendar.MINUTE, 1);
        Calendar endRefTime = Calendar.getInstance();
        endRefTime.set(2011, 11, 04, 14, 30);

        Date nearestHour = DateUtils.round(startRefTime.getTime(),
                Calendar.HOUR);
        startRefTime.setTime(nearestHour);

        nearestHour = DateUtils.round(endRefTime.getTime(), Calendar.HOUR);
        endRefTime.setTime(nearestHour);

        long milliseconds1 = startRefTime.getTimeInMillis();
        long milliseconds2 = endRefTime.getTimeInMillis();
        long diff = milliseconds2 - milliseconds1;
        long diffHours = diff / (60 * 60 * 1000);

        System.out.println(" diffHours = " + diffHours);

        for (int hour = 0; hour < diffHours; hour++) {
            System.out.println(" startRefTime = "
                    + startRefTime.getTime().toString());
            startRefTime.add(Calendar.HOUR_OF_DAY, 1);
        }
    }

    @Override
    public String getPluginName() {
        return "nctaf";
    }
}
