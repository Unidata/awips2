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
package com.raytheon.edex.plugin.ldadmanual.dao;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Record implementation for ldadmanual plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 30, 2009           vkorolev    Initial creation
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from
 *                                    PluginDataObject.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * Jul 23, 2014  3410     bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ldad_manualseq")
@Table(name = "ldad_manual", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ldad_manual", indexes = { @Index(name = "ldad_manual_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ManualLdadRecord extends PluginDataObject implements
        ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    // Time of the observation.
    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar observationTime;

    // Location
    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location; // latitude, longitude, elevation,
                                         // stationId

    // Provider of data
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String providerId; // * "050183" Data Provider

    // Name of station
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String stationName; // * "ALLENSPARK CO-OP"

    // Home WFO Id for the LDAD data
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String homeWFO; // * "SJU"

    // Type of report
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short reportType; // short
                              // 1 - Regular
                              // 2 - Corrected

    // Units Code
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short unitsCode; // short
                             // 8 - English
                             // 9 - Standard International

    // Current 24 hr precipitation total
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code10; // inches

    // Incremental precip since previous 7 a.m.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code11; // inches

    // Precip Criteria report from flash flood observer
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code12; // inches

    // 4 hr precip total at previous 7 a.m. criteria report
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code13; // inches

    // 24 hr precipitation total at 7 a.m. 2 days ago
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code14; // inches

    // Storm total precipitation
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code15; // inches

    // Weekly total precipitation
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code16; // inches

    // Monthly total precipitation
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code17; // inches

    // Off-Time precipitation report
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code18; // inches

    // Short intense precipitation durations
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code19; // hours

    // Precipitation type
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code20; // short
                          // 0 "Ice Prism"
                          // 1 "Rain"
                          // 2 "Freezing Rain"
                          // 3 "Drizzle"
                          // 4 "Freezing Drizzle"
                          // 5 "Snow"
                          // 6 "Snow Pellets"
                          // 7 "Snow Grains"
                          // 8 "Ice Pellets"
                          // 9 "Hail"

    // Current air temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code21; // degrees F

    // Daily maximum air temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code22; // degrees F

    // Daily minimum air temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code23; // degrees F

    // Average weekly maximum air temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code24; // degrees F

    // Average weekly minimum air temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code25; // degrees F

    // Current water temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code26; // degrees F

    // Daily maximum soil temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code27; // degrees F

    // Daily minimum soil temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code28; // degrees F

    // Wet Bulb temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code29; // degrees F

    // Number of hours temperature is below 25 degrees F
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code30; // Hours

    // Number of hours temperature is below 32 degrees F
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code31; // Hours

    // Dew point temperature
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code32; // degrees F

    // River Stage at specified ob time
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code33; // feet

    // River Stage at previous 1 a.m.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code34; // feet

    // River Stage at previous 7 p.m.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code35; // feet

    // River Stage at previous 1 p.m.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code36; // feet

    // River Stage at previous 7 a.m.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code37; // feet

    // River Stage at 7 a.m. 2 days ago
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code38; // feet

    // River Stage at observed crest time
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String code39; // char
                           // Chars0_1 "month"
                           // Chars2_3 "day"
                           // Chars4_5 "hour"
                           // Chars6_7 "minute"

    // River Stage at observed crest
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code40; // feet

    // River Stage Trend
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code41; // short
                          // 0" "River below flood stage - Stationary"
                          // 1" "River below flood stage - Rising"
                          // 3" "River below flood stage - Falling"
                          // 4" "River above flood stage - Stationary"
                          // 5" "River above flood stage - Rising"
                          // 6" "River above flood stage - Falling"

    // private Double code42;--??????????????????????????????????????

    // River Discharge instantaneous measured Kilo Cubic Feet Per Second
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code43; // kcfs

    // River Discharge mean daily measured
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code44; // kcfs

    // River Discharge instantaneous computed
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code45; // kcfs

    // River Discharge mean daily computed
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code46; // kcfs

    // River Discharge instantaneous from rating
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code47; // kcfs

    // River Discharge mean daily from rating
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code48; // kcfs

    // River Discharge peak
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code49; // kcfs

    // River Discharge canal diversion
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code50; // kcfs

    // private Float code51; //??????????????????????????????????

    // Reservoir pool elevation at specified ob time
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code52; // feet

    // Reservoir pool elevation at previous 6Z
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code53; // feet

    // Reservoir pool forecast, Day 1
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code54; // feet

    // Reservoir pool forecast, Day 2
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code55; // feet

    // Reservoir pool forecast, Day 3
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code56; // feet

    // Reservoir tailwater elevation
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code57; // feet

    // Reservoir inflow, instantanious
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code58; // kcfs

    // Reservoir inflow, mean daily
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code59; // kcfs

    // Reservoir outflow, instantanious
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code60; // kcfs

    // Reservoir outflow, mean daily
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code61; // kcfs

    // Reservoir outflow forecast, mean daily, Day 1
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code62; // kcfs

    // Reservoir outflow forecast, mean daily, Day 2
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code63; // kcfs

    // Reservoir outflow forecast, mean daily, Day 3
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code64; // kcfs

    // Reservoir storage at specified ob time KAF=thousand acre feet
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code65; // kaf

    // Reservoir evaporation, 24 hour total, computed
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code66; // inches

    // Snow cover, areal extent
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code67; // percent

    // Snow depth, total on ground
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code68; // inches

    // Snow depth, new snow
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code69; // inches

    // Snow density
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code70; // inches/inches

    // Snow, Water equivalent, total of snow and ice on ground
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code71; // inches

    // Snow report ref LDAD Table?
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String code72;

    // FirstDigit0 "Snow structure - no report"
    // FirstDigit1 "Snow structure - losely packed"
    // FirstDigit2 "Snow structure - densely packed"
    // SecondDigit0 "Base of snowcover - no report"
    // SecondDigit1 "Base of snowcover - wet snow"
    // SecondDigit2 "Base of snowcover - dry snow"
    // SecondDigit3 "Base of snowcover - ice layer"
    // ThirdDigit0 "Surface of snowcover - no report"
    // ThirdDigit1 "Surface of snowcover - snow crust"
    // ThirdDigit2 "Surface of snowcover - loose"
    // ThirdDigit3 "Surface of snowcover - ice"
    // FourthDigit0 "Area description - no report"
    // FourthDigit1 "Area description - uniform"
    // FourthDigit2 "Area description - some drifts"
    // FourthDigit3 "Area description - drifts"

    // Ice cover, areal extent
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code73; // percent

    // Ice extent from reporting area, up to downstream
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code74; // miles

    // Ice open water, extent from reporting area, up or downstream
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code75; // miles

    // Ice thickness
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code76; // inches

    // Ice report ref LDAD Table
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String code77;

    // FirstDigit0 "Ice structure - no report"
    // FirstDigit1 "Ice structure - breaking ice"
    // FirstDigit2 "Ice structure - floating (running)"
    // FirstDigit3 "Ice structure - hanging"
    // FirstDigit4 "Ice structure - honeycomb"
    // FirstDigit5 "Ice structure - layered"
    // FirstDigit6 "Ice structure - rotten"
    // FirstDigit7 "Ice structure - stationary"
    // FirstDigit8 "Ice structure - stopped"
    // FirstDigit9 "Ice structure - slush"
    // SecondDigit0 "Ice type - no report"
    // SecondDigit1 "Ice type - anchor (also bottom ice)"
    // SecondDigit2 "Ice type - cake"
    // SecondDigit3 "Ice type - clear"
    // SecondDigit4 "Ice type - frazzle"
    // SecondDigit5 "Ice type - ice gorge (also jammed ice)"
    // SecondDigit6 "Ice type - locally formed"
    // SecondDigit7 "Ice type - sheet ice"
    // SecondDigit8 "Ice type - sheet ice (also on bridges)"
    // SecondDigit9 "Ice type - shore ice"
    // ThirdDigit0 "Ice cover - no ice"
    // ThirdDigit1 "Ice cover - 1/10 cover"
    // ThirdDigit2 "Ice cover - 2/10 cover"
    // ThirdDigit3 "Ice cover - 3/10 cover"
    // ThirdDigit4 "Ice cover - 4/10 cover"
    // ThirdDigit5 "Ice cover - 5/10 cover"
    // ThirdDigit6 "Ice cover - 6/10 cover"
    // ThirdDigit7 "Ice cover - 7/10 cover"
    // ThirdDigit8 "Ice cover - 8/10 - 9/10 cover"
    // ThirdDigit9 "Ice cover - fully covered"

    // Depth of frost
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code78; // inches

    // Depth of frost thawed
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code79; // inches

    // Frost structure report
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code80; // short
                          // 0 "Frost intensity - no frost"
                          // 1 "Frost intensity - light frost"
                          // 2 "Frost intensity - moderate frost"
                          // 3 "Frost intensity - heavy frost"

    // Surface frost intensity
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code81; // short
                          // 0 "Frost intensity - no frost"
                          // 1 "Frost intensity - light frost"
                          // 2 "Frost intensity - moderate frost"
                          // 3 "Frost intensity - heavy frost"

    // State of ground
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code82; // short
                          // 0 Surface of ground dry"
                          // 1 Surface of ground moist"
                          // 2 Surface of ground wet"
                          // 3 Flooded"
                          // 5 Glaze on ground"
                          // 6 Loose dry dust or sand not covering ground
                          // completely"
                          // 7 Thin cover of loose dry dust or sand covering
                          // ground completely"
                          // 8 Moderate or thick cover of loose dry dust or sand
                          // covering ground completely"
                          // 9 Ground extra dry with cracks"
                          // 10 Ground predominately covered with ice"
                          // 11 Compact or wet snow (with or without ice)
                          // covering less than one half of the ground"
                          // 12 Compact or wet snow (with or without ice)
                          // covering less than one half of the ground but not
                          // completely covered"
                          // 13 Even layer of comapct or wet snow covering
                          // ground completely"
                          // 14 Uneven layer of comapct or wet snow covering
                          // ground completely"
                          // 15 Loose dry snow covering less than one half of
                          // the ground"
                          // 16 Loose dry snow covering at least one half of the
                          // ground (but not completely)"
                          // 17 Even layers of loose dry snow covering ground
                          // completely"
                          // 18 Uneven layers of loose dry snow covering ground
                          // completely"
                          // 19 Snow covering ground completely; Deep drifts"
                          // 20 Sleet or hail covering ground completely"

    // Soil moisture
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code83; // inches

    // Present weather
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code84; // short
                          // 5 "Haze"
                          // 7 "Dust or sand raised by wind"
                          // 8 "Well developed dust whirls or sand whirls"
                          // 13 "Lightning (no thunder heard)"
                          // 17 "Thunderstorm, but no precip at ob time"
                          // 19
                          // "Funnel cloud at or within sight of station during previous hour or at ob time"
                          // 41 "Fog or ice fog in patches"
                          // 42 "Fog or ice fog (sky visible)"
                          // 43 "Fog or ice fog (sky not visible)"
                          // 51 "Drizzle, not freezing, slight at ob time"
                          // 53 "Drizzle, not freezing, moderate at ob time"
                          // 55 "Drizzle, not freezing, heavy at ob time"
                          // 56 "Drizzle, freezing, light"
                          // 57 "Drizzle, freezing, moderate or heavy"
                          // 61
                          // "Rain, not freezing, continuous, slight at ob time"
                          // 63
                          // "Rain, not freezing, continuous, moderate at ob time"
                          // 65
                          // "Rain, not freezing, continuous, heavy at ob time"
                          // 66 "Rain, freezing, light"
                          // 67 "Rain, freezing, moderate or heavy"
                          // 68 "Rain or drizzle and snow, light"
                          // 69 "Rain or drizzle and snow, moderate or heavy"
                          // 71 "Snow, continuous, light"
                          // 73 "Snow, continuous, moderate"
                          // 75 "Snow, continuous, heavy"
                          // 79 "Ice Pellets or sleet"
                          // 80 "Rainshower, light"
                          // 81 "Rainshower, moderate or heavy"
                          // 82 "Rainshower, violent"
                          // 83 "Rain and snowshowers, light"
                          // 84 "Rain and snowshowers, moderate or heavy"
                          // 85 "Snowshowers, light"
                          // 86 "Snowshowers, moderate or heavy"
                          // 89
                          // "Showers of hail, with or without rain or rain and snow mixed at ob time"
                          // 95
                          // "Thunderstorm, slight or moderate, without hail, but with rain and/or snow"
                          // 96 "Thunderstorm, slight or moderate, with hail"
                          // 97
                          // "Thunderstorm, heavy, without hail, but with rain and/or snow"
                          // 98 "Thunderstorm, with dust or sand storm"
                          // 99 "Thunderstorm, heavy with hail"

    // Past 6 hour weather
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code85; // short
                          // 0
                          // "Cloud covering one half or less of the sky throughout the appropriate period"
                          // 1
                          // "Cloud covering more than one half the sky throughout the appropriate period and covering one half or less during part of the period"
                          // 2
                          // "Cloud covering more than one half the sky throughout the appropriate period"
                          // 3 "Sandstorm, duststrom, or blowing snow"
                          // 4 "Fog or ice fog or thick haze"
                          // 5 "Drizzle"
                          // 6 "Rain"
                          // 7 "Snow, or rain and snow mixed"
                          // 8 "Showers"
                          // 9 "Thunderstorm(s) with or without precip"

    // Relative humidity
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code86; // percent

    // Evaporation, measured, Class A pan or other
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code87; // inches

    // Wind speed
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code88; // miles per hour

    // Wind direction
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code89; // tens of degrees

    // Sunshine, hours per day
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code90; // hours

    // Solar energy, accumulated incoming
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code91; // ly

    // short Dew intensity
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Short code92;

    // 0 "No dew"
    // 1 "Light dew"
    // 2 "Moderate dew"
    // 3 "Heavy dew"

    // Leaf wetness
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code93; // hours ????????????????????

    // Water pan temperature maximum
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code94; // degrees F

    // Water pan temperature minimum
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code95; // degrees F

    // 24 hour wind flow
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Float code96; // miles

    // Raw observation text - ROSA raw message
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String rawMessage; // rawMessage

    /**
     * 
     */
    public ManualLdadRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public ManualLdadRecord(String uri) {
        super(uri);
    }

    /**
     * @return the code12
     */
    public Float getCode12() {
        return code12;
    }

    /**
     * @param code12
     *            the code12 to set
     */
    public void setCode12(Float code12) {
        this.code12 = code12;
    }

    /**
     * @return the code13
     */
    public Float getCode13() {
        return code13;
    }

    /**
     * @param code13
     *            the code13 to set
     */
    public void setCode13(Float code13) {
        this.code13 = code13;
    }

    /**
     * @return the code14
     */
    public Float getCode14() {
        return code14;
    }

    /**
     * @param code14
     *            the code14 to set
     */
    public void setCode14(Float code14) {
        this.code14 = code14;
    }

    /**
     * @return the code15
     */
    public Float getCode15() {
        return code15;
    }

    /**
     * @param code15
     *            the code15 to set
     */
    public void setCode15(Float code15) {
        this.code15 = code15;
    }

    /**
     * @return the code16
     */
    public Float getCode16() {
        return code16;
    }

    /**
     * @param code16
     *            the code16 to set
     */
    public void setCode16(Float code16) {
        this.code16 = code16;
    }

    /**
     * @return the code17
     */
    public Float getCode17() {
        return code17;
    }

    /**
     * @param code17
     *            the code17 to set
     */
    public void setCode17(Float code17) {
        this.code17 = code17;
    }

    /**
     * @return the code18
     */
    public Float getCode18() {
        return code18;
    }

    /**
     * @param code18
     *            the code18 to set
     */
    public void setCode18(Float code18) {
        this.code18 = code18;
    }

    /**
     * @return the code19
     */
    public Float getCode19() {
        return code19;
    }

    /**
     * @param code19
     *            the code19 to set
     */
    public void setCode19(Float code19) {
        this.code19 = code19;
    }

    /**
     * @return the code20
     */
    public Short getCode20() {
        return code20;
    }

    /**
     * @param code20
     *            the code20 to set
     */
    public void setCode20(Short code20) {
        this.code20 = code20;
    }

    /**
     * @return the code21
     */
    public Float getCode21() {
        return code21;
    }

    /**
     * @param code21
     *            the code21 to set
     */
    public void setCode21(Float code21) {
        this.code21 = code21;
    }

    /**
     * @return the code22
     */
    public Float getCode22() {
        return code22;
    }

    /**
     * @param code22
     *            the code22 to set
     */
    public void setCode22(Float code22) {
        this.code22 = code22;
    }

    /**
     * @return the code23
     */
    public Float getCode23() {
        return code23;
    }

    /**
     * @param code23
     *            the code23 to set
     */
    public void setCode23(Float code23) {
        this.code23 = code23;
    }

    /**
     * @return the code24
     */
    public Float getCode24() {
        return code24;
    }

    /**
     * @param code24
     *            the code24 to set
     */
    public void setCode24(Float code24) {
        this.code24 = code24;
    }

    /**
     * @return the code25
     */
    public Float getCode25() {
        return code25;
    }

    /**
     * @param code25
     *            the code25 to set
     */
    public void setCode25(Float code25) {
        this.code25 = code25;
    }

    /**
     * @return the code26
     */
    public Float getCode26() {
        return code26;
    }

    /**
     * @param code26
     *            the code26 to set
     */
    public void setCode26(Float code26) {
        this.code26 = code26;
    }

    /**
     * @return the code27
     */
    public Float getCode27() {
        return code27;
    }

    /**
     * @param code27
     *            the code27 to set
     */
    public void setCode27(Float code27) {
        this.code27 = code27;
    }

    /**
     * @return the code28
     */
    public Float getCode28() {
        return code28;
    }

    /**
     * @param code28
     *            the code28 to set
     */
    public void setCode28(Float code28) {
        this.code28 = code28;
    }

    /**
     * @return the code29
     */
    public Float getCode29() {
        return code29;
    }

    /**
     * @param code29
     *            the code29 to set
     */
    public void setCode29(Float code29) {
        this.code29 = code29;
    }

    /**
     * @return the code30
     */
    public Float getCode30() {
        return code30;
    }

    /**
     * @param code30
     *            the code30 to set
     */
    public void setCode30(Float code30) {
        this.code30 = code30;
    }

    /**
     * @return the code31
     */
    public Float getCode31() {
        return code31;
    }

    /**
     * @param code31
     *            the code31 to set
     */
    public void setCode31(Float code31) {
        this.code31 = code31;
    }

    /**
     * @return the code32
     */
    public Float getCode32() {
        return code32;
    }

    /**
     * @param code32
     *            the code32 to set
     */
    public void setCode32(Float code32) {
        this.code32 = code32;
    }

    /**
     * @return the code33
     */
    public Float getCode33() {
        return code33;
    }

    /**
     * @param code33
     *            the code33 to set
     */
    public void setCode33(Float code33) {
        this.code33 = code33;
    }

    /**
     * @return the code34
     */
    public Float getCode34() {
        return code34;
    }

    /**
     * @param code34
     *            the code34 to set
     */
    public void setCode34(Float code34) {
        this.code34 = code34;
    }

    /**
     * @return the code35
     */
    public Float getCode35() {
        return code35;
    }

    /**
     * @param code35
     *            the code35 to set
     */
    public void setCode35(Float code35) {
        this.code35 = code35;
    }

    /**
     * @return the code36
     */
    public Float getCode36() {
        return code36;
    }

    /**
     * @param code36
     *            the code36 to set
     */
    public void setCode36(Float code36) {
        this.code36 = code36;
    }

    /**
     * @return the code37
     */
    public Float getCode37() {
        return code37;
    }

    /**
     * @param code37
     *            the code37 to set
     */
    public void setCode37(Float code37) {
        this.code37 = code37;
    }

    /**
     * @return the code38
     */
    public Float getCode38() {
        return code38;
    }

    /**
     * @param code38
     *            the code38 to set
     */
    public void setCode38(Float code38) {
        this.code38 = code38;
    }

    /**
     * @return the code39
     */
    public String getCode39() {
        return code39;
    }

    /**
     * @param code39
     *            the code39 to set
     */
    public void setCode39(String code39) {
        this.code39 = code39;
    }

    /**
     * @return the code40
     */
    public Float getCode40() {
        return code40;
    }

    /**
     * @param code40
     *            the code40 to set
     */
    public void setCode40(Float code40) {
        this.code40 = code40;
    }

    /**
     * @return the code41
     */
    public Short getCode41() {
        return code41;
    }

    /**
     * @param code41
     *            the code41 to set
     */
    public void setCode41(Short code41) {
        this.code41 = code41;
    }

    /**
     * @return the code43
     */
    public Float getCode43() {
        return code43;
    }

    /**
     * @param code43
     *            the code43 to set
     */
    public void setCode43(Float code43) {
        this.code43 = code43;
    }

    /**
     * @return the code44
     */
    public Float getCode44() {
        return code44;
    }

    /**
     * @param code44
     *            the code44 to set
     */
    public void setCode44(Float code44) {
        this.code44 = code44;
    }

    /**
     * @return the code45
     */
    public Float getCode45() {
        return code45;
    }

    /**
     * @param code45
     *            the code45 to set
     */
    public void setCode45(Float code45) {
        this.code45 = code45;
    }

    /**
     * @return the code46
     */
    public Float getCode46() {
        return code46;
    }

    /**
     * @param code46
     *            the code46 to set
     */
    public void setCode46(Float code46) {
        this.code46 = code46;
    }

    /**
     * @return the code47
     */
    public Float getCode47() {
        return code47;
    }

    /**
     * @param code47
     *            the code47 to set
     */
    public void setCode47(Float code47) {
        this.code47 = code47;
    }

    /**
     * @return the code48
     */
    public Float getCode48() {
        return code48;
    }

    /**
     * @param code48
     *            the code48 to set
     */
    public void setCode48(Float code48) {
        this.code48 = code48;
    }

    /**
     * @return the code49
     */
    public Float getCode49() {
        return code49;
    }

    /**
     * @param code49
     *            the code49 to set
     */
    public void setCode49(Float code49) {
        this.code49 = code49;
    }

    /**
     * @return the code50
     */
    public Float getCode50() {
        return code50;
    }

    /**
     * @param code50
     *            the code50 to set
     */
    public void setCode50(Float code50) {
        this.code50 = code50;
    }

    /**
     * @return the code52
     */
    public Float getCode52() {
        return code52;
    }

    /**
     * @param code52
     *            the code52 to set
     */
    public void setCode52(Float code52) {
        this.code52 = code52;
    }

    /**
     * @return the code53
     */
    public Float getCode53() {
        return code53;
    }

    /**
     * @param code53
     *            the code53 to set
     */
    public void setCode53(Float code53) {
        this.code53 = code53;
    }

    /**
     * @return the code54
     */
    public Float getCode54() {
        return code54;
    }

    /**
     * @param code54
     *            the code54 to set
     */
    public void setCode54(Float code54) {
        this.code54 = code54;
    }

    /**
     * @return the code55
     */
    public Float getCode55() {
        return code55;
    }

    /**
     * @param code55
     *            the code55 to set
     */
    public void setCode55(Float code55) {
        this.code55 = code55;
    }

    /**
     * @return the code56
     */
    public Float getCode56() {
        return code56;
    }

    /**
     * @param code56
     *            the code56 to set
     */
    public void setCode56(Float code56) {
        this.code56 = code56;
    }

    /**
     * @return the code57
     */
    public Float getCode57() {
        return code57;
    }

    /**
     * @param code57
     *            the code57 to set
     */
    public void setCode57(Float code57) {
        this.code57 = code57;
    }

    /**
     * @return the code58
     */
    public Float getCode58() {
        return code58;
    }

    /**
     * @param code58
     *            the code58 to set
     */
    public void setCode58(Float code58) {
        this.code58 = code58;
    }

    /**
     * @return the code59
     */
    public Float getCode59() {
        return code59;
    }

    /**
     * @param code59
     *            the code59 to set
     */
    public void setCode59(Float code59) {
        this.code59 = code59;
    }

    /**
     * @return the code60
     */
    public Float getCode60() {
        return code60;
    }

    /**
     * @param code60
     *            the code60 to set
     */
    public void setCode60(Float code60) {
        this.code60 = code60;
    }

    /**
     * @return the code61
     */
    public Float getCode61() {
        return code61;
    }

    /**
     * @param code61
     *            the code61 to set
     */
    public void setCode61(Float code61) {
        this.code61 = code61;
    }

    /**
     * @return the code62
     */
    public Float getCode62() {
        return code62;
    }

    /**
     * @param code62
     *            the code62 to set
     */
    public void setCode62(Float code62) {
        this.code62 = code62;
    }

    /**
     * @return the code63
     */
    public Float getCode63() {
        return code63;
    }

    /**
     * @param code63
     *            the code63 to set
     */
    public void setCode63(Float code63) {
        this.code63 = code63;
    }

    /**
     * @return the code64
     */
    public Float getCode64() {
        return code64;
    }

    /**
     * @param code64
     *            the code64 to set
     */
    public void setCode64(Float code64) {
        this.code64 = code64;
    }

    /**
     * @return the code65
     */
    public Float getCode65() {
        return code65;
    }

    /**
     * @param code65
     *            the code65 to set
     */
    public void setCode65(Float code65) {
        this.code65 = code65;
    }

    /**
     * @return the code66
     */
    public Float getCode66() {
        return code66;
    }

    /**
     * @param code66
     *            the code66 to set
     */
    public void setCode66(Float code66) {
        this.code66 = code66;
    }

    /**
     * @return the code67
     */
    public Float getCode67() {
        return code67;
    }

    /**
     * @param code67
     *            the code67 to set
     */
    public void setCode67(Float code67) {
        this.code67 = code67;
    }

    /**
     * @return the code68
     */
    public Float getCode68() {
        return code68;
    }

    /**
     * @param code68
     *            the code68 to set
     */
    public void setCode68(Float code68) {
        this.code68 = code68;
    }

    /**
     * @return the code69
     */
    public Float getCode69() {
        return code69;
    }

    /**
     * @param code69
     *            the code69 to set
     */
    public void setCode69(Float code69) {
        this.code69 = code69;
    }

    /**
     * @return the code70
     */
    public Float getCode70() {
        return code70;
    }

    /**
     * @param code70
     *            the code70 to set
     */
    public void setCode70(Float code70) {
        this.code70 = code70;
    }

    /**
     * @return the code71
     */
    public Float getCode71() {
        return code71;
    }

    /**
     * @param code71
     *            the code71 to set
     */
    public void setCode71(Float code71) {
        this.code71 = code71;
    }

    /**
     * @return the code72
     */
    public String getCode72() {
        return code72;
    }

    /**
     * @param code72
     *            the code72 to set
     */
    public void setCode72(String code72) {
        this.code72 = code72;
    }

    /**
     * @return the code73
     */
    public Float getCode73() {
        return code73;
    }

    /**
     * @param code73
     *            the code73 to set
     */
    public void setCode73(Float code73) {
        this.code73 = code73;
    }

    /**
     * @return the code74
     */
    public Float getCode74() {
        return code74;
    }

    /**
     * @param code74
     *            the code74 to set
     */
    public void setCode74(Float code74) {
        this.code74 = code74;
    }

    /**
     * @return the code75
     */
    public Float getCode75() {
        return code75;
    }

    /**
     * @param code75
     *            the code75 to set
     */
    public void setCode75(Float code75) {
        this.code75 = code75;
    }

    /**
     * @return the code76
     */
    public Float getCode76() {
        return code76;
    }

    /**
     * @param code76
     *            the code76 to set
     */
    public void setCode76(Float code76) {
        this.code76 = code76;
    }

    /**
     * @return the code77
     */
    public String getCode77() {
        return code77;
    }

    /**
     * @param code77
     *            the code77 to set
     */
    public void setCode77(String code77) {
        this.code77 = code77;
    }

    /**
     * @return the code78
     */
    public Float getCode78() {
        return code78;
    }

    /**
     * @param code78
     *            the code78 to set
     */
    public void setCode78(Float code78) {
        this.code78 = code78;
    }

    /**
     * @return the code79
     */
    public Float getCode79() {
        return code79;
    }

    /**
     * @param code79
     *            the code79 to set
     */
    public void setCode79(Float code79) {
        this.code79 = code79;
    }

    /**
     * @return the code80
     */
    public Short getCode80() {
        return code80;
    }

    /**
     * @param code80
     *            the code80 to set
     */
    public void setCode80(Short code80) {
        this.code80 = code80;
    }

    /**
     * @return the code81
     */
    public Short getCode81() {
        return code81;
    }

    /**
     * @param code81
     *            the code81 to set
     */
    public void setCode81(Short code81) {
        this.code81 = code81;
    }

    /**
     * @return the code82
     */
    public Short getCode82() {
        return code82;
    }

    /**
     * @param code82
     *            the code82 to set
     */
    public void setCode82(Short code82) {
        this.code82 = code82;
    }

    /**
     * @return the code83
     */
    public Float getCode83() {
        return code83;
    }

    /**
     * @param code83
     *            the code83 to set
     */
    public void setCode83(Float code83) {
        this.code83 = code83;
    }

    /**
     * @return the code84
     */
    public Short getCode84() {
        return code84;
    }

    /**
     * @param code84
     *            the code84 to set
     */
    public void setCode84(Short code84) {
        this.code84 = code84;
    }

    /**
     * @return the code85
     */
    public Short getCode85() {
        return code85;
    }

    /**
     * @param code85
     *            the code85 to set
     */
    public void setCode85(Short code85) {
        this.code85 = code85;
    }

    /**
     * @return the code86
     */
    public Float getCode86() {
        return code86;
    }

    /**
     * @param code86
     *            the code86 to set
     */
    public void setCode86(Float code86) {
        this.code86 = code86;
    }

    /**
     * @return the code87
     */
    public Float getCode87() {
        return code87;
    }

    /**
     * @param code87
     *            the code87 to set
     */
    public void setCode87(Float code87) {
        this.code87 = code87;
    }

    /**
     * @return the code88
     */
    public Float getCode88() {
        return code88;
    }

    /**
     * @param code88
     *            the code88 to set
     */
    public void setCode88(Float code88) {
        this.code88 = code88;
    }

    /**
     * @return the code89
     */
    public Float getCode89() {
        return code89;
    }

    /**
     * @param code89
     *            the code89 to set
     */
    public void setCode89(Float code89) {
        this.code89 = code89;
    }

    /**
     * @return the code90
     */
    public Float getCode90() {
        return code90;
    }

    /**
     * @param code90
     *            the code90 to set
     */
    public void setCode90(Float code90) {
        this.code90 = code90;
    }

    /**
     * @return the code91
     */
    public Float getCode91() {
        return code91;
    }

    /**
     * @param code91
     *            the code91 to set
     */
    public void setCode91(Float code91) {
        this.code91 = code91;
    }

    /**
     * @return the code92
     */
    public Short getCode92() {
        return code92;
    }

    /**
     * @param code92
     *            the code92 to set
     */
    public void setCode92(Short code92) {
        this.code92 = code92;
    }

    /**
     * @return the code93
     */
    public Float getCode93() {
        return code93;
    }

    /**
     * @param code93
     *            the code93 to set
     */
    public void setCode93(Float code93) {
        this.code93 = code93;
    }

    /**
     * @return the code94
     */
    public Float getCode94() {
        return code94;
    }

    /**
     * @param code94
     *            the code94 to set
     */
    public void setCode94(Float code94) {
        this.code94 = code94;
    }

    /**
     * @return the code95
     */
    public Float getCode95() {
        return code95;
    }

    /**
     * @param code95
     *            the code95 to set
     */
    public void setCode95(Float code95) {
        this.code95 = code95;
    }

    /**
     * @return the code96
     */
    public Float getCode96() {
        return code96;
    }

    /**
     * @param code96
     *            the code96 to set
     */
    public void setCode96(Float code96) {
        this.code96 = code96;
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
    public Float getLatitude() {
        return location.getLatitude();
    }

    /**
     * Get the geometry longitude.
     * 
     * @return The geometry longitude.
     */
    public Float getLongitude() {
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
     * @return the timeObs
     */
    public Calendar getObservationTime() {
        return observationTime;
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setObservationTime(Calendar observationTime) {
        this.observationTime = observationTime;
    }

    /**
     * 
     */
    public void setSpatialObject(SurfaceObsLocation loc) {
        location = loc;
    }

    /**
     * 
     */
    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    /**
     * @param providerId
     *            the providerId to set
     */
    public void setProviderId(String providerId) {
        this.providerId = providerId;
    }

    /**
     * @return the providerId
     */
    public String getProviderId() {
        return providerId;
    }

    /**
     * @param stationName
     *            the stationName to set
     */
    public void setStationName(String stationName) {
        this.stationName = stationName;
    }

    /**
     * @return the stationName
     */
    public String getStationName() {
        return stationName;
    }

    /**
     * @param homeWFO
     *            the homeWFO to set
     */
    public void setHomeWFO(String homeWFO) {
        this.homeWFO = homeWFO;
    }

    /**
     * @return the homeWFO
     */
    public String getHomeWFO() {
        return homeWFO;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(Short reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the reportType
     */
    public Short getReportType() {
        return reportType;
    }

    /**
     * @param unitsCode
     *            the unitsCode to set
     */
    public void setUnitsCode(Short unitsCode) {
        this.unitsCode = unitsCode;
    }

    /**
     * @return the unitsCode
     */
    public Short getUnitsCode() {
        return unitsCode;
    }

    /**
     * @param code10
     *            the code10 to set
     */
    public void setCode10(Float code10) {
        this.code10 = code10;
    }

    /**
     * @return the code10
     */
    public Float getCode10() {
        return code10;
    }

    /**
     * @param code11
     *            the code11 to set
     */
    public void setCode11(Float code11) {
        this.code11 = code11;
    }

    /**
     * @return the code11
     */
    public Float getCode11() {
        return code11;
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
     * @param rawMessage
     *            the rawMessage to set
     */
    public void setRawMessage(String rawMessage) {
        this.rawMessage = rawMessage;
    }

    /**
     * @return the rawMessage
     */
    public String getRawMessage() {
        return rawMessage;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "ldadmanual";
    }
}
