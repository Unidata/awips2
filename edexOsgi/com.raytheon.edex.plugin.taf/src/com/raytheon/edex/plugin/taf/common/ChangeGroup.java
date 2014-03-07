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

package com.raytheon.edex.plugin.taf.common;

import static com.raytheon.edex.plugin.taf.common.TafConstants.ALTIMETER_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_BECMG;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_FM;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_PROB;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_PROB_TEMPO;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CG_TEMPO;
import static com.raytheon.edex.plugin.taf.common.TafConstants.CHANGE_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.ICING_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.SKY_COVER_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.TEMP_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.TURBULENCE_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.VISIBILITY_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.VS_6PLUS_M;
import static com.raytheon.edex.plugin.taf.common.TafConstants.VS_6PLUS_SM;
import static com.raytheon.edex.plugin.taf.common.TafConstants.WIND_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.WIND_SHEAR_GROUP_EXP;
import static com.raytheon.edex.plugin.taf.common.TafConstants.WX_CAVOK;
import static com.raytheon.edex.plugin.taf.common.TafConstants.WX_NSW;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * This class represents a forecast group found in a taf message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2006            bphillip     Initial Creation
 * Jun 21, 2007  180       bphillip     Updated for use with plugin persistance pattern
 * Jun 03. 2008  1001      jkorman      Numerous changes to the the decode
 *                                      method. 
 * Sep 04, 2008  1444      grichard     Move constants to TafConstants class.
 * Oct 21, 2008  1515      jkorman      Added 30 Hour capability changes.
 * Jun 28, 2012  #827      dgilling     Annotate id field for
 *                                      serialization.
 * Nov 01, 2013  2361      njensen     Remove XML annotations
 * Feb 10, 2014  2777      rferrel      set the parentId when assinging sets.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "taf_change_groups")
@DynamicSerialize
public class ChangeGroup extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    /** An identifier used to link this ChangeGroup to its parent TAF */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private TafRecord parentID;

    @Id
    @GeneratedValue
    private int id;

    /** A String containing the change group */
    @Column
    @DynamicSerializeElement
    private String changeGroup;

    /** The period for which the TAF is valid */
    @Transient
    private TafPeriod tafValidPeriod;

    /**
     * The period for which the change group conditions apply.
     */
    @DynamicSerializeElement
    @Embedded
    private TafPeriod tafChangePeriod;

    /**
     * The sequence id is used to physically order a collection of ChangeGroups.
     * This is required because the start times may be ambiguous i.e. A BECMG
     * and TEMPO change group could share the same start time.
     */
    @DynamicSerializeElement
    @Column
    private Integer sequenceId;

    /**
     * The change group indicator i.e. BECMG, FM, TEMPO, etc
     */
    @DynamicSerializeElement
    @Column(length = 10)
    private String change_indicator;

    /**
     * The probability percentage for PROB and PROB TEMPO change groups.
     */
    @DynamicSerializeElement
    @Column
    private Integer probability;

    /** Wind direction in degrees */
    @DynamicSerializeElement
    @Column(length = 3)
    private String wind_dir_degrees;

    /** Wind speed in knots */
    @DynamicSerializeElement
    @Column
    private Integer wind_speed_kt;

    /** Wind gust in knots */
    @DynamicSerializeElement
    @Column
    private Integer wind_gust_kt;

    /** Wind shear height above ground level */
    @DynamicSerializeElement
    @Column
    private Integer wind_shear_hgt_ft_agl;

    /** Wind shear direction in degrees */
    @DynamicSerializeElement
    @Column
    private Integer wind_shear_dir_degrees;

    /** Wind shear speed in knots */
    @DynamicSerializeElement
    @Column
    private Integer wind_shear_speed_kt;

    /** Visibility (horizontal) in miles */
    @DynamicSerializeElement
    @Column(length = 8)
    private String visibility_mi;

    /** Altimeter reading in inches of mercury */
    @DynamicSerializeElement
    @Column(length = 16)
    private String altim_in_hg;

    /** Vertical visibility */
    @DynamicSerializeElement
    @Column(length = 8)
    private String vert_vis_ft;

    /**
     * Weather and obscurations
     */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    private Set<TafWeatherCondition> weather;

    /** Sky coverage */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    private Set<TafSkyCover> sky_cover;

    /** The turbulence layers */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    private Set<TurbulenceLayer> turbulence_layers;

    /** The icing layers */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    private Set<IcingLayer> icing_layers;

    /** The temperature forecasts */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    private Set<TemperatureForecast> temp_forecasts;

    /** Maximum temperature */
    @DynamicSerializeElement
    @Column
    private Integer max_temp_c;

    /** Minimum temperature */
    @DynamicSerializeElement
    @Column
    private Integer min_temp_c;

    @DynamicSerializeElement
    @Column
    private String remarks;

    public ChangeGroup() {

    }

    /**
     * Constructor for ChangeGroup
     * 
     * @param group
     *            A string containing the change group
     * @param tafIssuePeriod
     *            The valid time period of the TAF
     */
    public ChangeGroup(String group, TafPeriod tafIssuePeriod) {
        identifier = UUID.randomUUID().toString();
        changeGroup = group;
        tafValidPeriod = tafIssuePeriod;

        tafChangePeriod = new TafPeriod(TimeTools.copy(tafValidPeriod
                .getStartDate()), TimeTools.copy(tafValidPeriod.getEndDate()));

        decode(extractTime(group));
    }

    /**
     * Constructor for use where the issue period and group valid period are
     * determined in advance of decoding the change group data.
     * 
     * @param group
     * @param fcstData
     *            A String containing the change group with the group type and
     *            time information removed.
     * @param tafIssuePeriod
     *            The valid period of the enclosing TAF.
     * @param groupValidPeriod
     *            The change group valid period.
     */
    public ChangeGroup(String group, String fcstData, TafPeriod tafIssuePeriod,
            TafPeriod groupValidPeriod) {
        identifier = UUID.randomUUID().toString();
        changeGroup = fcstData;
        change_indicator = group;
        tafValidPeriod = tafIssuePeriod;
        tafChangePeriod = groupValidPeriod;

        decode(fcstData);
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

    /**
     * 
     * @param groupData
     * @return
     */
    private String extractTime(String groupData) {
        StringBuilder grpData = new StringBuilder(groupData);
        checkGroupDataEnd(grpData);
        Pattern groupPattern = Pattern.compile(CHANGE_GROUP_EXP);
        Matcher matcher = groupPattern.matcher(grpData);

        int stopPos = -1;
        if (matcher.find()) {

            stopPos = matcher.end();
            String s = matcher.group(2);
            String chgGrp = (s != null) ? s.trim() : null;

            String prob = matcher.group(6);
            if (prob != null) {
                probability = Integer.parseInt(prob.substring(4, 6));

                s = (matcher.group(7) != null) ? matcher.group(7).trim() : null;
                if (CG_TEMPO.equals(s)) {
                    chgGrp = CG_PROB_TEMPO;
                } else {
                    chgGrp = CG_PROB;
                }
            }

            change_indicator = chgGrp;

            s = matcher.group(9);
            if (s != null) {

                int t1 = Integer.parseInt(s.substring(0, 2));
                int t2 = Integer.parseInt(s.substring(2, 4));

                // FM group is hour/minute
                if (CG_FM.equals(chgGrp)) {
                    tafChangePeriod = TafPeriod.determineChangeGroupPeriodSSss(
                            t1, t2, tafValidPeriod);
                } else {
                    // All the rest are hour/hour
                    tafChangePeriod = TafPeriod.determineChangeGroupPeriodSSEE(
                            t1, t2, tafValidPeriod,
                            CG_BECMG.equals(change_indicator));
                }
            }
        }
        if (stopPos > -1) {
            grpData.delete(0, stopPos);
        }

        return grpData.toString();
    }

    /**
     * Extracts information from the change group and stores in class fields
     * 
     */
    private void decode(String fcstData) {

        StringBuilder grpData = new StringBuilder(fcstData);
        weather = new HashSet<TafWeatherCondition>();
        sky_cover = new HashSet<TafSkyCover>();
        turbulence_layers = new HashSet<TurbulenceLayer>();
        icing_layers = new HashSet<IcingLayer>();
        temp_forecasts = new HashSet<TemperatureForecast>();

        Pattern groupPattern = Pattern.compile(WIND_GROUP_EXP);
        // Extracting the wind group
        checkGroupDataEnd(grpData);
        Matcher matcher = groupPattern.matcher(grpData);

        int stopPos = -1;
        if (matcher.find()) {
            stopPos = matcher.end();
            wind_dir_degrees = matcher.group(1);

            String s = matcher.group(2);
            Integer ws = (s != null) ? Integer.parseInt(s) : -9999;
            s = matcher.group(5);
            Integer wg = (s != null) ? Integer.parseInt(s) : -9999;
            if ("MPS".equals(matcher.group(6))) {
                Double wspd = (ws >= 0) ? ws * 1.943 : -9999.0;
                wind_speed_kt = (wspd >= 0) ? wspd.intValue() : null;

                wspd = (wg >= 0) ? wg * 1.943 : -9999.0;
                wind_gust_kt = (wspd >= 0) ? wspd.intValue() : null;
            } else if ("KMH".equals(matcher.group(6))) {
                Double wspd = (ws >= 0) ? ws * 0.53995 : -9999.0;
                wind_speed_kt = (wspd >= 0) ? wspd.intValue() : null;

                wspd = (wg >= 0) ? wg * 0.53995 : -9999.0;
                wind_gust_kt = (wspd >= 0) ? wspd.intValue() : null;
            } else {
                wind_speed_kt = (ws >= 0) ? ws : null;
                wind_gust_kt = (wg >= 0) ? wg : null;
            }
        }
        if (stopPos > -1) {
            grpData.delete(0, stopPos);
        }

        // Extracting icing information
        groupPattern = Pattern.compile(ICING_GROUP_EXP);
        checkGroupDataEnd(grpData);
        int start = grpData.length();
        int stop = -1;
        matcher = groupPattern.matcher(grpData);
        while (matcher.find()) {
            icing_layers.add(new IcingLayer(this, matcher.group(1), matcher
                    .group(2), matcher.group(3)));
            start = Math.min(start, matcher.start());
            stop = Math.max(stop, matcher.end());
        }
        if ((start < stop) && (stop > 0)) {
            grpData.delete(start, stop);
        }

        // Extracting turbulence information
        groupPattern = Pattern.compile(TURBULENCE_GROUP_EXP);
        checkGroupDataEnd(grpData);
        start = grpData.length();
        stop = -1;
        matcher = groupPattern.matcher(grpData);
        while (matcher.find()) {
            turbulence_layers.add(new TurbulenceLayer(this, matcher.group(1),
                    matcher.group(2), matcher.group(3)));
            start = Math.min(start, matcher.start());
            stop = Math.max(stop, matcher.end());
        }
        if ((start < stop) && (stop > 0)) {
            grpData.delete(start, stop);
        }

        groupPattern = Pattern.compile(TEMP_GROUP_EXP);
        checkGroupDataEnd(grpData);
        start = grpData.length();
        stop = -1;
        matcher = groupPattern.matcher(grpData);
        while (matcher.find()) {
            temp_forecasts.add(new TemperatureForecast(this, matcher.group(1),
                    matcher.group(2), matcher.group(3)));
            start = Math.min(start, matcher.start());
            stop = Math.max(stop, matcher.end());
        }
        if ((start < stop) && (stop > 0)) {
            grpData.delete(start, stop);
        }

        // Extracting altimeter information
        groupPattern = Pattern.compile(ALTIMETER_GROUP_EXP);
        checkGroupDataEnd(grpData);
        matcher = groupPattern.matcher(grpData);
        if (matcher.find()) {
            altim_in_hg = Double
                    .toString(Double.parseDouble(matcher.group(1)) / 100);
            grpData.delete(matcher.start(), matcher.end());
        }

        // Extracting wind shear information
        groupPattern = Pattern.compile(WIND_SHEAR_GROUP_EXP);
        checkGroupDataEnd(grpData);
        matcher = groupPattern.matcher(grpData);
        if (matcher.find()) {

            this.wind_shear_hgt_ft_agl = Integer.parseInt(matcher.group(1)) * 100;
            this.wind_shear_dir_degrees = Integer.parseInt(matcher.group(2));
            this.wind_shear_speed_kt = Integer.parseInt(matcher.group(3));
            grpData.delete(matcher.start(), matcher.end());
        }

        // Extracting the visibility
        groupPattern = Pattern.compile(WX_CAVOK);
        checkGroupDataEnd(grpData);
        matcher = groupPattern.matcher(grpData);
        if (matcher.find()) {
            // CAVOK implies 6+ visibility and no cloud or weather groups!
            visibility_mi = VS_6PLUS_SM;
            grpData.delete(matcher.start(), matcher.end());
        } else {
            // Extracting the visibility
            groupPattern = Pattern.compile(VISIBILITY_GROUP_EXP);
            matcher = groupPattern.matcher(grpData);

            if (matcher.find()) {
                visibility_mi = matcher.group(1);
                grpData.delete(matcher.start(), matcher.end());
            } else {
                // Statue miles vis didn't work so check metric
                groupPattern = Pattern.compile("\\d{4}");
                matcher = groupPattern.matcher(grpData);
                if (matcher.find()) {
                    if (matcher.end() < 6) {
                        visibility_mi = matcher.group();
                        if (VS_6PLUS_M.equals(visibility_mi)) {
                            visibility_mi = VS_6PLUS_SM;
                        }
                        grpData.delete(matcher.start(), matcher.end());
                    }
                }
            }

            // Extracting the weather
            groupPattern = Pattern.compile(WX_NSW);
            checkGroupDataEnd(grpData);
            matcher = groupPattern.matcher(grpData);
            if (matcher.find()) {
                TafWeatherCondition wxCon = new TafWeatherCondition();
                wxCon.setSequenceId(1);
                wxCon.setOther(WX_NSW);
                weather.add(wxCon);
                wxCon.setParentID(this);
                grpData.delete(matcher.start(), matcher.end());
            } else {
                TAFWeatherTools wxTool = new TAFWeatherTools();
                Set<TafWeatherCondition> cond = wxTool.parseWeather(grpData);
                if (cond != null) {
                    for (TafWeatherCondition c : cond) {
                        c.setIdentifier(identifier);
                        c.setParentID(this);
                        weather.add(c);
                    }
                }
                cond = null;
            }

            // Extracting the sky cover
            groupPattern = Pattern.compile(SKY_COVER_GROUP_EXP);
            checkGroupDataEnd(grpData);
            matcher = groupPattern.matcher(grpData);
            start = grpData.length();
            stop = -1;

            while (matcher.find()) {
                // keep track of the start - stop for all sky cover groups.
                start = Math.min(start, matcher.start());
                stop = Math.max(stop, matcher.end());

                // if SKC | CLR | NSC then no cloud info present, exit!
                String grp = matcher.group(8);
                if (grp != null) {
                    TafSkyCover cover = new TafSkyCover();
                    cover.setParentID(this);
                    cover.setType(grp);
                    cover.setHeight(-1);
                    sky_cover.add(cover);
                    break;
                }
                grp = matcher.group(3);
                if ("VV".equals(grp)) {
                    vert_vis_ft = matcher.group(5);
                }
                TafSkyCover cover = new TafSkyCover();
                cover.setParentID(this);
                cover.setGenus(matcher.group(7));
                cover.setType(matcher.group(3));
                grp = matcher.group(5);
                if (grp != null) {
                    if ("///".equals(grp)) {
                        // For now we're not going to handle surface based
                        // obscuration.
                        cover = null;
                    } else {
                        int height = Integer.parseInt(grp) * 100;
                        cover.setHeight(height);
                    }
                }
                if (cover != null) {
                    sky_cover.add(cover);
                }
            }
            if ((start < stop) && (stop > 0)) {
                grpData.delete(start, stop);
            }
        }

        // Anything left will be considered remarks for this group!
        if (grpData.length() > 0) {
            remarks = grpData.toString();
            remarks = remarks.replaceAll("[;\\$=]", "");
            grpData = new StringBuilder(remarks);
            while ((grpData.length() > 0)
                    && (grpData.charAt(grpData.length() - 1) == ' ')) {
                grpData.deleteCharAt(grpData.length() - 1);
            }
            if (remarks.length() == 0) {
                remarks = null;
            }
        }
        int maxTemp = -999;
        int minTemp = 999;

        Iterator<TemperatureForecast> tFcsts = getTemp_forecasts().iterator();
        while (tFcsts.hasNext()) {

            int tFcst = tFcsts.next().getSfc_temp_c();
            minTemp = Math.min(minTemp, tFcst);
            maxTemp = Math.max(maxTemp, tFcst);
        }
        min_temp_c = (minTemp != 999) ? minTemp : null;
        max_temp_c = (maxTemp != -999) ? maxTemp : null;

        if (weather.size() == 0) {
            weather = null;
        }
        if (sky_cover.size() == 0) {
            sky_cover = null;
        }
        if (turbulence_layers.size() == 0) {
            turbulence_layers = null;
        }
        if (icing_layers.size() == 0) {
            icing_layers = null;
        }
        if (temp_forecasts.size() == 0) {
            temp_forecasts = null;
        }
    }

    /**
     * Create a string representation of this ChangeGroup.
     * 
     * @return The string representation of this ChangeGroup.
     */
    @Override
    public String toString() {
        StringBuilder retVal = new StringBuilder("___CHANGE GROUP___");

        retVal.append("Change Indicator: ");
        if (change_indicator != null) {
            retVal.append(change_indicator);
        }
        retVal.append(":Prob: ");
        if (probability != null) {
            retVal.append(probability);
        }
        retVal.append(" :Valid Period: ");
        if (tafChangePeriod != null) {
            retVal.append(tafChangePeriod);
        }
        retVal.append("\nWDir: ");
        if (wind_dir_degrees != null) {
            retVal.append(wind_dir_degrees);
        }
        retVal.append("WSpd: ");
        if (wind_speed_kt != null) {
            retVal.append(wind_speed_kt);
            retVal.append(" ");
        }
        if (wind_gust_kt != null) {
            retVal.append("G");
            retVal.append(wind_gust_kt);
            retVal.append(" ");
        }
        if (altim_in_hg != null) {
            retVal.append(":Altimeter: ");
            retVal.append(altim_in_hg);
            retVal.append(" ");
        }
        if (max_temp_c != null) {
            retVal.append("MxTmp: ");
            retVal.append(max_temp_c);
        }
        if (min_temp_c != null) {
            retVal.append(":MnTmp: ");
            retVal.append(min_temp_c);
            retVal.append("\n");
        }
        if (wind_shear_hgt_ft_agl != null) {
            retVal.append("\nWind Shear Height: ");
            retVal.append(wind_shear_hgt_ft_agl);
            retVal.append(" ");
        }
        if (wind_shear_dir_degrees != null) {
            retVal.append(":WShearDir: ");
            retVal.append(wind_shear_dir_degrees);
            retVal.append(" ");
        }
        if (wind_shear_speed_kt != null) {
            retVal.append("WShearSpd: ");
            retVal.append(wind_shear_speed_kt);
            retVal.append("\n");
        }
        if (visibility_mi != null) {
            retVal.append("\nVisibility: ");
            retVal.append(visibility_mi);
        }
        if (vert_vis_ft != null) {
            retVal.append("\nVertical Visibility: ");
            retVal.append(vert_vis_ft);
        }
        if ((weather != null) && (weather.size() > 0)) {
            retVal.append("\nWeather Phenomena: ");
            for (Iterator<TafWeatherCondition> iter = this.getWeather()
                    .iterator(); iter.hasNext();) {
                TafWeatherCondition condition = iter.next();
                // retVal.append(condition);
                condition.toString(retVal);
            }
        }
        if ((sky_cover != null) && (sky_cover.size() > 0)) {
            retVal.append("\n");
            retVal.append("Sky Conditions: ");
            for (Iterator<TafSkyCover> iter = this.getSky_cover().iterator(); iter
                    .hasNext();) {
                TafSkyCover cover = iter.next();
                retVal.append(cover);
            }

        }
        if ((turbulence_layers != null) && (turbulence_layers.size() > 0)) {
            for (Iterator<TurbulenceLayer> iter = this.getTurbulence_layers()
                    .iterator(); iter.hasNext();) {
                TurbulenceLayer layer = iter.next();
                retVal.append("\n");
                retVal.append(layer);
            }
        }
        if ((icing_layers != null) && (icing_layers.size() > 0)) {

            for (Iterator<IcingLayer> iter = this.getIcing_layers().iterator(); iter
                    .hasNext();) {
                IcingLayer layer = iter.next();
                retVal.append(layer);
            }
        }
        if ((temp_forecasts != null) && (temp_forecasts.size() > 0)) {
            for (Iterator<TemperatureForecast> iter = this.getTemp_forecasts()
                    .iterator(); iter.hasNext();) {
                TemperatureForecast forecast = iter.next();
                retVal.append(forecast);
            }
        }

        return retVal.toString();
    }

    public TafRecord getParentID() {
        return parentID;
    }

    public void setParentID(TafRecord parentID) {
        this.parentID = parentID;
    }

    public TafPeriod getTafChangePeriod() {
        return tafChangePeriod;
    }

    public void setTafChangePeriod(TafPeriod tafChangePeriod) {
        this.tafChangePeriod = tafChangePeriod;
    }

    public String getWind_dir_degrees() {
        return wind_dir_degrees;
    }

    public void setWind_dir_degrees(String wind_dir_degrees) {
        this.wind_dir_degrees = wind_dir_degrees;
    }

    public Integer getWind_speed_kt() {
        return wind_speed_kt;
    }

    public void setWind_speed_kt(Integer wind_speed_kt) {
        this.wind_speed_kt = wind_speed_kt;
    }

    public Integer getWind_gust_kt() {
        return wind_gust_kt;
    }

    public void setWind_gust_kt(Integer wind_gust_kt) {
        this.wind_gust_kt = wind_gust_kt;
    }

    public Integer getWind_shear_hgt_ft_agl() {
        return wind_shear_hgt_ft_agl;
    }

    public void setWind_shear_hgt_ft_agl(Integer wind_shear_hgt_ft_agl) {
        this.wind_shear_hgt_ft_agl = wind_shear_hgt_ft_agl;
    }

    public Integer getWind_shear_dir_degrees() {
        return wind_shear_dir_degrees;
    }

    public void setWind_shear_dir_degrees(Integer wind_shear_dir_degrees) {
        this.wind_shear_dir_degrees = wind_shear_dir_degrees;
    }

    public Integer getWind_shear_speed_kt() {
        return wind_shear_speed_kt;
    }

    public void setWind_shear_speed_kt(Integer wind_shear_speed_kt) {
        this.wind_shear_speed_kt = wind_shear_speed_kt;
    }

    public String getVisibility_mi() {
        return visibility_mi;
    }

    public void setVisibility_mi(String visibility_mi) {
        this.visibility_mi = visibility_mi;
    }

    public String getAltim_in_hg() {
        return altim_in_hg;
    }

    public void setAltim_in_hg(String altim_in_hg) {
        this.altim_in_hg = altim_in_hg;
    }

    public String getVert_vis_ft() {
        return vert_vis_ft;
    }

    public void setVert_vis_ft(String vert_vis_ft) {
        this.vert_vis_ft = vert_vis_ft;
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

    public Set<TurbulenceLayer> getTurbulence_layers() {
        return turbulence_layers;
    }

    public void setTurbulence_layers(Set<TurbulenceLayer> turbulence_layers) {
        this.turbulence_layers = turbulence_layers;
        if ((turbulence_layers != null) && (turbulence_layers.size() > 0)) {
            for (TurbulenceLayer turbulence_layer : turbulence_layers) {
                turbulence_layer.setParentID(this);
            }
        }
    }

    public Set<IcingLayer> getIcing_layers() {
        return icing_layers;
    }

    public void setIcing_layers(Set<IcingLayer> icing_layers) {
        this.icing_layers = icing_layers;
        if ((icing_layers != null) && (icing_layers.size() > 0)) {
            for (IcingLayer icing_layer : icing_layers) {
                icing_layer.setParentID(this);
            }
        }
    }

    public Set<TemperatureForecast> getTemp_forecasts() {
        return temp_forecasts;
    }

    public void setTemp_forecasts(Set<TemperatureForecast> temp_forecasts) {
        this.temp_forecasts = temp_forecasts;
        if ((temp_forecasts != null) && (temp_forecasts.size() > 0)) {
            for (TemperatureForecast temForecast : temp_forecasts) {
                temForecast.setParentID(this);
            }
        }
    }

    public Set<TafWeatherCondition> getWeather() {
        return weather;
    }

    public void setWeather(Set<TafWeatherCondition> weather) {
        this.weather = weather;
        if ((weather != null) && (weather.size() > 0)) {
            for (TafWeatherCondition twc : weather) {
                twc.setParentID(this);
            }
        }
    }

    public Set<TafSkyCover> getSky_cover() {
        return sky_cover;
    }

    public void setSky_cover(Set<TafSkyCover> sky_cover) {
        this.sky_cover = sky_cover;
        if ((sky_cover != null) && (sky_cover.size() > 0)) {
            for (TafSkyCover tsc : sky_cover) {
                tsc.setParentID(this);
            }
        }
    }

    private void checkGroupDataEnd(StringBuilder group) {
        if ((group != null) && (group.length() > 1)) {
            if (group.charAt(group.length() - 1) != ' ') {
                group.append(' ');
            }
        }
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}
