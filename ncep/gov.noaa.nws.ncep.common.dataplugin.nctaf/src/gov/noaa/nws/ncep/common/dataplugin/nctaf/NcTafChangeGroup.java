/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/


package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.ALTIMETER_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_BECMG;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_FM;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_PROB;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_PROB_TEMPO;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CG_TEMPO;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.CHANGE_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.ICING_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.SKY_COVER_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.TEMP_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.TURBULENCE_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.VISIBILITY_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.VS_6PLUS_M;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.VS_6PLUS_SM;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.WIND_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.WIND_SHEAR_GROUP_EXP;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.WX_CAVOK;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.WX_NSW;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * 
 * This class represents a forecast group found in a taf message.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 *                 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 10/26/2011               sgurung     Added probable parameters (for TEMPO/PROB) 
 * 11/03/2011               sgurung     Added probable weather and sky cover, set probability * 10 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcTafChangeGroup implements Serializable, ISerializableObject{

    private static final long serialVersionUID = 1L;

    /** An identifier used to link this ChangeGroup to its parent TAF */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafRecord parentID;

    @Id
    private int id;

    /** A String containing the change group */
    @DynamicSerializeElement
    @XmlElement
    private String changeGroup;

    /** The period for which the TAF is valid */
    @Transient
    private NcTafPeriod tafValidPeriod;

    /**
     * The period for which the change group conditions apply.
     */
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
    @Column
    private Calendar startDate;

    // This time is only used for BECMG groups. It marks the end time of the
    // BECMG transition period.
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Calendar transitionEndDate;

    /**
     * The forecast valid ending date
     */
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Calendar endDate;
    
    /**
     * The sequence id is used to physically order a collection of ChangeGroups.
     * This is required because the start times may be ambiguous i.e. A BECMG
     * and TEMPO change group could share the same start time.
     */
    @DynamicSerializeElement
    @XmlElement
    private Integer sequenceId;

    /**
     * The change group indicator i.e. BECMG, FM, TEMPO, etc
     */
    @DynamicSerializeElement
    @XmlElement
    private String change_indicator;

    /**
     * The probability percentage for PROB and PROB TEMPO change groups.
     */
    @DynamicSerializeElement
    @XmlElement
    private Integer probability;

    /** Wind direction in degrees */
    @DynamicSerializeElement
    @XmlElement
    private String wind_dir_degrees;

    /** Wind speed in knots */
    @DynamicSerializeElement
    @XmlElement
    private Integer wind_speed_kt;

    /** Wind gust in knots */
    @DynamicSerializeElement
    @XmlElement
    private Integer wind_gust_kt;

    /** Wind shear height above ground level */
    @DynamicSerializeElement
    @XmlElement
    private Integer wind_shear_hgt_ft_agl;

    /** Wind shear direction in degrees */
    @DynamicSerializeElement
    @XmlElement
    private Integer wind_shear_dir_degrees;

    /** Wind shear speed in knots */
    @DynamicSerializeElement
    @XmlElement
    private Integer wind_shear_speed_kt;

    /** Visibility (horizontal) in miles */
    @DynamicSerializeElement
    @XmlElement
    private String visibility_mi;

    /** Altimeter reading in inches of mercury */
    @DynamicSerializeElement
    @XmlElement
    private String altim_in_hg;

    /** Vertical visibility */
    @DynamicSerializeElement
    @XmlElement
    private String vert_vis_ft;
    
    /** Wind direction in degrees (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private String probable_wind_dir_degrees;

    /** Wind speed in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probable_wind_speed_kt;

    /** Wind gust in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probable_wind_gust_kt;

    /** Wind shear height above ground level (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probable_wind_shear_hgt_ft_agl;

    /** Wind shear direction in degrees (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probable_wind_shear_dir_degrees;

    /** Wind shear speed in knots (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer probable_wind_shear_speed_kt;

    /** Visibility (horizontal) in miles (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private String probable_visibility_mi;

    /** Vertical visibility (TEMPO/PROB) */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private String probable_vert_vis_ft;
        
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

    /** Maximum temperature */
    @DynamicSerializeElement
    @XmlElement
    private Integer max_temp_c;

    /** Minimum temperature */
    @DynamicSerializeElement
    @XmlElement
    private Integer min_temp_c;

    @DynamicSerializeElement
    @XmlElement
    private String remarks;

    /**
	 * No-Arg Constructor
	 */
	public NcTafChangeGroup() {
    	changeGroup = " ";
    	altim_in_hg = " ";
    	change_indicator = " ";
    	max_temp_c = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	min_temp_c = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	probability = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	remarks = " ";
    	sequenceId = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	vert_vis_ft = " ";
    	visibility_mi = " ";
    	wind_dir_degrees = " ";
    	wind_gust_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	wind_speed_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	wind_shear_hgt_ft_agl = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	wind_shear_dir_degrees = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	wind_shear_speed_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	startDate = null;
    	endDate = null;
    	transitionEndDate = null;
    	probable_vert_vis_ft = " ";
    	probable_visibility_mi = " ";
    	probable_wind_dir_degrees = " ";
    	probable_wind_gust_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	probable_wind_speed_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	probable_wind_shear_hgt_ft_agl = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	probable_wind_shear_dir_degrees = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    	probable_wind_shear_speed_kt = IDecoderConstantsN.NEGATIVE_INTEGER_MISSING;
    }

    /**
     * Constructor for ChangeGroup
     * 
     * @param group
     *            A string containing the change group
     * @param tafIssuePeriod
     *            The valid time period of the TAF
     */
    public NcTafChangeGroup(String group, NcTafPeriod tafIssuePeriod) {
        //identifier = java.util.UUID.randomUUID().toString();
        id = java.util.UUID.randomUUID().hashCode();
        changeGroup = group;
        tafValidPeriod = tafIssuePeriod;

        tafChangePeriod = new NcTafPeriod(TimeTools.copy(tafValidPeriod
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
    public NcTafChangeGroup(String group, String fcstData, NcTafPeriod tafIssuePeriod,
            NcTafPeriod groupValidPeriod) {
        //identifier = UUID.randomUUID().toString();
        id = UUID.randomUUID().hashCode();
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
                probability = Integer.parseInt(prob.substring(4, 6)) * 10;
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
                    tafChangePeriod = NcTafPeriod.determineChangeGroupPeriodSSss(
                            t1, t2, tafValidPeriod);
                } else {
                    // All the rest are hour/hour
                    tafChangePeriod = NcTafPeriod.determineChangeGroupPeriodSSEE(
                            t1, t2, tafValidPeriod, CG_BECMG
                                    .equals(change_indicator));
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
        weather = new HashSet<NcTafWeatherCondition>();
        probable_weather = new HashSet<NcTafWeatherCondition>();
        sky_cover = new HashSet<NcTafSkyCover>();
        probable_sky_cover = new HashSet<NcTafSkyCover>();
        turbulence_layers = new HashSet<NcTafTurbulenceLayer>();
        icing_layers = new HashSet<NcTafIcingLayer>();
        temp_forecasts = new HashSet<NcTafTemperatureForecast>();        
        		
        Pattern groupPattern = Pattern.compile(WIND_GROUP_EXP);
        // Extracting the wind group
        checkGroupDataEnd(grpData);
        Matcher matcher = groupPattern.matcher(grpData);

        int stopPos = -1;
        if (matcher.find()) {
            stopPos = matcher.end();
            
            if (!isTempGroup(change_indicator)) {
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
            } else {
            	probable_wind_dir_degrees = matcher.group(1);
            	 String s = matcher.group(2);
                Integer ws = (s != null) ? Integer.parseInt(s) : -9999;
                s = matcher.group(5);
                Integer wg = (s != null) ? Integer.parseInt(s) : -9999;
                if ("MPS".equals(matcher.group(6))) {
                    Double wspd = (ws >= 0) ? ws * 1.943 : -9999.0;
                    probable_wind_speed_kt = (wspd >= 0) ? wspd.intValue() : null;

                    wspd = (wg >= 0) ? wg * 1.943 : -9999.0;
                    probable_wind_gust_kt = (wspd >= 0) ? wspd.intValue() : null;
                } else if ("KMH".equals(matcher.group(6))) {
                    Double wspd = (ws >= 0) ? ws * 0.53995 : -9999.0;
                    probable_wind_speed_kt = (wspd >= 0) ? wspd.intValue() : null;

                    wspd = (wg >= 0) ? wg * 0.53995 : -9999.0;
                    probable_wind_gust_kt = (wspd >= 0) ? wspd.intValue() : null;
                } else {
                	probable_wind_speed_kt = (ws >= 0) ? ws : null;
                	probable_wind_gust_kt = (wg >= 0) ? wg : null;
                }                 
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
            icing_layers.add(new NcTafIcingLayer(this, matcher.group(1), matcher
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
            turbulence_layers.add(new NcTafTurbulenceLayer(this, matcher.group(1),
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
            temp_forecasts.add(new NcTafTemperatureForecast(this, matcher.group(1),
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
        	if (!isTempGroup(change_indicator)) {
        		this.wind_shear_hgt_ft_agl = Integer.parseInt(matcher.group(1)) * 100;
 	            this.wind_shear_dir_degrees = Integer.parseInt(matcher.group(2));
 	            this.wind_shear_speed_kt = Integer.parseInt(matcher.group(3));
 	        } else {
	            this.probable_wind_shear_hgt_ft_agl = Integer.parseInt(matcher.group(1)) * 100;
	            this.probable_wind_shear_dir_degrees = Integer.parseInt(matcher.group(2));
	            this.probable_wind_shear_speed_kt = Integer.parseInt(matcher.group(3));
        	}
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
            if (isTempGroup(change_indicator)) {
            	probable_visibility_mi = visibility_mi;
            	visibility_mi = " ";
            }

            // Extracting the weather
            groupPattern = Pattern.compile(WX_NSW);
            checkGroupDataEnd(grpData);
            matcher = groupPattern.matcher(grpData);
            if (matcher.find()) {
                NcTafWeatherCondition wxCon = new NcTafWeatherCondition();
                wxCon.setSequenceId(1);
                wxCon.setOther(WX_NSW);
                weather.add(wxCon);
                wxCon.setParentID(this);                
                grpData.delete(matcher.start(), matcher.end());
            } else {
                NcTafWeatherTools wxTool = new NcTafWeatherTools();
                Set<NcTafWeatherCondition> cond = wxTool.parseWeather(grpData);
                if (cond != null) {
                    for (NcTafWeatherCondition c : cond) {
                        /* shova : c.setIdentifier(identifier);*/
                        c.setParentID(this);
                        weather.add(c);
                    }
                }
                cond = null;
            }

            if (isTempGroup(change_indicator)) {
            	probable_weather = weather;
            	weather = new HashSet<NcTafWeatherCondition>();
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
                    NcTafSkyCover cover = new NcTafSkyCover();
                    cover.setParentID(this);
                    cover.setType(grp);
                    cover.setHeight(-1);
                    sky_cover.add(cover);
                    break;
                }
                grp = matcher.group(3);
                if ("VV".equals(grp)) {
                    vert_vis_ft = matcher.group(5);
                    if (isTempGroup(change_indicator)) {
                    	probable_vert_vis_ft = vert_vis_ft;
                    	vert_vis_ft = " ";
                    }
                }
                NcTafSkyCover cover = new NcTafSkyCover();
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
        
        if (isTempGroup(change_indicator)) {
        	probable_sky_cover = sky_cover;
        	sky_cover = new HashSet<NcTafSkyCover>();
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

        Iterator<NcTafTemperatureForecast> tFcsts = getTemp_forecasts().iterator();
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
        if (probable_weather.size() == 0) {
        	probable_weather = null;
        }
        if (sky_cover.size() == 0) {
            sky_cover = null;
        }
        if (probable_sky_cover.size() == 0) {
            probable_sky_cover = null;
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
        
        retVal.append("\nProbable WDir: ");
        if (probable_wind_dir_degrees != null) {
            retVal.append(probable_wind_dir_degrees);
        }
        retVal.append("Probable WSpd: ");
        if (probable_wind_speed_kt != null) {
            retVal.append(probable_wind_speed_kt);
            retVal.append(" ");
        }
        if (probable_wind_gust_kt != null) {
            retVal.append("G");
            retVal.append(probable_wind_gust_kt);
            retVal.append(" ");
        }
        if (probable_wind_shear_hgt_ft_agl != null) {
            retVal.append("\nProbable Wind Shear Height: ");
            retVal.append(probable_wind_shear_hgt_ft_agl);
            retVal.append(" ");
        }
        if (probable_wind_shear_dir_degrees != null) {
            retVal.append(":Probable WShearDir: ");
            retVal.append(probable_wind_shear_dir_degrees);
            retVal.append(" ");
        }
        if (probable_wind_shear_speed_kt != null) {
            retVal.append("Probable WShearSpd: ");
            retVal.append(probable_wind_shear_speed_kt);
            retVal.append("\n");
        }
        if (probable_visibility_mi != null) {
            retVal.append("\nProbable Visibility: ");
            retVal.append(visibility_mi);
        }
        if (probable_vert_vis_ft != null) {
            retVal.append("\nProbable Vertical Visibility: ");
            retVal.append(probable_vert_vis_ft);
        }
        if ((weather != null) && (weather.size() > 0)) {
            retVal.append("\nWeather Phenomena: ");
            for (Iterator<NcTafWeatherCondition> iter = this.getWeather()
                    .iterator(); iter.hasNext();) {
                NcTafWeatherCondition condition = iter.next();
                // retVal.append(condition);
                condition.toString(retVal);
            }
        }
        if ((sky_cover != null) && (sky_cover.size() > 0)) {
            retVal.append("\n");
            retVal.append("Sky Conditions: ");
            for (Iterator<NcTafSkyCover> iter = this.getSky_cover().iterator(); iter
                    .hasNext();) {
                NcTafSkyCover cover = iter.next();
                retVal.append(cover);
            }

        }
        if ((turbulence_layers != null) && (turbulence_layers.size() > 0)) {
            for (Iterator<NcTafTurbulenceLayer> iter = this.getTurbulence_layers()
                    .iterator(); iter.hasNext();) {
                NcTafTurbulenceLayer layer = iter.next();
                retVal.append("\n");
                retVal.append(layer);
            }
        }
        if ((icing_layers != null) && (icing_layers.size() > 0)) {

            for (Iterator<NcTafIcingLayer> iter = this.getIcing_layers().iterator(); iter
                    .hasNext();) {
                NcTafIcingLayer layer = iter.next();
                retVal.append(layer);
            }
        }
        if ((temp_forecasts != null) && (temp_forecasts.size() > 0)) {
            for (Iterator<NcTafTemperatureForecast> iter = this.getTemp_forecasts()
                    .iterator(); iter.hasNext();) {
                NcTafTemperatureForecast forecast = iter.next();
                retVal.append(forecast);
            }
        }

        return retVal.toString();
    }

    public NcTafRecord getParentID() {
        return parentID;
    }

    public void setParentID(NcTafRecord parentID) {
        this.parentID = parentID;
    }

    public NcTafPeriod getTafChangePeriod() {
        return tafChangePeriod;
    }

    public void setTafChangePeriod(NcTafPeriod tafChangePeriod) {
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
    
    public String getProbable_wind_dir_degrees() {
        return probable_wind_dir_degrees;
    }

    public void setProbable_wind_dir_degrees(String wind_dir_degrees) {
        this.probable_wind_dir_degrees = wind_dir_degrees;
    }

    public Integer getProbable_wind_speed_kt() {
        return probable_wind_speed_kt;
    }

    public void setProbable_wind_speed_kt(Integer wind_speed_kt) {
        this.probable_wind_speed_kt = wind_speed_kt;
    }

    public Integer getProbable_wind_gust_kt() {
        return probable_wind_gust_kt;
    }

    public void setProbable_wind_gust_kt(Integer wind_gust_kt) {
        this.probable_wind_gust_kt = wind_gust_kt;
    }

    public Integer getProbable_wind_shear_hgt_ft_agl() {
        return probable_wind_shear_hgt_ft_agl;
    }

    public void setProbable_wind_shear_hgt_ft_agl(Integer wind_shear_hgt_ft_agl) {
        this.probable_wind_shear_hgt_ft_agl = wind_shear_hgt_ft_agl;
    }

    public Integer getProbable_wind_shear_dir_degrees() {
        return probable_wind_shear_dir_degrees;
    }

    public void setProbable_wind_shear_dir_degrees(Integer wind_shear_dir_degrees) {
        this.probable_wind_shear_dir_degrees = wind_shear_dir_degrees;
    }

    public Integer getProbable_wind_shear_speed_kt() {
        return probable_wind_shear_speed_kt;
    }

    public void setProbable_wind_shear_speed_kt(Integer wind_shear_speed_kt) {
        this.probable_wind_shear_speed_kt = wind_shear_speed_kt;
    }

    public String getProbable_visibility_mi() {
        return probable_visibility_mi;
    }

    public void setProbable_visibility_mi(String visibility_mi) {
        this.probable_visibility_mi = visibility_mi;
    }

    public String getProbable_vert_vis_ft() {
        return probable_vert_vis_ft;
    }

    public void setProbable_vert_vis_ft(String vert_vis_ft) {
        this.probable_vert_vis_ft = vert_vis_ft;
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

    public Set<NcTafTurbulenceLayer> getTurbulence_layers() {
        return turbulence_layers;
    }

    public void setTurbulence_layers(Set<NcTafTurbulenceLayer> turbulence_layers) {
        this.turbulence_layers = turbulence_layers;
    }

    public Set<NcTafIcingLayer> getIcing_layers() {
        return icing_layers;
    }

    public void setIcing_layers(Set<NcTafIcingLayer> icing_layers) {
        this.icing_layers = icing_layers;
    }

    public Set<NcTafTemperatureForecast> getTemp_forecasts() {
        return temp_forecasts;
    }

    public void setTemp_forecasts(Set<NcTafTemperatureForecast> temp_forecasts) {
        this.temp_forecasts = temp_forecasts;
    }

    public Set<NcTafWeatherCondition> getWeather() {
        return weather;
    }

    public void setWeather(Set<NcTafWeatherCondition> weather) {
        this.weather = weather;
    }

    public Set<NcTafSkyCover> getSky_cover() {
        return sky_cover;
    }

    public void setSky_cover(Set<NcTafSkyCover> sky_cover) {
        this.sky_cover = sky_cover;
    }
   
	public Set<NcTafSkyCover> getProbable_sky_cover() {
        return probable_sky_cover;
    }

    public void setProbable_sky_cover(Set<NcTafSkyCover> sky_cover) {
        this.probable_sky_cover = sky_cover;
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
     * 
     * @param groupValue
     * @return
     */
    public static boolean isTempGroup(String groupValue) {

        boolean isTemp = CG_TEMPO.equals(groupValue);
        isTemp = isTemp || CG_PROB_TEMPO.equals(groupValue);
        isTemp = isTemp || CG_PROB.equals(groupValue);
        return isTemp;
    }

}
