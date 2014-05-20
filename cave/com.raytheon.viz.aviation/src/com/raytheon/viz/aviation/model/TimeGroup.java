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

package com.raytheon.viz.aviation.model;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.dataplugin.taf.TafConstants;
import com.raytheon.viz.aviation.model.CloudGroup.CloudCategory;

/**
 * TimeGroup class represents the common part of a normalized TAF and a
 * normalized METAR. The key field is the time of the observation in minutes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/13/2008    933         grichard    Initial creation.
 * 4/15/2008    934         grichard    Added wind shear.
 * 5/29/2008    937         grichard    Taf refactor first cut.
 * 5/30/2008    937         grichard    Added Weather Group.
 * 6/2/2008     937         grichard    Added date formatter methods.
 * 6/3/2008     937         grichard    Added probability to TAFs.
 * 6/4/2008     937         grichard    Ordered cloud groups by height.
 * 6/4/2008     937         grichard    Ordered wx groups by significance.
 * 6/24/2008    934         grichard    Updated ceiling thresholding.
 * 7/30/2008    1139        grichard    Added TAF Valid Period support.
 * 9/4/2008     1444        grichard    Import constants from TafConstants class.
 * 12/2/2008    1588        grichard    Updated metar guidance contents.
 * 12/3/2008    1515        grichard    Implement 30 hour TAF modifications.
 * 1/15/2009    1816        grichard    Correct TAF to conform to 10-813 NWSI.
 * 4/29/2009    1982        grichard    Correct TAF to conform to 10-813 NWSI.
 * 5/11/2009    1816        grichard    Corrected visibility for metar change.
 * May 15, 2014 3002        bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 * 
 */

public class TimeGroup implements Comparable<TimeGroup> {

    /**
     * The integer default for observations consistent with the metar (obs) and
     * taf plugins
     */
    public static final int INT_DEFAULT = -9999;

    /** Raw data from the message */
    private String rawData;

    /** WMO Header for the data */
    private String wmoHeader;

    /** Station Identifier for the data */
    private String stationId;

    /** The Change Indicator of a forecast */
    private ChangeIndicator changeIndicator;

    /** Probability percentage for PROB change group */
    private Integer probability;

    /** The Correction Indicator of a forecast */
    private String tafCorIndicator;

    /** The Amendment Indicator of a forecast */
    private String amdIndicator;

    /** Start Time of forecast/observation in minutes */
    private Calendar startTime;

    /** End Time of forecast/observation in minutes */
    private Calendar endTime;

    /** Issue date */
    private Calendar issueTime;

    /** Valid Period Start Time */
    private Calendar validPeriodStartTime;

    /** Valid Period End Time */
    private Calendar validPeriodEndTime;

    /** METAR/SPECI Report type extracted from WMO header * */
    private String reportType;

    /** The Correction Indicator of an observation */
    private String obsCorIndicator;

    /**
     * A string denoting the type of automated station (AO1 or AO2) for an
     * observation
     */
    private String autoStationType;

    /** The latitude of the station for an observation */
    private double stationLat = INT_DEFAULT;

    /** The longitude of the station for an observation */
    private double stationLon = INT_DEFAULT;

    /** The sea level pressure in millibars for an observation */
    private float seaLevelPress = INT_DEFAULT;

    /** The temperature in degrees Celsius in an observation */
    private int temperature = INT_DEFAULT;

    /** The current dew point in degrees Celsius in an observation */
    private int dewPoint = INT_DEFAULT;

    /** The altimeter reading in in/Hg in an observation */
    private float altimeter = INT_DEFAULT;

    /** The precipitation reading in inches in an hourly observation */
    private float precip1Hour = INT_DEFAULT;

    /** Any remarks contained in the TAF record */
    private String remarks;

    /** Wind direction in degrees from north */
    private String windDir;

    /** Wind speed in knots */
    private int windSpeed = INT_DEFAULT;

    /** Wind gusts in knots */
    private int windGust = INT_DEFAULT;

    /** Wind shear in knots */
    private int windShear = INT_DEFAULT;

    /** Visibility in statute miles */
    private String visibility;

    /** A string denoting the vertical visibility */
    private int vertVisibility = INT_DEFAULT;

    /** Sky Conditions */
    private ArrayList<CloudGroup> cloudGroup = new ArrayList<CloudGroup>();

    /** Weather Conditions */
    private ArrayList<WeatherGroup> weatherGroup = new ArrayList<WeatherGroup>();

    /**
     * The Flight Category of a forecast.
     */
    private FlightCategory flightCategory;

    /**
     * The Ceiling Threshold of a forecast.
     */
    private CeilingThreshold ceilingThreshold;

    /**
     * The Visibility Threshold of a forecast.
     */
    private VisibilityThreshold visibilityThreshold;

    /**
     * Tool Tip is text that contains a message, a forecast, and an observation.
     */
    private String toolTip = "";

    /**
     * Flight Categories
     */
    public static enum FlightCategory {
        /* Low Instrument Flight Rules (LIFR) */
        /* Instrument Flight Rules (IFR) */
        /* Marginal Visual Flight Rules (MVFR) */
        /* Visual Flight Rules (VFR) */
        LIFR, IFR, MVFR, VFR;
        /*
         * See: NWS Operations Manual W/OM12 Part D-31 for more information.
         */
    }

    /**
     * Method that gets the flight category for a forecast.
     * 
     * @return flightCategory
     */
    public FlightCategory getCategory() {
        return flightCategory;
    }

    /**
     * Method that sets the flight category for a forecast.
     * 
     * @param fltCat
     */
    public void setCategory(FlightCategory fltCat) {
        flightCategory = fltCat;
    }

    /**
     * Method that gets the ceiling threshold for a forecast.
     * 
     * @return ceilingThreshold
     */
    public CeilingThreshold getCigThresh() {
        return ceilingThreshold;
    }

    /**
     * Method that sets the ceiling threshold for a forecast.
     * 
     * @param ceilingThreshold
     */
    public void setCigThresh(CeilingThreshold ceilingThreshold) {
        this.ceilingThreshold = ceilingThreshold;
    }

    /**
     * Method that determines the ceiling threshold for a forecast.
     * 
     * @param cldGp
     *            -- a list of cloud groups
     */
    public void determineCigThresh(ArrayList<CloudGroup> cldGp) {
        for (CloudGroup cg : cldGp) {
            CloudCategory cldCat = cg.getCldCat();
            try {
                // Set default ceiling threshold to sunny and a million.
                ceilingThreshold = CeilingThreshold.FOURTEEN;
                // Determine actual ceiling threshold if broken or
                // overcast sky cover.
                if (cldCat == CloudCategory.BKN || cldCat == CloudCategory.OVC) {
                    if (cg.getCldHgt() <= 200) {
                        ceilingThreshold = CeilingThreshold.ONE;
                    } else if (cg.getCldHgt() <= 500) {
                        ceilingThreshold = CeilingThreshold.TWO;
                    } else if (cg.getCldHgt() <= 1000) {
                        ceilingThreshold = CeilingThreshold.THREE;
                    } else if (cg.getCldHgt() <= 3000) {
                        ceilingThreshold = CeilingThreshold.FOUR;
                    } else if (cg.getCldHgt() <= 3500) {
                        ceilingThreshold = CeilingThreshold.FIVE;
                    } else if (cg.getCldHgt() <= 4000) {
                        ceilingThreshold = CeilingThreshold.SIX;
                    } else if (cg.getCldHgt() <= 5000) {
                        ceilingThreshold = CeilingThreshold.SEVEN;
                    } else if (cg.getCldHgt() <= 8000) {
                        ceilingThreshold = CeilingThreshold.EIGHT;
                    } else if (cg.getCldHgt() <= 10000) {
                        ceilingThreshold = CeilingThreshold.NINE;
                    } else if (cg.getCldHgt() <= 12000) {
                        ceilingThreshold = CeilingThreshold.TEN;
                    } else if (cg.getCldHgt() <= 15000) {
                        ceilingThreshold = CeilingThreshold.ELEVEN;
                    } else if (cg.getCldHgt() <= 18000) {
                        ceilingThreshold = CeilingThreshold.TWELVE;
                    } else if (cg.getCldHgt() <= 20000) {
                        ceilingThreshold = CeilingThreshold.THIRTEEN;
                    } else {
                        ceilingThreshold = CeilingThreshold.FOURTEEN;
                    }
                    break;
                }
            } catch (NullPointerException e) {
                // use default ceiling information
                ceilingThreshold = CeilingThreshold.ONE;
            }
        }
    }

    /**
     * The AvnFPS Ceiling (CIG) Threshold enumeration
     */
    public static enum CeilingThreshold {
        ONE(0), TWO(200), THREE(500), FOUR(1000), FIVE(3000), SIX(3500), SEVEN(
                4000), EIGHT(5000), NINE(8000), TEN(10000), ELEVEN(12000), TWELVE(
                15000), THIRTEEN(18000), FOURTEEN(20000);
        CeilingThreshold(int value) {
            this.value = value;
        }

        /**
         * Value of Ceiling Height in units of feet.
         */
        private final int value;

        public int value() {
            return value;
        }
    }

    /**
     * Method that gets the visibility threshold for a forecast.
     * 
     * @return visibilityThreshold
     */
    public VisibilityThreshold getVsbyThresh() {
        return visibilityThreshold;
    }

    /**
     * Method that sets the visibility threshold for a forecast.
     * 
     * @param vsbyThresh
     */
    public void setVsbyThresh(VisibilityThreshold vsbyThresh) {
        visibilityThreshold = vsbyThresh;
    }

    /**
     * The AvnFPS Visibility (VIS) Threshold enumeration
     */
    public static enum VisibilityThreshold {
        ONE(0), TWO(0.5), THREE(1), FOUR(3), FIVE(5);
        VisibilityThreshold(double value) {
            this.value = value;
        }

        /**
         * Value of Visibility in units of statute miles.
         */
        private final double value;

        public double value() {
            return value;
        }
    }

    /**
     * Method that gets the visibility of a forecast.
     * 
     * @return the visibility
     */
    public String getVisibility() {
        return visibility;
    }

    /**
     * Overloaded method that sets the visibility of a forecast.
     * 
     * @param visibility
     *            the visibility to set
     */
    public void setVisibility(float visibility) {
        String[] vsbyFloatRepresentation = String.valueOf(visibility).split(
                "\\.");
        this.visibility = vsbyFloatRepresentation[0];
    }

    /**
     * Overloaded method that sets the visibility of a forecast.
     * 
     * @param visibility
     *            the visibility to set
     */
    public void setVisibility(String visibility) {
        this.visibility = visibility;
    }

    /**
     * Method that determines the visibility threshold of a forecast.
     */
    public void determineVisibilityThreshold() {
        if (getVisibility().matches("[0]|(1\\u002F16)|([1]\\u002F[248])")) {
            setVsbyThresh(VisibilityThreshold.ONE);
        } else if (getVisibility().matches("([3]\\u002F[4])|[1]")) {
            setVsbyThresh(VisibilityThreshold.TWO);
        } else if (getVisibility()
                .matches(
                        "([12]\\u0020[1]\\u002F[24])|([12]\\u0020[3]\\u002F[4])|([23])")) {
            setVsbyThresh(VisibilityThreshold.THREE);
        } else if (getVisibility()
                .matches(
                        "([34]\\u0020[1]\\u002F[24])|([34]\\u0020[3]\\u002F[4])|([45])")) {
            setVsbyThresh(VisibilityThreshold.FOUR);
        } else {
            setVsbyThresh(VisibilityThreshold.FIVE);
        }
    }

    /**
     * Method that determines the flight category of a report. The method
     * applied is based on AvnLib.py's flightCategory method.
     * 
     * @param vsby
     *            -- the visibility in statute miles
     * @param cig
     *            -- the ceiling category
     */
    public void determineFlightCategory(float vsby, CeilingThreshold cig) {

        if (cig.value < 500 || vsby < 1.0) {
            flightCategory = FlightCategory.LIFR;
        } else if (cig.value < 1000 || vsby < 3.0) {
            flightCategory = FlightCategory.IFR;
        } else if (cig.value < 3100 || vsby < 6.0) {
            flightCategory = FlightCategory.MVFR;
        } else {
            flightCategory = FlightCategory.VFR;
        }
    }

    /**
     * Method that gets the vertical visibility of a forecast.
     * 
     * @return the vertical visibility
     */
    public int getVertVisibility() {
        return vertVisibility;
    }

    /**
     * Method that sets the vertical visibility of a forecast.
     * 
     * @param vertVisibility
     *            the vertical visibility to set
     */
    public void setVertVisibility(int vertVisibility) {
        this.vertVisibility = vertVisibility;
    }

    /**
     * Method that gets the wind direction of a forecast.
     * 
     * @return the windDir
     */
    public String getWindDir() {
        return windDir;
    }

    /**
     * Method that sets the wind direction of a forecast.
     * 
     * @param windDir
     *            the windDir to set
     */
    public void setWindDir(String windDir) {
        this.windDir = windDir;
    }

    /**
     * Method that gets the wind speed of a forecast.
     * 
     * @return the windSpeed
     */
    public int getWindSpeed() {
        return windSpeed;
    }

    /**
     * Method that sets the wind speed of a forecast.
     * 
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(int windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * Method that gets the wind gust of a forecast.
     * 
     * @return the windGust
     */
    public int getWindGust() {
        return windGust;
    }

    /**
     * Method that sets the wind gust of a forecast.
     * 
     * @param windGust
     *            the windGust to set
     */
    public void setWindGust(int windGust) {
        this.windGust = windGust;
    }

    /**
     * Method that gets the wind shear of a forecast.
     * 
     * @return the windShear
     */
    public int getWindShear() {
        return windShear;
    }

    /**
     * Method that sets the wind shear of a forecast.
     * 
     * @param windShear
     *            the windShear to set
     */
    public void setWindShear(int windShear) {
        this.windShear = windShear;
    }

    /**
     * Method that gets the start time of a forecast/observation.
     * 
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * Method that sets the start time of a forecast/observation.
     * 
     * @param startTime
     *            the start time of the forecast/observation to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * Method that gets the start time of a forecast/observation in DD format.
     * 
     * @return the startTime
     */
    public String getStartTimeInDdFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("dd");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(startTime.getTime()));
    }

    /**
     * Method that gets the start time of a forecast/observation in HH format.
     * 
     * @return the startTime
     */
    public String getStartTimeInHhFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("HH");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(startTime.getTime()));
    }

    /**
     * Method that gets the start time of a forecast/observation in DDHH format.
     * 
     * @return the startTime
     */
    public String getStartTimeInDdHhFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("ddHH");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(startTime.getTime())) + "/";
    }

    /**
     * Method that gets the start time of a forecast/observation in DDHHMM
     * format.
     * 
     * @return the startTime
     */
    public String getStartTimeInDdHhMmFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(startTime.getTime()));
    }

    /**
     * Method that gets the end time of a forecast/observation.
     * 
     * @return the endTime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * Method that sets the end time of a forecast/observation.
     * 
     * @param endTime
     *            the end time of the forecast/observation to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * Method that gets the end time of a forecast/observation in ddHH format.
     * 
     * @return the endTime
     */
    public String getEndTimeInDdHhFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("ddHH");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(endTime.getTime()));
    }

    /**
     * Method that gets the end time of a forecast/observation in HH format.
     * 
     * @return the endTime
     */
    public String getEndTimeInHhFormat() {
        SimpleDateFormat formatter = new SimpleDateFormat("HH");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return (formatter.format(endTime.getTime()));
    }

    /**
     * Method that gets the issue time for a forecast.
     * 
     * @return the issueTime
     */
    public Calendar getIssueTime() {
        return issueTime;
    }

    /**
     * Method that sets the issue time for a forecast.
     * 
     * @param issueTime
     *            the issueTime to set
     */
    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * Method that gets the remarks for a forecast.
     * 
     * @return the remarks
     */
    public String getRemarks() {
        return remarks;
    }

    /**
     * Method that sets the remarks for a forecast.
     * 
     * @param remarks
     *            the remarks to set
     */
    public void setRemarks(String remarks) {
        this.remarks = remarks;
    }

    /**
     * Method that gets the cloud group for a forecast.
     * 
     * @return the cloud group
     */
    public ArrayList<CloudGroup> getCloudGroup() {
        return cloudGroup;
    }

    /**
     * Method that sets the cloud group for a forecast.
     * 
     * @param the
     *            cloud group
     */
    public void setCloudGroup(ArrayList<CloudGroup> cloudGroup) {
        this.cloudGroup = cloudGroup;
    }

    /**
     * Method that adds the cloud group for a forecast.
     * 
     * @param cloudGp
     *            the cloud group to add
     */
    public void addCloudGroup(CloudGroup cloudGp) {
        // TODO Enforce rules from NWSI 10-813
        // http://www.nws.noaa.gov/directives/.
        // Clouds sort by height, so height is a key for performing an insertion
        // sort by cloud height.
        // cloudGroup.add(cloudGp);
        if (cloudGroup.isEmpty()) {
            cloudGroup.add(cloudGp);
        } else {
            if (cloudGp.getCldHgt() >= cloudGroup.get(cloudGroup.size() - 1)
                    .getCldHgt()) {
                cloudGroup.add(cloudGp);
            } else {
                cloudGroup.add(cloudGroup.size() - 1, cloudGp);
            }
        }
    }

    /**
     * Method to aggregate the cloud group based on the observation.
     * 
     * @param mr
     *            -- the metar record
     */
    public void aggregateCloudGroup(MetarRecord mr) {
        for (SkyCover sc : mr.getSkyCoverage()) {
            CloudGroup cldGp = new CloudGroup();
            cldGp.setCldCat(CloudGroup.CloudCategory.valueOf(sc.getType()));
            if (sc.getHeight() != null) {
                cldGp.setCldHgt(sc.getHeight());
            } else {
                cldGp.setCldHgt(-1);
            }
            if (sc.getGenus() != null) {
                cldGp.setGenus(sc.getGenus());
            }
            addCloudGroup(cldGp);
        }
    }

    /**
     * Method that gets the weather group for a forecast.
     * 
     * @return the weather group
     */
    public ArrayList<WeatherGroup> getWeatherGroup() {
        return weatherGroup;
    }

    /**
     * Method that sets the weather group for a forecast.
     * 
     * @param the
     *            weather group
     */
    public void setWeatherGroup(ArrayList<WeatherGroup> weatherGroup) {
        this.weatherGroup = weatherGroup;
    }

    /**
     * Method that adds the weather group for a forecast.
     * 
     * @param wxGp
     *            the weather group to add
     */
    public void addWeatherGroup(WeatherGroup wxGp) {
        // TODO Enforce rules from NWSI 10-813
        // http://www.nws.noaa.gov/directives/.
        // This document contains some hints as to how to combine elements.
        // weatherGroup.add(wxGp);
        if (weatherGroup.isEmpty()) {
            weatherGroup.add(wxGp);
        } else {
            if ((!(wxGp.getDescriptor().equals("TS")))
                    && (!(wxGp.getDescriptor().equals("SH")))
                    && (!(wxGp.getPrecipitation().equals("RA")))
                    && (!(wxGp.getPrecipitation().equals("DZ")))
                    && ((!(wxGp.getPrecipitation().equals("SN"))) || (((wxGp
                            .getPrecipitation().equals("SN"))) && ((!(wxGp
                            .getDescriptor().equals(""))))))) {
                weatherGroup.add(wxGp);
            } else if ((!weatherGroup.get(0).getDescriptor().equals("TS"))
                    && (!weatherGroup.get(0).getDescriptor().equals("SH"))
                    && (!weatherGroup.get(0).getPrecipitation().equals("RA"))
                    && (!weatherGroup.get(0).getPrecipitation().equals("DZ"))) {
                weatherGroup.add(0, wxGp);
            } else {
                weatherGroup.add(1, wxGp);
            }
        }
    }

    /**
     * Method that gets the change indicator for a forecast.
     * 
     * @return changeIndicator
     */
    public ChangeIndicator getChgInd() {
        return changeIndicator;
    }

    /**
     * Method that sets the change indicator for a forecast.
     * 
     * @param chgInd
     */
    public void setChgInd(ChangeIndicator chgInd) {
        changeIndicator = chgInd;
    }

    /**
     * Change Indicators for forecasts
     */
    public static enum ChangeIndicator {
        /* From (FM) */
        /* Becoming (BECMG) */
        /* Tempo (TEMPO) */
        /* Probability (PROB) */
        FM(TafConstants.CG_FM), BECMG(TafConstants.CG_BECMG), INITIAL(
                TafConstants.CG_INITIAL), TEMPO(TafConstants.CG_TEMPO), PROB(
                TafConstants.CG_PROB), PROB_TEMPO(TafConstants.CG_PROB_TEMPO);
        /*
         * See: NWS Operations Manual W/OM12 Part D-31 for more information.
         */

        ChangeIndicator(String value) {
            this.value = value;
        }

        private final String value;

        public String value() {
            return value;
        }
    }

    /**
     * Method that gets the correction indicator for a forecast.
     * 
     * @return the tafCorIndicator
     */
    public String getTafCorIndicator() {
        return tafCorIndicator;
    }

    /**
     * Method that sets the correction indicator for a forecast.
     * 
     * @param corIndicator
     *            the tafCorIndicator to set
     */
    public void setTafCorIndicator(String corIndicator) {
        this.tafCorIndicator = corIndicator;
    }

    /**
     * Method that gets the amendment indicator for a forecast.
     * 
     * @return the amdIndicator
     */
    public String getAmdIndicator() {
        return amdIndicator;
    }

    /**
     * Method that sets the amendment indicator for a forecast.
     * 
     * @param amdIndicator
     *            the amdIndicator to set
     */
    public void setAmdIndicator(String amdIndicator) {
        this.amdIndicator = amdIndicator;
    }

    /**
     * Method that gets the raw data for a forecast or observation.
     * 
     * @return the rawData
     */
    public String getRawData() {
        return rawData;
    }

    /**
     * Method that sets the raw data for a forecast or observation.
     * 
     * @param rawData
     */
    public void setRawData(String rawData) {
        this.rawData = rawData;
    }

    /**
     * Method that gets the station identifier for a forecast.
     * 
     * @return the station identifier
     */
    public String getStationId() {
        return stationId;
    }

    /**
     * Method that sets the station identifier for a forecast.
     * 
     * @param stationID
     */
    public void setStationId(String stationID) {
        this.stationId = stationID;
    }

    /**
     * Method that gets the WMO header for a forecast.
     * 
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Method that sets the WMO header for a forecast.
     * 
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * Method that gets the tool tip for a forecast.
     * 
     * @return tool tip
     */
    public String getToolTip() {
        return toolTip;
    }

    /**
     * Method that sets the tool tip for a forecast.
     * 
     * @param tip
     *            -- the tool tip
     */
    public void setToolTip(String tip) {
        toolTip = tip;
    }

    /**
     * Method that gets the probability of a forecast.
     * 
     * @return the probability
     */
    public Integer getProbability() {
        return probability;
    }

    /**
     * Method that sets the probability of a forecast.
     * 
     * @param probability
     *            the probability to set
     */
    public void setProbability(Integer probability) {
        this.probability = probability;
    }

    /**
     * Method that gets the report type for an observation.
     * 
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * Method that sets the report type for an observation.
     * 
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * Method that gets the correction indicator for an observation.
     * 
     * @return the obsCorIndicator
     */
    public String getObsCorIndicator() {
        return obsCorIndicator;
    }

    /**
     * Method that sets the correction indicator for an observation.
     * 
     * @param corIndicator
     *            the obsCorIndicator to set
     */
    public void setObsCorIndicator(String corIndicator) {
        this.obsCorIndicator = corIndicator;
    }

    /**
     * Method that gets the temperature for an observation.
     * 
     * @return the temperature
     */
    public int getTemperature() {
        return temperature;
    }

    /**
     * Method that sets the temperature for an observation.
     * 
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(int temperature) {
        this.temperature = temperature;
    }

    /**
     * Method that gets the dew point for an observation.
     * 
     * @return the dewPoint
     */
    public int getDewPoint() {
        return dewPoint;
    }

    /**
     * Method that sets the dew point for an observation.
     * 
     * @param dewPoint
     *            the dewPoint to set
     */
    public void setDewPoint(int dewPoint) {
        this.dewPoint = dewPoint;
    }

    /**
     * Method that gets the altimeter for an observation.
     * 
     * @return the altimeter
     */
    public float getAltimeter() {
        return altimeter;
    }

    /**
     * Method that sets the altimeter for an observation.
     * 
     * @param altimeter
     *            the altimeter to set
     */
    public void setAltimeter(float altimeter) {
        this.altimeter = altimeter;
    }

    /**
     * Method that gets the auto station type for an observation.
     * 
     * @return the autoStationType
     */
    public String getAutoStationType() {
        return autoStationType;
    }

    /**
     * Method that sets the auto station type for an observation.
     * 
     * @param autoStationType
     *            the autoStationType to set
     */
    public void setAutoStationType(String autoStationType) {
        this.autoStationType = autoStationType;
    }

    /**
     * Method that gets the station latitude for an observation.
     * 
     * @return the stationLat
     */
    public double getStationLat() {
        return stationLat;
    }

    /**
     * Method that sets the station latitude for an observation.
     * 
     * @param stationLat
     *            the stationLat to set
     */
    public void setStationLat(double stationLat) {
        this.stationLat = stationLat;
    }

    /**
     * Method that gets the station longitude for an observation.
     * 
     * @return the stationLon
     */
    public double getStationLon() {
        return stationLon;
    }

    /**
     * Method that sets the station longitude for an observation.
     * 
     * @param stationLon
     *            the stationLon to set
     */
    public void setStationLon(double stationLon) {
        this.stationLon = stationLon;
    }

    /**
     * Method that gets the sea level pressure for an observation.
     * 
     * @return the seaLevelPress
     */
    public float getSeaLevelPress() {
        return seaLevelPress;
    }

    /**
     * Method that sets the sea level pressure for an observation.
     * 
     * @param seaLevelPress
     *            the seaLevelPress to set
     */
    public void setSeaLevelPress(float seaLevelPress) {
        this.seaLevelPress = seaLevelPress;
    }

    /**
     * Method that gets the one hour precipitation for an observation.
     * 
     * @return the precip1Hour
     */
    public float getPrecip1Hour() {
        return precip1Hour;
    }

    /**
     * Method that sets the one hour precipitation for an observation.
     * 
     * @param precip1Hour
     *            the precip1Hour to set
     */
    public void setPrecip1Hour(float precip1Hour) {
        this.precip1Hour = precip1Hour;
    }

    /**
     * Method that gets the valid period start time.
     * 
     * @return the validPeriodStartTime
     */
    public Calendar getValidPeriodStartTime() {
        return validPeriodStartTime;
    }

    /**
     * Method that sets the valid period start time.
     * 
     * @param validPeriodStartTime
     *            the validPeriodStartTime to set
     */
    public void setValidPeriodStartTime(Calendar validPeriodStartTime) {
        this.validPeriodStartTime = validPeriodStartTime;
    }

    /**
     * Method that gets the valid period end time.
     * 
     * @return the validPeriodEndTime
     */
    public Calendar getValidPeriodEndTime() {
        return validPeriodEndTime;
    }

    /**
     * Method that sets the valid period end time.
     * 
     * @param validPeriodEndTime
     *            the validPeriodEndTime to set
     */
    public void setValidPeriodEndTime(Calendar validPeriodEndTime) {
        this.validPeriodEndTime = validPeriodEndTime;
    }

    /**
     * Method that implements comparable interface for sorting.
     */
    @Override
    public int compareTo(TimeGroup t) {
        return startTime.compareTo(t.startTime);
    }

}