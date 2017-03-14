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
package com.raytheon.viz.hydrocommon.data;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for packaging the gage data neatly.
 * 
 * <pre>
 * SOFTWARE HISTOR
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05Nov2008    ---        dhladky     Initial Creation
 * 14Mar2012    1790       rferrel     Fix Comparable to remove eclipse warnings.
 * Aug 05, 2015 4486       rjpeter     Changed Timestamp to Date.
 * 05Oct2015    17978      lbousaidi   Added getParamCode(), getShefDurCode(), convertDur(), 
 *                                     getDataFormat().
 * May 03, 2016 5623       bkowal      Cleanup and formatting.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class GageData implements Comparable<GageData> {

    public static final double MISSING = HydroConstants.MISSING_VALUE;

    /**
     * Location ID.
     */
    private String lid = null;

    /**
     * Site name.
     */
    private String name = null;

    /**
     * The latitude of the station point.
     */
    private double lat = MISSING;

    /**
     * The longitude of the station point.
     */
    private double lon = MISSING;

    /**
     * Gage elevation.
     */
    private double elevation = MISSING;

    /**
     * River ID.
     */
    private String riverID = null;

    private Calendar time = null;

    /**
     * Physical Element.
     */
    private String pe = null;

    /**
     * Duration.
     */
    private long dur;

    /**
     * Type source.
     */
    private String ts = null;

    /**
     * Extremum.
     */
    private String extremum;

    /**
     * Probability.
     */
    private double probability;

    /**
     * SHEF quality code.
     */
    private String shefQualCode = null;

    /**
     * Quality code.
     */
    private long quality_code;

    /**
     * Value.
     */
    private double value;

    /**
     * Second Value.
     */
    private double value2;

    /**
     * Valid Time.
     */
    private Date validtime;

    /**
     * Forecast Basis time.
     */
    private Date basistime;

    /**
     * Indicates whether to show this value or filter it.
     */
    private boolean use;

    /**
     * This is the riverstatus threat index. It may have the following values:
     * "R" - MOFO stage/flow is at or above flood stage/flow. Red. "Y" - MOFO
     * stage/flow is at or above action stage/flow. Yellow. "G" - MOFO
     * stage/flow is below action stage/flow. Green. "M" - MOFO stage/flow is
     * available, but action or flood stage is missing. "Z" - Threat is not
     * available. Missing.
     */
    private ThreatIndex threatIndex = null;

    /**
     * Minor flood category, also the flood stage.
     */
    private double minor_stage = MISSING;

    private double moderate_stage = MISSING;

    private double major_stage = MISSING;

    private double minor_flow = MISSING;

    private double moderate_flow = MISSING;

    private double major_flow = MISSING;

    /**
     * The display class of this station.
     */
    private String dispClass = null;

    /**
     * Pixel value corresponding to the longitude.
     */
    private int x;

    /**
     * Pixel value corresponding to the latitude.
     */
    private int y;

    /**
     * X direction shift of a station for decluttering purposes.
     */
    int x_shift;

    /**
     * Y direction shift of a station for decluttering purposes.
     */
    int y_shift;

    /**
     * The coordinate of this gage.
     */
    private Coordinate coordinate = null;

    public static final String OFFICIAL_RIVER = "F";

    public static final String RESERVOIR = "D";

    public static final String RIVER = "R";

    public static final String PRECIP = "P";

    public static final String SNOW = "S";

    public static final String TEMPERATURE = "T";

    public static final String OTHER = "O";

    public static final String UNKNOWN = "U";

    private boolean forecastSite = false;

    private List<Colorvalue> colorSet = null;

    public static enum ThreatIndex {
        /*
         * "R" - MOFO stage/flow is at or above flood stage/flow. Red. "Y" -
         * MOFO stage/flow is at or above action stage/flow. Yellow. "G" - MOFO
         * stage/flow is below action stage/flow. Green. "M" - MOFO stage/flow
         * is available, but action or flood stage is missing. "Z" - Threat is
         * not available. Missing.
         */
        THREAT_MISSING_DATA("Z"), THREAT_MISSING_STAGE("M"), THREAT_NONE("G"), THREAT_ACTION(
                "Y"), THREAT_FLOOD("R");

        private String threatIndex;

        ThreatIndex(String value) {
            threatIndex = value;
        }

        public String getThreatIndex() {
            return threatIndex;
        }
    }

    public GageData() {
        getColorSet();
    }

    /**
     * public constructor
     * 
     * @param data
     */
    public GageData(Object[] data) {
        if (data[0] != null) {
            setLid((String) data[0]);
        }
        if (data[1] != null) {
            setName((String) data[1]);
        }
        if (data[2] != null) {
            setLon((Double) data[2]);
        }
        if (data[3] != null) {
            setLat((Double) data[3]);
        }
        if (data[4] != null) {
            setElevation((Double) data[4]);
        }
        if (data[5] != null) {
            setRiverID((String) data[5]);
        }
        if (data[6] != null) {
            setValue((Double) data[6]);
        }
        if (data[7] != null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTimeInMillis(((Date) data[7]).getTime());
            setTime(cal);
        }
        if (data[8] != null) {
            setPe((String) data[8]);
        }
        if (data[9] != null) {
            setMinorStage((Double) data[9]);
        }
        if (data[10] != null) {
            setModerateStage((Double) data[10]);
        }
        if (data[11] != null) {
            setMajorStage((Double) data[11]);
        }
        if (data[12] != null) {
            setModerateFlow((Double) data[12]);
        }
        if (data[13] != null) {
            setMajorFlow((Double) data[13]);
        }

        getColorSet();
    }

    /**
     * 
     * @param lid
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * 
     * @return
     */
    public String getLid() {
        return lid;
    }

    /**
     * 
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * 
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * 
     * @param lon
     */
    public void setLon(double lon) {
        // west leaning coordinates
        if (lon > 0) {
            lon *= -1;
        }
        this.lon = lon;
    }

    /**
     * 
     * @return
     */
    public double getLon() {
        return lon;
    }

    /**
     * 
     * @param lat
     */
    public void setLat(double lat) {
        this.lat = lat;
    }

    /**
     * 
     * @return
     */
    public double getLat() {
        return lat;
    }

    /**
     * sets the elevation
     * 
     * @param elev
     */
    public void setElevation(double elev) {
        elevation = elev;
    }

    /**
     * gets the elevation
     * 
     * @return
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * gets the riverID
     * 
     * @return
     */
    public String getRiverID() {
        return riverID;
    }

    /**
     * sets the riverID
     * 
     * @param elevation
     */
    public void setRiverID(String riverID) {
        this.riverID = riverID;
    }

    /**
     * 
     * @return
     */
    public double getGageValue() {
        return value;
    }

    /**
     * 
     * @return
     */
    public double getGageValue2() {
        return value2;
    }

    /**
     * 
     * @param time
     */
    public void setTime(Calendar time) {
        this.time = time;
    }

    /**
     * 
     * @return
     */
    public Calendar getTime() {
        return time;
    }

    /**
     * Sets the PE
     * 
     * @param pe
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * Gets the PE string.
     * 
     * @return
     */
    public String getPe() {
        return pe;
    }

    /**
     * 
     * @param minor_stage
     */
    public void setMinorStage(double minor_stage) {
        this.minor_stage = minor_stage;
    }

    /**
     * 
     * @return
     */
    public double getMinorStage() {
        return minor_stage;
    }

    /**
     * 
     * @param moderate_stage
     */
    public void setModerateStage(double moderate_stage) {
        this.moderate_stage = moderate_stage;
    }

    /**
     * 
     * @return
     */
    public double getModerateStage() {
        return moderate_stage;
    }

    /**
     * 
     * @param major_stage
     */
    public void setMajorStage(double major_stage) {
        this.major_stage = major_stage;
    }

    /**
     * 
     * @return
     */
    public double getMajorStage() {
        return major_stage;
    }

    /**
     * 
     * @param minorFlow
     */
    public void setMinorFlow(double minorFlow) {
        minor_flow = minorFlow;
    }

    /**
     * 
     * @return
     */
    public double getMinorFlow() {
        return minor_flow;
    }

    /**
     * 
     * @param moderate_flow
     */
    public void setModerateFlow(double moderate_flow) {
        this.moderate_flow = moderate_flow;
    }

    /**
     * 
     * @return
     */
    public double getModerateFlow() {
        return moderate_flow;
    }

    /**
     * 
     * @param major_flow
     */
    public void setMajorFlow(double major_flow) {
        this.major_flow = major_flow;
    }

    /**
     * 
     * @return
     */
    public double getMajorFlow() {
        return major_flow;
    }

    /**
     * Get the color
     * 
     * @return The RGB color
     */
    public RGB getColor() {
        getColorSet();

        /* Default to No Data, 0 = no data */
        String colorName = colorSet.get(0).getColorname().getColorName();
        RGB returnColor = RGBColors.getRGBColor(colorName);
        if (threatIndex == null) {
            return returnColor;
        }

        switch (threatIndex) {
        case THREAT_MISSING_DATA:
            returnColor = RGBColors.getRGBColor(colorName);
            break;

        case THREAT_MISSING_STAGE:
            colorName = colorSet.get(1).getColorname().getColorName();
            returnColor = RGBColors.getRGBColor(colorName);
            break;

        case THREAT_NONE:
            colorName = colorSet.get(2).getColorname().getColorName();
            returnColor = RGBColors.getRGBColor(colorName);
            break;

        case THREAT_ACTION:
            colorName = colorSet.get(3).getColorname().getColorName();
            returnColor = RGBColors.getRGBColor(colorName);
            break;

        case THREAT_FLOOD:
            colorName = colorSet.get(4).getColorname().getColorName();
            returnColor = RGBColors.getRGBColor(colorName);
            break;

        default:
            colorName = colorSet.get(0).getColorname().getColorName();
            returnColor = RGBColors.getRGBColor(colorName);
            break;
        }

        return returnColor;
    }

    private void getColorSet() {
        String userId = System.getProperty("user.name");

        colorSet = HydroDisplayManager.getInstance().getGageColorMap(userId,
                "HEIGHT", 3600);
    }

    /**
     * @return the dur
     */
    public long getDur() {
        return dur;
    }

    /**
     * @param dur
     *            the dur to set
     */
    public void setDur(long dur) {
        this.dur = dur;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the extremum
     */
    public String getExtremum() {
        return extremum;
    }

    /**
     * @param extremum
     *            the extremum to set
     */
    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the probability
     */
    public double getProbability() {
        return probability;
    }

    /**
     * @param probability
     *            the probability to set
     */
    public void setProbability(double probability) {
        this.probability = probability;
    }

    /**
     * @return the shefQualCode
     */
    public String getShefQualCode() {
        return shefQualCode;
    }

    /**
     * @param shefQualCode
     *            the shefQualCode to set
     */
    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

    /**
     * @return the quality_code
     */
    public long getQuality_code() {
        return quality_code;
    }

    /**
     * @param quality_code
     *            the quality_code to set
     */
    public void setQuality_code(long quality_code) {
        this.quality_code = quality_code;
    }

    /**
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @return the value2
     */
    public double getValue2() {
        return value2;
    }

    /**
     * @param value2
     *            the value2 to set
     */
    public void setValue2(double value2) {
        this.value2 = value2;
    }

    /**
     * @return the validtime
     */
    public Date getValidtime() {
        return validtime;
    }

    /**
     * @param validtime
     *            the validtime to set
     */
    public void setValidtime(Date validtime) {
        this.validtime = validtime;
    }

    /**
     * @return the basistime
     */
    public Date getBasistime() {
        return basistime;
    }

    /**
     * @param basistime
     *            the basistime to set
     */
    public void setBasistime(Date basistime) {
        this.basistime = basistime;
    }

    /**
     * @return the use
     */
    public boolean isUse() {
        return use;
    }

    /**
     * @param use
     *            the use to set
     */
    public void setUse(boolean use) {
        this.use = use;
    }

    /**
     * @return the threatIndex
     */
    public ThreatIndex getThreatIndex() {
        return threatIndex;
    }

    /**
     * @param threatIndex
     *            the threatIndex to set
     */
    public void setThreatIndex(ThreatIndex threatIndex) {
        this.threatIndex = threatIndex;
    }

    /**
     * @return the dispClass
     */
    public String getDispClass() {
        return dispClass;
    }

    /**
     * @param dispClass
     *            the dispClass to set
     */
    public void setDispClass(String dispClass) {
        this.dispClass = dispClass;
    }

    /**
     * @return the x
     */
    public int getX() {
        return x;
    }

    /**
     * @param x
     *            the x to set
     */
    public void setX(int x) {
        this.x = x;
    }

    /**
     * @return the y
     */
    public int getY() {
        return y;
    }

    /**
     * @param y
     *            the y to set
     */
    public void setY(int y) {
        this.y = y;
    }

    /**
     * @return the x_shift
     */
    public int getX_shift() {
        return x_shift;
    }

    /**
     * @param x_shift
     *            the x_shift to set
     */
    public void setX_shift(int x_shift) {
        this.x_shift = x_shift;
    }

    /**
     * @return the y_shift
     */
    public int getY_shift() {
        return y_shift;
    }

    /**
     * @param y_shift
     *            the y_shift to set
     */
    public void setY_shift(int y_shift) {
        this.y_shift = y_shift;
    }

    /**
     * @return the forecastSite
     */
    public boolean isForecastSite() {
        return forecastSite;
    }

    /**
     * @param forecastSite
     *            the forecastSite to set
     */
    public void setForecastSite(boolean forecastSite) {
        this.forecastSite = forecastSite;
    }

    /**
     * @return the coordinate
     */
    public Coordinate getCoordinate() {
        return coordinate;
    }

    /**
     * @param coordinate
     *            the coordinate to set
     */
    public void setCoordinate(Coordinate coordinate) {
        this.coordinate = coordinate;
    }

    @Override
    public int compareTo(GageData o) {
        int retVal = 0;

        if (lid.compareTo(o.getLid()) > 0) {
            retVal = 1;
        } else if (lid.compareTo(o.getLid()) == 0) {
            retVal = 0;
        } else {
            retVal = -1;
        }

        return retVal;
    }

    @Override
    public String toString() {
        return this.getLid();
    }

    /**
     * Get Parameter Code
     * 
     * @return String Parameter Code
     * 
     */
    public String getParamCode() {
        return getPe() + getShefDurCode() + getTs() + getExtremum();
    }

    /**
     * Get Shef Duration Code
     * 
     * @return String Shef Duration Code
     * 
     */
    public String getShefDurCode() {

        String shefDurCode;
        if (getPe().equalsIgnoreCase("PC")) {

            // PC is always "I", but sometimes the duration might have been
            // screwed up

            shefDurCode = "I";
        } else {
            shefDurCode = convertDur((int) getDur());
            if (shefDurCode == null) {
                shefDurCode = "?";
            }
        }
        return shefDurCode;

    }

    /**
     * Convert duration int to String character.
     * 
     * @param dur
     *            The duration value
     * @return The single character duration value
     */
    public static String convertDur(int dur) {
        String value = null;

        switch (dur) {
        case 0:
            value = "I";
            break;
        case 1:
            value = "U";
            break;
        case 5:
            value = "E";
            break;
        case 10:
            value = "G";
            break;
        case 15:
            value = "C";
            break;
        case 30:
            value = "J";
            break;
        case 1001:
            value = "H";
            break;
        case 1002:
            value = "B";
            break;
        case 1003:
            value = "T";
            break;
        case 1004:
            value = "F";
            break;
        case 1006:
            value = "Q";
            break;
        case 1008:
            value = "A";
            break;
        case 1012:
            value = "K";
            break;
        case 1018:
            value = "L";
            break;
        case 2001:
            value = "D";
            break;
        case 2007:
            value = "W";
            break;
        case 3001:
            value = "M";
            break;
        case 4001:
            value = "Y";
            break;
        case 5004:
            value = "P";
            break;
        case 5001:
            value = "S";
            break;
        case 5002:
            value = "R";
            break;
        case 5005:
            value = "X";
            break;
        }

        return value;
    }

    /**
     * Get data format
     * 
     * @param pe
     *            - element type
     * 
     * @return String - format string
     */
    public static String getDataFormat(String pe) {
        String format = "6.2f";

        if (pe.toUpperCase().startsWith("H")) {
            /* Height data */
            format = "%6.2f";
        } else if (pe.toUpperCase().startsWith("P")) {
            /* Precip/Pressure data */
            format = "%6.2f";
        } else if (pe.toUpperCase().startsWith("T")) {
            /* Temperature data */
            format = "%6.0f";
        } else if (pe.toUpperCase().startsWith("S")) {
            /* Snow data */
            if (pe.equalsIgnoreCase("SL")) {
                format = "%6.2f";
            } else {
                format = "%6.1f";
            }
        } else if (pe.toUpperCase().startsWith("U")) {
            /* Wind data */
            if (pe.equalsIgnoreCase("UQ")) {
                format = "%8.4f";
            } else {
                format = "%6.0f";
            }
        } else if (pe.toUpperCase().startsWith("X")) {
            /* Weather data */
            format = "%5.0f";
        } else if (pe.toUpperCase().startsWith("Q")) {
            /* Flow/Runoff data */
            if (!pe.equalsIgnoreCase("QB")) {
                format = "%6.0f";
            } else {
                format = "%6.2f";
            }
        }

        return format;
    }
}