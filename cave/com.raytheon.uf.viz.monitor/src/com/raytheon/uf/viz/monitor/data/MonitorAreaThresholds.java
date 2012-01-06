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
package com.raytheon.uf.viz.monitor.data;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.common.monitor.data.ObConst.VarName;

/**
 * This class relies upon the Monitoring Area to determine the threat level.
 * This class contains the getThreatLevel method that returns the product's
 * display threat level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1999       grichard    Initial creation.
 * 3/16/2009    2047       grichard    Add threat monitoring routines.
 * Dec 24, 2009 3424       zhao        added getDualValuedThresholdMap and getSingleValuedThresholdMap
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class MonitorAreaThresholds {

    // Private constructor -- all contents must be public static
    private MonitorAreaThresholds() {
    }

    // Indicator for SafeSeas
    private static boolean rankHighSwellPeriods;

    // Map containing the monitor thresholds
    private static Map<String, MonitorThresholdsSet> zoneMonitorThresholds = new HashMap<String, MonitorThresholdsSet>();

    // Map containing the display thresholds
    private static Map<String, DisplayThresholdsSet> zoneDisplayThresholds = new HashMap<String, DisplayThresholdsSet>();

    /**
     * This method receives an observation report and a variable name, and
     * returns the threat level of that single variable.
     * 
     * @param report
     *            -- the observation report
     * @param varName
     *            -- the variable name within the report
     * @return -- the threat level
     */
    public static ObConst.ThreatLevel getThreatLevel(ObReport report,
            VarName varName) {

        ThreatLevel threatLevel = ThreatLevel.GRAY;
        ThreatLevel temp = ThreatLevel.GRAY;
        float varValue = ObConst.MISSING;

        try {
            varValue = getReportVarValue(report, varName);

            String zoneId = report.getZoneId();
            // TEMPORARILY USE DEFAULT ZONE ID, NAMELY, DEFAULT STATION.
            zoneId = ObConst.DEFAULT_STATION_NAME;
            if (report.isStationary()) {
                if (MonitoringArea.listZonesToPlatform(report.getPlatformId())
                        .isEmpty()) {
                    // use the default zone if there are no zones
                    temp = getZoneThreatLevel(zoneId, varName, varValue);
                    if (temp.ordinal() < threatLevel.ordinal()) {
                        threatLevel = temp;
                    }
                } else {
                    for (String z : MonitoringArea.listZonesToPlatform(report
                            .getPlatformId())) {
                        temp = getZoneThreatLevel(z, varName, varValue);
                        if (temp.ordinal() < threatLevel.ordinal()) {
                            threatLevel = temp;
                        }
                    }
                }
            } else {
                temp = getZoneThreatLevel(zoneId, varName, varValue);
            }
        } catch (Exception e) {
            // return the default threat level
        }

        return threatLevel;
    }

    /**
     * This method receives an observation report and a variable name, and
     * returns the threat level of that single variable.
     * 
     * @param report
     *            -- the observation report
     * @param chosenAppKey
     *            -- the application key
     * @return -- the threat level
     */
    public static ObConst.ThreatLevel getThreatLevel(ObReport report,
            ChosenAppKey chosenAppKey) {

        ThreatLevel threatLevel = ThreatLevel.GRAY;
        ThreatLevel temp = ThreatLevel.GRAY;

        try {
            if (report.isStationary()) {
                if (chosenAppKey == ChosenAppKey.SAFESEAS) {
                    if (MonitoringArea.listZonesToPlatform(
                            report.getPlatformId()).isEmpty()) {
                        // use the default zone if there are no zones
                        for (VarName v : VarName.values()) {
                            if (v == VarName.TEMPERATURE
                                    || v == VarName.WIND_CHILL
                                    || v == VarName.SNOW_DEPTH) {
                                continue;
                            } else if (v == VarName.STATIONARY) {
                                break;
                            }
                            temp = getZoneThreatLevel(
                                    ObConst.DEFAULT_STATION_NAME, v,
                                    getReportVarValue(report, v));
                            if (temp.ordinal() < threatLevel.ordinal()) {
                                threatLevel = temp;
                            }
                        }
                    } else {
                        for (String z : MonitoringArea
                                .listZonesToPlatform(report.getPlatformId())) {
                            for (VarName v : VarName.values()) {
                                if (v == VarName.TEMPERATURE
                                        || v == VarName.WIND_CHILL
                                        || v == VarName.SNOW_DEPTH) {
                                    continue;
                                } else if (v == VarName.STATIONARY) {
                                    break;
                                }
                                temp = getZoneThreatLevel(z, v,
                                        getReportVarValue(report, v));
                                if (temp.ordinal() < threatLevel.ordinal()) {
                                    threatLevel = temp;
                                }
                            }
                        }
                    }
                } else if (chosenAppKey == ChosenAppKey.SNOW) {
                    if (MonitoringArea.listZonesToPlatform(
                            report.getPlatformId()).isEmpty()) {
                        // use the default zone if there are no zones
                        for (VarName v : VarName.values()) {
                            if (v == VarName.PRIM_SWELL_HT) {
                                break;
                            }
                            temp = getZoneThreatLevel(
                                    ObConst.DEFAULT_STATION_NAME, v,
                                    getReportVarValue(report, v));
                            if (temp.ordinal() < threatLevel.ordinal()) {
                                threatLevel = temp;
                            }
                        }

                    } else {
                        for (String z : MonitoringArea
                                .listZonesToPlatform(report.getPlatformId())) {
                            for (VarName v : VarName.values()) {
                                if (v == VarName.PRIM_SWELL_HT) {
                                    break;
                                }
                                temp = getZoneThreatLevel(z, v,
                                        getReportVarValue(report, v));
                                if (temp.ordinal() < threatLevel.ordinal()) {
                                    threatLevel = temp;
                                }
                            }
                        }
                    }
                } else {
                    if (MonitoringArea.listZonesToPlatform(
                            report.getPlatformId()).isEmpty()) {
                        // use the default zone if there are no zones
                        temp = getZoneThreatLevel(ObConst.DEFAULT_STATION_NAME,
                                VarName.PRES_WX, report.getPresentWx());
                        if (temp.ordinal() < threatLevel.ordinal()) {
                            threatLevel = temp;
                        }
                        temp = getZoneThreatLevel(ObConst.DEFAULT_STATION_NAME,
                                VarName.VISIBILITY, getReportVarValue(report,
                                        VarName.VISIBILITY));
                        if (temp.ordinal() < threatLevel.ordinal()) {
                            threatLevel = temp;
                        }
                    } else {
                        for (String z : MonitoringArea
                                .listZonesToPlatform(report.getPlatformId())) {
                            temp = getZoneThreatLevel(z, VarName.PRES_WX,
                                    report.getPresentWx());
                            if (temp.ordinal() < threatLevel.ordinal()) {
                                threatLevel = temp;
                            }
                            temp = getZoneThreatLevel(z, VarName.VISIBILITY,
                                    getReportVarValue(report,
                                            VarName.VISIBILITY));
                            if (temp.ordinal() < threatLevel.ordinal()) {
                                threatLevel = temp;
                            }
                        }
                    }
                }
            } else {
                if (chosenAppKey == ChosenAppKey.SAFESEAS) {
                    String zoneId = report.getZoneId();
                    // TEMPORARILY USE DEFAULT ZONE ID, NAMELY, DEFAULT STATION.
                    zoneId = ObConst.DEFAULT_STATION_NAME;
                    for (VarName v : VarName.values()) {
                        if (v == VarName.TEMPERATURE || v == VarName.WIND_CHILL
                                || v == VarName.SNOW_DEPTH) {
                            continue;
                        } else if (v == VarName.STATIONARY) {
                            break;
                        }
                        temp = getZoneThreatLevel(zoneId, v, getReportVarValue(
                                report, v));
                        if (temp.ordinal() < threatLevel.ordinal()) {
                            threatLevel = temp;
                        }
                    }
                } else if (chosenAppKey == ChosenAppKey.SNOW) {
                    String zoneId = report.getZoneId();
                    // TEMPORARILY USE DEFAULT ZONE ID, NAMELY, DEFAULT STATION.
                    zoneId = ObConst.DEFAULT_STATION_NAME;
                    for (VarName v : VarName.values()) {
                        if (v == VarName.PRIM_SWELL_HT) {
                            break;
                        }
                        temp = getZoneThreatLevel(zoneId, v, getReportVarValue(
                                report, v));
                        if (temp.ordinal() < threatLevel.ordinal()) {
                            threatLevel = temp;
                        }
                    }
                } else {
                    String zoneId = report.getZoneId();
                    // TEMPORARILY USE DEFAULT ZONE ID, NAMELY, DEFAULT STATION.
                    zoneId = ObConst.DEFAULT_STATION_NAME;
                    temp = getZoneThreatLevel(report.getZoneId(),
                            VarName.PRES_WX, report.getPresentWx());
                    if (temp.ordinal() < threatLevel.ordinal()) {
                        threatLevel = temp;
                    }

                    temp = getZoneThreatLevel(zoneId, VarName.VISIBILITY,
                            getReportVarValue(report, VarName.VISIBILITY));
                    if (temp.ordinal() < threatLevel.ordinal()) {
                        threatLevel = temp;
                    }

                }
            }
        } catch (Exception e) {
            // return the default threat level
        }

        return threatLevel;
    }

    /**
     * Determines whether rank high swell period indicator is set.
     * 
     * @return indicator
     */
    public static boolean isRankHighSwellPeriods() {
        return rankHighSwellPeriods;
    }

    /**
     * Sets the rank high swell period indicator.
     * 
     * @param rankHighSwellPeriods
     *            -- the indicator
     */
    public static void setRankHighSwellPeriods(boolean rankHighSwellPeriods) {
        MonitorAreaThresholds.rankHighSwellPeriods = rankHighSwellPeriods;
    }

    /**
     * This method sets an entry in the zone monitor thresholds map for a given
     * zone identifier and its associated red and yellow threshold level
     * structures.
     * 
     * @param zoneId
     *            -- the zone identifier
     * @param redThresh
     *            -- the red threshold structure
     * @param yellowThresh
     *            -- the yellow threshold structure
     */
    public static void setZoneMonitorThreshold(String zoneId,
            SafeSeasThresholdStruct redThresh,
            SafeSeasThresholdStruct yellowThresh) {
        zoneMonitorThresholds.put(zoneId, new MonitorThresholdsSet(redThresh,
                yellowThresh));
    }

    /**
     * This method returns a variable's monitor threat level.
     * 
     * @param zoneId
     *            -- the zone identifier
     * @param varName
     *            -- the variable name
     * @param varValue
     *            -- the variable value
     * @return
     */
    public static ObConst.ThreatLevel getZoneThreatLevel(String zoneId,
            VarName varName, float varValue) {

        float red, red2, yellow, yellow2;
        Float[] values;

        ObConst.ThreatLevel threatLevel = ObConst.ThreatLevel.GRAY;
        // First check monitored variables for which lower values are
        // more dangerous Note that temperature is common to SAFESEAS
        // and SNOW. While SAFESEAS emphasizes higher values in the
        // display table instead of lower, only SNOW employs temperature
        // for background monitoring, so lower temperatures are assumed
        // to be red in this method. The swell period parameters rank
        // lower to higher if the "higher to lower" flag is NOT set.
        try {
            if (varName == VarName.VISIBILITY
                    || varName == VarName.WIND_CHILL
                    || varName == VarName.TEMPERATURE
                    || (varName == VarName.PRIM_SWELL_PD && !isRankHighSwellPeriods())
                    || (varName == VarName.SEC_SWELL_PD && !isRankHighSwellPeriods())) {
                red = zoneMonitorThresholds.get(zoneId).getThreshold(varName,
                        ObConst.ThreatLevel.RED);
                yellow = zoneMonitorThresholds.get(zoneId).getThreshold(
                        varName, ObConst.ThreatLevel.YELLOW);

                // Set appropriate lower boundary limit. For Vis and swell
                // periods, lowest possible value is 0, but temperatures and
                // wind chills can have valid below-zero values. Their lower
                // limit will be the MISSING value.

                float lowerBound = 0.0f;
                if (varName == VarName.TEMPERATURE
                        || varName == VarName.WIND_CHILL) {
                    // Kelvin to Fahrenheit
                    if (varName == VarName.TEMPERATURE
                            && varValue != ObConst.MISSING) {
                        varValue = 1.8f * (varValue - 273.15f) + 32.0f;
                    }
                    lowerBound = ObConst.MISSING;
                }

                if (varValue <= red && varValue >= lowerBound
                        && varValue != ObConst.MISSING) {
                    threatLevel = ObConst.ThreatLevel.RED;
                } else if (varValue > red && varValue <= yellow) {
                    threatLevel = ObConst.ThreatLevel.YELLOW;
                } else if (varValue > yellow) {
                    threatLevel = ObConst.ThreatLevel.GREEN;
                } else {
                    threatLevel = ObConst.ThreatLevel.GRAY;
                }
            } else if (varName == VarName.PRIM_SWELL_DIR
                    || varName == VarName.SEC_SWELL_DIR
                    || varName == VarName.WIND_DIR) {
                // Directional parameters. Wind direction not called for
                // monitoring by SAFESEAS or SNOW, but included here for
                // completeness and possible future implementation.

                values = zoneMonitorThresholds.get(zoneId).getThresholds(
                        varName, ObConst.ThreatLevel.RED);
                red = values[0];
                red2 = values[1];
                values = zoneMonitorThresholds.get(zoneId).getThresholds(
                        varName, ObConst.ThreatLevel.YELLOW);
                yellow = values[0];
                yellow2 = values[1];
                varValue = map360To0(varValue);

                // 0 to 360 bounds check.
                if (varValue < 0.0) {
                    return ObConst.ThreatLevel.GRAY;
                }
                // Unassigned threat level check.
                if (red == red2 && yellow == yellow2) {
                    return ObConst.ThreatLevel.GRAY;
                }

                if (red != red2) {
                    if (isThreat(red, red2, varValue)) {
                        return ObConst.ThreatLevel.RED;
                    }
                }

                if (yellow != yellow2) {
                    if (isThreat(yellow, yellow2, varValue)) {
                        return ObConst.ThreatLevel.YELLOW;
                    }
                }

                return ObConst.ThreatLevel.GREEN;

            } else {
                red = zoneMonitorThresholds.get(zoneId).getThreshold(varName,
                        ObConst.ThreatLevel.RED);
                yellow = zoneMonitorThresholds.get(zoneId).getThreshold(
                        varName, ObConst.ThreatLevel.YELLOW);

                if (varValue >= red) {
                    threatLevel = ObConst.ThreatLevel.RED;
                } else if (varValue < red && varValue >= yellow) {
                    threatLevel = ObConst.ThreatLevel.YELLOW;
                } else if (varValue < yellow && varValue >= 0) {
                    threatLevel = ObConst.ThreatLevel.GREEN;
                } else {
                    threatLevel = ObConst.ThreatLevel.GRAY;
                }
            }

        } catch (Exception e) {
            // return the default value -- see below
        }

        return threatLevel;
    }

    /**
     * This method returns a variable's monitor threat level.
     * 
     * @param zoneId
     *            -- the zone identifier
     * @param varName
     *            -- the variable name
     * @param varValue
     *            -- the variable value
     * @return
     */
    public static ObConst.ThreatLevel getZoneThreatLevel(String zoneId,
            VarName varName, String varValue) {
        // Check for present Wx. variable.
        if (varName == VarName.PRES_WX) {

            if (isObstructionReported(varValue)) {
                return ObConst.ThreatLevel.YELLOW;
            } else {
                return ObConst.ThreatLevel.GREEN;
            }
        } else {
            return ObConst.ThreatLevel.GRAY;
        }
    }

    /**
     * Method that tests the threat level for swell direction.
     * 
     * @param low
     * @param hi
     * @param dirValue
     * @return
     */
    private static boolean isThreat(float low, float hi, float dirValue) {

        // does user want this threat level?
        if (low == hi) {
            return false;
        }
        // endpoints.
        if (dirValue == low || dirValue == hi) {
            return true;
        }

        // range does not include north.
        if (low < hi) {
            return ((low < dirValue) && (dirValue < hi));
        }

        // range includes north.
        return ((low < dirValue) || (dirValue < hi));
    }

    /**
     * Method that maps a direction of 360 degrees to 0 degrees.
     * 
     * @param dir
     *            -- the direction in degrees
     * @return -- the corrected direction
     */
    private static float map360To0(float dir) {
        float direction = dir;
        if (dir > 360.0f || dir < 0.0f) {
            direction = ObConst.MISSING;
        }
        if (dir == 360.0f) {
            direction = 0.0f;
        }
        return direction;
    }

    /**
     * Method that receives a present weather string as input, and looks for
     * obstruction strings as defined as FMH-1. Blowing Snow is also sought, due
     * to its effects on visibility. If an obstruction string is found, the
     * method returns "true".
     * 
     * @param presentWx
     * @return
     */
    public static boolean isObstructionReported(final String presentWx) {
        if ((presentWx.contains("BR")) || (presentWx.contains("FG"))
                || (presentWx.contains("FU")) || (presentWx.contains("VA"))
                || (presentWx.contains("DU")) || (presentWx.contains("SA"))
                || (presentWx.contains("HZ")) || (presentWx.contains("PY"))
                || (presentWx.contains("BLSN"))) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * This method returns the report's value for a given variable name.
     * 
     * @param varName
     *            -- the variable name
     * @return -- the value within the report
     */
    public static float getReportVarValue(ObReport obReport, VarName varName) {
        float result = ObConst.MISSING;
        switch (varName) {
        case WIND_SPEED:
            result = obReport.getWindSpeed();
            break;
        case MAX_WIND_SPEED:
            result = obReport.getMaxWindSpeed();
            break;
        case GUST_SPEED:
            result = obReport.getWindGust();
            break;
        case VISIBILITY:
            result = obReport.getVisibility();
            break;
        case WAVE_HEIGHT:
            result = obReport.getHighResWaveHeight();
            break;
        case PRIM_SWELL_HT:
            result = obReport.getPSwellHeight();
            break;
        case PRIM_SWELL_PD:
            result = obReport.getPSwellPeriod();
            break;
        case PRIM_SWELL_DIR:
            result = obReport.getPSwellDir();
            result = map360To0(result);
            break;
        case SEC_SWELL_HT:
            result = obReport.getSSwellHeight();
            break;
        case SEC_SWELL_PD:
            result = obReport.getPSwellPeriod();
            break;
        case SEC_SWELL_DIR:
            result = obReport.getSSwellDir();
            result = map360To0(result);
            break;
        case TEMPERATURE:
            result = obReport.getTemperature();
            break;
        case WIND_CHILL:
            result = obReport.getWindChill();
            break;
        case SNOW_DEPTH:
            result = obReport.getSnowDepth();
            break;
        default:
            result = ObConst.MISSING;
            break;
        }
        return result;
    }
    
    /**
     * [Dec 24, 2009, zhao]
     * @param zone the zone ID
     * @param varName enumerated-type variable name
     * @return single-valued threshold map, or null if the variable 
     * name is invalid or if the map contains no mapping for the key
     */
    public static Map<ObConst.ThreatLevel,Float> getSingleValuedThresholdMap(String zone, ObConst.VarName varName) {
    	if (varName == VarName.WIND_DIR || varName == VarName.PRIM_SWELL_DIR || varName == VarName.SEC_SWELL_DIR ) {
    		return null;
    	}
    	return zoneMonitorThresholds.get(zone).getSingleValuedThresholdMap(varName);
    }
    
    /**
     * [Dec 24, 2009, zhao]
     * @param zone the zone ID
     * @param varName enumerated-type variable name 
     * @return duel-valued threshold map, or null if the variable 
     * name is invalid or if the map contains no mapping for the key
     */
    public static Map<ObConst.ThreatLevel,Float[]> getDualValuedThresholdMap(String zone, ObConst.VarName varName) {
    	if (varName != VarName.WIND_DIR || varName != VarName.PRIM_SWELL_DIR || varName != VarName.SEC_SWELL_DIR ) {
    		return null;
    	}
    	return zoneMonitorThresholds.get(zone).getDualValuedThresholdMap(varName);
    }
}
