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
package com.raytheon.uf.viz.monitor.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.wxmath.CalcRH;
import com.raytheon.uf.viz.monitor.data.ObReport;

/**
 * This class contains utility methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009 1999       grichard    Initial creation.
 * 2/25/2009    2047       grichard    Added SNOW report generation method.
 * Jan 19, 2010 4240       zhao        Modified generateObReportSnow method
 * Aug 14, 2013 2262       dgilling    Use new wxmath method for calcrh.
 * Oct 23, 2013 2361       njensen     Removed two unused methods
 * Jan 06, 2014 2653       skorolev    Removed not used methods.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class ObUtil {

    // Private constructor -- all contents must be public static
    private ObUtil() {
    }

    /**
     * Method that fetches the time of a threat.
     * 
     * @return Date
     */
    public static Date getThreatTime() {
        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar threatTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        threatTime.setTime(now);
        return threatTime.getTime();
    }

    /**
     * Method that fetches the time of an aged out observation.
     * 
     * @return Date
     */
    public static Date getDropTime() {
        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar dropTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        dropTime.setTime(now);
        dropTime.add(Calendar.HOUR, -(ObConst.THREAT_INTERVAL_HOURS));
        return dropTime.getTime();
    }

    /**
     * Method that gets the current time.
     * 
     * @return current time.
     */
    public static Calendar getTimeNow() {
        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar nowTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        nowTime.setTime(now);

        return nowTime;
    }

    /**
     * Checks for the existence of a localization-created flag file which
     * indicates that SAFESEAS should rank swell periods high to low. This
     * function returns a true boolean if the file exists, and false if it does
     * not.
     * 
     * @return flag indicating SafeSeas should rank swell periods high to low.
     */
    public static boolean setHighSwellPeriodRankings() {
        // TODO Implement logic based on ./dm/safeseas/FileUtils.C
        return false;
    }

    /**
     * Returns the display string for the date
     * 
     * @param date
     *            The date to display.
     * @param sdf
     *            The format of the date.
     * @return The formatted display string.
     */
    public static String getDisplayString(Date date, SimpleDateFormat sdf) {
        return (date != null) ? sdf.format(date) : "";
    }

    /**
     * Method to obtain the display string for an object using a format.
     * 
     * @param formatString
     *            -- the format string to apply
     * @param dbVal
     *            -- the object to apply the format to
     * @return
     */
    public static String getDisplayString(String formatString, Float dbVal) {
        return String.format(formatString, (dbVal != null) ? dbVal : "");
    }

    /**
     * Method to obtain the display string for an object using a format.
     * 
     * @param formatString
     *            -- the format string to apply
     * @param dbVal
     *            -- the object to apply the format to
     * @return
     */
    public static String getDisplayString(String formatString, Integer dbVal) {
        return String.format(formatString, (dbVal != null) ? dbVal : "");
    }

    /**
     * Method to obtain the display string for an object using a format.
     * 
     * @param formatString
     *            -- the format string to apply
     * @param dbVal
     *            -- the object to apply the format to
     * @return
     */
    public static String getDisplayString(String formatString, Short dbVal) {
        return String.format(formatString, (dbVal != null) ? dbVal : "");
    }

    /**
     * This method first gets the time that the fog threat level file for
     * SAFESEAS was last modified to determine whether or not the most recent
     * fog threat level is too old. If it is not too old, the fog threat level
     * is read from the file. The time the file was written and the fog threat
     * level are returned to the caller as calling argument values. If the file
     * is too old, the fog threat level will be GRAY.
     * 
     * @return the fog threat level
     */
    public static ThreatLevel readFogThreatLevel() {
        // TODO Determine the actual fog threat level for SafeSeas.
        return ThreatLevel.GRAY;
    }

    /**
     * This method supplements generation of an observation report for FOG
     * 
     * @param obReport
     *            -- the observation report
     * @return -- the generated observation report
     */
    public static ObReport generateObReportFog(ObReport obReport,
            Set<SkyCover> skyCov, int vertVis) {

        // Call routine to get the relative humidity
        getRH(obReport);

        // Call routine to get the ceiling value
        getCeiling(obReport, skyCov, vertVis);

        return obReport;
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
    public static float findMetarCeilingFromLayers(Set<SkyCover> skyCov) {
        float ceiling = 1e20f;
        // Find a ceiling in a METAR report.
        try {
            for (SkyCover sc : skyCov) {
                if (sc.getType().equals("CLR")) {
                    ceiling = ObConst.CLR_SKY_CONDITION;
                    break;
                } else if (sc.getType().equals("SKC")) {
                    ceiling = ObConst.SKC_SKY_CONDITION;
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
        return ceiling >= 1e20f ? ObConst.MISSING : ceiling;
    }

    /**
     * This method determines the RH from temperature and dew point in degrees
     * celsius.
     * 
     * @param obReport
     *            -- the observation report
     * @return success
     */
    public static boolean getRH(ObReport obReport) {
        // Initialize relative humidity (RH) to missing
        obReport.setRelativeHumidity(ObConst.MISSING);

        // Calculate the relative humidity (RH)
        if (obReport.getTemperature() != ObConst.MISSING
                && obReport.getTemperature() < 1e30f
                && obReport.getDewpoint() != ObConst.MISSING
                && obReport.getDewpoint() < 1e30f) {
            // Call calcrh from meteoLib.
            float relHum = CalcRH.calcrh(obReport.getTemperature(),
                    obReport.getDewpoint());
            if (Float.isNaN(relHum) || relHum < 0f || relHum > 100f) {
                obReport.setRelativeHumidity(ObConst.MISSING);
            } else {
                obReport.setRelativeHumidity(relHum);
            }
        }

        return true;
    }

    /**
     * This method determines the ceiling value of a METAR.
     * 
     * @param obReport
     *            -- the observation report
     * @return success
     */
    public static boolean getCeiling(ObReport obReport, Set<SkyCover> skyCov,
            float vertVis) {

        // Get the ceiling value for METARs.

        // If a vertical visibility has been reported, the sky is obscured
        // and the vertical visibility becomes the ceiling.
        if (vertVis >= 0 && vertVis < 1e20f) {
            obReport.setCeiling(vertVis);
        } else {
            // Otherwise, determine the ceiling value.
            obReport.setCeiling(findMetarCeilingFromLayers(skyCov));
        }

        return true;
    }
}
