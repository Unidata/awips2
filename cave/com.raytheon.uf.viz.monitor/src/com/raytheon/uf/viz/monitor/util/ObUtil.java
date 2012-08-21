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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Scanner;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.viz.aviation.model.CloudGroup;

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
     * Method that gets the time that the fog threat level file for SAFESEAS was
     * last modified.
     * 
     * @return the time when last modified
     */
    private static Date readFogThreatTime() {
        // TODO Obtain the actual time that the fog threat level file for
        // SafeSeas was last modified.
        return getDropTime();
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
     * This method supplements generation of an observation report for SNOW
     * 
     * @param obReport
     *            -- the observation report
     * @return -- the generated observation report
     */
    public static ObReport generateObReportSnow(ObReport obReport,
            String remarks) {
        // Check parameters for wind chill/frostbite
        // time calculation (upper limit for wind chill
        // is set at 40F) :
        if ((obReport.getTemperature() == ObConst.MISSING)
                || (obReport.getTemperature() > 277.6) // 40 F
                || (obReport.getWindSpeed() == ObConst.MISSING)
                || (obReport.getWindSpeed() > 1e36f)) {
            obReport.setWindChill(ObConst.MISSING);
            obReport.setFrostbiteTime(ObConst.MISSING);
        } else {
            // Kelvin to Celsius
            // float tempC = obReport.getTemperature() - 273.15f;

            // obReport.getTemperature() in Celsius already
            // [Jan 19, 2010, zhao]
            float tempC = obReport.getTemperature();

            // mps to kph
            // float speedKPH = obReport.getWindSpeed() * 3.6f;

            // mph to kph
            // [Jan 19, 2010, zhao]
            float speedKPH = obReport.getWindSpeed() * 1.6f;

            // Call AWIPS wind chill routine. Return units
            // are in Celsius.
            float windChillC = calcWindChill(tempC, speedKPH);
            float windChillK = 273.15f;
            // Check for too-warm default (default should come back
            // as 1e37 if conditions are too warm, so we'll check one
            // order of magnitude lower, for > 1e36.) Actual limit
            // is 16C or ~61F. If the wind chill is valid, carry
            // it through the active program as Fahrenheit, but store
            // it in the NetCDF files as Kelvin (to match the other
            // temperature parameters).

            if (windChillC > 1e36f) {
                obReport.setWindChill(ObConst.MISSING);
                windChillK = ObConst.MISSING;
            } else {
                // Carry through the program in Fahrenheit...
                obReport.setWindChill(1.8f * windChillC + 32.0f);
                windChillK += windChillC;
            }

            // Calculate frostbite time.

            float fbMinutes = calcFrostbiteTime(speedKPH, tempC);
            obReport.setFrostbiteTime(fbMinutes);

        }

        // Check for Snow Increasing Rapidly (SNINCR) in the remarks.
        // First, retrieve the current raw report (don't want to have
        // to store it in the Report class).

        // Call routine to get the hourly and total values, if present.
        getSNINCR(remarks, obReport);

        // Call routine to get snow depth, if present.
        getSnowDepth(remarks, obReport);

        return obReport;
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
     * This method builds a string representing the present sky coverage given a
     * set of sky coverage data from an actual observation (metar). The
     * mathematical set appears to contain elements that originally were space
     * delimited strings. The set is of course unordered. Guidance/clues for
     * detecting the implied order comes from NWSI(s).
     * 
     * @param skyCov
     *            -- the set of sky coverage data
     * @return -- a string representing the sky cover
     */
    public static String buildPresentSkyCov(Set<SkyCover> skyCov) {
        StringBuilder sb = new StringBuilder();
        /** Sky Conditions */
        ArrayList<CloudGroup> cloudGroup = new ArrayList<CloudGroup>();
        // Capture the distinct parts of the sky coverage. Then
        // reorder these parts using guidance from an NWSI.
        // Put the cloud group settings into the observation.
        try {
            for (SkyCover sc : skyCov) {
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
                cloudGroup.add(cldGp);
            }
        } catch (RuntimeException e) {
            // ignore cloud cover that is null
        }
        // Initialize the present sky cover string to the empty string.
        sb.append("");
        // Sky Coverage
        for (CloudGroup cg : cloudGroup) {
            sb.append(cg.getCldCat().value());
            if (cg.getCldHgt() > 0) {
                int cldHgtHds = cg.getCldHgt() / 100;
                if (cldHgtHds < 10) {
                    sb.append("00");
                } else if (cldHgtHds < 100) {
                    sb.append("0");
                }
                sb.append(cldHgtHds);
            }
            if (cg.getGenus() != null) {
                sb.append(cg.getGenus());
            }
            sb.append(" ");
        }
        return sb.toString();
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
     * This method calculates the windChill from temperature and windSpeed.
     * 
     * @param temp
     *            -- temperature in degrees Celsius
     * @param windSpd
     *            -- wind speed in kilometers per hour
     * @return -- wind chill in degrees Celsius
     */
    public static float calcWindChill(float temp, float windSpd) {
        float spd;

        /* arbitrarily do the calculation only for temps at or below 60F */
        if (temp > 16.)
            return 1e37f;

        /* no chilling if speed < 4 mph = 6.44km/h */
        if (windSpd < 6.4)
            return temp;
        /* peg speed at 80 mph (= 128.75 km/h) */
        if (windSpd > 128.75)
            spd = 128.75f;
        else
            spd = windSpd;

        spd = (float) Math.pow(spd, 0.16);
        float windChillTemp = 13.12f + 0.6215f * temp - 11.37f * spd + 0.3965f
                * temp * spd;
        return windChillTemp;
    }

    /**
     * This method calculates the amount of time needed for frostbite to occur
     * on exposed skin.
     * 
     * @param windspeedKPH
     *            -- wind speed in kilometers per hour
     * @param temperatureC
     *            -- temperature in degrees Celsius
     * @return -- time in minutes
     */
    public static float calcFrostbiteTime(float windspeedKPH, float temperatureC) {
        float fbMinutes = ObConst.MISSING;

        // Temperature must be lower than -4.8C (23F) to avoid a calculation
        // error (a negative number to -1.668 power is NAN)

        if (temperatureC < -4.8)
            fbMinutes = ((-24.5f * ((0.667f * windspeedKPH) + 4.8f)) + 2111f)
                    * (float) Math.pow((-4.8 - temperatureC), -1.668);
        else
            return ObConst.MISSING;

        // Check for frostbite boundaries

        if (!(fbMinutes <= 30 && windspeedKPH > 25.0 && windspeedKPH <= 80.5))
            fbMinutes = ObConst.MISSING;

        return fbMinutes;
    }

    /**
     * This method retrieves "snow increasing rapidly" parameters -- hourly and
     * total snowfall in inches -- from a METAR's remarks section. This method
     * update the observation report with the "snow increasing rapidly"
     * parameters.
     * 
     * @param remarks
     *            -- the METAR remarks
     * @param obReport
     *            -- the observation report
     * @return success
     */
    public static boolean getSNINCR(String remarks, ObReport obReport) {

        // New up a Scanner to use to parse the raw data for desired pattern.
        Scanner sc = new Scanner(remarks);
        String whatMatched;
        whatMatched = sc.findWithinHorizon("REMARK_EXPR", 0);
        if (whatMatched != null) {
            whatMatched = sc.findWithinHorizon("SNINCR", 0);
            if (whatMatched != null) {
                sc.useDelimiter("/");
                obReport.setSnincrHourly(sc.nextInt());
                sc.reset();
                obReport.setSnincrTotal(sc.nextInt());
            } else {
                obReport.setSnincrHourly(ObConst.MISSING);
                obReport.setSnincrTotal(ObConst.MISSING);
            }
        } else {
            obReport.setSnincrHourly(ObConst.MISSING);
            obReport.setSnincrTotal(ObConst.MISSING);
        }

        return true;
    }

    /**
     * This method retrieves "snow depth" parameters -- total snow depth in
     * inches -- from a METAR's remarks section. This method update the
     * observation report with the "snow depth" parameters.
     * 
     * @param remarks
     *            -- the METAR remarks
     * @param obReport
     *            -- the observation report
     * @return success
     */
    public static boolean getSnowDepth(String remarks, ObReport obReport) {

        // New up a Scanner to use to parse the raw data for desired pattern.
        Scanner sc = new Scanner(remarks);
        String whatMatched;
        whatMatched = sc.findWithinHorizon("REMARK_EXPR", 0);
        if (whatMatched != null) {
            whatMatched = sc.findWithinHorizon("4/", 0);
            if (whatMatched != null) {
                obReport.setSnowDepth(sc.nextInt());
            } else {
                obReport.setSnowDepth(ObConst.MISSING);
            }
        } else {
            obReport.setSnowDepth(ObConst.MISSING);
        }

        return true;
    }

    /**
     * This method determines the RH from temperature and dew point in degrees
     * celsius. It calls the "calcrh" Meteolib method that is wrapped by the
     * "Controller" class.
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

            float[] tempC = { obReport.getTemperature() };
            float[] dewptC = { obReport.getDewpoint() };

            // Set up dimension values for impending calcrh() call
            // (no arrays, just sending individual values, so
            // set dims to 1)
            int ndim1 = 1, idim = 1, jdim = 1;

            float[] relHum = { ObConst.MISSING };

            // Call calcrh from meteoLib.
            // calcrh(&tempC, &dewptC, &ndim1, &idim, &jdim, &relHum);
            relHum = com.raytheon.edex.meteoLib.Controller.calcrh(tempC,
                    dewptC, ndim1, idim, jdim);

            if (relHum[0] < 0f || relHum[0] > 100f) {
                obReport.setRelativeHumidity(ObConst.MISSING);
            } else {
                obReport.setRelativeHumidity(relHum[0]);
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
