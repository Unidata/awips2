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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Review Hourly Radar Data Access.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009 2675       mpduff      Initial creation
 * Aug 13, 2009 2675       mpduff      TIM changes added
 * Aug 11, 2015 4500       rjpeter     Fix type casts.
 * Sep 05, 2024 2037782    jsebahar    Remove DPA radar data, this includes data
 *                                     from DPARadar and DPAAdapt tables.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class RadarDataManager {
    private static RadarDataManager instance = null;

    /**
     * logger
     */
    protected final IUFStatusHandler logger = UFStatus.getHandler(getClass());

    /**
     * ngrd = number of HRAP bins in 230 km maximum value allowed = 65 SJU (San
     * Juan) would have value = 69 without check on max value
     */
    private int ngrd = -999;

    /** Private constructor */
    private RadarDataManager() {

    }

    /**
     * Get an instance of this class.
     *
     * @return The instance of the class
     */
    public static synchronized RadarDataManager getInstance() {
        if (instance == null) {
            instance = new RadarDataManager();
        }

        return instance;
    }

    /**
     * Get the DAA filename.
     *
     * @param radId
     *            The radar id
     * @param dtg
     *            The obstime for the record and the associated file name
     * @return The filename
     * @throws VizException
     */
    public String getDAAFileName(String radId, Date dtg) throws VizException {
        String filename = null;

        String where = " where radId = '" + radId + "' and obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";
        String query = "select grid_filename from daaRadar " + where;
        System.out.println(query);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            filename = (String) rs.get(0)[0];
            System.out.println("DAA filename: " + filename);
        }

        return filename;
    }

    /**
     * Get the closest obstime to the TOH (top of hour) from the DaaRadar table,
     * within the time window.
     *
     * @param topOfHourDateTime
     *            The current time looking for
     * @return The best obstime for topOfHourDateTime
     * @throws VizException
     */

    public Date getClosestObstimeToTOH(String radId, Date topOfHourDateTime)

    {
        AppsDefaults ad = AppsDefaults.getInstance();

        int timeWindow = 0;

        timeWindow = ad.getInt("daa_wind", 10);

        Date beforeTime = getLatestObstimeBeforeTOH(radId, topOfHourDateTime);
        Date atOrAfterTime = getEarliestObstimeOnOrAfterTOH(radId,
                topOfHourDateTime);

        Date bestObstime = getBestObsTime(topOfHourDateTime, beforeTime,
                atOrAfterTime, timeWindow);

        return bestObstime;
    }

    public Date getObstimeAtTOH(String radId, Date topOfHourDateTime) {

        Date bestObstime = null;

        // Get the next hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(topOfHourDateTime);
        // cal.add(Calendar.HOUR, 1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime = '"
                + HydroConstants.DATE_FORMAT.format(cal.getTime())
                + "' and radId = '" + radId + "'";
        String query = "select obstime from DAARadar " + where;

        try {

            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            if (!rs.isEmpty()) {
                bestObstime = topOfHourDateTime;
            } else {
                bestObstime = null;
            }

        }

        catch (VizException e) {
            logger.error("Error reading DAARadar obstime- " + query, e);
            bestObstime = null;
        }

        return bestObstime;

    }

    public Date getLatestObstimeBeforeTOH(String radId,
            Date topOfHourDateTime) {
        Date latestObsTime = null;

        // Get the next hour
        Calendar topOfHourCalendar = new GregorianCalendar();
        topOfHourCalendar.setTime(topOfHourDateTime);

        Calendar previousTopOfHourCalendar = new GregorianCalendar();
        previousTopOfHourCalendar.setTime(topOfHourDateTime);
        previousTopOfHourCalendar.add(Calendar.HOUR, -1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime < '"
                + HydroConstants.DATE_FORMAT.format(topOfHourCalendar.getTime())
                + "' AND  obstime >= '"
                + HydroConstants.DATE_FORMAT
                        .format(previousTopOfHourCalendar.getTime())
                + "' AND radId = '" + radId + "'";

        String query = "select max(obstime) from DAARadar " + where;

        try {

            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            if (!rs.isEmpty()) {
                latestObsTime = (Date) rs.get(0)[0];
            } else {
                latestObsTime = null;
            }

        } catch (VizException e) {
            logger.error("Error reading DAARadar max obstime- " + query, e);
            latestObsTime = null;
        }

        return latestObsTime;
    }

    public Date getEarliestObstimeOnOrAfterTOH(String radId,
            Date topOfHourDateTime) {
        Date earliestObsTime = null;

        // Get the next hour
        Calendar topOfHourCalendar = new GregorianCalendar();
        topOfHourCalendar.setTime(topOfHourDateTime);

        Calendar nextTopOfHourCalendar = new GregorianCalendar();
        nextTopOfHourCalendar.setTime(topOfHourDateTime);
        nextTopOfHourCalendar.add(Calendar.HOUR, 1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime >= '"
                + HydroConstants.DATE_FORMAT.format(topOfHourCalendar.getTime())
                + "' AND  obstime < '"
                + HydroConstants.DATE_FORMAT
                        .format(nextTopOfHourCalendar.getTime())
                + "' AND radId = '" + radId + "'";

        String query = "select min(obstime) from DAARadar " + where;

        try {

            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            if (!rs.isEmpty()) {
                earliestObsTime = (Date) rs.get(0)[0];
            } else {
                earliestObsTime = null;
            }

        } catch (VizException e) {
            logger.error("Error reading DAARadar min obstime- " + query, e);
            earliestObsTime = null;
        }

        return earliestObsTime;
    }

    private Date getBestObsTime(Date topOfHourDateTime, Date beforeTOHDateTime,
            Date atOrAfterTOHDateTime, int timeWindowInMinutes) {
        Date bestDateTime = null;

        final long MILLIS_PER_MINUTE = 1000 * 60;

        long startTime = topOfHourDateTime.getTime()
                - timeWindowInMinutes * MILLIS_PER_MINUTE;
        long endTime = topOfHourDateTime.getTime()
                + timeWindowInMinutes * MILLIS_PER_MINUTE;

        long topOfHourTime = topOfHourDateTime.getTime();
        long atOrAfterTime = -1;
        long beforeTime = -1;

        long millisAfter = -1;
        long millisBefore = -1;

        // check the obstime that is at or after the TOH
        if (atOrAfterTOHDateTime != null) {
            atOrAfterTime = atOrAfterTOHDateTime.getTime();

            // Check the time window
            if ((atOrAfterTime >= topOfHourTime)
                    && (atOrAfterTime <= endTime)) {
                millisAfter = atOrAfterTime - topOfHourTime;

                // Check for exactly TOH
                if (millisAfter == 0) {
                    return topOfHourDateTime; // found record exactly at TOH
                }
            }
        }

        if (beforeTOHDateTime != null) {
            beforeTime = beforeTOHDateTime.getTime();

            if ((beforeTime >= startTime) && (beforeTime < topOfHourTime)) {
                millisBefore = topOfHourTime - beforeTime;
            }
        }

        // both valid
        if ((millisAfter >= 0) && (millisBefore > 0)) {
            if (millisAfter <= millisBefore) {
                // after time wins
                bestDateTime = atOrAfterTOHDateTime;
            } else {
                // before time wins
                bestDateTime = beforeTOHDateTime;
            }

        }

        // before is null
        else if (millisAfter >= 0) {
            bestDateTime = atOrAfterTOHDateTime;
        } else if (millisBefore > 0) {
            bestDateTime = beforeTOHDateTime;
        }

        else // There are no valid records within the time window
        {

            bestDateTime = null;
        }

        return bestDateTime;
    }

    /**
     * Get the latest obstime from the DaaRadar table.
     *
     * @param date
     *            The current time looking for
     * @return The max obstime for that date
     * @throws VizException
     */

    /*
     * this function was previously used (erroneously) to find the date/time for
     * a search in the RWRadarResult table for the 4 panel window it is not used
     * anywhere else PST - 1/10/14
     */
    // public Date getLatestObstimeDpaRadar(String radId, Date date)
    public Date getLatestObstimeDaaRadar(String radId, Date date)
            throws VizException {
        Date latest = null;

        // Get the next hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(date);
        cal.add(Calendar.HOUR, 1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime < '"
                + HydroConstants.DATE_FORMAT.format(cal.getTime())
                + "' and radId = '" + radId + "'";
        String query = "select max(obstime) from daaradar" + where;

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (!rs.isEmpty()) {
            latest = (Date) rs.get(0)[0];
        } else {
            latest = cal.getTime();
        }
        if (latest == null) {
            latest = cal.getTime();
        }

        return latest;
    }

    /**
     * Get the number of grid cells on each side of the radar site
     *
     * @param radId
     *            The radar id
     * @return The number of grid cells above, below and to the sides of the
     *         radar site
     * @throws VizException
     */
    public int getNgrd(String radId) throws VizException {
        if ((radId != null) && (radId.length() > 0)) {
            if (ngrd == -999) {
                String query = "select lat, lon from radarloc "
                        + "where use_radar = 'T' and radid = '" + radId + "'";

                List<Object[]> rs = DirectDbQuery.executeQuery(query,
                        HydroConstants.IHFS, QueryLanguage.SQL);

                Object[] oa = rs.get(0);
                double lat = (Double) oa[0];

                double z = 4.7625 / ((1 + Math.sin((Math.PI * 60 / 180)))
                        / (1 + Math.sin((Math.PI * lat / 180.))));
                ngrd = (int) (230 / z + 1);
                if (ngrd > 65) {
                    ngrd = 65;
                }
            }
        }

        return ngrd;
    }

    /**
     * Get the latitude/longitude for the given site.
     *
     * @param radId
     *            The radar id
     * @return the lat/lon in a double array
     */
    public double[] getLatLon(String radId) {
        double[] latlon = new double[2];

        String query = "select lat, lon from radarloc where "
                + "use_radar='T' and radid = '" + radId + "'";

        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);

            Object[] arr = rs.get(0);
            latlon[0] = (Double) arr[1];
            latlon[0] *= -1;
            latlon[1] = (Double) arr[0];

        } catch (VizException e) {
            logger.error("Error reading latlon- " + query, e);
            latlon[0] = -999;
            latlon[1] = -999;
        }

        return latlon;
    }

    /**
     * Get the ignore_radar value in IHFS.
     *
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return true if ignore_radar set to 'y'
     * @throws VizException
     */
    public boolean getIgnoreRadarSP(String radId, Date dpaDate)
            throws VizException {
        boolean ignore = false;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";
        final String query = "select ignore_radar from rwRadarResult ";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            String value = (String) rs.get(0)[0];
            if (value.equalsIgnoreCase("Y")) {
                ignore = true;
            }
        }

        return ignore;
    }

    /**
     * Get the ignore_radar value in IHFS.
     *
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return true if ignore_radar set to 'y'
     * @throws VizException
     */
    public boolean getIgnoreRadarDP(String radId, Date daaDate)
            throws VizException {
        boolean ignore = false;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";
        final String query = "select ignore_radar from DAARadarResult ";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + " " + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            String value = (String) rs.get(0)[0];
            if (value.equalsIgnoreCase("Y")) {
                ignore = true;
            }
        }

        return ignore;
    }

    /**
     * Update the ignore_radar field in rwRadarResult table in IHFS.
     *
     * @param radId
     *            The radar id field
     * @param ignoreRadar
     *            true to ignore radar
     * @return number of rows modified
     * @throws VizException
     */
    public int updateIgnoreRadarSP(String radId, Date dpaDate,
            boolean ignoreRadar) throws VizException {
        int status = 1;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";

        String ignore = null;
        if (ignoreRadar) {
            ignore = "y";
        } else {
            ignore = "n";
        }

        final String sql = "update rwRadarResult set ignore_radar = '" + ignore
                + "' ";

        status = DirectDbQuery.executeStatement(sql + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        return status;
    }

    /**
     * Update the ignore_radar field in DAARadarResult table in IHFS.
     *
     * @param radId
     *            The radar id field
     * @param ignoreRadar
     *            true to ignore radar
     * @return number of rows modified
     * @throws VizException
     */
    public int updateIgnoreRadarDP(String radId, Date daaDate,
            boolean ignoreRadar) throws VizException {
        int status = 1;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";

        String ignore = null;
        if (ignoreRadar) {
            ignore = "y";
        } else {
            ignore = "n";
        }

        final String sql = "update DAARadarResult set ignore_radar = '" + ignore
                + "' ";

        status = DirectDbQuery.executeStatement(sql + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        return status;
    }

    /**
     * Get the rad_avail value from RWRadarResult table
     *
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return int 0 = available with > 0 precip 1 = not available 2 = available
     *         with all precip == 0
     */
    public int getAvailableRadarSP(String radId, Date dpaDate)
            throws VizException {

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";
        final String query = "select rad_avail from rwRadarResult ";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        // if record not found, then rs.size() = 0
        int avail = 1;
        if (!rs.isEmpty()) {
            String value = (String) rs.get(0)[0];

            if ("y".equals(value)) {
                avail = 0;
            } else if ("z".equals(value)) {
                avail = 2;
            }
        }

        return avail;
    }

//---------------------------------------------
    /**
     * Get the rad_avail value from the DAARadarResult table
     *
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return int 0 = available with > 0 precip 1 = not available 2 = available
     *         with all precip == 0
     */
    public int getAvailableRadarDP(String radId, Date daaDate)
            throws VizException {

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";
        final String query = "select rad_avail from DAARadarResult ";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        // if record not found, then rs.size() = 0
        int avail = 1;
        if (!rs.isEmpty()) {
            String value = (String) rs.get(0)[0];

            if ("y".equals(value)) {
                avail = 0;
            } else if ("z".equals(value)) {
                avail = 2;
            }
        }

        return avail;
    }

}
