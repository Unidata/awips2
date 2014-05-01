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
 * Jul 28, 2009 2675       mpduff     Initial creation
 * Aug 13, 2009 2675       mpduff     TIM changes added
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarDataManager {
    private static RadarDataManager instance = null;

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
     * Get the DPA filename.
     * 
     * @param radId
     *            The radar id
     * @param dtg
     *            The obstime for the file
     * @return The filename
     * @throws VizException
     */
    public String getDPAFileName(String radId, Date dtg) throws VizException {
        String filename = null;
        // String fname = FileUtil.join(dirname, radId.toUpperCase()
        // + MPEConstants.DATE_FORMAT_MMDDYYYYHHMM.format(dtg) + "Z");

        String where = " where radId = '" + radId + "' and obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";
        String query = "select grid_filename from dpaRadar " + where;
        System.out.println(query);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            filename = (String) rs.get(0)[0];
            System.out.println("DPA filename: " + filename);
        }

        return filename;
    }

    /**
     * Get the latest obstime from the DpaRadar table.
     * 
     * @param date
     *            The current time looking for
     * @return The max obstime for that date
     * @throws VizException
     */
    public Date getLatestObstimeDpaRadar(String radId, Date date)
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
        String query = "select max(obstime) from dparadar" + where;

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
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

                double z = 4.7625 / ((1 + Math.sin((Math.PI * 60 / 180))) / (1 + Math
                        .sin((Math.PI * lat / 180.))));
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
            e.printStackTrace();
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
    public boolean getIgnoreRadar(String radId, Date dpaDate)
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
     * Update the ignore_radar field in rwRadarResult table in IHFS.
     * 
     * @param radId
     *            The radar id field
     * @param ignoreRadar
     *            true to ignore radar
     * @return number of rows modified
     * @throws VizException
     */
    public int updateIgnoreRadar(String radId, Date dpaDate, boolean ignoreRadar)
            throws VizException {
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
     * Get the Adaptable Parameters from the dpaAdapt table in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dtg
     *            The DPA date/time group
     * @return Object[] of data, null if no data or problem
     * @throws VizException
     */
    public DPAAdaptableParam getAdaptableParameters(String radId, Date dtg)
            throws VizException {
        DPAAdaptableParam data = null;
        String query = "select min_reflth, max_reflth, ref_tltest, rng_tltin, rng_tltout, max_birng,"
                + "min_birng, min_echoar, min_awrefl, max_pctred, mlt_zrcoef, pwr_zrcoef, min_zrefl,"
                + "max_zrefl, max_stmspd, max_timdif, min_artcon, tim_p1cont, tim_p2cont, max_ecarch,"
                + "rng_cutoff, rng_e1coef, rng_e2coef, rng_e3coef, min_prate, max_prate, tim_restrt,"
                + "max_timint, min_timprd, thr_hlyout, end_timgag, max_prdval, max_hlyval, tim_biest,"
                + "thr_nosets, res_bias, longest_lag, bias_applied from dpaAdapt ";
        String where = " WHERE radid='" + radId + "' and obstime='"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            Object[] retArr = rs.get(0);
            data = new DPAAdaptableParam();
            data.setMin_reflth((Float) retArr[0]);
            data.setMax_reflth((Float) retArr[1]);
            data.setRef_tltest((Float) retArr[2]);
            data.setRng_tltin((Float) retArr[3]);
            data.setRng_tltout((Float) retArr[4]);
            data.setMax_birng((Float) retArr[5]);
            data.setMin_birng((Float) retArr[6]);
            data.setMin_echoar((Float) retArr[7]);
            data.setMin_awrefl((Float) retArr[8]);
            data.setMax_pctred((Float) retArr[9]);
            data.setMlt_zrcoef((Float) retArr[10]);
            data.setPwr_zrcoef((Float) retArr[11]);
            data.setMin_zrefl((Float) retArr[12]);
            data.setMax_zrefl((Float) retArr[13]);
            data.setMax_stmspd((Float) retArr[14]);
            data.setMax_timdif((Float) retArr[15]);
            data.setMin_artcon((Float) retArr[16]);
            data.setTim_p1cont((Float) retArr[17]);
            data.setTim_p2cont((Float) retArr[18]);
            data.setMax_ecarch((Float) retArr[19]);
            data.setRng_cutoff((Float) retArr[20]);
            data.setRng_e1coef((Float) retArr[21]);
            data.setRng_e2coef((Float) retArr[22]);
            data.setRng_e3coef((Float) retArr[23]);
            data.setMin_prate((Float) retArr[24]);
            data.setMax_prate((Float) retArr[25]);
            data.setTim_restrt((Float) retArr[26]);
            data.setMax_timint((Float) retArr[27]);
            data.setMin_timprd((Float) retArr[28]);
            data.setThr_hlyout((Float) retArr[29]);
            data.setEnd_timgag((Float) retArr[30]);
            data.setMax_prdval((Float) retArr[31]);
            data.setMax_hlyval((Float) retArr[32]);
            data.setTim_biest((Float) retArr[33]);
            data.setThr_nosets((Float) retArr[34]);
            data.setRes_bias((Float) retArr[35]);
            data.setLongest_lag((Float) retArr[36]);
            data.setBias_applied(String.valueOf(retArr[37]));
        }
        return data;
    }

    /**
     * Get the supplemental data from the dpaRadar table in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dtg
     *            The DPA date/time group
     * @return Object[] of data, null if no data or problem
     * @throws VizException
     */
    public DPASupplementalData getSupplementalData(String radId, Date dtg)
            throws VizException {
        DPASupplementalData data = null;
        String query = "select nisolbin, noutint, noutrep, nbadscan, nhourout, volcovpat, "
                + "opermode, areared, biscanr, supplmess, minoff, maxvald, maxvalh, s1_bias_value, "
                + "producttime from dpaRadar where radid = '"
                + radId
                + "' "
                + " AND obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg)
                + "'";

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);
        if (rs.size() > 0) {
            Object[] retArr = rs.get(0);
            data = new DPASupplementalData();
            data.setNisolbin((Integer) retArr[0]);
            data.setNoutint((Integer) retArr[1]);
            data.setNoutrep((Integer) retArr[2]);
            data.setNbadscan((Integer) retArr[3]);
            data.setNhourout((Integer) retArr[4]);
            data.setVolcovpat((Integer) retArr[5]);
            data.setOpermode((Integer) retArr[6]);
            data.setAreared((Float) retArr[7]);
            data.setBiscanr((Float) retArr[8]);
            data.setSupplmess((Integer) retArr[9]);
            data.setMinoff((Integer) retArr[10]);
            data.setMaxvald((Float) retArr[11]);
            data.setMaxvalh((Float) retArr[12]);
            data.setS1_bias_value((Float) retArr[13]);
            data.setProducttime((Date) retArr[14]);
        }
        return data;
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
    public int getAvailableRadar(String radId, Date dpaDate)
            throws VizException {
        int avail = 0;

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

        if (rs.size() > 0) {
            String value = (String) rs.get(0)[0];
            avail = 1;
            if ("y".equals(value)) {
                avail = 0;
            } else if ("z".equals(value)) {
                avail = 2;
            }
        }

        return avail;
    }

}
