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
package com.raytheon.uf.edex.ohd.pproc;

import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * GAFF Database access class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2011            mpduff      Initial creation
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * Apr 21, 2014   2060     njensen     Remove dependency on grid dataURI column
 * Jul 09, 2015 4500       rjpeter     Fix SQL Injection concern.
 * Aug 05, 2015 4486       rjpeter     Changed Timestamp to Date.
 * Sep 08, 2015 4846       rjpeter     Fixed paramMap data types.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GAFFDB {
    private static final String IHFS = "ihfs";

    private static final int DEFAULT_QC_VALUE = 1879048191;

    private static final String GEOAREA_QUERY = "select area_id from GeoArea where "
            + "boundary_type = 'BASIN' AND area_id IN (SELECT area_id FROM linesegs)";

    private static final String LINESEGS_QUERY = "select hrap_row, hrap_beg_col, "
            + "hrap_end_col, area from linesegs";

    private static final IUFStatusHandler log = UFStatus
            .getHandler(GAFFDB.class);

    /**
     * Default constructor.
     */
    public GAFFDB() {

    }

    /**
     * Write processing info to PerfLog Table. We could set an elapsed time here
     * but A1 does not.
     * 
     * @param processName
     *            The name of the process
     * @param start
     *            The start time of the process in milliseconds
     * @throws DataAccessLayerException
     */
    public void insertPerflog(String processName, long start)
            throws DataAccessLayerException {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(start);

        String sql = "insert into perflog (process, start_time, num_processed, "
                + "num_reads, num_inserts, num_updates, num_deletes, "
                + "elapsed_time, cpu_time, io_time) values (:process,"
                + " :startTime, 0, 0, 0, 0, 0, 0, 0, 0)";
        Map<String, Object> paramMap = new HashMap<>(2, 1);
        paramMap.put("process", processName);
        paramMap.put("startTime", cal);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
        dao.executeSQLUpdate(sql, paramMap);
    }

    /**
     * This function finds the last run time from the PerfLog table.
     * 
     * If no records are found for the process, then return 00z and today's date
     * 
     * @param processName
     *            The process name
     * @return The last run time in milliseconds
     */
    public long getLastRunTime(String processName) {
        try {
            String sql = "select max(start_time) from perflog where "
                    + "process = '" + processName + "'";
            CoreDao dao = null;
            dao = new CoreDao(DaoConfig.forDatabase(IHFS));
            Object[] rs = dao.executeSQLQuery(sql);
            Date ts = null;
            if ((rs != null) && (rs.length > 0)) {
                if (rs[0] instanceof Object[]) {
                    Object[] oa = (Object[]) rs[0];
                    if ((oa != null) && (oa[0] instanceof Date)) {
                        ts = (Date) oa[0];
                    }
                } else if (rs[0] instanceof Date) {
                    ts = (Date) rs[0];
                } else {
                    throw new Exception("Error getting Last Run Time");
                }

                log.debug("Last run time: " + ts.toString());
                return ts.getTime();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.HOUR, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MILLISECOND, 0);

        log.debug("Last run time set to 00Z");

        return c.getTimeInMillis();
    }

    /**
     * Get the grid record
     * 
     * @param rfc
     *            The RFC
     * @param duration
     *            The duration
     * @param today
     *            Today's date in database string format
     * @return The database uri, or null if no data
     * @throws DataAccessLayerException
     */
    public GridRecord getGridRecord(String rfc, String duration, String today)
            throws DataAccessLayerException {
        GridRecord rec = null;
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.DATASET_ID, "FFG-" + rfc);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "FFG"
                + duration + "24hr");
        query.addQueryParam("dataTime.refTime", today, ">=");
        query.addOrder(GridConstants.SECONDARY_ID, false);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase("metadata"));
        List<?> rs = dao.queryByCriteria(query);
        if ((rs != null) && (!rs.isEmpty())) {
            Object result = rs.get(0);
            if ((result != null) && (result instanceof GridRecord)) {
                rec = ((GridRecord) result);
            }
        }

        return rec;
    }

    /**
     * writes records to the ContingencyValue table. SiteSpecific process reads
     * these records
     * 
     * @param areaId
     *            The Area Id
     * @param validTimeMillis
     *            The valid time in milliseconds
     * @param duration
     *            The duration
     * @param avgVal
     *            The average value
     * @throws DataAccessLayerException
     */
    public void writeContingency(String areaId, long validTimeMillis,
            int duration, double avgVal) throws DataAccessLayerException {
        // validtime = timestamp from selected gridded FFG file
        Calendar validDate = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        validDate.setTimeInMillis(validTimeMillis);
        Calendar postDate = Calendar.getInstance(
                TimeZone.getTimeZone("GMT"));


        // duration in units of hours, possible values = 1,3,6,12,24
        short dur = 0;
        if (duration == 1) {
            dur = 1001;
        } else if (duration == 3) {
            dur = 1003;
        } else if (duration == 6) {
            dur = 1006;
        } else if (duration == 12) {
            dur = 1012;
        } else if (duration == 24) {
            dur = 2001;
        }

        // Do we insert or update here
        String countSql = "select count(*) from contingencyvalue where "
                + "lid = :areaId and pe = 'PP' and dur = :dur"
                + " and ts = 'CP' and extremum = 'Z' and probability = -1.0 "
                + " and validtime = :validDate and basistime = :validDate";
        Map<String, Object> paramMap = new HashMap<>(8, 1);
        paramMap.put("areaId", areaId);
        paramMap.put("dur", dur);
        paramMap.put("validDate", validDate);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));

        Object[] rs = dao.executeSQLQuery(countSql, paramMap);
        BigInteger count = new BigInteger("0");

        if ((rs != null) && (rs.length > 0)) {
            count = (BigInteger) rs[0];
        }

        paramMap.put("avgVal", avgVal);
        paramMap.put("qc", DEFAULT_QC_VALUE);
        paramMap.put("postDate", postDate);

        if (count == BigInteger.ZERO) {
            // Write data to the table
            String sql = "insert into contingencyvalue (lid, pe, dur, ts, "
                    + "extremum, probability, validtime, basistime, value, "
                    + "shef_qual_code, quality_code, revision, product_id, "
                    + "producttime, postingtime) values (:areaId, 'PP', "
                    + ":dur, 'CP', 'Z', -1.0, :validDate, :validDate, :avgVal, "
                    + "'Z', :qc, 0, 'GRIDFFG', :validDate, :postDate)";

            log.debug(sql);
            dao.executeSQLUpdate(sql, paramMap);
        } else {
            // Need to do an update to the row
            String updateSql = "update contingencyvalue set value = :avgVal, "
                    + "shef_qual_code = 'Z', quality_code = :qc, revision = 0, "
                    + "product_id = 'GRIDFFG', producttime = :validDate, "
                    + "postingtime = :postDate where lid = :areaId and pe = 'PP' "
                    + "and dur = :dur and ts = 'CP' and extremum = 'Z' and probability = -1.0 "
                    + " and validtime = :validDate and basistime = :validDate";
            dao.executeSQLUpdate(updateSql, paramMap);
        }
    }

    /**
     * Get the HSA identifier from the Admin table.
     * 
     * @return The HSA or null if no value
     */
    public String getHsa() {
        String hsa = null;
        String sql = "select hsa from admin";

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
        Object[] rs = dao.executeSQLQuery(sql);
        if ((rs != null) && (rs.length > 0)) {
            if ((rs[0] != null) && (rs[0] instanceof String)) {
                hsa = (String) rs[0];
                log.debug("HSA: " + hsa);
            }
        }

        return hsa;
    }

    public Object[] getGeoAreaIds() {
        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
        Object[] rs = dao.executeSQLQuery(GEOAREA_QUERY);

        return rs;
    }

    public Object[] getLineSegs(String areaId) {
        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
        Object[] rs = dao.executeSQLQuery(LINESEGS_QUERY
                + " where area_Id = :areaId", "areaId", areaId);

        return rs;
    }
}
