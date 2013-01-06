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
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
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
 * Jan 11, 2011            mpduff     Initial creation
 * 
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

    private Log log = LogFactory.getLog("GenArealFFG");

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
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(start);

        String startTime = sdf.format(cal.getTime());
        String process = processName;
        String sql = "insert into perflog (process, start_time, num_processed, "
                + "num_reads, num_inserts, num_updates, num_deletes, "
                + "elapsed_time, cpu_time, io_time) values ('"
                + process
                + "', " + " '" + startTime + "', 0, 0, 0, 0, 0, 0, 0, 0)";

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
        dao.executeNativeSql(sql, false);

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
            Timestamp ts = null;
            if ((rs != null) && (rs.length > 0)) {
                if (rs[0] instanceof Object[]) {
                    Object[] oa = (Object[]) rs[0];
                    if ((oa != null) && (oa[0] instanceof Timestamp)) {
                        ts = (Timestamp) oa[0];
                    }
                } else if (rs[0] instanceof Timestamp) {
                    ts = (Timestamp) rs[0];
                } else {
                    throw new Exception("Error getting Last Run Time");
                }
                if (log.isDebugEnabled()) {
                    log.debug("Last run time: " + ts.toString());
                }
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

        if (log.isDebugEnabled()) {
            log.debug("Last run time set to 00Z");
        }

        return c.getTimeInMillis();
    }

    /**
     * Get the data URI
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
    public String getDataURI(String rfc, String duration, String today)
            throws DataAccessLayerException {
        String uri = null;

        // Query for uri
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addReturnedField("dataURI");
        query.addQueryParam(GridConstants.DATASET_ID, "FFG-" + rfc);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "FFG"
                + duration + "24hr");
        query.addQueryParam("dataTime.refTime", today, ">=");
        query.addOrder(GridConstants.SECONDARY_ID, false);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase("metadata"));
        List<?> rs = dao.queryByCriteria(query);
        if ((rs != null) && (!rs.isEmpty())) {
            if ((rs.get(0) != null) && (rs.get(0) instanceof String)) {
                uri = (String) rs.get(0);
            }
        } else {
            uri = null;
        }

        return uri;
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
        Calendar validTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        validTime.setTimeInMillis(validTimeMillis);

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String postDate = sdf.format(Calendar.getInstance(
                TimeZone.getTimeZone("GMT")).getTime());
        String validDate = sdf.format(validTime.getTime());
        String basisTime = validDate;

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
                + "lid = '" + areaId + "' and pe = 'PP' and dur = " + dur
                + " and ts = 'CP' and extremum = 'Z' and probability = -1.0 "
                + " and validtime = '" + validDate + "' and basistime = '"
                + basisTime + "'";

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));

        Object[] rs = dao.executeSQLQuery(countSql);
        BigInteger count = new BigInteger("0");

        if ((rs != null) && (rs.length > 0)) {
            count = (BigInteger) rs[0];
        }

        if (count == BigInteger.ZERO) {

            // Write data to the table
            String sql = "insert into contingencyvalue (lid, pe, dur, ts, "
                    + "extremum, probability, validtime, basistime, value, "
                    + "shef_qual_code, quality_code, revision, product_id, "
                    + "producttime, postingtime) values ('" + areaId + "', "
                    + "'PP', " + dur + ", 'CP', 'Z', -1.0, '" + validDate
                    + "', '" + basisTime + "', " + avgVal + ", 'Z', "
                    + DEFAULT_QC_VALUE + ", " + "0, 'GRIDFFG', '" + validDate
                    + "', '" + postDate + "')";

            if (log.isDebugEnabled()) {
                log.debug(sql);
            }

            dao.executeNativeSql(sql, false);
        } else {
            // Need to do an update to the row
            String updateSql = "update contingencyvalue set value = "
                    + avgVal
                    + ", shef_qual_code = 'Z', quality_code = "
                    + DEFAULT_QC_VALUE
                    + ", revision = 0, product_id = "
                    + "'GRIDFFG', producttime = '"
                    + validDate
                    + "', "
                    + " postingtime = '"
                    + postDate
                    + "' where "
                    + "lid = '"
                    + areaId
                    + "' and pe = 'PP' and dur = "
                    + dur
                    + " and ts = 'CP' and extremum = 'Z' and probability = -1.0 "
                    + " and validtime = '" + validDate + "' and basistime = '"
                    + basisTime + "'";

            dao.executeSQLUpdate(updateSql);
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
                if (log.isDebugEnabled()) {
                    log.debug("HSA: " + hsa);
                }
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
        Object[] rs = dao.executeSQLQuery(LINESEGS_QUERY + " where area_Id = '"
                + areaId + "'");

        return rs;
    }
}
