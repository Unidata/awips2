package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Record implementation for geomag k1minDao.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
 * 03/03/2014   #1110      qzhou              Added method getRangeK1min(), Cleaned code 
 * 03/13/2014              sgurung            Added method purgeDataByRefTime()
 * 10/16/2014   3454       bphillip           Upgrading to Hibernate 4
 * 04/05/2014   R4078      sgurung            Added methods getEstKIndex1min(), getEstKpIndex1min(), 
 *                                            getLatestEstKIndex() and getLastDataDate().
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagK1minDao extends CoreDao {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /**
     * Creates a new GeoMagK1minDao
     */
    public GeoMagK1minDao() {
        super(DaoConfig.forClass(GeoMagK1min.class));
    }

    /**
     * Retrieves data from postGres
     * 
     * @return Criteria list
     */

    @SuppressWarnings("unchecked")
    public List<GeoMagK1min> getRangeK1min(final String stationCode,
            final Date start, final Date end) {
        return (List<GeoMagK1min>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess.createCriteria(GeoMagK1min.class);
                        Criterion where1 = Restrictions.eq("stationCode",
                                stationCode);
                        crit.add(where1);
                        Criterion where2 = Restrictions.ge("refTime", start);
                        crit.add(where2);
                        Criterion where3 = Restrictions.lt("refTime", end);
                        crit.add(where3);
                        crit.addOrder(Order.asc("refTime"));

                        return crit.list();
                    }
                });
    }

    @SuppressWarnings("unchecked")
    public List<GeoMagK1min> getSingleK1min(final String stationCode,
            final Date date) {

        return (List<GeoMagK1min>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess.createCriteria(GeoMagK1min.class);
                        Criterion where1 = Restrictions.eq("stationCode",
                                stationCode);
                        crit.add(where1);
                        Criterion where2 = Restrictions.eq("refTime", date);
                        crit.add(where2);
                        return crit.list();
                    }
                });
    }

    @SuppressWarnings("unchecked")
    public List<GeoMagK1min> getEstKIndex1min(final List<String> stations,
            final Date start, final Date end) {

        return (List<GeoMagK1min>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess.createCriteria(GeoMagK1min.class);
                        if (stations != null) {
                            Criterion where1 = Restrictions.in("stationCode",
                                    stations);
                            crit.add(where1);
                        }
                        Criterion where2 = Restrictions.gt("refTime", start);
                        crit.add(where2);
                        Criterion where3 = Restrictions.le("refTime", end);
                        crit.add(where3);
                        // Criterion where4 = Restrictions.ge("ks", 9.0);
                        // crit.add(where4);
                        crit.addOrder(Order.asc("refTime"));
                        crit.addOrder(Order.asc("stationCode"));
                        return crit.list();
                    }
                });
    }

    /**
     * Retrieves data from geomag_k1min table for the provided list of stations,
     * given start date and end date. <br>
     * 
     * @param stations
     *            A list of stations
     * @param start
     *            start date
     * @param end
     *            end date
     * @return List of data map containing "reftime", "ks_avg", "station_count"
     * @throws Exception
     */
    public List<Map<String, Object>> getEstKpIndex1min(
            final List<String> stations, final Date start, final Date end)
            throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT a.reftime, AVG(a.ks) ks_avg, COUNT(a.stationcode) station_count FROM geomag_k1min a ");
        sql.append(" INNER JOIN geomag_k3hr b ON b.stationcode = a.stationcode ");
        sql.append(" INNER JOIN geomag_k3hr_state c ON c.k3hrid = b.id ");
        sql.append(" INNER JOIN geomag_states d ON d.stateid = c.stateid  ");
        sql.append(" WHERE ");
        if (stations != null) {
            sql.append(" a.stationcode IN ('");

            String station = null;
            for (int i = 0; i < stations.size(); i++) {
                station = stations.get(i);
                sql.append(station).append("'");
                if (i != (stations.size() - 1)) {
                    sql.append(",'");
                }

            }
            sql.append(") AND");
        }
        sql.append(" a.reftime > '");
        sql.append(start).append("' AND a.reftime <= '").append(end)
                .append("'");
        sql.append(" AND a.ks <= 9.0 ");
        sql.append(" AND d.processingstate <= 'Active' ");
        sql.append(" AND b.reftime = date_trunc('hour', a.reftime - cast(date_part('HOUR',a.reftime) as bigint) % 3 * INTERVAL  '1 hour' + interval '0 hour') ");
        sql.append(" GROUP BY a.reftime ORDER BY a.reftime ASC");

        // logger.info(" Inside GeoMagK1minDao.getEstKpIndex1min(), sql = "
        // + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return new ArrayList<Map<String, Object>>();
        }

        String[] fieldNames = { "reftime", "ks_avg", "station_count" };
        List<Map<String, Object>> resultMaps = new ArrayList<Map<String, Object>>(
                results.length);
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }
            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            Map<String, Object> resultMap = new HashMap<String, Object>(
                    objs.length * 2);
            for (int i = 0; i < fieldNames.length; ++i) {
                resultMap.put(fieldNames[i], objs[i]);
            }
            resultMaps.add(resultMap);
        }

        return resultMaps;

    }

    /**
     * Get a list of maps containing the latest estimate kindex and ks values
     * for a given list of station codes, start and end dates.
     * 
     * @param stationCodes
     *            A list of station codes
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List<Map>
     */
    public List<Map<String, Object>> getLatestEstKIndex(
            final List<String> stationCodes, final Date start, final Date end)
            throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" select a.stationcode, a.reftime, a.kestindex, ks from geomag_k1min a "
                + " inner join (SELECT max(reftime) as latestreftime, stationcode FROM geomag_k1min ");

        if (stationCodes != null || (start != null && end != null)) {
            sql.append(" WHERE ");

            if (stationCodes != null) {
                sql.append(" stationcode IN ('");

                String station = null;
                for (int i = 0; i < stationCodes.size(); i++) {
                    station = stationCodes.get(i);
                    sql.append(station).append("'");
                    if (i != (stationCodes.size() - 1)) {
                        sql.append(",'");
                    }

                }
                sql.append(" ) ");
            }

            if ((stationCodes != null) && (start != null && end != null)) {
                sql.append(" AND ");
            }

            if (start != null && end != null) {
                sql.append("  reftime > '");
                sql.append(start).append("' AND reftime <= '").append(end)
                        .append("'");
            }
        }

        sql.append(" group by stationcode) b "
                + " ON a.stationcode = b.stationcode "
                + " AND a.reftime = b.latestreftime");

        sql.append(" order by a.stationcode asc ");

        // logger.info(" Inside GeoMagK1minDao.getLatestEstKIndex(), sql = "
        // + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return new ArrayList<Map<String, Object>>();
        }

        String[] fieldNames = { "stationcode", "reftime", "kestindex", "ks" };
        List<Map<String, Object>> resultMaps = new ArrayList<Map<String, Object>>(
                results.length);
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }
            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            Map<String, Object> resultMap = new HashMap<String, Object>(
                    objs.length * 2);
            for (int i = 0; i < fieldNames.length; ++i) {
                resultMap.put(fieldNames[i], objs[i]);

            }
            resultMaps.add(resultMap);
        }

        return resultMaps;

    }

    /**
     * Get a list of maps containing the latest estimate kindex and ks values
     * for a given list of station codes, start and end dates.
     * 
     * @param stationCodes
     *            A list of station codes
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List<Map>
     */
    public Date getLastDataDate(final List<String> stationCodes,
            final Date start, final Date end) throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" select max(reftime) as lastDataDate from geomag_k1min ");

        if (stationCodes != null || (start != null && end != null)) {
            sql.append(" WHERE ");

            if (stationCodes != null) {
                sql.append(" stationcode IN ('");

                String station = null;
                for (int i = 0; i < stationCodes.size(); i++) {
                    station = stationCodes.get(i);
                    sql.append(station).append("'");
                    if (i != (stationCodes.size() - 1)) {
                        sql.append(",'");
                    }

                }
                sql.append(" ) ");
            }

            if ((stationCodes != null) && (start != null && end != null)) {
                sql.append(" AND ");
            }

            if (start != null && end != null) {
                sql.append("  reftime > '");
                sql.append(start).append("' AND reftime <= '").append(end)
                        .append("'");
            }

            sql.append(" AND ks <= 9.0 ");
        }

        // logger.info(" Inside GeoMagK1minDao.getLastDataDate(), sql = "
        // + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return new Date();
        }

        Date lastDataDate = new Date();
        String[] fieldNames = { "lastDataDate" };
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }

            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            lastDataDate = (Date) objs[0];
        }

        return lastDataDate;

    }

    public int purgeDataByRefTime(Date refTime) throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("refTime", refTime);
        return this.deleteByCriteria(deleteStmt);
    }
}
