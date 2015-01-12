package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;

import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Record implementation for geomag k3hrDao.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
 * 03/13/2014              sgurung            Added method purgeDataByRefTime()
 * 10/16/2014   3454       bphillip           Upgrading to Hibernate 4
 * 07/01/2014   R4078       sgurung            Added method getStationMaxPrevTime()
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagK3hrDao extends CoreDao {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /**
     * Creates a new GribModelDao
     */
    public GeoMagK3hrDao() {
        super(DaoConfig.forClass(GeoMagK3hr.class));
    }

    /**
     * Retrieves data from postGres
     * 
     * @return Criteria list
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagK3hr> getRangeK3hr(final String stationCode,
            final Date start, final Date end) {
        return (List<GeoMagK3hr>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                Session sess = getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagK3hr.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);
                Criterion where2 = Restrictions.gt("refTime", start);
                crit.add(where2);
                Criterion where3 = Restrictions.lt("refTime", end);
                crit.add(where3);

                return crit.list();
            }
        });
    }

    @SuppressWarnings("unchecked")
    public List<GeoMagK3hr> getK3hr(final String stationCode, final Date time) {
        return (List<GeoMagK3hr>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                Session sess = getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagK3hr.class);
                if (stationCode != null) {
                    Criterion where1 = Restrictions.eq("stationCode",
                            stationCode);
                    crit.add(where1);
                }
                if (time != null) {
                    Criterion where2 = Restrictions.eq("refTime", time);
                    crit.add(where2);
                }
                return crit.list();
            }
        });
    }

    /**
     * Returns the record with the max previous time for a given station and
     * time tag<br>
     * 
     * @param stationCode
     *            stationCode
     * @param reftime
     *            time tag
     * @return GeoMagK3hr
     * @throws Exception
     */
    public Date getStationMaxPrevTime(final String stationCode,
            final Date reftime) throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT reftime FROM geomag_k3hr "
                + " WHERE stationcode = '" + stationCode + "'"
                + " AND reftime < '" + reftime + "'");
        sql.append(" ORDER BY reftime DESC");
        sql.append(" LIMIT 1");

        // logger.info("Inside GeoMagK3hrDao.getStationMaxPrevTime(), sql = "
        // + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return null;
        }

        String[] fieldNames = { "reftime" };
        Object obj = results[0];
        if (obj instanceof Object[] == false) {
            obj = new Object[] { obj };
        }
        Object[] objs = (Object[]) obj;
        if (objs.length != fieldNames.length) {
            throw new Exception(
                    "Column count returned does not match expected column count");
        }
        Date previousTime = (Date) objs[0];

        return previousTime;

    }

    public int purgeDataByRefTime(Date refTime) throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("refTime", refTime);

        return this.deleteByCriteria(deleteStmt);
    }
}
