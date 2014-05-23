package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;

import java.util.Date;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
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
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagK1minDao extends CoreDao {
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
                        HibernateTemplate ht = getHibernateTemplate();
                        Session sess = ht.getSessionFactory()
                                .getCurrentSession();
                        Criteria crit = sess.createCriteria(GeoMagK1min.class);
                        Criterion where1 = Restrictions.eq("stationCode",
                                stationCode);
                        crit.add(where1);
                        Criterion where2 = Restrictions.ge("refTime", start);
                        crit.add(where2);
                        Criterion where3 = Restrictions.lt("refTime", end);
                        crit.add(where3);

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
                        HibernateTemplate ht = getHibernateTemplate();
                        Session sess = ht.getSessionFactory()
                                .getCurrentSession();
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

    public int purgeDataByRefTime(Date refTime) throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("refTime", refTime);
        return this.deleteByCriteria(deleteStmt);
    }
}
