package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;

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
 * Record implementation for geomag k3hrDao.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
 * 03/13/2014              sgurung            Added method purgeDataByRefTime()
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagK3hrDao extends CoreDao {
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
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
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
    public List<GeoMagK3hr> getSingleK3hr(final String stationCode,
            final Date time) {
        return (List<GeoMagK3hr>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagK3hr.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);
                Criterion where2 = Restrictions.eq("refTime", time);
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
