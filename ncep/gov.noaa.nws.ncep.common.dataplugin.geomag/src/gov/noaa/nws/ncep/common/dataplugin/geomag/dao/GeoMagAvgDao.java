package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;

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
 * Record implementation for geomag avgDao.
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

public class GeoMagAvgDao extends CoreDao {
    /**
     * Creates a new GribModelDao
     */
    public GeoMagAvgDao() {
        super(DaoConfig.forClass(GeoMagAvg.class));
    }

    /**
     * Retrieves a GeoMagAvgId based on the given id
     * 
     * @param id
     *            The given ID number
     * @return The GeoMagAvgId
     */
    public GeoMagAvg queryById(int id) {
        return (GeoMagAvg) super.queryById(id);
    }

    /**
     * Retrieves data from postGres
     * 
     * @return Criteria list
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagAvg> getAvgForStation(final String stationCode,
            final Date start, final Date end) {
        return (List<GeoMagAvg>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagAvg.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);

                Criterion where2 = Restrictions.between("avgTime", start, end);
                crit.add(where2);
                return crit.list();
            }
        });
    }

    @SuppressWarnings("unchecked")
    public List<GeoMagAvg> getSingleAvg(final String stationCode,
            final Date date) {
        return (List<GeoMagAvg>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagAvg.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);
                Criterion where2 = Restrictions.eq("avgTime", date);
                crit.add(where2);
                return crit.list();
            }
        });
    }

    public int purgeDataByRefTime(Date refTime) throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        // add 30 minutes to get hourly average reference time
        Date avgTime = new Date(refTime.getTime() + (30 * 60000));
        deleteStmt.addQueryParam("avgTime", avgTime);
        return this.deleteByCriteria(deleteStmt);
    }
}
