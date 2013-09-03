package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import java.util.Date;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Record implementation for geomag avgDao. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
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

    public int getAreaId (int id){
        return queryById(id).getId();
    }
    

    @SuppressWarnings("unchecked")
    public List<GeoMagAvg> getAvgForStation(final String stationCode, final Date start, final Date end) {
        return (List<GeoMagAvg>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagAvg.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);
//                Criterion where2 = Restrictions.gt("avgTime", start);
//                crit.add(where2);
//                Criterion where3 = Restrictions.le("avgTime", end);
//                crit.add(where3);
                Criterion where2 = Restrictions.between("avgTime", start, end);//include bounds, but don't need bounds
                crit.add(where2); 
                return crit.list();
            }
        });
    }
    
    @SuppressWarnings("unchecked")
    public List<GeoMagAvg> getSingleAvg(final String stationCode, final Date date) {
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
}
