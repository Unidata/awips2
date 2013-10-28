package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;


import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Record implementation for geomag k1minDao. 
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

public class GeoMagK1minDao extends CoreDao {
	/**
     * Creates a new GribModelDao
     */
    public GeoMagK1minDao() {
        super(DaoConfig.forClass(GeoMagK1min.class));
    }
    
    /**
     * Retrieves a GeoMagAvgId based on the given id
     *
     * @param id
     *            The given ID number
     * @return The GeoMagAvgId
     */
    public GeoMagK1min queryById(int id) {
        return (GeoMagK1min) super.queryById(id);
    }

    public int getAreaId (int id){
        return queryById(id).getId();
    }

    @SuppressWarnings("unchecked")
    public List<GeoMagK1min> getSingleK1min(final String stationCode,  final Date date) {
    	
        return (List<GeoMagK1min>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagK1min.class);
                Criterion where1 = Restrictions.eq("stationCode", stationCode);
                crit.add(where1);
                Criterion where2 = Restrictions.eq("refTime", date);
                crit.add(where2); 
                return crit.list();
            }
        });
    }
}
