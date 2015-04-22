/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagState;

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Dao class for GeoMagState.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 06/27/2014   R4078       sgurung            Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class GeoMagStateDao extends CoreDao {

    /**
     * Creates a new GeoMagStateDao
     */
    public GeoMagStateDao() {
        super(DaoConfig.forClass(GeoMagState.class));
    }

    /**
     * Retrieves a GeoMagState based on the given id
     * 
     * @param id
     *            The given ID number
     * @return The GeoMagState object associated with the given id
     */
    public GeoMagState queryById(int id) {
        return (GeoMagState) super.queryById(id);
    }

    /**
     * Retrieves list of all GeoMagState objects
     * 
     * @return list of GeoMagState objects
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagState> getAllStates() {
        return (List<GeoMagState>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess.createCriteria(GeoMagState.class);
                        return crit.list();
                    }
                });
    }

    /**
     * Retrieves a GeoMagState object based on the given processing state
     * 
     * @param procState
     *            The processing state
     * @return The GeoMagState object associated with the processing state
     */
    @SuppressWarnings("unchecked")
    public GeoMagState getStateByProcessingState(final String procState) {
        return (GeoMagState) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                Session sess = getCurrentSession();
                Criteria crit = sess.createCriteria(GeoMagState.class);
                Criterion where1 = Restrictions
                        .eq("processingState", procState);
                crit.add(where1);
                if (crit.list() != null && crit.list().size() > 0)
                    return crit.list().get(0);
                else
                    return null;
            }
        });
    }

}
