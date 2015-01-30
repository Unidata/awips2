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

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagStationStateChange;

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
 * Dao class for GeoMagStationStateChange.
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
public class GeoMagStationStateChangeDao extends CoreDao {

    /**
     * Creates a new GeoMagStationStateChangeDao
     */
    public GeoMagStationStateChangeDao() {
        super(DaoConfig.forClass(GeoMagStationStateChange.class));
    }

    /**
     * Retrieves a GeoMagStationStateChange based on the given id
     * 
     * @param id
     *            The given ID number
     * @return The GeoMagStationStateChange object associated with the given id
     */
    public GeoMagStationStateChange queryById(int id) {
        return (GeoMagStationStateChange) super.queryById(id);
    }

    /**
     * Retrieves list of all GeoMagStationStateChange objects
     * 
     * @return list of GeoMagStationStateChange objects
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagStationStateChange> getAllStationStateChanges() {
        return (List<GeoMagStationStateChange>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess
                                .createCriteria(GeoMagStationStateChange.class);
                        return crit.list();
                    }
                });
    }

    /**
     * Retrieves list of all GeoMagStationStateChange objects for a given
     * stationCode
     * 
     * @param stationCode
     *            The Station code
     * @return list of GeoMagStationStateChange objects
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagStationStateChange> getAllStationStateChangesByStation(
            final String stationCode) {
        return (List<GeoMagStationStateChange>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess
                                .createCriteria(GeoMagStationStateChange.class);
                        Criterion where1 = Restrictions.eq("stationCode",
                                stationCode);
                        crit.add(where1);
                        return crit.list();
                    }
                });
    }

}
