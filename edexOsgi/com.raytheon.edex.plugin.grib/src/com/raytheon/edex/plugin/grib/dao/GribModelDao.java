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

package com.raytheon.edex.plugin.grib.dao;

import java.io.FileNotFoundException;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Deprecated, use grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Deprecated
public class GribModelDao extends CoreDao {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(GribModelDao.class);

    /**
     * Creates a new GribModelDao
     */
    public GribModelDao() {
        super(DaoConfig.forClass(GribModel.class));
    }

    /**
     * Checks the database to see if a model matching the provided model exists
     * already
     * 
     * @param model
     *            The model to check
     * @return The model object from the database.
     * @throws DataAccessLayerException
     *             If problems occur while querying
     */
    public GribModel checkModel(GribModel model)
            throws DataAccessLayerException {
        if (model.getId() == null) {
            model.generateId();
        }
        return (GribModel) this.queryById(model.getId());
    }

    @SuppressWarnings("unchecked")
    public List<String> getModelNamesForCoverage(final int coverageId) {
        return (List<String>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Criteria crit = sess.createCriteria(GribModel.class);
                Criterion where = Restrictions.eq("location.id", coverageId);
                crit.add(where);
                crit.setProjection(Projections.distinct(Projections
                        .property("modelName")));
                return crit.list();
            }
        });
    }

    public int deleteModelAndAssociatedData(final String modelName) {
        int rval = 0;
        // try/catch until successful or 3 tries
        // have to retry in case purge is running same time or
        // inserts that will cause PK violations.
        int times = 0;
        boolean retry = true;
        HibernateTemplate ht = getHibernateTemplate();

        while (retry) {
            retry = false;
            StatelessSession sess = null;
            Transaction tx = null;

            try {
                GribDao recordDao = new GribDao();
                recordDao.purgeModelData(modelName);
                try {
                    recordDao.purgeHdf5ModelData(modelName);
                } catch (FileNotFoundException e) {
                    // ignore
                }
                sess = ht.getSessionFactory().openStatelessSession();
                tx = sess.beginTransaction();
                rval += sess
                        .createQuery(
                                "DELETE FROM GribModel where modelName = :modelName")
                        .setString("modelName", modelName).executeUpdate();
                tx.commit();
            } catch (Exception e) {
                if (tx != null) {
                    tx.rollback();
                }

                times++;
                if (times < 3) {
                    retry = true;
                } else {
                    handler.error(
                            "Failed " + times
                                    + " times to delete record/hdf5 for model "
                                    + modelName
                                    + ".  Manual clean up may be required.", e);
                }

            } finally {
                if (sess != null) {
                    sess.close();
                }
                tx = null;
                sess = null;
            }
        }
        return rval;
    }
}
