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

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data access object for retrieving GridCoverage objects
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
public class GridCoverageDao extends CoreDao {

    protected static final float QUERY_TOLERANCE = 0.1f;

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(GridCoverageDao.class);

    public GridCoverageDao() {
        super(DaoConfig.forClass(GridCoverage.class));
    }

    public GridCoverageDao(DaoConfig config) {
        super(config);
    }

    protected QueryParam addQueryTolerance(String field, double value) {
        String between = String.valueOf(value - QUERY_TOLERANCE) + "--"
                + String.valueOf(value + QUERY_TOLERANCE);
        return new QueryParam(field, between, QueryOperand.BETWEEN);
    }

    @SuppressWarnings("unchecked")
    public GridCoverage queryByGridNumber(Integer number)
            throws DataAccessLayerException {
        List<GridCoverage> coverages = (List<GridCoverage>) this
                .queryBySingleCriteria("name", String.valueOf(number));
        if (coverages.isEmpty()) {
            return null;
        } else {
            return coverages.get(0);
        }
    }

    /**
     * Method used by subclasses to pick the known grid if multiple results from
     * the query are returned and the results include generated unknown grib
     * coverage objects
     * 
     * @param coverages
     *            The grib coverages to examine
     * @return The 'known' coverage object if one is contained in the list
     */
    protected GridCoverage selectKnownGrid(
            List<? extends GridCoverage> coverages) {
        if (coverages.isEmpty()) {
            return null;
        } else if (coverages.size() == 1) {
            return coverages.get(0);
        } else {
            for (GridCoverage coverage : coverages) {
                if (!coverage.getDescription().startsWith("Unknown")) {
                    return coverage;
                }
            }
            return coverages.get(0);
        }
    }

    @SuppressWarnings("unchecked")
    public List<? extends GridCoverage> loadBaseGrids() {
        return (List<GridCoverage>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        HibernateTemplate ht = getHibernateTemplate();
                        Session sess = ht.getSessionFactory()
                                .getCurrentSession();
                        Criteria crit = sess.createCriteria(GridCoverage.class);
                        Criterion where = Restrictions.not(Restrictions.like(
                                "name", "Unknown%"));
                        where = Restrictions.and(where, Restrictions
                                .not(Restrictions.like("name", "%SubGrid%")));
                        crit.add(where);
                        return crit.list();
                    }
                });
    }

    @SuppressWarnings("unchecked")
    public List<? extends GridCoverage> loadSubGrids() {
        return (List<GridCoverage>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        HibernateTemplate ht = getHibernateTemplate();
                        Session sess = ht.getSessionFactory()
                                .getCurrentSession();
                        Criteria crit = sess.createCriteria(GridCoverage.class);
                        Criterion where = Restrictions
                                .like("name", "%SubGrid%");
                        crit.add(where);
                        return crit.list();
                    }
                });
    }

    @SuppressWarnings("unchecked")
    public List<? extends GridCoverage> loadUnknownGrids() {
        return (List<GridCoverage>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        HibernateTemplate ht = getHibernateTemplate();
                        Session sess = ht.getSessionFactory()
                                .getCurrentSession();
                        Criteria crit = sess.createCriteria(GridCoverage.class);
                        Criterion where = Restrictions.like("name", "Unknown%");
                        crit.add(where);
                        return crit.list();
                    }
                });
    }

    public boolean deleteCoverageAssociatedData(GridCoverage cov,
            boolean deleteCoverage) {
        boolean rval = false;
        try {
            GribModelDao modelDao = new GribModelDao();
            // query for models to delete
            List<String> modelNames = modelDao.getModelNamesForCoverage(cov
                    .getId());
            for (String modelName : modelNames) {
                handler.info("Coverage for model " + modelName
                        + " has changed.  Deleting prior records");

                modelDao.deleteModelAndAssociatedData(modelName);
            }

            // delete coverage
            if (deleteCoverage) {
                this.delete(cov);
            }

            rval = true;
        } catch (Exception e) {
            handler.error("Error occurred deleting coverage associated data", e);
        }
        return rval;
    }
}
