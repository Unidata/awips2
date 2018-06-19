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
package com.raytheon.uf.edex.gridcoverage;

import java.util.List;
import java.util.concurrent.locks.Lock;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.request.GetGridCoverageRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.cluster.lock.EdexClusterLockMgr;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Handles database access of grid coverage, GetGridCoverageRequest can be used
 * to do spatial lookup and creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 26, 2012           bsteffen    Initial creation
 * Mar 07, 2013  1771     bsteffen    fix gridcoverage duplicate checks.
 * Mar 20, 2013  2910     bsteffen    Commit transaction within cluster locks.
 * May 10, 2016  5642     rjpeter     Remove transaction nesting.
 * Jun 24, 2016  ASM18440 dfriedman   Fix spatial tolerance for degree values.
 * Jan 19, 2017  3440     njensen     Inject and use EdexClusterLockMgr
 * Feb 27, 2017  3440     njensen     More unique lock name
 * Apr 21, 2017  5838     bsteffen    Do not restrict checks by lo1.
 * 
 * </pre>
 * 
 * @author bsteffen
 */

public class GetGridCoverageHandler
        implements IRequestHandler<GetGridCoverageRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetGridCoverageHandler.class);

    private final CoreDao dao;

    private final EdexClusterLockMgr lockMgr;

    public GetGridCoverageHandler(EdexClusterLockMgr lockMgr) {
        dao = new CoreDao(DaoConfig.forClass(GridCoverage.class));
        this.lockMgr = lockMgr;
    }

    @Override
    public GridCoverage handleRequest(GetGridCoverageRequest request)
            throws Exception {
        GridCoverage coverage = request.getCoverage();
        GridCoverage rval = checkDatabase(coverage, false);

        if ((rval == null) && request.getCreate()) {
            /*
             * Get cluster lock to ensure only 1 coverage created for a given
             * area
             */
            Lock lock = lockMgr.allocateLock(
                    "gridcoverage_create_" + coverage.getProjectionType());
            lock.lock();
            try {
                rval = checkDatabase(coverage, true);
            } finally {
                lock.unlock();
            }
        }

        return rval;
    }

    private GridCoverage checkDatabase(GridCoverage coverage, boolean create)
            throws Exception {
        GridCoverage rval = null;
        Session sess = null;
        Transaction trans = null;

        try {
            sess = dao.getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(coverage.getClass());
            double spacingUnitTolerance = coverage.getSpacingUnitTolerance();

            /*
             * The criteria will limit the results to a relatively small number
             * of coverages, but ultimately spatialEquals() is used to test if
             * the coverage matches any in the database.
             */
            crit.add(Restrictions.eq("nx", coverage.getNx()));
            crit.add(Restrictions.eq("ny", coverage.getNy()));
            crit.add(Restrictions.between("dx",
                    coverage.getDx() - spacingUnitTolerance,
                    coverage.getDx() + spacingUnitTolerance));
            crit.add(Restrictions.between("dy",
                    coverage.getDy() - spacingUnitTolerance,
                    coverage.getDy() + spacingUnitTolerance));
            crit.add(Restrictions.between("la1",
                    coverage.getLa1() - GridCoverage.SPATIAL_TOLERANCE_DEG,
                    coverage.getLa1() + GridCoverage.SPATIAL_TOLERANCE_DEG));
            /*
             * Do not restrict lo1 because depending on the central meridian it
             * could be off by 360°, let spatialEquals figure it out.
             */
            List<?> vals = crit.list();

            for (Object val : vals) {
                if (((GridCoverage) val).spatialEquals(coverage)) {
                    rval = (GridCoverage) val;
                }
            }

            // if it still does not exist, create it if requested
            if ((rval == null) && create) {
                coverage.initialize();
                sess.saveOrUpdate(coverage);
                trans.commit();
                rval = coverage;
            }
        } catch (Exception e) {
            statusHandler.error("Error occurred looking up GridCoverage["
                    + coverage.getName() + "]", e);

            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    statusHandler.error(
                            "Error occurred rolling back transaction", e1);
                }
            }

            throw e;
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    statusHandler.error("Error occurred closing session", e);
                }
            }
        }

        return rval;
    }
}
