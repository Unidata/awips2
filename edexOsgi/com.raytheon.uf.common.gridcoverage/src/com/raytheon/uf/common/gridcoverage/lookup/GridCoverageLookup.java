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
package com.raytheon.uf.common.gridcoverage.lookup;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.request.GetGridCoverageRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Caching and convenience for finding coverages in the Database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridCoverageLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridCoverageLookup.class);

    private static GridCoverageLookup instance;

    public static GridCoverageLookup getInstance() {
        if (instance == null) {
            instance = new GridCoverageLookup();
        }
        return instance;
    }

    // Maps id to coverage, this is a mirror of what is in the database.
    private Map<Integer, GridCoverage> idToCoverage;

    private Map<GridCoverage, Integer> coverageToId;

    private GridCoverageLookup() {
        initializeMaps();
        DbQueryRequest query = new DbQueryRequest();
        query.setEntityClass(GridCoverage.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            for (Map<String, Object> map : resp.getResults()) {
                GridCoverage coverage = (GridCoverage) map.get(null);
                coverageToId.put(coverage, coverage.getId());
                idToCoverage.put(coverage.getId(), coverage);
            }
        } catch (Exception e) {
            // do not rethrow, the lookup is not broken at this point so if the
            // problems persist then more exceptions will come from the actual
            // lookup methods themselves.
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred retrieving coverages from server.", e);
        }
    }

    private void initializeMaps() {
        idToCoverage = Collections
                .synchronizedMap(new HashMap<Integer, GridCoverage>());
        coverageToId = Collections
                .synchronizedMap(new GridCoverageSpatialMap());
    }

    public GridCoverage getCoverage(int id) throws GridCoverageLookupException {
        GridCoverage result = idToCoverage.get(id);
        if (result != null) {
            return result;
        }
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("id", new RequestConstraint(Integer.toString(id)));
        DbQueryRequest query = new DbQueryRequest();
        query.setConstraints(constraints);
        query.setEntityClass(GridCoverage.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            if (!resp.getResults().isEmpty()) {
                result = (GridCoverage) resp.getResults().get(0).get(null);
                if (result != null) {
                    coverageToId.put(result, result.getId());
                    idToCoverage.put(result.getId(), result);
                }
                return result;
            }
        } catch (Exception e) {
            throw new GridCoverageLookupException(
                    "Error occurred retrieving GridCoverage information from server.",
                    e);
        }
        return null;
    }

    /**
     * bulk request multiple coverages by id, for any coverages not in the cache
     * this can be significantly faster than requesting multiple coverages
     * individually.
     * 
     * @param ids
     * @return
     */
    public Map<Integer, GridCoverage> getCoverages(List<Integer> ids)
            throws GridCoverageLookupException {
        RequestConstraint idConstraint = new RequestConstraint(null,
                ConstraintType.IN);
        Map<Integer, GridCoverage> result = new HashMap<Integer, GridCoverage>(
                ids.size());
        for (Integer id : ids) {
            GridCoverage cov = idToCoverage.get(id);
            if (cov == null) {
                idConstraint.addToConstraintValueList(id.toString());
            }
            result.put(id, cov);
        }
        if (idConstraint.getConstraintValue() == null) {
            // everything was a cache hit.
            return result;
        }
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("id", idConstraint);
        DbQueryRequest query = new DbQueryRequest();
        query.setConstraints(constraints);
        query.setEntityClass(GridCoverage.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            for (Map<String, Object> thing : resp.getResults()) {
                GridCoverage respCov = (GridCoverage) thing.get(null);
                coverageToId.put(respCov, respCov.getId());
                idToCoverage.put(respCov.getId(), respCov);
                result.put(respCov.getId(), respCov);
            }
        } catch (Exception e) {
            throw new GridCoverageLookupException(
                    "Error occurred retrieving GridCoverage information from server.",
                    e);
        }
        return result;
    }

    public GridCoverage getCoverage(GridCoverage coverage, boolean create)
            throws GridCoverageLookupException {
        Integer id = coverageToId.get(coverage);
        if (id != null) {
            return getCoverage(id);
        }
        try {
            GridCoverage result = (GridCoverage) RequestRouter
                    .route(new GetGridCoverageRequest(coverage, create));
            if (result != null) {
                coverageToId.put(result, result.getId());
                idToCoverage.put(result.getId(), result);
            }
            return result;
        } catch (Exception e) {
            throw new GridCoverageLookupException(
                    "Error occurred retrieving GridCoverage information from server.",
                    e);
        }
    }

}
