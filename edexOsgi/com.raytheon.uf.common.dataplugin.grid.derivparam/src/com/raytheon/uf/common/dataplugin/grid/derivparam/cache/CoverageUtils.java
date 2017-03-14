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
package com.raytheon.uf.common.dataplugin.grid.derivparam.cache;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridCacheUpdater.GridUpdateListener;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * Cache for coverages as well as several utility methods for reprojecting data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------
 * Jan 08, 2010  3965     rjpeter   Initial creation
 * Jul 25, 2013  2112     bsteffen  Fix volume browser sounding errors.
 * May 19, 2014  2913     bsteffen  Remove dead code.
 * Mar 03, 2016  5439     bsteffen  Move to common
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class CoverageUtils implements GridUpdateListener {
    private static CoverageUtils instance;

    private final Map<String, Set<UniqueIdGridCoverageWrapper>> coverageCache = new HashMap<>();

    private boolean hasPerformedBulkQuery = false;

    private boolean updatesEnabled = false;

    private CoverageUtils() {
    }

    public static synchronized CoverageUtils getInstance() {
        if (instance == null) {
            instance = new CoverageUtils();
            GridCacheUpdater.getInstance().addListener(instance);
        }

        return instance;
    }

    /**
     * Return an unordered collection of all GridCoverages that are used for a
     * given datasetId.
     * 
     * @param datasetId
     * @return
     * @throws VizException
     */
    public Collection<GridCoverage> getCoverages(String datasetId)
            throws DataCubeException {
        Collection<UniqueIdGridCoverageWrapper> rval = coverageCache
                .get(datasetId);

        if (rval == null) {
            DbQueryRequest query = new DbQueryRequest();
            query.setEntityClass(GridInfoRecord.class.getName());
            query.setDistinct(true);
            query.addRequestField(GridInfoConstants.DATASET_ID);
            query.addRequestField(GridInfoConstants.LOCATION_ID);
            if (hasPerformedBulkQuery || !updatesEnabled) {
                /*
                 * first time through just request everything. if the cache is
                 * empty request data for all models.
                 */
                query.addConstraint(GridInfoConstants.DATASET_ID,
                        new RequestConstraint(datasetId));
            }
            hasPerformedBulkQuery = updatesEnabled;
            DbQueryResponse resp = null;
            try {
                resp = (DbQueryResponse) RequestRouter.route(query);
            } catch (Exception e) {
                throw new DataCubeException(
                        "Error requesting coverage information.", e);
            }
            /*
             * do a bulk request to GridCoverageLookup as it enables more
             * possible optimizations.
             */
            List<Integer> locationsToRequest = new ArrayList<>(resp
                    .getResults().size());
            for (Map<String, Object> map : resp.getResults()) {
                Integer locationId = (Integer) map
                        .get(GridInfoConstants.LOCATION_ID);
                locationsToRequest.add(locationId);
            }
            Map<Integer, GridCoverage> requestedLocations = GridCoverageLookup
                    .getInstance().getCoverages(locationsToRequest);
            for (Map<String, Object> map : resp.getResults()) {
                Integer locationId = (Integer) map
                        .get(GridInfoConstants.LOCATION_ID);
                String resultId = (String) map
                        .get(GridInfoConstants.DATASET_ID);
                GridCoverage coverage = requestedLocations.get(locationId);
                Set<UniqueIdGridCoverageWrapper> set = coverageCache
                        .get(resultId);
                if (set == null) {
                    set = new HashSet<>();
                    coverageCache.put(resultId, set);
                }
                set.add(new UniqueIdGridCoverageWrapper(coverage));
            }
            rval = coverageCache.get(datasetId);
            if (rval == null) {
                HashSet<UniqueIdGridCoverageWrapper> set = new HashSet<>(0);
                coverageCache.put(datasetId, set);
                rval = set;

            }
        }
        List<GridCoverage> finalSet = new ArrayList<>(rval.size());
        for (UniqueIdGridCoverageWrapper wrapper : rval) {
            finalSet.add(wrapper.getGridCoverage());
        }
        return finalSet;
    }

    /**
     * Update the cache for the given model with the supplied coverage. Should
     * only be used for non-grid sources being imported as grid data.
     * 
     * @param modelName
     * @param coverage
     */
    public void setCoverage(String modelName, GridCoverage coverage) {
        if (modelName != null && coverage != null) {
            Set<UniqueIdGridCoverageWrapper> set = coverageCache.get(modelName);
            if (set == null) {
                set = new HashSet<>();
                coverageCache.put(modelName, set);
            }
            set.add(new UniqueIdGridCoverageWrapper(coverage));
        }
    }

    @Override
    public void update(GridRecord record) {
        String datasetId = record.getDatasetId();
        GridCoverage coverage = record.getLocation();
        Set<UniqueIdGridCoverageWrapper> set = coverageCache.get(datasetId);
        if (set != null && coverage != null) {
            set.add(new UniqueIdGridCoverageWrapper(coverage));
        }
    }

    @Override
    public void enableUpdates() {
        updatesEnabled = true;
    }

    @Override
    public void disableUpdates() {
        updatesEnabled = false;
        coverageCache.clear();
        hasPerformedBulkQuery = false;

    }

    /*
     * This class exists so that two coverages that are otherwise equal can be
     * stored together in a set if they have different IDs.
     */
    private static class UniqueIdGridCoverageWrapper {
        private final GridCoverage gridCoverage;

        public UniqueIdGridCoverageWrapper(GridCoverage gridCoverage) {
            super();
            this.gridCoverage = gridCoverage;
        }

        public GridCoverage getGridCoverage() {
            return gridCoverage;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((gridCoverage == null) ? 0 : gridCoverage.hashCode());
            result = prime
                    * result
                    + ((gridCoverage.getId() == null) ? 0 : gridCoverage
                            .getId().hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            UniqueIdGridCoverageWrapper other = (UniqueIdGridCoverageWrapper) obj;
            if (gridCoverage == null) {
                if (other.gridCoverage != null)
                    return false;
            } else if (!gridCoverage.equals(other.gridCoverage)) {
                return false;
            } else if (!gridCoverage.getId().equals(
                    other.getGridCoverage().getId())) {
                return false;
            }
            return true;
        }

    }

}
