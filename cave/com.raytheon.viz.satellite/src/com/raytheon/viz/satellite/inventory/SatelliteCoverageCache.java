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
package com.raytheon.viz.satellite.inventory;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * A cache of {@link SatMapCoverage} based off the gid field. This cache can be
 * used to some types of satellite queries that need the SatMapCoverage because
 * the query can only request the gid field and then load the full coverage from
 * cache.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Apr 09, 2014  2947     bsteffen  Initial creation
 * Jun 19, 2015  4554     bsteffen  Limit the damage done when the bulk query fails.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SatelliteCoverageCache {
    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(SatelliteCoverageCache.class);

    private static final String GID = "gid";

    /**
     * Map containing cached coverages, all access to this map must be
     * syncronized.
     */
    private final Map<Integer, SatMapCoverage> cache = new HashMap<Integer, SatMapCoverage>();

    public SatelliteCoverageCache() {
        /* Prepopulate the cache. */
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(SatMapCoverage.class);
        /*
         * On normal systems there are not more than a few hundred coverages so
         * the limit statement does nothing. There are certain error conditions
         * on the server that can cause it to have more coverages so the limit
         * statement is in place to prevent the query from failing due to the
         * size of the results. In this case any coverages that are needed
         * beyond the original 1024 will end up being requested individually.
         */
        request.setLimit(1024);
        try {
            handleRequest(request);
        } catch (DataCubeException e) {
            /*
             * This is non-fatal because the coverages will be requested
             * indivdually when they are needed.
             */
            logger.handle(Priority.DEBUG, getClass().getSimpleName()
                    + " failed to bulk retrieve coverages.", e);
        }
    }

    public SatMapCoverage get(int gid) throws DataCubeException {
        synchronized (cache) {
            SatMapCoverage result = cache.get(gid);
            if (result == null) {
                DbQueryRequest request = new DbQueryRequest();
                request.setEntityClass(SatMapCoverage.class);
                request.addConstraint(GID,
                        new RequestConstraint(Integer.toString(gid)));
                handleRequest(request);
                result = cache.get(gid);
            }
            return result;
        }
    }

    private void handleRequest(DbQueryRequest request) throws DataCubeException {
        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            throw new DataCubeException(e);
        }
        SatMapCoverage[] coverages = response
                .getEntityObjects(SatMapCoverage.class);
        synchronized (cache) {
            for (SatMapCoverage coverage : coverages) {
                cache.put(coverage.getGid(), coverage);
            }
        }
    }

}
