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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SatelliteCoverageCache {

    private static final String GID = "gid";

    /**
     * Map containing cached coverages, all access to this map must be
     * syncronized.
     */
    private final Map<Integer, SatMapCoverage> cache = new HashMap<Integer, SatMapCoverage>();

    public SatelliteCoverageCache() throws DataCubeException {
        /* Prepopulate the cache. */
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(SatMapCoverage.class);
        handleRequest(request);
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
