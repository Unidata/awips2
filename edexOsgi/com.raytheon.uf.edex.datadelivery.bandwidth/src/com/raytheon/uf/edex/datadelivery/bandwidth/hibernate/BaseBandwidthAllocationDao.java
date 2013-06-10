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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Abstract DAO instance providing common queries among
 * {@link BandwidthAllocation} DAOs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
abstract class BaseBandwidthAllocationDao<ENTITY extends BandwidthAllocation>
        extends SessionManagedDao<Long, ENTITY> implements IBaseBandwidthAllocationDao<ENTITY> {

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_SUBSCRIPTION_ID = "from %s res where res.bandwidthSubscription.id = :subscriptionId";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_NETWORK = "from %s res where res.network = :network";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_STATE = "from %s res where res.status = :state";

    private static final String GET_DEFERRED = "from %s alloc where "
            + "alloc.status = :status and "
            + "alloc.network = :network and "
            + "alloc.endTime <= :endTime";

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ENTITY> getBySubscriptionId(Long subscriptionId) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("subscriptionId", subscriptionId);
        return query(String.format(
                GET_BANDWIDTH_ALLOCATIONS_BY_SUBSCRIPTION_ID, getEntityClass()
                        .getSimpleName()), params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ENTITY> getByNetwork(Network network) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("network", network);
        return query(String.format(GET_BANDWIDTH_ALLOCATIONS_BY_NETWORK,
                getEntityClass().getSimpleName()), params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ENTITY> getByState(RetrievalStatus state) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("state", state);
        return query(String.format(GET_BANDWIDTH_ALLOCATIONS_BY_STATE,
                getEntityClass().getSimpleName()), params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ENTITY> getDeferred(Network network,
            Calendar endTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("status", RetrievalStatus.DEFERRED);
        params.put("network", network);
        params.put("endTime", endTime);
        return query(
                String.format(GET_DEFERRED, getEntityClass().getSimpleName()),
                params);
    }
}
