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

import java.util.List;
import java.util.SortedSet;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;

/**
 * Hibernate {@link IBandwidthBucketDao}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2013 2106       djohnson     Initial creation
 * Dec 3,  2013  1736      dhladky      Bandwidth bucket size attenuation.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthBucketDao extends
        SessionManagedDao<Long, BandwidthBucket> implements IBandwidthBucketDao {

    private static final String DELETE_EMPTY_UP_TO_TIME = "delete from BandwidthBucket bb where bb.network = :network and bb.currentSize = 0";

    private static final String GET_ALL_FOR_NETWORK = "from BandwidthBucket bb where bb.network = :network";

    private static final String GET_BY_START_TIME = "from BandwidthBucket bb where bb.network = :network and bb.bucketStartTime = :bucketStartTime";

    private static final String GET_WHERE_START_TIME_IS_LESS_THAN_OR_EQUAL = "from BandwidthBucket bb where bb.network = :network and bb.bucketStartTime <= :bucketStartTime";

    private static final String GET_BY_EARLIEST_START_TIME = "from BandwidthBucket bb where bb.network = :network and bb.bucketStartTime = "
            + "(select min(bucketStartTime) from BandwidthBucket bb where bb.network = :network)";

    private static final String GET_BY_LATEST_START_TIME = "from BandwidthBucket bb where bb.network = :network and bb.bucketStartTime = "
            + "(select max(bucketStartTime) from BandwidthBucket bb where bb.network = :network)";

    private static final String GET_WHERE_START_TIME_IS_BETWEEN_INCLUSIVE = "from BandwidthBucket bb where bb.network = :network and bb.bucketStartTime between :earliestTime and :latestTime";

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTime(long timeToDeleteUpTo,
            Network network) throws DataAccessLayerException {
        executeHQLStatement(DELETE_EMPTY_UP_TO_TIME, "network", network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthBucket> getAll(Network network) {
        return executeHQLQuery(GET_ALL_FOR_NETWORK, "network", network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthBucket getLastBucket(Network network) {
        return uniqueResult(GET_BY_LATEST_START_TIME, "network", network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthBucket getFirstBucket(Network network) {
        return uniqueResult(GET_BY_EARLIEST_START_TIME, "network", network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthBucket> getWhereStartTimeIsLessThanOrEqualTo(
            long time, Network network) {
        return executeHQLQuery(GET_WHERE_START_TIME_IS_LESS_THAN_OR_EQUAL,
                "network", network, "bucketStartTime", time);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthBucket getByStartTime(long startTime, Network network) {
        return uniqueResult(GET_BY_START_TIME, "network", network,
                "bucketStartTime", startTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<BandwidthBucket> getBucketsInWindow(Long startMillis,
            Long endMillis, Network network) {
        return Sets.<BandwidthBucket> newTreeSet(this
                .<BandwidthBucket> executeHQLQuery(
                        GET_WHERE_START_TIME_IS_BETWEEN_INCLUSIVE, "network",
                        network, "earliestTime", startMillis, "latestTime",
                        endMillis));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copyState(IBandwidthBucketDao bucketsDao) {
        deleteAll(getAll());
        for (Network network : Network.values()) {
            for (BandwidthBucket bucket : bucketsDao.getAll(network)) {
                create(bucket.copy());
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<BandwidthBucket> getEntityClass() {
        return BandwidthBucket.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthBucket getBucketContainingTime(long millis,
            Network network) {

        List<BandwidthBucket> buckets = getWhereStartTimeIsLessThanOrEqualTo(
                millis, network);
        // last bucket.
        if (!buckets.isEmpty()) {
            return buckets.get(buckets.size() -1);
        } else {
            return null;
        }
    }
}