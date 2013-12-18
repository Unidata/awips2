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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;

/**
 * Holds the associations between {@link BandwidthBucket}s and their
 * {@link BandwidthAllocation}s and {@link BandwidthReservation}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2013 2106       djohnson     Extracted from {@link BandwidthBucket} and {@link RetrievalPlan}.
 * Dec 17, 2013 2636       bgonzale     Prevent stale BandwidthAllocation updates by retrieving 
 *                                      them from the dao before updating.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class InMemoryBandwidthBucketAllocationAssociator implements
        IBandwidthBucketAllocationAssociator {

    private final IBandwidthBucketDao bucketsDao;

    private final IBandwidthDao bandwidthDao;

    private final Multimap<Long, BandwidthReservation> reservations = ArrayListMultimap
            .create();

    private final Multimap<Long, BandwidthAllocation> allocations = ArrayListMultimap
            .create();

    public InMemoryBandwidthBucketAllocationAssociator(
            IBandwidthDao bandwidthDao, IBandwidthBucketDao bucketsDao) {
        this.bandwidthDao = bandwidthDao;
        this.bucketsDao = bucketsDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addToBucket(BandwidthBucket bucket,
            BandwidthAllocation allocation) {
        allocations.put(bucket.getIdentifier(), allocation);
        bucketsDao.update(bucket);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addToBucket(BandwidthBucket bucket,
            BandwidthReservation reservation) {
        reservations.put(bucket.getIdentifier(), reservation);
        bucketsDao.update(bucket);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeFromBucket(BandwidthBucket bucket,
            BandwidthAllocation reservation) {
        allocations.remove(bucket.getIdentifier(), reservation);
        bucketsDao.update(bucket);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeFromBucket(BandwidthBucket bucket,
            BandwidthReservation reservation) {
        reservations.remove(bucket.getIdentifier(), reservation);
        bucketsDao.update(bucket);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String showBucket(BandwidthBucket bucket) {
        StringBuilder sb = new StringBuilder();
        sb.append(bucket.toString()).append("\n");

        for (BandwidthAllocation allocation : allocations.get(bucket
                .getIdentifier())) {
            sb.append("  ").append(allocation.toString()).append("\n");
        }
        for (BandwidthReservation reservation : reservations.get(bucket
                .getIdentifier())) {
            sb.append("  ").append(reservation.toString()).append("\n");
        }

        return sb.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthAllocation getNextReservation(BandwidthBucket bucket,
            String agentType) {
        BandwidthAllocation allocation = null;
        for (BandwidthAllocation o : allocations.get(bucket.getIdentifier())) {
            if (RetrievalStatus.READY.equals(o.getStatus())
                    && o.getAgentType().equals(agentType)) {
                allocation = bandwidthDao
                        .getBandwidthAllocation(o.getId());
                if (allocation == null) {
                    // allocation was removed from persistence, sync the
                    // mapping
                    allocations.remove(o.getId(), o);
                } else {
                    allocation.setStatus(RetrievalStatus.PROCESSING);
                    bandwidthDao.createOrUpdate(allocation);
                }
                break;
            }
        }
        return allocation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocationsForBucket(
            BandwidthBucket bucket) {
        return new ArrayList<BandwidthAllocation>(allocations.get(bucket
                .getIdentifier()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthReservation> getBandwidthReservationsForBucket(
            BandwidthBucket bucket) {
        return new ArrayList<BandwidthReservation>(reservations.get(bucket
                .getIdentifier()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copyState(IBandwidthBucketAllocationAssociator other) {
        for (Network network : Network.values()) {
            final List<BandwidthBucket> buckets = bucketsDao.getAll(network);
            for (BandwidthBucket bucket : buckets) {

                final List<BandwidthAllocation> bandwidthAllocationsForBucket = other
                        .getBandwidthAllocationsForBucket(bucket);
                for (BandwidthAllocation allocation : bandwidthAllocationsForBucket) {
                    this.addToBucket(bucket, allocation);
                }

                final List<BandwidthReservation> bandwidthReservationsForBucket = other
                        .getBandwidthReservationsForBucket(bucket);
                for (BandwidthReservation reservation : bandwidthReservationsForBucket) {
                    this.addToBucket(bucket, reservation);
                }
            }
        }
    }
}
