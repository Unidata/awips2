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

import java.util.List;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;

/**
 * Provides associations between bandwidth buckets and their allocations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2013 2106       djohnson     Extracted from {@link BandwidthBucket}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface IBandwidthBucketAllocationAssociator {

    /**
     * Adds the {@link BandwidthAllocation} to the specified bucket.
     * 
     * @param bucket
     *            the bucket
     * @param allocation
     *            the allocation
     * @throws NullPointerException
     *             if the bucket start time is invalid
     */
    void addToBucket(BandwidthBucket bucket, BandwidthAllocation allocation);

    /**
     * Adds the {@link BandwidthReservation} to the specified bucket.
     * 
     * @param bucket
     *            the bucket
     * @param reservation
     *            the reservation
     * @throws NullPointerException
     *             if the bucket start time is invalid
     */
    void addToBucket(BandwidthBucket bucket, BandwidthReservation reservation);

    /**
     * Remove the {@link BandwidthAllocation} from the {@link BandwidthBucket}.
     * 
     * @param bucket
     * @param reservation
     */
    void removeFromBucket(BandwidthBucket bucket,
            BandwidthAllocation reservation);

    /**
     * Remove the {@link BandwidthAllocation} from the
     * {@link BandwidthReservation}.
     * 
     * @param bucket
     * @param reservation
     */
    void removeFromBucket(BandwidthBucket bucket,
            BandwidthReservation reservation);

    /**
     * @param bucket
     * @return
     */
    String showBucket(BandwidthBucket bucket);

    /**
     * Get the next allocation to be processed.
     * 
     * @param bucket
     * @param agentType
     *            the agent type
     * @return the allocation, or null if there is none to process
     */
    BandwidthAllocation getNextReservation(BandwidthBucket bucket,
            String agentType);

    /**
     * Get the {@link BandwidthAllocation}s for the {@link BandwidthBucket}.
     * 
     * @param bucket
     * @return
     */
    List<BandwidthAllocation> getBandwidthAllocationsForBucket(
            BandwidthBucket bucket);

    /**
     * Get the {@link BandwidthReservation}s for the {@link BandwidthBucket}.
     * 
     * @param bucket
     * @return
     */
    List<BandwidthReservation> getBandwidthReservationsForBucket(
            BandwidthBucket bucket);

    /**
     * Copy the state from another instance.
     * 
     * @param other
     */
    void copyState(IBandwidthBucketAllocationAssociator other);
}
