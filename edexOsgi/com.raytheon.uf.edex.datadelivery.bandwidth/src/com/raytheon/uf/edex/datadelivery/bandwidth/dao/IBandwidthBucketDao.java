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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.util.List;
import java.util.SortedSet;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Interface for a DAO that manages {@link BandwidthBucket} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013 2106       djohnson     Initial creation
 * Dec 2, 2013  1736       dhladky      Needed to add registry bandwidth utilization attenuation.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthBucketDao {

    /**
     * Create the bandwidth bucket.
     * 
     * @param bandwidthBucket
     */
    void create(BandwidthBucket bandwidthBucket);

    /**
     * Update the bandwidth bucket.
     * 
     * @param bandwidthBucket
     */
    void update(BandwidthBucket bandwidthBucket);

    /**
     * Delete all bandwidth buckets up to and including the specified time.
     * 
     * @param timeToDeleteUpTo
     * @param network
     *            the network
     * @throws DataAccessLayerException
     */
    void deleteEmptyBucketsUpToTime(long timeToDeleteUpTo, Network network)
            throws DataAccessLayerException;

    /**
     * Get all bandwidth buckets.
     * 
     * @param network
     *            the network
     * @return all bandwidth buckets for the network
     */
    List<BandwidthBucket> getAll(Network network);

    /**
     * Get the {@link BandwidthBucket} with the latest start time.
     * 
     * @param network
     *            the network
     * @return
     */
    BandwidthBucket getLastBucket(Network network);

    /**
     * Get the {@link BandwidthBucket} with the earliest start time.
     * 
     * @param network
     *            the network
     * @return
     */
    BandwidthBucket getFirstBucket(Network network);

    /**
     * Get where the bucket start time is less than or equal to the specified
     * time.
     * 
     * @param time
     *            the latest time to include
     * @param network
     *            the network
     * @return the buckets
     */
    List<BandwidthBucket> getWhereStartTimeIsLessThanOrEqualTo(long time,
            Network network);

    /**
     * Get the bucket by its start time.
     * 
     * @param startTime
     * @param network
     *            the network
     * @return
     */
    BandwidthBucket getByStartTime(long startTime, Network network);

    /**
     * Return the buckets in the specified window, both boundaries are
     * inclusive. Buckets will be in order of their start time.
     * 
     * @param startMillis
     *            the start time for buckets to include
     * @param endMillis
     *            the end time for buckets to include
     * @param network
     *            the network
     * @return the buckets in the window, sorted in ascending order of start
     *         time
     */
    public SortedSet<BandwidthBucket> getBucketsInWindow(Long startMillis,
            Long endMillis, Network network);

    /**
     * Copy the state from another bucket dao.
     * 
     * @param bucketsDao
     */
    void copyState(IBandwidthBucketDao bucketsDao);
    
    /**
     * Finds the Bandwidth Bucket that contains the given time, null if none exists.
     * @param millis
     * @param Network
     * @return
     */
    public BandwidthBucket getBucketContainingTime(long millis, Network network);

}
