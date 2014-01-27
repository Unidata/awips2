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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthBucketDescription;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;

/**
 * Test class for {@link BandwidthGraphdataAdapter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2013            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BandwidthGraphDataAdapterTest {

    private static final int BUCKET_SIZE = 100;

    private static final RetrievalManager retrievalManagerMock = mock(RetrievalManager.class);

    private static final RetrievalPlan retrievalPlan = new RetrievalPlan();

    private static final SortedSet<BandwidthBucket> bucketSet = new TreeSet<BandwidthBucket>();

    private static final BandwidthGraphDataAdapter adapter = new BandwidthGraphDataAdapter(
            retrievalManagerMock);

    @Before
    public void classSetup() {
        bucketSet.clear();
        long bucketSizeMinutes = 3 * TimeUtil.MILLIS_PER_MINUTE;
        long bucketStart = bucketSizeMinutes;

        // Create 10 buckets
        for (int i = 0; i < 10; i++) {
            BandwidthBucket bucket = new BandwidthBucket(bucketStart,
                    BUCKET_SIZE, Network.OPSNET);
            bucketSet.add(bucket);
            bucketStart += bucketSizeMinutes;
        }
    }

    @Test
    public void testAdapterCreatesTheCorrectNumberObjects() {
        SortedSet<BandwidthBucketDescription> descriptions = adapter
                .toDescriptions(bucketSet);

        assertTrue("Incorrect number of descriptions created",
                descriptions.size() == bucketSet.size());
    }

    @Test
    public void testToDescriptionFillsOneBucketOnly() {
        Iterator<BandwidthBucket> iter = bucketSet.iterator();

        // Get the first bucket and fill it full
        BandwidthBucket bucket = iter.next();
        bucket.setCurrentSize(BUCKET_SIZE);

        int idx = 0;
        SortedSet<BandwidthBucketDescription> descriptions = adapter
                .toDescriptions(bucketSet);

        Iterator<BandwidthBucketDescription> it = descriptions.iterator();
        while (it.hasNext()) {
            BandwidthBucketDescription bbd = it.next();
            if (idx == 0) {
                assertTrue("First bucket should be full",
                        bbd.getUsedBytes() == bucket.getBucketSize());
                idx++;
            } else {
                assertTrue("Bucket should be empty", bbd.getUsedBytes() == 0);
            }
        }
    }

    @Test
    public void testToDescriptionFillsTwoBucketsFull() {
        Iterator<BandwidthBucket> iter = bucketSet.iterator();

        // Get the first 2 buckets and fill them full
        BandwidthBucket bucket = iter.next();
        bucket.setCurrentSize(BUCKET_SIZE);
        bucket = iter.next();
        bucket.setCurrentSize(BUCKET_SIZE);

        int idx = 0;
        SortedSet<BandwidthBucketDescription> descriptions = adapter
                .toDescriptions(bucketSet);

        Iterator<BandwidthBucketDescription> it = descriptions.iterator();
        while (it.hasNext()) {
            BandwidthBucketDescription bbd = it.next();
            if (idx == 0 || idx == 1) {
                assertTrue("First two buckets should be full",
                        bbd.getUsedBytes() == bucket.getBucketSize());
                idx++;
            } else {
                assertTrue("Bucket should be empty", bbd.getUsedBytes() == 0);
            }
        }
    }

    @Test
    public void testOverfilledBucketsOverflowToLaterBuckets() {
        Iterator<BandwidthBucket> iter = bucketSet.iterator();

        // Get the first bucket and fill it 4.5 buckets worth
        BandwidthBucket bucket = iter.next();
        bucket.setCurrentSize((BUCKET_SIZE * 4) + (BUCKET_SIZE / 2));

        int idx = 0;
        SortedSet<BandwidthBucketDescription> descriptions = adapter
                .toDescriptions(bucketSet);

        Iterator<BandwidthBucketDescription> it = descriptions.iterator();
        while (it.hasNext()) {
            BandwidthBucketDescription bbd = it.next();
            if (idx <= 3) {
                assertTrue("First four buckets should be full",
                        bbd.getUsedBytes() == bucket.getBucketSize());
                idx++;
            } else if (idx == 4) {
                assertTrue("Bucket should be half full",
                        bbd.getUsedBytes() == bucket.getBucketSize() / 2);
                idx++;
            } else {
                assertTrue("Bucket should be empty", bbd.getUsedBytes() == 0);
            }
        }
    }
}
