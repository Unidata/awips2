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

import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;

/**
 * Test {@link IBandwidthBucketDao} implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2013 2106       djohnson     Initial creation
 * Sep 17, 2013 2383       bgonzale     Added test for same start and end time.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractBandwidthBucketDaoTest {

    protected IBandwidthBucketDao dao;

    @Before
    public void setUp() {
        dao = getBandwidthBucketDao();
    }

    @Test
    public void createdBandwidthBucketIsRetrievable() {
        BandwidthBucket bucket = new BandwidthBucket(1L, 100, Network.OPSNET);

        dao.create(bucket);

        assertThat(
                "Should have been able to retrieve the bucket by its start time after creating it!",
                dao.getByStartTime(bucket.getBucketStartTime(),
                        bucket.getNetwork()), is(equalTo(bucket)));
    }

    @Test
    public void bandwidthBucketIsUpdatable() {
        BandwidthBucket bucket = new BandwidthBucket(1L, 100, Network.OPSNET);

        dao.create(bucket);

        bucket.setBucketSize(10L);

        dao.update(bucket);

        assertThat(
                "Should have been able to update the bandwidth bucket!",
                dao.getByStartTime(bucket.getBucketStartTime(),
                        bucket.getNetwork()).getBucketSize(),
                is(equalTo(bucket.getBucketSize())));
    }

    @Test
    public void deleteEmptyBucketsUpToTimeRemovesEmptyBucketsBeforeTime()
            throws DataAccessLayerException {
        List<BandwidthBucket> bucketsThatShouldBeDeleted = Arrays.asList(
                new BandwidthBucket(1L, 100, Network.OPSNET),
                new BandwidthBucket(2L, 100, Network.OPSNET),
                new BandwidthBucket(3L, 100, Network.OPSNET));

        for (BandwidthBucket bucket : bucketsThatShouldBeDeleted) {
            dao.create(bucket);
        }

        dao.deleteEmptyBucketsUpToTime(4L, Network.OPSNET);

        assertThat(dao.getByStartTime(1L, Network.OPSNET),
                is(nullValue(BandwidthBucket.class)));
        assertThat(dao.getByStartTime(2L, Network.OPSNET),
                is(nullValue(BandwidthBucket.class)));
        assertThat(dao.getByStartTime(3L, Network.OPSNET),
                is(nullValue(BandwidthBucket.class)));
    }

    @Test
    public void deleteEmptyBucketsUpToTimeRemovesEmptyBucketAtTime()
            throws DataAccessLayerException {
        final BandwidthBucket bucket = new BandwidthBucket(3L, 100,
                Network.OPSNET);

        dao.create(bucket);

        dao.deleteEmptyBucketsUpToTime(bucket.getBucketStartTime(),
                bucket.getNetwork());

        assertThat(
                dao.getByStartTime(bucket.getBucketStartTime(),
                        bucket.getNetwork()),
                is(nullValue(BandwidthBucket.class)));
    }

    @Test
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsBeforeTime()
            throws DataAccessLayerException {
        final BandwidthBucket bucketThatShouldNotBeDeleted = new BandwidthBucket(
                1L, 100, Network.OPSNET);
        bucketThatShouldNotBeDeleted.setCurrentSize(1L);

        dao.create(bucketThatShouldNotBeDeleted);

        dao.deleteEmptyBucketsUpToTime(4L,
                bucketThatShouldNotBeDeleted.getNetwork());

        assertThat(dao.getByStartTime(
                bucketThatShouldNotBeDeleted.getBucketStartTime(),
                bucketThatShouldNotBeDeleted.getNetwork()), is(notNullValue()));
    }

    @Test
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsAtTime()
            throws DataAccessLayerException {
        final BandwidthBucket bucketThatShouldNotBeDeleted = new BandwidthBucket(
                1L, 100, Network.OPSNET);
        bucketThatShouldNotBeDeleted.setCurrentSize(1L);

        dao.create(bucketThatShouldNotBeDeleted);

        dao.deleteEmptyBucketsUpToTime(
                bucketThatShouldNotBeDeleted.getBucketStartTime(),
                bucketThatShouldNotBeDeleted.getNetwork());

        assertThat(dao.getByStartTime(
                bucketThatShouldNotBeDeleted.getBucketStartTime(),
                bucketThatShouldNotBeDeleted.getNetwork()), is(notNullValue()));
    }

    @Test
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveEmptyBucketsAfterTime()
            throws DataAccessLayerException {
        final BandwidthBucket bucketThatShouldNotBeDeleted = new BandwidthBucket(
                4L, 100, Network.OPSNET);
        bucketThatShouldNotBeDeleted.setCurrentSize(100L);

        dao.create(bucketThatShouldNotBeDeleted);

        dao.deleteEmptyBucketsUpToTime(
                bucketThatShouldNotBeDeleted.getBucketStartTime() - 1,
                bucketThatShouldNotBeDeleted.getNetwork());

        assertThat(dao.getByStartTime(
                bucketThatShouldNotBeDeleted.getBucketStartTime(),
                bucketThatShouldNotBeDeleted.getNetwork()), is(notNullValue()));
    }

    @Test
    public void getAllForNetworkReturnsAllBucketsInNetwork() {
        Network network = Network.OPSNET;
        List<BandwidthBucket> bucketsThatShouldBeFound = Arrays.asList(
                new BandwidthBucket(1L, 100, network), new BandwidthBucket(2L,
                        100, network), new BandwidthBucket(3L, 100, network));

        for (BandwidthBucket bucket : bucketsThatShouldBeFound) {
            dao.create(bucket);
        }

        final List<BandwidthBucket> allForNetwork = dao.getAll(network);

        assertThat(allForNetwork, is(equalTo(bucketsThatShouldBeFound)));
    }

    @Test
    public void getAllForNetworkDoesNotReturnBucketsNotInNetwork() {
        Network network = Network.OPSNET;
        List<BandwidthBucket> bucketsThatShouldNotBeFound = Arrays.asList(
                new BandwidthBucket(1L, 100, network), new BandwidthBucket(2L,
                        100, network), new BandwidthBucket(3L, 100, network));

        for (BandwidthBucket bucket : bucketsThatShouldNotBeFound) {
            dao.create(bucket);
        }

        final List<BandwidthBucket> allForNetwork = dao.getAll(Network.SBN);

        assertThat(allForNetwork, is(empty()));
    }

    @Test
    public void getLastBucketForNetworkReturnsLastBucket() {
        Network network = Network.OPSNET;
        final BandwidthBucket lastBucket = new BandwidthBucket(3L, 100, network);
        final List<BandwidthBucket> bucketsForNetwork = Arrays.asList(
                new BandwidthBucket(1L, 100, network), new BandwidthBucket(2L,
                        100, network), lastBucket);

        for (BandwidthBucket bucket : bucketsForNetwork) {
            dao.create(bucket);
        }

        assertThat(dao.getLastBucket(network), is(equalTo(lastBucket)));
    }

    @Test
    public void getWhereStartTimeIsLessThanOrEqualToReturnsAllBucketsBeforeGivenTime() {
        final Network network = Network.OPSNET;
        List<BandwidthBucket> expectedBuckets = Arrays.asList(
                new BandwidthBucket(1L, 100, network), new BandwidthBucket(2L,
                        100, network), new BandwidthBucket(3L, 100, network));
        for (BandwidthBucket bucket : expectedBuckets) {
            dao.create(bucket);
        }

        final List<BandwidthBucket> returned = dao
                .getWhereStartTimeIsLessThanOrEqualTo(4L, network);

        assertThat(returned, is(equalTo(expectedBuckets)));
    }

    @Test
    public void getWhereStartTimeIsLessThanOrEqualToReturnsBucketAtGivenTime() {

        final Network network = Network.OPSNET;
        final BandwidthBucket bucketWithEqualStartTime = new BandwidthBucket(
                3L, 100, network);
        List<BandwidthBucket> buckets = Arrays.asList(new BandwidthBucket(1L,
                100, network), new BandwidthBucket(2L, 100, network),
                bucketWithEqualStartTime);

        for (BandwidthBucket bucket : buckets) {
            dao.create(bucket);
        }

        final List<BandwidthBucket> returned = dao
                .getWhereStartTimeIsLessThanOrEqualTo(
                        bucketWithEqualStartTime.getBucketStartTime(),
                        bucketWithEqualStartTime.getNetwork());

        assertThat(returned.contains(bucketWithEqualStartTime), is(true));
    }

    @Test
    public void getWhereStartTimeIsLessThanOrEqualToDoesNotReturnBucketAfterGivenTime() {
        final Network network = Network.OPSNET;
        final BandwidthBucket bucketWithLaterStartTime = new BandwidthBucket(
                3L, 100, network);
        List<BandwidthBucket> buckets = Arrays.asList(new BandwidthBucket(1L,
                100, network), new BandwidthBucket(2L, 100, network),
                bucketWithLaterStartTime);

        for (BandwidthBucket bucket : buckets) {
            dao.create(bucket);
        }

        final List<BandwidthBucket> returned = dao
                .getWhereStartTimeIsLessThanOrEqualTo(2L, network);

        assertThat(returned, not(hasItem(bucketWithLaterStartTime)));
    }

    @Test
    public void getFirstBucketForNetworkReturnsFirstBucket() {
        Network network = Network.OPSNET;
        final BandwidthBucket firstBucket = new BandwidthBucket(1L, 100,
                network);
        final List<BandwidthBucket> bucketsForNetwork = Arrays.asList(
                firstBucket, new BandwidthBucket(2L, 100, network),
                new BandwidthBucket(3L, 100, network));

        for (BandwidthBucket bucket : bucketsForNetwork) {
            dao.create(bucket);
        }

        assertThat(dao.getFirstBucket(network), is(equalTo(firstBucket)));
    }

    @Test
    public void getByStartTimeReturnsBucketWithGivenStartTime() {
        BandwidthBucket bucket = new BandwidthBucket(1L, 100, Network.OPSNET);
        BandwidthBucket bucket2 = new BandwidthBucket(1L, 100, Network.SBN);

        dao.create(bucket);
        dao.create(bucket2);

        assertThat(
                "Should have been able to retrieve the bucket by its start time after creating it!",
                dao.getByStartTime(bucket2.getBucketStartTime(),
                        bucket2.getNetwork()), is(equalTo(bucket2)));
    }

    @Test
    public void getBucketsInWindowsReturnsBucketsBetweenTimes() {

        final Network network = Network.OPSNET;

        List<BandwidthBucket> tooEarlyBuckets = Arrays.asList(
                new BandwidthBucket(1L, 100, network), new BandwidthBucket(2L,
                        100, network), new BandwidthBucket(3L, 100, network));

        // Leaving 4L open as a boundary range
        List<BandwidthBucket> expectedBuckets = Arrays.asList(
                new BandwidthBucket(5L, 100, network), new BandwidthBucket(6L,
                        100, network), new BandwidthBucket(7L, 100, network));

        // Leaving 8L open as a boundary range
        List<BandwidthBucket> tooLateBuckets = Arrays.asList(
                new BandwidthBucket(9L, 100, network), new BandwidthBucket(10L,
                        100, network), new BandwidthBucket(11L, 100, network));

        for (BandwidthBucket bucket : tooEarlyBuckets) {
            dao.create(bucket);
        }
        for (BandwidthBucket bucket : expectedBuckets) {
            dao.create(bucket);
        }
        for (BandwidthBucket bucket : tooLateBuckets) {
            dao.create(bucket);
        }

        final SortedSet<BandwidthBucket> returned = dao.getBucketsInWindow(4L,
                8L, network);

        // Implicitly checks that buckets earlier than the start time and later
        // than the end time are excluded
        assertThat(new ArrayList<BandwidthBucket>(returned),
                is(equalTo(expectedBuckets)));
    }

    @Test
    public void getBucketsInWindowsReturnsBucketAtStartTime() {
        final Network network = Network.OPSNET;

        final BandwidthBucket earliestBucket = new BandwidthBucket(4L, 100,
                network);
        List<BandwidthBucket> buckets = Arrays.asList(earliestBucket,
                new BandwidthBucket(5L, 100, network), new BandwidthBucket(6L,
                        100, network));

        for (BandwidthBucket bucket : buckets) {
            dao.create(bucket);
        }

        final SortedSet<BandwidthBucket> returned = dao.getBucketsInWindow(
                earliestBucket.getBucketStartTime(), 8L, network);

        assertThat(returned, hasItem(earliestBucket));
    }

    @Test
    public void getBucketsInWindowsReturnsBucketAtEndTime() {

        final Network network = Network.OPSNET;

        final BandwidthBucket latestBucket = new BandwidthBucket(6L, 100,
                network);
        List<BandwidthBucket> buckets = Arrays.asList(new BandwidthBucket(4L,
                100, network), new BandwidthBucket(5L, 100, network),
                latestBucket);

        for (BandwidthBucket bucket : buckets) {
            dao.create(bucket);
        }

        final SortedSet<BandwidthBucket> returned = dao.getBucketsInWindow(3L,
                latestBucket.getBucketStartTime(), network);

        assertThat(returned, hasItem(latestBucket));
    }

    @Test
    public void getBucketsInWindowsWhereStartTimeEqualsEndTime() {

        final Network network = Network.OPSNET;

        final BandwidthBucket expectedBucket = new BandwidthBucket(5L, 100,
                network);
        List<BandwidthBucket> buckets = Arrays.asList(new BandwidthBucket(4L,
                100, network), expectedBucket, new BandwidthBucket(6L, 100,
                network));

        for (BandwidthBucket bucket : buckets) {
            dao.create(bucket);
        }

        final SortedSet<BandwidthBucket> returned = dao.getBucketsInWindow(
                expectedBucket.getBucketStartTime(),
                expectedBucket.getBucketStartTime(), network);

        assertThat(returned, hasItem(expectedBucket));
    }

    @Test
    public void copyStateCopiesAllBuckets() {

        InMemoryBandwidthBucketDao daoToCopyFrom = new InMemoryBandwidthBucketDao();

        List<BandwidthBucket> opsnetBuckets = Arrays.asList(
                new BandwidthBucket(4L, 100, Network.OPSNET),
                new BandwidthBucket(5L, 100, Network.OPSNET),
                new BandwidthBucket(6L, 100, Network.OPSNET));
        List<BandwidthBucket> sbnBuckets = Arrays.asList(new BandwidthBucket(
                4L, 100, Network.SBN),
                new BandwidthBucket(5L, 100, Network.SBN), new BandwidthBucket(
                        6L, 100, Network.SBN));

        // Persist all buckets to the source dao
        for (BandwidthBucket bucket : opsnetBuckets) {
            daoToCopyFrom.create(bucket);
        }
        for (BandwidthBucket bucket : sbnBuckets) {
            daoToCopyFrom.create(bucket);
        }

        dao.copyState(daoToCopyFrom);

        assertThat(dao.getAll(Network.OPSNET), is(equalTo(opsnetBuckets)));
        assertThat(dao.getAll(Network.SBN), is(equalTo(sbnBuckets)));
    }

    /**
     * Return the implementation of {@link IBandwidthBucketDao} that is under
     * test.
     * 
     * @return the implementation
     */
    protected abstract IBandwidthBucketDao getBandwidthBucketDao();
}
