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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.util.Properties;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Provides setup functionality for a base {@link BandwidthManager} integration
 * test.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2012 1286       djohnson     Initial creation
 * Dec 11, 2012 1286       djohnson     Use a synchronous event bus for tests.
 * Dec 11, 2012 1403       djohnson     No longer valid to run without bandwidth management.
 * Feb 07, 2013 1543       djohnson     Remove unnecessary test setup methods.
 * Feb 20, 2013 1543       djohnson     Delegate to sub-classes for which route to create subscriptions for.
 * Mar 28, 2013 1841       djohnson     Subscription is now UserSubscription.
 * Apr 29, 2013 1910       djohnson     Always shutdown bandwidth managers in tests.
 * Jun 03, 2013 2095       djohnson     Move getPointDataSet in from subclass.
 * Jul 09, 2013 2106       djohnson     Add datadelivery handlers, since they are now dependency injected.
 * Sep 17, 2013 2383       bgonzale     Added "thrift.stream.maxsize" System property to setup.
 * Sep 25, 2013 1797       dhladky      separated time from gridded time
 * Oct 21, 2013 2292       mpduff       Implement multiple data types.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.EVENTBUS_COMMON_XML, SpringFiles.DATADELIVERY_HANDLERS_XML,
        SpringFiles.MEMORY_DATADELIVERY_HANDLERS_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_XML })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
@Ignore
public abstract class AbstractBandwidthManagerIntTest<T extends Time, C extends Coverage> {

    @Autowired
    protected ApplicationContext context;

    @Autowired
    protected EdexBandwidthManager<T, C> bandwidthManager;

    @Autowired
    protected RetrievalManager retrievalManager;

    protected IBandwidthDao<T, C> bandwidthDao;

    /**
     * Keeps track of which integers have already been used as seeds for a
     * subscription.
     */
    private int subscriptionSeed = 1;

    /**
     * The size of a bucket in bytes.
     */
    private long fullBucketSize;

    /**
     * The size of half of a bucket in bytes.
     */
    private long halfBucketSize;

    /**
     * The size of a third of a bucket in bytes.
     */
    private long thirdBucketSizeInBytes;

    @BeforeClass
    public static void staticSetup() throws IOException {
        PathManagerFactoryTest.initLocalization();
        Properties properties = PropertiesUtil
                .read(AbstractBandwidthManagerIntTest.class
                        .getResourceAsStream("/com.raytheon.uf.edex.datadelivery.bandwidth.properties"));
        System.getProperties().putAll(properties);

        TimeUtilTest.freezeTime(TimeUtil.MILLIS_PER_DAY * 2);
        System.setProperty("thrift.stream.maxsize", "200");
    }

    @AfterClass
    public static void staticTearDown() {
        TimeUtilTest.resumeTime();
    }

    @Before
    public void setUp() {
        retrievalManager = bandwidthManager.retrievalManager;
        bandwidthDao = IBandwidthDao.class
                .cast(context.getBean("bandwidthDao"));

        fullBucketSize = retrievalManager
                .getPlan(getRouteToUseForSubscription())
                .getBucket(TimeUtil.currentTimeMillis()).getBucketSize();
        halfBucketSize = fullBucketSize / 2;
        thirdBucketSizeInBytes = fullBucketSize / 3;
    }

    @After
    public void tearDown() {
        PathManagerFactoryTest.initLocalization();
        shutdownBandwidthManager(bandwidthManager);
        shutdownBandwidthManager(EdexBandwidthContextFactory.getInstance());
        new EdexBandwidthContextFactory(null);
    }

    /**
     * Shutdown the bandwidth manager safely.
     * 
     * @param instance
     */
    protected void shutdownBandwidthManager(BandwidthManager<T, C> bwManager) {
        if (bwManager != null) {
            try {
                bwManager.shutdown();
            } catch (IllegalArgumentException iae) {
                // ignore any exceptions occurring about not being a registered
                // event bus handler
            }
        }
    }

    /**
     * Create a subscription the fills up an entire bandwidth bucket.
     * 
     * @return the subscription
     */
    protected SiteSubscription<T, C> createSubscriptionThatFillsUpABucket() {
        return createSubscriptionWithDataSetSizeInBytes(fullBucketSize);
    }

    /**
     * Create a subscription the fills up ten buckets.
     * 
     * @return the subscription
     */
    protected SiteSubscription<T, C> createSubscriptionThatFillsUpTenBuckets() {
        return createSubscriptionWithDataSetSizeInBytes(fullBucketSize * 10);
    }

    /**
     * Create a subscription the fills up half of a bandwidth bucket.
     * 
     * @return the subscription
     */
    protected SiteSubscription<T, C> createSubscriptionThatFillsHalfABucket() {
        return createSubscriptionWithDataSetSizeInBytes(halfBucketSize);
    }

    /**
     * Create a subscription the fills up a third of a bandwidth bucket.
     * 
     * @return the subscription
     */
    protected SiteSubscription<T, C> createSubscriptionThatFillsAThirdOfABucket() {
        return createSubscriptionWithDataSetSizeInBytes(thirdBucketSizeInBytes);
    }

    /**
     * Create a subscription the fills up two bandwidth buckets.
     * 
     * @return the subscription
     */
    protected SiteSubscription<T, C> createSubscriptionThatFillsUpTwoBuckets() {
        return createSubscriptionWithDataSetSizeInBytes(fullBucketSize * 2);
    }

    protected SiteSubscription<T, C> createSubscriptionWithDataSetSizeInBytes(
            long bytes) {
        SiteSubscription<T, C> subscription = SiteSubscriptionFixture.INSTANCE
                .get(subscriptionSeed++, DataType.GRID);
        subscription.setDataSetSize(BandwidthUtil
                .convertBytesToKilobytes(bytes));
        subscription.setRoute(getRouteToUseForSubscription());
        return subscription;
    }

    /**
     * Get a point data subscription with the given retrieval interval.
     * 
     * @param retrievalInterval
     *            the retrieval interval
     * @return
     */
    protected Subscription<T, C> getPointDataSubscription(int retrievalInterval) {
        final PointTime pointTime = new PointTime();
        pointTime.setInterval(retrievalInterval);

        Subscription<PointTime, Coverage> subscription = SiteSubscriptionFixture.INSTANCE
                .get(DataType.GRID);
        subscription.setTime(pointTime);
        subscription.setDataSetType(DataType.POINT);
        subscription.setLatencyInMinutes(retrievalInterval);
        return (Subscription<T, C>) subscription;
    }

    /**
     * Retrieve the {@link Network} that subscriptions should be created for.
     * 
     * @return the {@link Network}
     */
    protected abstract Network getRouteToUseForSubscription();

    /**
     * Verify the bandwidth manager has a retrieval plan configured for the
     * specified route.
     * 
     * @param route
     */
    protected void verifyRetrievalPlanExistsForRoute(Network route) {
        final RetrievalPlan retrievalPlan = EdexBandwidthContextFactory
                .getInstance().retrievalManager.getPlan(route);

        assertThat(retrievalPlan, is(notNullValue(RetrievalPlan.class)));
    }

    /**
     * Verify the bandwidth manager does not have a retrieval plan configured
     * for the specified route.
     * 
     * @param route
     */
    protected void verifyRetrievalPlanDoesNotExistForRoute(Network route) {
        final RetrievalPlan retrievalPlan = EdexBandwidthContextFactory
                .getInstance().retrievalManager.getPlan(route);

        assertThat(retrievalPlan, is(nullValue(RetrievalPlan.class)));
    }
}
