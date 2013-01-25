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

import java.io.IOException;
import java.util.Properties;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationUtilTest;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBusTest;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractBandwidthManagerIntTest {

    protected ApplicationContext context;

    protected BandwidthManager bandwidthManager;

    protected RetrievalManager retrievalManager;

    protected IBandwidthDao bandwidthDao;

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
        Properties properties = PropertiesUtil
                .read(AbstractBandwidthManagerIntTest.class
                        .getResourceAsStream("/com.raytheon.uf.edex.datadelivery.bandwidth.properties"));
        System.getProperties().putAll(properties);

        SerializationUtilTest.initSerializationUtil();
        TimeUtilTest.freezeTime(TimeUtil.MILLIS_PER_DAY * 2);
        BandwidthEventBusTest.initSynchronous();
    }

    @AfterClass
    public static void staticTearDown() {
        TimeUtilTest.resumeTime();
    }

    @Before
    public void setUp() {
        PathManagerFactoryTest.initLocalization();
        DatabaseUtil.start();
        context = new ClassPathXmlApplicationContext(
                IntegrationTestBandwidthManager.INTEGRATION_TEST_SPRING_FILES,
                BandwidthManagerIntTest.class);
        bandwidthDao = (IBandwidthDao) context.getBean("bandwidthDao",
                IBandwidthDao.class);
        bandwidthManager = (BandwidthManager) context.getBean(
                "bandwidthManager",
                BandwidthManager.class);
        retrievalManager = bandwidthManager.retrievalManager;

        fullBucketSize = retrievalManager.getPlan(Network.OPSNET)
                .getBucket(TimeUtil.currentTimeMillis()).getBucketSize();
        halfBucketSize = fullBucketSize / 2;
        thirdBucketSizeInBytes = fullBucketSize / 3;
    }

    @After
    public void tearDown() {
        try {
            bandwidthManager.shutdown();
        } catch (IllegalArgumentException iae) {
            // ignore any exceptions occurring about not being a registered
            // event bus handler
            iae.printStackTrace();
        }
        DatabaseUtil.shutdown();
    }

    /**
     * Create a subscription the fills up an entire bandwidth bucket.
     * 
     * @return the subscription
     */
    protected Subscription createSubscriptionThatFillsUpABucket() {
        return createSubscriptionWithDataSetSizeInBytes(fullBucketSize);

    }

    /**
     * Create a subscription the fills up half of a bandwidth bucket.
     * 
     * @return the subscription
     */
    protected Subscription createSubscriptionThatFillsHalfABucket() {
        return createSubscriptionWithDataSetSizeInBytes(halfBucketSize);
    }

    /**
     * Create a subscription the fills up a third of a bandwidth bucket.
     * 
     * @return the subscription
     */
    protected Subscription createSubscriptionThatFillsAThirdOfABucket() {
        return createSubscriptionWithDataSetSizeInBytes(thirdBucketSizeInBytes);
    }

    /**
     * Create a subscription the fills up two bandwidth buckets.
     * 
     * @return the subscription
     */
    protected Subscription createSubscriptionThatFillsUpTwoBuckets() {
        return createSubscriptionWithDataSetSizeInBytes(fullBucketSize * 2);
    }

    protected Subscription createSubscriptionWithDataSetSizeInBytes(long bytes) {
        Subscription subscription = SubscriptionFixture.INSTANCE
                .get(subscriptionSeed++);
        subscription.setDataSetSize(BandwidthUtil
                .convertBytesToKilobytes(bytes));
        return subscription;
    }
}
