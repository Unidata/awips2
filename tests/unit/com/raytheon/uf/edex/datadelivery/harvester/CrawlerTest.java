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
package com.raytheon.uf.edex.datadelivery.harvester;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfigFixture;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.Crawler;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCommunicationStrategyDecorator;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCrawler;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCrawler.ModelCrawlConfiguration;

/**
 * Test {@link Crawler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2012 740        djohnson     Initial creation
 * Jul 26, 2012 955        djohnson     Add test for getModelConfiguration().
 * Aug 30, 2012 1123       djohnson     Override cleanupDatabase() for the test.
 * Sep 11, 2012 1154       djohnson     Tests for properly generated model crawl configurations.
 * Oct 23, 2012 1286       djohnson     Install test localization before each test.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore("Currently broken due to Crawler changes, need to determine if it's because of a bad test or broken code")
public class CrawlerTest {

    private final MainSequenceCommunicationStrategyDecorator mockCommunicationStrategy = Mockito
            .mock(MainSequenceCommunicationStrategyDecorator.class);

    @Before
    public void setUp() {
        // Install the fake path manager for testing purposes
        PathManagerFactoryTest.initLocalization();
    }

    @After
    public void cleanUp() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testDefaultPostedFileDelayOnlyCrawlsCurrentDay() {
        // 1970-01-02 00:00:00:0001, 1 millisecond into the day
        final long time = TimeUtil.MILLIS_PER_HOUR * 24 + 1;
        TimeUtilTest.freezeTime(time);

        HarvesterConfig harvesterConfig = HarvesterConfigFixture.INSTANCE.get();
        MainSequenceCrawler crawler = new MainSequenceCrawler(harvesterConfig,
                mockCommunicationStrategy);
        List<ModelCrawlConfiguration> modelCrawlConfigs = crawler
                .createModelCrawlConfigurations();

        assertEquals(
                "Incorrect number of model crawl configurations returned!", 1,
                modelCrawlConfigs.size());

        ModelCrawlConfiguration configuration = modelCrawlConfigs.iterator()
                .next();
        assertEquals("19700102", configuration.getDateFrag());
    }

    @Test
    public void testThreeDayPostedFileDelayCrawlsFourDays() {
        // 1971-01-01 00:00:00:0001, 1 millisecond into the day, 1970 wasn't a
        // leap year
        final long time = TimeUtil.MILLIS_PER_DAY * 365 + 1L;
        TimeUtilTest.freezeTime(time);

        HarvesterConfig harvesterConfig = HarvesterConfigFixture.INSTANCE.get();
        harvesterConfig.getProvider().setPostedFileDelay("3 DAYS");
        MainSequenceCrawler crawler = new MainSequenceCrawler(harvesterConfig,
                mockCommunicationStrategy);
        List<ModelCrawlConfiguration> modelCrawlConfigs = crawler
                .createModelCrawlConfigurations();

        assertEquals(
                "Incorrect number of model crawl configurations returned!", 4,
                modelCrawlConfigs.size());

        int idx = 0;
        assertEquals("19701229", modelCrawlConfigs.get(idx++).getDateFrag());
        assertEquals("19701230", modelCrawlConfigs.get(idx++).getDateFrag());
        assertEquals("19701231", modelCrawlConfigs.get(idx++).getDateFrag());
        assertEquals("19710101", modelCrawlConfigs.get(idx++).getDateFrag());
    }

    @Test
    public void testTwoMillisecondPostedFileDelayCrawlsTwoDays() {
        // 1970-01-02 00:00:00:0001, 1 millisecond into the day
        final long time = TimeUtil.MILLIS_PER_HOUR * 24 + 1;
        TimeUtilTest.freezeTime(time);

        HarvesterConfig harvesterConfig = HarvesterConfigFixture.INSTANCE.get();
        harvesterConfig.getProvider().setPostedFileDelay("2 MILLISECONDS");
        MainSequenceCrawler crawler = new MainSequenceCrawler(harvesterConfig,
                mockCommunicationStrategy);
        List<ModelCrawlConfiguration> modelCrawlConfigs = crawler
                .createModelCrawlConfigurations();

        assertEquals(
                "Incorrect number of model crawl configurations returned!", 2,
                modelCrawlConfigs.size());

        int idx = 0;
        assertEquals("19700101", modelCrawlConfigs.get(idx++).getDateFrag());
        assertEquals("19700102", modelCrawlConfigs.get(idx++).getDateFrag());
    }
}
