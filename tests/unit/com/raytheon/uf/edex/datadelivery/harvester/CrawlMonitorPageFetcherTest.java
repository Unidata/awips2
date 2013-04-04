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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import org.apache.http.HttpStatus;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.harvester.crawler.CommunicationStrategy;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CrawlMonitorPageFetcher;

import edu.uci.ics.crawler4j.crawler.CrawlConfig;
import edu.uci.ics.crawler4j.fetcher.CustomFetchStatus;
import edu.uci.ics.crawler4j.fetcher.PageFetchResult;
import edu.uci.ics.crawler4j.url.WebURL;

/**
 * Test {@link CrawlMonitorPageFetcher}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2012 740        djohnson     Initial creation
 * Aug 06, 2012 1022       djohnson     Rename CrawlMonitorLoggingAppender to CrawlMonitorPageFetcher.
 * Sep 06, 2012 1125       djohnson     Clear notification times before each test.
 * Sep 12, 2012 1154       djohnson     Constructor takes a provider name.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class CrawlMonitorPageFetcherTest {

    private static final int MORE_THAN_ONE_HOUR = 60000 * 61;

    private static final int LESS_THAN_ONE_HOUR = 60000 * 59;

    private static final String PROVIDER_NAME = "NOMADS";

    private static final WebURL WEB_URL = new WebURL();

    private static final CrawlConfig CONFIG = new CrawlConfig();

    @BeforeClass
    public static void staticSetup() {
        CONFIG.setUserAgentString("someAgent");
        WEB_URL.setURL("http://some.url/somePage");
    }

    private final PageFetchResult result = mock(PageFetchResult.class);

    private final CommunicationStrategy communicationStrategy = mock(CommunicationStrategy.class);

    private final CrawlMonitorPageFetcher pageFetcher = new CrawlMonitorPageFetcher(
            PROVIDER_NAME, CONFIG, communicationStrategy) {
    };

    @Before
    public void setUp() {
        CrawlMonitorPageFetcher.lastNotificationTimes.clear();
    }

    @Test
    public void testDoesNotSendExceptionOnNonReadTimedOut() {
        when(result.getStatusCode()).thenReturn(HttpStatus.SC_OK);

        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        verifyZeroInteractions(communicationStrategy);
    }

    @Test
    public void testExceptionMessageOnReadTimedOutContainsProviderName() {
        assertEquals("Did not receive the correct exception message!",
                CrawlMonitorPageFetcher.UNABLE_TO_COMMUNICATE_WITH_A_PROVIDER
                        + PROVIDER_NAME,
                CrawlMonitorPageFetcher.getExceptionMessage(PROVIDER_NAME));
    }

    @Test
    public void testExceptionOnReadTimedOutDoesNotNotifyIfLastNotifiedLessThanAnHourAgo() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        CrawlMonitorPageFetcher.lastNotificationTimes.put(PROVIDER_NAME,
                System.currentTimeMillis() - LESS_THAN_ONE_HOUR);

        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        verifyZeroInteractions(communicationStrategy);
    }

    @Test
    public void testExceptionOnReadTimedOutDoesNotUpdateNotifiedTimeIfAlreadyNotifiedInLastHour() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        final long originalNotifiedTime = System.currentTimeMillis()
                - LESS_THAN_ONE_HOUR;

        CrawlMonitorPageFetcher.lastNotificationTimes.put(PROVIDER_NAME,
                originalNotifiedTime);
        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        long newNotifiedTime = CrawlMonitorPageFetcher.lastNotificationTimes
                .get(PROVIDER_NAME);

        assertTrue("The last notified time should not have been updated!",
                originalNotifiedTime == newNotifiedTime);
    }

    @Test
    public void testExceptionOnReadTimedOutNotifiesIfLastNotifiedMoreThanAnHourAgo() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        CrawlMonitorPageFetcher.lastNotificationTimes.put(PROVIDER_NAME,
                System.currentTimeMillis() - MORE_THAN_ONE_HOUR);

        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        verify(communicationStrategy).sendException(any(Exception.class));
    }

    @Test
    public void testExceptionOnReadTimedOutNotifiesIfOnlyDifferentProviderNotifiedLessThanAnHourAgo() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        CrawlMonitorPageFetcher.lastNotificationTimes.put("someOtherProvider",
                System.currentTimeMillis() - LESS_THAN_ONE_HOUR);

        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        verify(communicationStrategy).sendException(any(Exception.class));
    }

    @Test
    public void testExceptionOnReadTimedOutUpdatesNotifiedTimeIfNotified() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        final long originalNotifiedTime = System.currentTimeMillis()
                - MORE_THAN_ONE_HOUR;

        CrawlMonitorPageFetcher.lastNotificationTimes.put(PROVIDER_NAME,
                originalNotifiedTime);
        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        long newNotifiedTime = CrawlMonitorPageFetcher.lastNotificationTimes
                .get(PROVIDER_NAME);
        assertFalse("The last notified time should have been updated!",
                originalNotifiedTime == newNotifiedTime);
    }

    @Test
    public void testSendsExceptionToCommunicationStrategyOnReadTimedOut() {
        when(result.getStatusCode()).thenReturn(
                CustomFetchStatus.FatalTransportError);

        pageFetcher.checkForFatalTransportError(WEB_URL, result);

        verify(communicationStrategy).sendException(any(Exception.class));
    }

}
