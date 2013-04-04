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
package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.http.HttpException;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.registry.RegistryException;

import edu.uci.ics.crawler4j.crawler.CrawlConfig;
import edu.uci.ics.crawler4j.fetcher.CustomFetchStatus;
import edu.uci.ics.crawler4j.fetcher.PageFetchResult;
import edu.uci.ics.crawler4j.fetcher.PageFetcher;
import edu.uci.ics.crawler4j.url.WebURL;

/**
 * Monitors the crawler4j package for read timed out errors, and sends
 * exceptions to the {@link MainSequenceHarvester} if they occur. Exception
 * messages are throttled to no more than one per hour per provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2012 740        djohnson     Initial creation
 * Aug 06, 2012 1022       djohnson     Changed to be a PageFetcher.
 * Aug 30, 2012 1123       djohnson     lastNotificationTimes needs to be static.
 * Sep 11, 2012 1154       djohnson     Provider name is passed in as a constructor parameter.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class CrawlMonitorPageFetcher extends PageFetcher {

    @VisibleForTesting
    public static final String UNABLE_TO_COMMUNICATE_WITH_A_PROVIDER = "Unable to communicate with a provider: ";

    @VisibleForTesting
    public static final Map<String, Long> lastNotificationTimes = new ConcurrentHashMap<String, Long>();

    /**
     * Get the exception message.
     * 
     * @param providerName
     *            the provider name
     * @return the exception message
     */
    @VisibleForTesting
    public static String getExceptionMessage(final String providerName) {
        return UNABLE_TO_COMMUNICATE_WITH_A_PROVIDER + providerName;
    }

    private final CommunicationStrategy communicationStrategy;

    private final String providerName;

    /**
     * Constructor.
     * 
     * @param providerName
     *            the provider name
     * 
     * @param config
     *            the configuration
     */
    public CrawlMonitorPageFetcher(String providerName, CrawlConfig config,
            CommunicationStrategy communicationStrategy) {
        super(config);
        this.providerName = providerName;
        this.communicationStrategy = communicationStrategy;
    }

    @VisibleForTesting
    public void checkForFatalTransportError(final WebURL webUrl,
            PageFetchResult result) {
        if (CustomFetchStatus.FatalTransportError == result.getStatusCode()) {
            Long lastNotifiedTime = lastNotificationTimes.get(providerName);
            long currentTimeMillis = TimeUtil.currentTimeMillis();
            if (lastNotifiedTime == null
                    || currentTimeMillis - lastNotifiedTime > TimeUtil.MILLIS_PER_HOUR) {
                getCommunicationStrategy()
                        .sendException(
                                new RegistryException(
                                        getExceptionMessage(providerName),
                                        new HttpException(
                                                String.format(
                                                        "Fatal transport error while fetching %s",
                                                        webUrl.getURL()))));
                lastNotificationTimes.put(providerName, currentTimeMillis);
            }
        }
    }

    @Override
    public PageFetchResult fetchHeader(final WebURL webUrl) {
        PageFetchResult result = super.fetchHeader(webUrl);
        checkForFatalTransportError(webUrl, result);

        return result;
    }

    /**
     * Returns the communication strategy that should be used to send
     * exceptions.
     * 
     * @return the communicationStrategy
     */
    @VisibleForTesting
    CommunicationStrategy getCommunicationStrategy() {
        return communicationStrategy;
    }
}
