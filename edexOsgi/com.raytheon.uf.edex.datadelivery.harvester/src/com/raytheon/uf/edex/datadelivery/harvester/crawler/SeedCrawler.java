package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Executors;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.config.ProtoCollection;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCrawler.ModelCrawlConfiguration;

import edu.uci.ics.crawler4j.crawler.CrawlConfig;
import edu.uci.ics.crawler4j.crawler.CrawlController;
import edu.uci.ics.crawler4j.crawler.WebCrawler;
import edu.uci.ics.crawler4j.fetcher.PageFetcher;
import edu.uci.ics.crawler4j.robotstxt.RobotstxtConfig;
import edu.uci.ics.crawler4j.robotstxt.RobotstxtServer;

/**
 * Crawl for MetaData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2012  1038       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class SeedCrawler extends Crawler {

    /**
     * Launches it
     * 
     * @param args
     * @throws Exception
     */
    public static void main(final String[] args) throws Exception {

        if (args == null || args.length != 2) {
            statusHandler.info("Invalid parameter in crawler main. Exiting.");
            return;
        }

        statusHandler.debug("ARG[0] in crawler: " + args[0]);
        statusHandler.debug("ARG[1] in crawler: " + args[1]);

        // We will do this if the lock can be acquired
        final File configFile = new File(args[1]);
        final File configFileLockDir = new File(args[0]);
        final HarvesterConfig config = readConfig(configFile);
        final String providerName = config.getProvider().getName();

        Runnable runWithinLock = new Runnable() {
            @Override
            public void run() {

                statusHandler.info("~~~~~~~~~~~~~~~~" + providerName
                        + " Seed Search Crawler~~~~~~~~~~~~~~~~~~");

                SeedCrawler crawl = new SeedCrawler(config);
                crawl.setAgent((CrawlAgent) config.getAgent());
                crawl.setProviderName(providerName);
                crawl.crawl();
            }
        };

        runIfLockCanBeAcquired(runWithinLock, configFileLockDir, providerName
                + "-seed");
    }

    /**
     * Construct a {@link SeedCrawler} that will be initialized from the
     * specified configuration file.
     * 
     * @param configFile
     *            the configuration file
     */
    public SeedCrawler(HarvesterConfig config) {
        this(
                config,
                new SeedCommunicationStrategyDecorator(
                        (CommunicationStrategy) EDEXUtil
                                .getESBComponent(COMMUNICATION_STRATEGY_BEAN_NAME),
                        Executors.newSingleThreadExecutor(THREAD_FACTORY)));
    }

    /**
     * Test-only constructor, hence package private.
     * 
     * @param config
     *            the configuration object
     * @param communicationStrategy
     *            the communication strategy
     */
    SeedCrawler(HarvesterConfig config,
            CommunicationStrategy communicationStrategy) {

        super(config, communicationStrategy);

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            if (proxyParameters != null) {
                statusHandler.debug(String.format(
                        "proxy host:[%s]  proxy port: [%s]",
                        proxyParameters[0], proxyParameters[1]));
            } else {
                statusHandler.debug("No proxy information configured.");
            }
        }
    }

    /**
     * DO the actual crawling of the site
     * 
     * @param hconfig
     * @return
     */
    @Override
    public void crawl() {

        performSeedCrawl();
        communicationStrategy.shutdown();
        cleanupDatabase();
    }

    @Override
    protected String getCrawlerFrontier() {
        return "/" + getProviderName() + "/seed";
    }

    @Override
    protected int getMaxDepthOfCrawl() {
        return getAgent().getMaxSeedDepth();
    }

    @Override
    protected int getMaxPages() {
        return getAgent().getMaxSeedPages();
    }

    /**
     * Performs the actual crawls based on the collection of
     * {@link ModelCrawlConfiguration} objects.
     * 
     * @param configurations
     *            the configuration objects
     */
    @VisibleForTesting
    private void performSeedCrawl() {

        Provider provider = hconfig.getProvider();
        CrawlAgent agent = (CrawlAgent) hconfig.getAgent();

        CrawlConfig config = getCrawlConfig();
        // Do fast for seed scans, don't be polite
        config.setPolitenessDelay(10);

        statusHandler.info(String.format(
                "Starting Seed crawl provider [%s], politeness delay [%s]",
                provider.getName(), config.getPolitenessDelay()));

        String searchUrl = provider.getUrl();

        // The crawler library objects. These must be created anew for each
        // loop, otherwise results are ignored.
        PageFetcher pageFetcher = new CrawlMonitorPageFetcher(
                provider.getName(), config, communicationStrategy);
        RobotstxtConfig robotstxtConfig = new RobotstxtConfig();
        robotstxtConfig.setEnabled(agent.isUseRobots());
        RobotstxtServer robotstxtServer = new RobotstxtServer(robotstxtConfig,
                pageFetcher);

        /*
         * Instantiate the controller for this crawl.
         */
        CrawlController crawlcontroller = null;
        try {
            crawlcontroller = new CrawlController(config, pageFetcher,
                    robotstxtServer);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        /*
         * For each crawl, you need to add some seed urls. These are the first
         * URLs that are fetched and then the crawler starts following links
         * which are found in these pages
         */
        crawlcontroller.addSeed(searchUrl);

        // if collections already exist for provider, we'll add those seed URL's
        // as ignores
        List<String> knownCollections = new ArrayList<String>();
        if (agent.getCollection() != null) {
            for (Collection coll : agent.getCollection()) {
                // compare to see if it is missing updates and such
                if (agent.isMature(coll.getName())) {
                    knownCollections.add(coll.getSeedUrl());
                }
            }
        }

        // SeedLinkStore seedLinks = new SeedLinkStore();
        HashMap<String, ProtoCollection> collections = new HashMap<String, ProtoCollection>();

        ArrayList<WebCrawler> webCrawlers = new ArrayList<WebCrawler>(1);
        webCrawlers.add(new SeedHarvester(searchUrl, agent.getSearchKey(),
                collections, knownCollections, agent.getIgnore()));

        // start the crawling, blocks thread till finished
        crawlcontroller.start(webCrawlers, webCrawlers.size());
        crawlcontroller.Shutdown();

        communicationStrategy.processCollections(hconfig, collections,
                provider, agent);

    }

}
