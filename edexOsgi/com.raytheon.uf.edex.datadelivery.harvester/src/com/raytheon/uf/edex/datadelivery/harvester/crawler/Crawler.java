package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.regex.Pattern;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCrawler.ModelCrawlConfiguration;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ConnectionUtil;
import com.sleepycat.je.Environment;
import com.sleepycat.je.EnvironmentConfig;

import edu.uci.ics.crawler4j.crawler.CrawlConfig;

/**
 * Crawler super class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2012   1038      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class Crawler {

    protected static final String COMMUNICATION_STRATEGY_BEAN_NAME = "crawlerCommunicationStrategy";

    protected String providerName;

    protected CrawlAgent agent;

    // TimeZone is not thread safe, but since we aren't modifying it and the JVM
    // better not either, then it's ok to be a constant
    protected static final TimeZone GMT_TIME_ZONE = TimeZone.getTimeZone("GMT");

    protected static final char FORWARD_SLASH = '/';

    protected static final Pattern FORWARD_SLASH_PATTERN = Pattern
            .compile(Character.toString(FORWARD_SLASH));

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Crawler.class);

    protected final CommunicationStrategy communicationStrategy;

    protected final HarvesterConfig hconfig;

    protected final String[] proxyParameters;

    protected static final ThreadFactory THREAD_FACTORY = new ThreadFactory() {
        @Override
        public Thread newThread(Runnable r) {
            Thread thread = Executors.defaultThreadFactory().newThread(r);
            // Uses the calling thread's name as a prefix on the spawned
            // thread's name
            thread.setName(Thread.currentThread().getName() + "["
                    + thread.getName() + "]");
            return thread;
        }
    };

    /**
     * Get the model configuration.
     * 
     * @param pmodelName
     *            the primary? model name
     * @param providerUrl
     *            the provider's url
     * @param dateFrag
     *            the date fragment
     * @param date
     * @return the model configuration
     */
    public static ModelCrawlConfiguration getModelConfiguration(
            String providerName, Collection collection, String providerUrl,
            String dateFrag, Date date, String searchKey, int politenessDelay) {

        String searchUrl = getUrl(providerUrl + collection.getSeedUrl(),
                collection.getUrlKey());

        if (!dateFrag.equals("")) {
            searchUrl = searchUrl + dateFrag + FORWARD_SLASH;
        }

        return new ModelCrawlConfiguration(providerName, collection.getName(),
                collection.getName(), searchUrl, dateFrag, date, searchKey,
                politenessDelay);
    }

    /**
     * Returns the URL to use concatenating the portions with the url separator
     * character.
     * 
     * @param portions
     *            the portions of the url
     * 
     * @return the url to use
     */
    protected static String getUrl(String... portions) {
        return com.raytheon.uf.common.util.StringUtil.join(portions,
                FORWARD_SLASH);
    }

    /**
     * Read in the config file
     * 
     * @param configFile
     *            the configuration file
     * 
     * @return
     */
    protected static HarvesterConfig readConfig(File configFile) {

        HarvesterConfig hc = null;

        try {
            hc = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    HarvesterConfig.class, configFile);
        } catch (SerializationException e1) {
            e1.printStackTrace();
        }

        return hc;

    }

    /**
     * @param runWithinLock
     */
    protected static void runIfLockCanBeAcquired(final Runnable runWithinLock,
            File lockFileDir, String crawlType) {
        FileChannel channel = null;
        FileOutputStream fos = null;
        // if lock file directories don't exist, create
        if (!lockFileDir.exists()) {
            lockFileDir.mkdirs();
        }
        
        File lockFile = new File(lockFileDir, crawlType + "-crawl.lock");
        
        try {
            // if lock file doesn't exist, create it
            if (!lockFile.exists()) {
                lockFile.createNewFile();
            }

            channel = new FileOutputStream(lockFile).getChannel();

            // Try acquiring the lock without blocking. This method returns
            // null or throws an exception if the file is already locked.
            FileLock lock = channel.tryLock();
            
            if (lock == null) {
                // Someone else has the lock
                statusHandler
                        .error("Unable to acquire lock, another instance must be running! "
                                + crawlType);
                System.exit(0);
            }

            // If we reach this point, we can run the task
            statusHandler.debug(crawlType + " Acquired file lock.");
            runWithinLock.run();
            lock.release();
            statusHandler.debug(crawlType + " Released file lock.");
        } catch (OverlappingFileLockException ovle) {
            statusHandler
                    .warn("Unable to acquire lock, another instance must be running! Perhaps you should lengthen the time between scans");
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            Util.close(channel);
            Util.close(fos);
        }
    }

    /**
     * Super class construct
     * 
     * @param hconfig
     * @param communicationStrategy
     */
    public Crawler(HarvesterConfig hconfig,
            CommunicationStrategy communicationStrategy) {
        this.communicationStrategy = communicationStrategy;
        this.hconfig = hconfig;
        proxyParameters = ConnectionUtil.getProxyParameters();
    }

    /**
     * Deletes the contents of the crawler4j database after each run.
     */
    @VisibleForTesting
    protected void cleanupDatabase() {
        try {
            // Delete the database after every run
            EnvironmentConfig envConfig = new EnvironmentConfig();
            envConfig.setAllowCreate(true);
            envConfig.setTransactional(false);
            envConfig.setLocking(false);

            File envHome = new File(
                    ((CrawlAgent) hconfig.getAgent()).getCrawlDir()
                            + getCrawlerFrontier() + "/frontier");
            Environment env = new Environment(envHome, envConfig);
            try {
                env.removeDatabase(null, "DocIDs");
            } catch (Exception e) {
                statusHandler.warn("No Database existing to be removed. "
                        + getCrawlerFrontier());
            }
        } catch (Throwable t) {
            statusHandler
                    .error("Unable to remove the existing crawler database!  "
                            + "This will cause DataSet information to be missed.",
                            t);
        }
    }

    /**
     * The crawling method call
     */
    protected abstract void crawl();

    public CrawlAgent getAgent() {
        return agent;
    }

    /**
     * Creates and returns the {@link CrawlConfig} to be used for crawling the
     * remote server.
     * 
     * @return the {@link CrawlConfig}
     */
    /**
     * Creates and returns the {@link CrawlConfig} to be used for crawling the
     * remote server.
     * 
     * @return the {@link CrawlConfig}
     */
    protected CrawlConfig getCrawlConfig() {

        CrawlConfig config = new CrawlConfig();
        CrawlAgent agent = (CrawlAgent) hconfig.getAgent();
        hconfig.getProvider().getName();

        config.setCrawlStorageFolder(agent.getCrawlDir() + getCrawlerFrontier());

        /*
         * You can set the maximum crawl depth here. The default value is -1 for
         * unlimited depth
         */
        config.setMaxDepthOfCrawling(getMaxDepthOfCrawl());

        /*
         * You can set the maximum number of pages to crawl. The default value
         * is -1 for unlimited number of pages
         */
        config.setMaxPagesToFetch(getMaxPages());

        /*
         * Do you need to set a proxy? If so, you can use:
         */
        if (proxyParameters != null) {
            String host = proxyParameters[0];
            String port = proxyParameters[1];

            config.setProxyHost(host);
            config.setProxyPort(new Integer(port).intValue());
        }

        /*
         * This config parameter can be used to set your crawl to be resumable
         * (meaning that you can resume the crawl from a previously
         * interrupted/crashed crawl). Note: if you enable resuming feature and
         * want to start a fresh crawl, you need to delete the contents of
         * rootFolder manually.
         */
        config.setResumableCrawling(false);

        return config;
    }

    protected abstract String getCrawlerFrontier();

    protected abstract int getMaxDepthOfCrawl();

    protected abstract int getMaxPages();

    public String getProviderName() {
        return providerName;
    }

    public void setAgent(CrawlAgent agent) {
        this.agent = agent;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

}
