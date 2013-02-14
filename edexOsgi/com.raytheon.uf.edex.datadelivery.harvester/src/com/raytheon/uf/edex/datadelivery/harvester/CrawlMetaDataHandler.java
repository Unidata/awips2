package com.raytheon.uf.edex.datadelivery.harvester;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Utils;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IProviderHandler;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.datadelivery.harvester.config.Agent;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CommunicationStrategy;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CrawlLauncher;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.HarvesterEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.IExtractMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.IParseMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.Link;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.ProviderCollectionLinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Harvest MetaData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * Jul 17, 2012    749      djohnson    Break out the use of files to communicate as a strategy.
 * Jul 24, 2012    955      djohnson    Use the Abstract Factory Pattern to simplify service specific access.
 * Aug 30, 2012   1123      djohnson    Rename CrawlerEvent to HarvesterEvent.
 * Sept 12,2012   1038      dhladky     Reconfigured config.
 * Oct 03, 2012   1241      djohnson    Use registry handler.
 * Nov 09, 2012   1263      dhladky     Changed to Site Level
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class CrawlMetaDataHandler {

    public static final String DASH = "-";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrawlMetaDataHandler.class);

    /** Path to crawler links directory. */
    private static final String PROCESSED_DIR = StringUtil.join(new String[] {
            "datadelivery", "harvester", "processed" }, File.separatorChar);

    /**
     * Read in the config file
     * 
     * @return
     */
    private static Map<String, HarvesterConfig> readCrawlConfigs() {

        Map<String, HarvesterConfig> hcs = null;

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        List<LocalizationFile> files = CrawlLauncher.getLocalizedFiles();

        if (files != null) {
            for (LocalizationFile file : files) {

                try {

                    HarvesterConfig hc = SerializationUtil
                            .jaxbUnmarshalFromXmlFile(HarvesterConfig.class,
                                    file.getFile());

                    Agent agent = hc.getAgent();

                    if (agent != null) {
                        // we only want crawler types for CrawlerMetadata
                        if (agent instanceof CrawlAgent) {
                            // create if null
                            if (hcs == null) {
                                hcs = new HashMap<String, HarvesterConfig>();
                            }
                            // place into config map by provider name key
                            hcs.put(hc.getProvider().getName(), hc);
                        }
                    }
                } catch (SerializationException e1) {
                    statusHandler.error(
                            "Serialization Error Reading Crawler Config files",
                            e1);
                }
            }

            if (hcs == null) {
                File path = pm.getFile(lc, "datadelivery/harvester");
                statusHandler.info("No Crawler Configs found: "
                        + path.getAbsolutePath());
            }
        }

        return hcs;
    }

    private final CommunicationStrategy communicationStrategy;

    private final File timesDir;

    private Map<String, HarvesterConfig> hconfigs = null;

    public CrawlMetaDataHandler(CommunicationStrategy communicationStrategy) {
        this.communicationStrategy = communicationStrategy;

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.CONFIGURED);
        LocalizationFile lf = pm.getLocalizationFile(lc, PROCESSED_DIR);
        timesDir = lf.getFile();

        statusHandler
                .info("<<<<<<<<<<<<<<<<<<<<< INITIALIZING CRAWL META DATA HANDLER >>>>>>>>>>>>>>>>>>>>>>");
        hconfigs = readCrawlConfigs();

        final IProviderHandler handler = DataDeliveryHandlers
                .getProviderHandler();

        if (hconfigs != null) {
            for (Entry<String, HarvesterConfig> entry : hconfigs.entrySet()) {
                try {

                    Provider provider = entry.getValue().getProvider();
                    statusHandler.info("Inserting/Updating Provider: "
                            + provider.getName() + ": "
                            + provider.getServiceType());
                    handler.update(provider);

                } catch (Exception e) {
                    statusHandler.error("Error inserting/updating Provider! ",
                            e);
                }
            }
        }
    }

    @SuppressWarnings({ "unchecked", "deprecation" })
    private Set<String> getPreviousRun(String collectionName,
            String providerName, String dateString) {

        // default if it is brand new
        Set<String> previousRun = new HashSet<String>();

        try {
            File file = getPreviousRunsFile(providerName, collectionName,
                    dateString);
            if (file != null && file.exists() && file.length() > 0) {
                previousRun = (Set<String>) SerializationUtil
                        .transformFromThrift(FileUtil.file2bytes(file));
            }
            statusHandler.info("Read previous URLs for " + providerName + " : "
                    + collectionName + " : " + dateString);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to read previous runs file!", e);
        }

        return previousRun;
    }

    private File getPreviousRunsFile(String providerName,
            String collectionName, String dateString) {

        return new File(timesDir, providerName + DASH + collectionName + DASH
                + dateString + ".bin");
    }

    private void handleErrors() {
        List<Throwable> errors = communicationStrategy.getErrors();

        for (Throwable throwable : errors) {
            EventBus.getInstance().publish(
                    new HarvesterEvent(throwable.getMessage()));
        }
    }

    /**
     * Check the files found by the crawler
     * 
     * @throws Exception
     */
    public void metaDataCheck() {

        statusHandler.info("Checking for new MetaData.....");
        hconfigs = readCrawlConfigs();

        handleErrors();

        ProviderCollectionLinkStore providerCollectionLinkStore = null;

        while ((providerCollectionLinkStore = communicationStrategy
                .getNextLinkStore()) != null) {
            String collectionName = providerCollectionLinkStore
                    .getCollectionName();
            String providerName = providerCollectionLinkStore.getProviderName();
            LinkStore store = providerCollectionLinkStore.getLinkStore();
            Set<String> previousRun = getPreviousRun(collectionName,
                    providerName, store.getDateString());

            if (hconfigs != null) {
                HarvesterConfig hc = hconfigs.get(providerName);
                CrawlAgent agent = (CrawlAgent) hc.getAgent();
                Collection collection = agent
                        .getCollectionByName(collectionName);

                if (store != null && collection != null) {

                    Provider provider = hc.getProvider();
                    ServiceFactory serviceFactory = ServiceTypeFactory
                            .retrieveServiceFactory(provider);

                    IExtractMetaData mde = serviceFactory.getExtractor();

                    // remove previous run
                    store.getLinkKeys().removeAll(previousRun);

                    // extract metadata, process each link
                    List<String> removes = new ArrayList<String>();

                    for (String linkKey : store.getLinkKeys()) {
                        Link link = store.getLink(linkKey);
                        String url = link.getUrl();
                        try {
                            mde.setUrl(url);
                            link.setLinks(mde.extractMetaData());
                            mde.setDataDate();
                        } catch (Exception e) {
                            final String userFriendly = String
                                    .format("Unable to retrieve metadata for dataset group %s: %s.",
                                            collectionName, url);
                            statusHandler.error(userFriendly, e);

                            communicationStrategy
                                    .sendException(new RuntimeException(
                                            userFriendly, e));

                            // If we can't extract it, we can't parse it, so
                            // remove
                            removes.add(linkKey);
                        }
                    }

                    // remove failed entries
                    if (!removes.isEmpty()) {
                        store.getLinkKeys().removeAll(removes);
                    }

                    if (!store.getLinkKeys().isEmpty()) {
                        // now start parsing the metadata objects
                        String directoryDateFormat = collection.getDateFormat();
                        String dataDateFormat = agent.getDateFormat();
                        Date lastUpdate = null;

                        try {
                            if (!directoryDateFormat.equals("")) {
                                lastUpdate = Utils.convertDate(
                                        directoryDateFormat,
                                        store.getDateString());
                            } else {
                                // use current time
                                lastUpdate = new Date(
                                        System.currentTimeMillis());
                            }
                        } catch (ParseException e1) {
                            throw new IllegalArgumentException(
                                    "Unable to parse a date!", e1);
                        }

                        IParseMetaData mdp = serviceFactory
                                .getParser(lastUpdate);

                        try {
                            mdp.parseMetaData(provider, store, collection,
                                    dataDateFormat);
                            previousRun.addAll(store.getLinkKeys());
                        } catch (Exception e) {
                            final String userFriendly = String
                                    .format("Unable to parse metadata for dataset group %s.",
                                            collectionName);

                            statusHandler.handle(Priority.PROBLEM,
                                    userFriendly, e);

                            communicationStrategy
                                    .sendException(new RuntimeException(
                                            userFriendly, e));
                        }

                        writePreviousRun(previousRun, collectionName,
                                providerName, store.getDateString());
                    } else {
                        statusHandler.info("No new data for " + providerName
                                + " : " + collectionName + " : "
                                + store.getDateString());
                    }
                } else {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Collection and or store are null, please check your configuration!");
                }
            }
        }
    }

    /**
     * Write the previous keys
     * 
     * @return
     */
    private void writePreviousRun(Set<String> previousRun,
            String collectionName, String providerName, String dateString) {

        try {
            File file = getPreviousRunsFile(providerName, collectionName,
                    dateString);

            FileUtil.bytes2File(
                    SerializationUtil.transformToThrift(previousRun), file);

        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        statusHandler.info("Wrote URLs: " + providerName + " : "
                + collectionName + " : " + dateString + " size: "
                + previousRun.size());
    }
}
