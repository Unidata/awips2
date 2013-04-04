package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.config.Agent;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.cron.HarvesterJobController;

/**
 * Launch crawler in the same JVM.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2012  1038      dhladky     Initial creation
 * Nov 19, 2012 1166      djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SeedCrawlLauncher extends CrawlLauncher {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SeedCrawlLauncher.class);

    private final List<HarvesterJobController<SeedCrawlLauncher>> harvesterJobs = new ArrayList<HarvesterJobController<SeedCrawlLauncher>>();

    public SeedCrawlLauncher() {
        harvesterJobs.clear();
        init();
    }

    @Override
    public void addHarvesterJobs(String providerName, CrawlAgent agent) {

        getHarvesterJobs().add(
                new HarvesterJobController<SeedCrawlLauncher>(providerName
                        + "-" + getType(), agent.getSeedScan(),
                        SeedCrawlLauncher.class));
        statusHandler.handle(Priority.DEBUG, "Added/Updated " + providerName
                + " Seed Scan entry.");
    }

    /**
     * launches the crawl
     */
    @Override
    public void crawl(String providerName) {

        try {
            // first get the Localization directory and find all harvester
            // configs
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            LocalizationFile lfds = pm.getLocalizationFile(lc,
                    "datadelivery/harvester");
            final String siteConfig = lfds.getFile().getAbsolutePath();
            // if many, start many
            for (LocalizationFile lf : getLocalizedFiles()) {

                HarvesterConfig hc = (HarvesterConfig) new JAXBManager(
                        HarvesterConfig.class).jaxbUnmarshalFromXmlFile(
                                lf.getFile());

                if (hc.getProvider().getName().equals(providerName)) {
                    if (hc.getAgent() != null) {
                        // we only want crawler types for CrawlerMetadata
                        Agent agent = hc.getAgent();

                        if (agent instanceof CrawlAgent) {
                            // Send the actual file with the paths for context

                            final String[] args = new String[] { siteConfig,
                                    lf.getFile().getAbsolutePath() };
                            // start it
                            SeedCrawler.main(args);
                            break;
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Crawler failed to initialize!", e);
        }

    }

    public List<HarvesterJobController<SeedCrawlLauncher>> getHarvesterJobs() {
        return harvesterJobs;
    }

    @Override
    public String getType() {
        return "seed";
    }
}
