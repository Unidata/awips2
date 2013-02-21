package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
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
 * Mar 14, 2012 00357      dhladky     Initial creation
 * Jun 12, 2012 00609      djohnson    Update path to crawl script.
 * Aug 06, 2012 01022      djohnson    Launch the crawler in the same JVM.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MainSequenceCrawlLauncher extends CrawlLauncher {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MainSequenceCrawlLauncher.class);

    private final List<HarvesterJobController<MainSequenceCrawlLauncher>> harvesterJobs = new ArrayList<HarvesterJobController<MainSequenceCrawlLauncher>>();

    public MainSequenceCrawlLauncher() {
        harvesterJobs.clear();
        init();
    }

    @Override
    public void addHarvesterJobs(String providerName, CrawlAgent agent) {
        getHarvesterJobs().add(
                new HarvesterJobController<MainSequenceCrawlLauncher>(
                        providerName + "-" + getType(), agent.getMainScan(),
                        MainSequenceCrawlLauncher.class));
        statusHandler.handle(Priority.DEBUG, "Added " + providerName
                + " Main Scan entry.");
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

                HarvesterConfig hc = SerializationUtil
                        .jaxbUnmarshalFromXmlFile(HarvesterConfig.class,
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
                            MainSequenceCrawler.main(args);
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Crawler failed to initialize!", e);
        }

    }

    public List<HarvesterJobController<MainSequenceCrawlLauncher>> getHarvesterJobs() {
        return harvesterJobs;
    }

    @Override
    public String getType() {
        return "main";
    }
}
