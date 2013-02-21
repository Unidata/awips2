package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

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

/**
 * Abstract Crawl Launcher
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

public abstract class CrawlLauncher implements Job {

    /**
     * Gets site and base level configs
     * 
     * @return
     */
    public static List<LocalizationFile> getLocalizedFiles() {
        // first get the Localization directory and find all harvester
        // configs
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lcConfigured = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

        IPathManager pm2 = PathManagerFactory.getPathManager();
        LocalizationContext lcBase = pm2.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        LocalizationFile[] sitefiles = pm.listFiles(lcConfigured,
                "datadelivery/harvester", new String[] { "xml" }, false, true);

        LocalizationFile[] baseFiles = pm2.listFiles(lcBase,
                "datadelivery/harvester", new String[] { "xml" }, false, true);

        ArrayList<LocalizationFile> files = new ArrayList<LocalizationFile>();
        // collect files where there is an existing configured variation
        HashMap<String, LocalizationFile> fileMap = new HashMap<String, LocalizationFile>();

        for (LocalizationFile file : sitefiles) {
            fileMap.put(file.getName(), file);
        }

        // compare to base files
        for (LocalizationFile file : baseFiles) {
            String baseKey = file.getName();
            if (!fileMap.keySet().contains(baseKey)) {
                files.add(file);
            }
        }

        // add the configured to the base list
        for (Entry<String, LocalizationFile> entry : fileMap.entrySet()) {
            files.add(entry.getValue());
        }

        return files;
    }

    private String providerName;

    private boolean initial = true;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrawlLauncher.class);

    public abstract void addHarvesterJobs(String providername, CrawlAgent agent);

    public abstract void crawl(String jobName);

    /**
     * execute the job
     */
    @Override
    public void execute(JobExecutionContext arg0) throws JobExecutionException {

        final String originalThreadName = Thread.currentThread().getName();

        try {
            Thread.currentThread().setName(
                    "crawlerThreadPool-[" + originalThreadName + "]");
            String jobName = arg0.getJobDetail().getName().split("-")[0];
            crawl(jobName);

        } catch (Exception e) {
            statusHandler
                    .error("Incorrect naming for job:  Should be [ProviderName-CrawlerType] ex. [NOMADS-main].",
                            e);
        } finally {
            Thread.currentThread().setName(originalThreadName);
        }

    }

    public String getProviderName() {
        return providerName;
    }

    public abstract String getType();

    /**
     * Run this at instantiation for crawlers
     */
    protected void init() {

        try {
            if (isInitial()) {
                // if many, start many
                List<LocalizationFile> files = CrawlLauncher
                        .getLocalizedFiles();

                if (files != null) {
                    JAXBManager jaxbMan = new JAXBManager(HarvesterConfig.class);
                    for (LocalizationFile lf : files) {

                        HarvesterConfig hc = (HarvesterConfig) jaxbMan
                                .jaxbUnmarshalFromXmlFile(lf.getFile());
                        if (hc.getAgent() != null) {
                            // we only want crawler types for CrawlerMetadata
                            Agent agent = hc.getAgent();

                            if (agent instanceof CrawlAgent) {
                                // create our quartz beans
                                addHarvesterJobs(hc.getProvider().getName(),
                                        (CrawlAgent) agent);
                            }
                        }
                    }
                }
            }

            setInitial(false);

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Crawler Cron Beans failed to initialize!", e);
        }
    }

    public boolean isInitial() {
        return initial;
    }

    public void setInitial(boolean initial) {
        this.initial = initial;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }
}
