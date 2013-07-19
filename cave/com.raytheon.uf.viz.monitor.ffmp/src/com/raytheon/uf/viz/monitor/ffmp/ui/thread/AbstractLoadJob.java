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
package com.raytheon.uf.viz.monitor.ffmp.ui.thread;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;

import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;

/**
 * FFMP load job that retrieves and loads data. Created by refactoring and
 * separating out the logic in the FFMPDataLoader.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013 2075       njensen     Initial creation
 * Jun 07, 2013 2075       njensen     Added progress monitoring
 * Jul 03, 2013 2152       njensen     Override shouldRun()
 * Jul 15, 2013 2184       dhladky     Remove all HUC's for storage except ALL
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractLoadJob extends Job {

    protected static final int PROGRESS_FACTOR = 10;

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractLoadJob.class);

    protected ProductXML product = null;

    protected ProductRunXML productRun;

    protected FFMPResourceData resourceData;

    // TODO contemplate making the two times into a TimeRange
    protected Date startTime = null;

    protected Date endTime = null;

    protected FFMPMonitor ffmpMonitor;

    /**
     * Constructor
     * 
     * @param name
     *            name of the job
     * @param resourceData
     *            the resource data that is loading
     * @param timeBack
     *            the oldest time to load data for
     * @param mostRecentTime
     *            the newest time to load for
     * @param hucsToLoad
     *            the hucs to load
     */
    public AbstractLoadJob(String name, FFMPResourceData resourceData,
            Date timeBack, Date mostRecentTime, List<String> hucsToLoad) {
        super(name);
        this.setSystem(false);

        this.resourceData = resourceData;
        this.startTime = timeBack;
        this.endTime = mostRecentTime;

        // configure FFMP
        FFMPRunXML runXML = FFMPRunConfigurationManager.getInstance()
                .getRunner(resourceData.wfo);
        this.productRun = runXML.getProduct(resourceData.siteKey);
        this.product = resourceData.getProduct();
        this.ffmpMonitor = FFMPMonitor.getInstance();
    }

    /**
     * Preloads the available URIs. Should NOT be called by the update job.
     */
    protected void preloadAvailableUris() {
        // preload all the uris except guidance. Guidance loads data
        // much further back and it is not efficient to group with the
        // rest.
        Set<String> sources = new HashSet<String>();
        sources.add(product.getRate());
        sources.add(product.getQpe());
        sources.add(product.getVirtual());
        for (String qpfType : productRun.getQpfTypes(product)) {
            for (SourceXML qpfSource : productRun.getQpfSources(product,
                    qpfType)) {
                sources.add(qpfSource.getSourceName());
            }
        }
        ffmpMonitor.preloadAvailableUris(resourceData.siteKey,
                resourceData.dataKey, sources, startTime);
    }

    /**
     * Gets and processes the rate URIs. Should only be used by the initial job
     * and update jobs.
     */
    protected void doRate() {
        String rateURI = null;
        if (!product.getRate().equals(product.getQpe())) {
            Map<Date, List<String>> rateURIs = ffmpMonitor.getAvailableUris(
                    resourceData.siteKey, resourceData.dataKey,
                    product.getRate(), endTime);
            if (rateURIs.containsKey(endTime)) {
                rateURI = rateURIs.get(endTime).get(0);
            }
        }
        if (rateURI != null) {
            ffmpMonitor.processUri(rateURI, resourceData.siteKey,
                    product.getRate(), startTime);
        }
    }

    /**
     * Gets the available QPE URIs
     * 
     * @return
     */
    protected NavigableMap<Date, List<String>> getQpeUris() {
        return ffmpMonitor.getAvailableUris(resourceData.siteKey,
                resourceData.dataKey, product.getQpe(), startTime);
    }

    /**
     * Processes the qpe URIs
     * 
     * @param qpeURIs
     */
    protected void doQpe(NavigableMap<Date, List<String>> qpeURIs,
            SubMonitor smonitor) {
        if (!qpeURIs.isEmpty()) {
            smonitor.beginTask(null, PROGRESS_FACTOR);
            ffmpMonitor.processUris(qpeURIs, resourceData.siteKey,
                    product.getQpe(), startTime,
                    smonitor.newChild(PROGRESS_FACTOR));
        }
    }

    /**
     * Gets the available QPF URIs for a particular source
     * 
     * @param sourceName
     * @param qpfTime
     * @return
     */
    protected NavigableMap<Date, List<String>> getQpfUris(String sourceName,
            Date qpfTime) {
        return ffmpMonitor.getAvailableUris(resourceData.siteKey,
                resourceData.dataKey, sourceName, qpfTime);
    }

    /**
     * Gets the available QPF URIs for all sources
     * 
     * @param qpfTime
     * @return
     */
    protected List<NavigableMap<Date, List<String>>> getQpfUris(Date qpfTime) {
        ArrayList<NavigableMap<Date, List<String>>> qpfs = new ArrayList<NavigableMap<Date, List<String>>>();
        for (String qpfType : productRun.getQpfTypes(product)) {
            for (SourceXML qpfSource : productRun.getQpfSources(product,
                    qpfType)) {
                NavigableMap<Date, List<String>> qpfURIs = getQpfUris(
                        qpfSource.getSourceName(), qpfTime);

                if (qpfURIs != null && !qpfURIs.isEmpty()) {
                    qpfs.add(qpfURIs);
                }
            }
        }

        return qpfs;
    }

    /**
     * Processes the QPF URIs
     * 
     * @param qpfURIs
     * @param productQpf
     */
    protected void doQpf(NavigableMap<Date, List<String>> qpfURIs,
            String productQpf, SubMonitor smonitor) {
        // Use this method of QPF data retrieval if you don't have cache
        // files
        if (!qpfURIs.isEmpty()) {
            smonitor.beginTask(null, PROGRESS_FACTOR);

            ffmpMonitor.processUris(qpfURIs, resourceData.siteKey, productQpf,
                    startTime, smonitor.newChild(PROGRESS_FACTOR));

        }
    }

    /**
     * Processes the available virtual URIs
     */
    protected void doVirtual(SubMonitor smonitor) {
        NavigableMap<Date, List<String>> virtualURIs = ffmpMonitor
                .getAvailableUris(resourceData.siteKey, resourceData.dataKey,
                        product.getVirtual(), startTime);
        if (!virtualURIs.isEmpty()) {
            ffmpMonitor.processUris(virtualURIs, resourceData.siteKey,
                    product.getVirtual(), startTime, smonitor);
        }
    }

    /**
     * Gets the available guidance URIs for a particular source
     * 
     * @param sourceName
     * @param guidTime
     * @return
     */
    protected NavigableMap<Date, List<String>> getGuidURIs(String sourceName,
            Date guidTime) {
        NavigableMap<Date, List<String>> retVal = null;
        if (guidTime != null) {
            retVal = ffmpMonitor.getAvailableUris(resourceData.siteKey,
                    resourceData.dataKey, sourceName, guidTime);
        }
        return retVal;
    }

    /**
     * Gets and processes the available guidance URIs
     * 
     * @param guidTime
     */
    protected void doGuidance(Date guidTime, SubMonitor smonitor) {
        List<String> guidanceTypes = productRun.getGuidanceTypes(product);
        smonitor.beginTask(null, guidanceTypes.size() * PROGRESS_FACTOR);
        for (String type : guidanceTypes) {
            List<SourceXML> guidanceSources = productRun.getGuidanceSources(
                    product, type);
            int subWork = guidanceSources.size();
            for (SourceXML guidSource : guidanceSources) {
                NavigableMap<Date, List<String>> iguidURIs = getGuidURIs(
                        guidSource.getSourceName(), guidTime);
                if (iguidURIs != null && !iguidURIs.isEmpty()) {
                    ffmpMonitor.processUris(iguidURIs, resourceData.siteKey,
                            guidSource.getSourceName(), startTime,

                            smonitor.newChild(PROGRESS_FACTOR / subWork));
                }
            }
        }
    }

    @Override
    public boolean shouldRun() {
        return (super.shouldRun() && FFMPMonitor.isRunning());
    }

}
