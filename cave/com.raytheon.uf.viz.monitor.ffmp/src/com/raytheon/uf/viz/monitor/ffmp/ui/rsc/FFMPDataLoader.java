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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.concurrent.CountDownLatch;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPAggregateRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoadListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoaderEvent;

/**
 * Place holder more or less for a ResourceData Object This dosen't do anything
 * currently.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 Feb, 2011   7587    dhladky     Initial creation
 * 25 Jan, 2012   DR13839 gzhang      Handle Uris and Huc processing
 * 01/27/13     1478      D. Hladky   revamped the cache file format to help NAS overloading
 * 02/01/13      1569    D. Hladky   Changed to reading aggregate records from pypies
 * Feb 28, 2013  1729      dhladky   Changed the way status messages are sent to the FFMP Dialog.
 * Mar 6, 2013   1769     dhladky    Changed threading to use count down latch.
 * Apr 9, 2013   1890     dhladky    removed loading of phantom Virtual template and cache file processing.
 * Apr 18, 2013 1912       bsteffen    Increase bulk requests to pypies.
 * Apr 26, 2013 1954       bsteffen    Minor code cleanup throughout FFMP.
 * May 22, 2013 1902       mpduff      Check for null times.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class FFMPDataLoader extends Thread {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataLoader.class);

    private ProductXML product = null;

    private FFMPRunXML runner = null;

    private Date timeBack = null;

    private Date mostRecentTime = null;

    public LOADER_TYPE loadType = null;

    private String siteKey = null;

    private String dataKey = null;

    private List<String> hucsToLoad = null;

    private String wfo = null;

    private FFMPResourceData resourceData = null;

    private FFMPConfig config = null;

    private final ArrayList<FFMPLoadListener> loadListeners = new ArrayList<FFMPLoadListener>();

    private final CountDownLatch latch;

    public FFMPDataLoader(FFMPResourceData resourceData, Date timeBack,
            Date mostRecentTime, LOADER_TYPE loadType, List<String> hucsToLoad) {

        this.product = resourceData.getProduct();
        this.siteKey = resourceData.siteKey;
        this.dataKey = resourceData.dataKey;
        this.timeBack = timeBack;
        this.mostRecentTime = mostRecentTime;
        this.loadType = loadType;
        this.hucsToLoad = hucsToLoad;
        this.wfo = resourceData.wfo;
        this.resourceData = resourceData;
        this.runner = FFMPRunConfigurationManager.getInstance().getRunner(wfo);
        this.config = FFMPConfig.getInstance();
        this.latch = new CountDownLatch(1);

        if ((loadType == LOADER_TYPE.INITIAL)
                || (loadType == LOADER_TYPE.GENERAL)) {
            this.setPriority(MAX_PRIORITY);
        } else {
            this.setPriority(MIN_PRIORITY);
        }
    }

    public void waitFor() throws InterruptedException {
        latch.await();
    }

    /**
     * Add listener
     * 
     * @param fl
     */
    public synchronized void addListener(FFMPLoadListener fl) {
        loadListeners.add(fl);
    }

    /**
     * Remove listener
     * 
     * @param fl
     */
    public synchronized void removeListener(FFMPLoadListener fl) {
        loadListeners.remove(fl);
    }

    // kills the loader
    public void kill() {
        latch.countDown();
    }

    @Override
    public void run() {

        long time = System.currentTimeMillis();

        try {
            resourceData.setLoader(loadType);
            FFMPMonitor monitor = getMonitor();
            FFMPSourceConfigurationManager sourceConfig = monitor
                    .getSourceConfig();

            ProductRunXML productRun = runner.getProduct(siteKey);
            List<SourceXML> qpfSources = new ArrayList<SourceXML>();
            String layer = config.getFFMPConfigData().getLayer();
            String rateURI = null;

            if (loadType != LOADER_TYPE.GENERAL) {
                // preload all the uris except guidance. Guidance loads data
                // much further back and it is not efficient to group with the
                // rest.
                Set<String> sources = new HashSet<String>();
                sources.add(product.getRate());
                sources.add(product.getQpe());
                sources.add(product.getVirtual());
                for (String qpfType : productRun.getQpfTypes(product)) {
                    for (SourceXML qpfSource : productRun.getQpfSources(
                            product, qpfType)) {
                        sources.add(qpfSource.getSourceName());
                    }
                }
                monitor.preloadAvailableUris(siteKey, dataKey, sources,
                        timeBack);
            }
            if ((loadType == LOADER_TYPE.INITIAL || loadType == LOADER_TYPE.GENERAL)
                    && !product.getRate().equals(product.getQpe())) {
                Map<Date, List<String>> rateURIs = monitor.getAvailableUris(
                        siteKey, dataKey, product.getRate(), mostRecentTime);
                if (rateURIs.containsKey(mostRecentTime)) {
                    rateURI = rateURIs.get(mostRecentTime).get(0);
                }
            }

            NavigableMap<Date, List<String>> qpeURIs = monitor
                    .getAvailableUris(siteKey, dataKey, product.getQpe(),
                            timeBack);

            ArrayList<NavigableMap<Date, List<String>>> qpfs = new ArrayList<NavigableMap<Date, List<String>>>();

            for (String qpfType : productRun.getQpfTypes(product)) {
                for (SourceXML qpfSource : productRun.getQpfSources(product,
                        qpfType)) {

                    NavigableMap<Date, List<String>> qpfURIs = null;
                    Date qpfTime = timeBack;

                    if (loadType == LOADER_TYPE.GENERAL) {
                        qpfTime = monitor.getPreviousQueryTime(siteKey,
                                qpfSource.getSourceName());
                    }

                    qpfURIs = monitor.getAvailableUris(siteKey, dataKey,
                            qpfSource.getSourceName(), qpfTime);

                    if (qpfURIs != null && !qpfURIs.isEmpty()) {
                        qpfs.add(qpfURIs);
                        qpfSources.add(qpfSource);
                    }
                }
            }

            NavigableMap<Date, List<String>> virtualURIs = monitor
                    .getAvailableUris(siteKey, dataKey, product.getVirtual(),
                            timeBack);

            HashMap<String, NavigableMap<Date, List<String>>> guids = new HashMap<String, NavigableMap<Date, List<String>>>();

            for (String type : productRun.getGuidanceTypes(product)) {
                for (SourceXML guidSource : productRun.getGuidanceSources(
                        product, type)) {

                    NavigableMap<Date, List<String>> iguidURIs = null;
                    Date guidTime = timeBack;
                    if (loadType == LOADER_TYPE.GENERAL) {
                        guidTime = monitor.getPreviousQueryTime(siteKey,
                                guidSource.getSourceName());
                    }
                    if (guidTime == null) {
                        continue;
                    }

                    iguidURIs = monitor.getAvailableUris(siteKey, dataKey,
                            guidSource.getSourceName(), guidTime);

                    if (iguidURIs != null && !iguidURIs.isEmpty()) {
                        guids.put(guidSource.getSourceName(), iguidURIs);
                    }
                }
            }
            // We only load all for long range data, all + layer for medium
            // range
            if (loadType == LOADER_TYPE.TERTIARY) {
                hucsToLoad.clear();
                hucsToLoad.add(FFMPRecord.ALL);
            } else {
                // Only used as place holder name, No data is linked to it, uses
                // ALL
                hucsToLoad.remove(FFMPRecord.VIRTUAL);
            }

            if (isDone()) {
                return;
            }

            // rate
            if (rateURI != null) {
                fireLoaderEvent(loadType, "Processing " + product.getRate(),
                        isDone());
                for (String phuc : hucsToLoad) {
                    monitor.processUri(rateURI, siteKey, product.getRate(),
                            timeBack, phuc);
                }
                fireLoaderEvent(loadType, product.getRate(), isDone());
            }

            // qpes
            fireLoaderEvent(loadType, "Processing " + product.getQpe(),
                    isDone());
            FFMPAggregateRecord qpeCache = null;

            if (loadType == LOADER_TYPE.INITIAL) {

                SourceXML source = sourceConfig.getSource(product.getQpe());

                qpeCache = readAggregateRecord(source, dataKey, wfo);

                if (qpeCache != null) {
                    monitor.insertFFMPData(qpeCache, qpeURIs, siteKey,
                            product.getQpe());
                }
            }

            // Use this method of QPE data retrieval if you don't have cache
            // files
            if (!qpeURIs.isEmpty()) {
                for (String phuc : hucsToLoad) {
                    if (phuc.equals(layer) || phuc.equals(FFMPRecord.ALL)) {
                        monitor.processUris(qpeURIs, siteKey, product.getQpe(),
                                timeBack, phuc);
                    }
                }
            }

            fireLoaderEvent(loadType, product.getQpe(), isDone());

            int i = 0;
            for (NavigableMap<Date, List<String>> qpfURIs : qpfs) {
                // qpf
                fireLoaderEvent(loadType, "Processing " + product.getQpf(i),
                        isDone());
                FFMPAggregateRecord qpfCache = null;

                if (loadType == LOADER_TYPE.INITIAL) {

                    SourceXML source = qpfSources.get(i);

                    String pdataKey = findQPFHomeDataKey(source);
                    qpfCache = readAggregateRecord(source, pdataKey, wfo);

                    if (qpfCache != null) {
                        monitor.insertFFMPData(qpfCache, qpfURIs, siteKey,
                                source.getSourceName());
                    }
                }

                // Use this method of QPF data retrieval if you don't have cache
                // files
                if (!qpfURIs.isEmpty()) {
                    for (String phuc : hucsToLoad) {
                        if (phuc.equals(layer) || phuc.equals(FFMPRecord.ALL)) { // old
                            monitor.processUris(qpfURIs, siteKey,
                                    product.getQpf(i), timeBack, phuc);
                        }
                    }
                }

                fireLoaderEvent(loadType, product.getQpf(i), isDone());

                i++;
            }

            fireLoaderEvent(loadType, "Processing " + product.getVirtual(),
                    isDone());
            // process virtual all for all only, never uses cache files
            if (!virtualURIs.isEmpty()) {
                monitor.processUris(virtualURIs, siteKey, product.getVirtual(),
                        timeBack, FFMPRecord.ALL);
            }

            fireLoaderEvent(loadType, product.getVirtual(), isDone());

            // process guidance all for all only, never uses cache files
            for (String type : productRun.getGuidanceTypes(product)) {

                ArrayList<SourceXML> guidSources = productRun
                        .getGuidanceSources(product, type);
                for (SourceXML guidSource : guidSources) {

                    NavigableMap<Date, List<String>> iguidURIs = guids
                            .get(guidSource.getSourceName());

                    fireLoaderEvent(loadType,
                            "Processing " + guidSource.getSourceName(),
                            isDone());

                    monitor.processUris(iguidURIs, siteKey,
                            guidSource.getSourceName(), timeBack,
                            FFMPRecord.ALL);

                    fireLoaderEvent(loadType, guidSource.getSourceName(),
                            isDone());

                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "General Problem in Loading FFMP Data", e);
        } finally {
            latch.countDown();
            synchronized (this) {
                this.notifyAll();
            }
        }

        String message = null;
        if (loadType == LOADER_TYPE.INITIAL) {
            message = "Finished Initial Load";
        } else {
            message = "Finished General Data Load";
        }

        long endTime = (System.currentTimeMillis()) - time;
        System.out.println(loadType.loaderType + " Loader took: " + endTime
                / 1000 + " seconds");
        fireLoaderEvent(loadType, message, isDone());
    }

    /**
     * Fire loader updates to the front end displays
     * 
     * @param FFMPLoaderStatus
     **/
    public void fireLoaderEvent(LOADER_TYPE ltype, String lmessage,
            boolean lstatus) {

        FFMPLoaderStatus sstatus = new FFMPLoaderStatus(ltype, lmessage,
                lstatus);

        FFMPLoaderEvent fle = new FFMPLoaderEvent(sstatus);
        Iterator<FFMPLoadListener> iter = loadListeners.iterator();

        while (iter.hasNext()) {
            FFMPLoadListener listener = iter.next();
            listener.loadStatus(fle);
        }

    }

    private FFMPMonitor getMonitor() {
        if (FFMPMonitor.isRunning()) {
            return FFMPMonitor.getInstance();
        } else {
            latch.countDown();
            return null;
        }
    }

    public enum LOADER_TYPE {

        INITIAL("Initial"), GENERAL("General"), SECONDARY("Secondary"), TERTIARY(
                "Tertiary");

        private final String loaderType;

        private LOADER_TYPE(String name) {
            loaderType = name;
        }

        public String getLoaderType() {
            return loaderType;
        }
    };

    /**
     * Loads the Cache files
     * 
     * @param sourceName
     * @param huc
     * @param wfo
     * @return
     */
    private FFMPAggregateRecord readAggregateRecord(SourceXML source,
            String pdataKey, String wfo) throws Exception {

        FFMPAggregateRecord record = null;
        String sourceSiteDataKey = getSourceSiteDataKey(source, pdataKey);

        try {

            File hdf5File = FFMPUtils.getHdf5File(wfo, sourceSiteDataKey);
            IDataStore dataStore = DataStoreFactory.getDataStore(hdf5File);
            IDataRecord rec = dataStore.retrieve(wfo, sourceSiteDataKey,
                    Request.ALL);
            byte[] bytes = ((ByteDataRecord) rec).getByteData();
            record = SerializationUtil.transformFromThrift(
                    FFMPAggregateRecord.class, bytes);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Couldn't read Aggregate Record" + sourceSiteDataKey);
        }

        return record;
    }

    /**
     * Finds the home datakey identifier for QPF sources
     * 
     * @param source
     * @return
     */
    private String findQPFHomeDataKey(SourceXML source) {

        FFMPRunConfigurationManager runManager = FFMPRunConfigurationManager
                .getInstance();

        for (ProductRunXML product : runManager.getProducts()) {

            try {
                // we are just checking if it exists or not
                String pdataKey = product.getProductKey();
                String sourceSiteDataKey = getSourceSiteDataKey(source,
                        pdataKey);
                File hdf5File = FFMPUtils.getHdf5File(wfo, sourceSiteDataKey);
                DataStoreFactory.getDataStore(hdf5File);

                return pdataKey;
            } catch (Exception e) {
                // not the right key, doesn't exist
                continue;
            }
        }

        return siteKey;
    }

    /**
     * Get the sourceSiteDataKey for this piece of data
     * 
     * @param source
     * @param pdataKey
     * @return
     */
    private String getSourceSiteDataKey(SourceXML source, String pdataKey) {
        return source.getSourceName() + "-" + siteKey + "-" + pdataKey;
    }

    public boolean isDone() {
        return latch.getCount() == 0;
    }

}