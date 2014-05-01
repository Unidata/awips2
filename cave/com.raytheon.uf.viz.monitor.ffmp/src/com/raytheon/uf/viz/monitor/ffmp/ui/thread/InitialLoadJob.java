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

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.NavigableMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPAggregateRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;

/**
 * The initial FFMP load job for the data required initially by the displays.
 * Attempts to use the FFMPAggregateRecords for faster loading.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013 2075       njensen     Initial creation
 * Jun 07, 2013 2075       njensen     Added progress monitoring
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InitialLoadJob extends AbstractLoadJob {

    public InitialLoadJob(FFMPResourceData resourceData, Date timeBack,
            Date mostRecentTime, List<String> hucsToLoad) {
        super("Initial FFMP Load", resourceData, timeBack, mostRecentTime,
                hucsToLoad);
        this.setPriority(INTERACTIVE);
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        SubMonitor smonitor = SubMonitor.convert(monitor, "Loading Data", 1200);
        long t0 = System.currentTimeMillis();
        smonitor.subTask("Preloading URIs...");
        preloadAvailableUris();
        smonitor.worked(100);

        // Rate
        smonitor.subTask("Processing Rate...");
        doRate();
        smonitor.worked(200);

        // QPE
        smonitor.subTask("Processing QPE...");
        NavigableMap<Date, List<String>> qpeURIs = getQpeUris();
        FFMPSourceConfigurationManager sourceConfig = this.ffmpMonitor
                .getSourceConfig();
        SourceXML source = sourceConfig.getSource(product.getQpe());
        FFMPAggregateRecord qpeCache = readAggregateRecord(source,
                resourceData.dataKey, resourceData.wfo);
        if (qpeCache != null) {
            this.ffmpMonitor.insertFFMPData(qpeCache, qpeURIs,
                    resourceData.siteKey, product.getQpe());
        }
        smonitor.worked(25);
        doQpe(qpeURIs, smonitor.newChild(225));
        if (!this.shouldRun()) {
            return Status.CANCEL_STATUS;
        }

        // QPF
        smonitor.subTask("Processing QPF...");
        List<NavigableMap<Date, List<String>>> qpfs = new ArrayList<NavigableMap<Date, List<String>>>();
        List<SourceXML> qpfSources = new ArrayList<SourceXML>();
        for (String qpfType : productRun.getQpfTypes(product)) {
            for (SourceXML qpfSource : productRun.getQpfSources(product,
                    qpfType)) {

                NavigableMap<Date, List<String>> qpfURIs = getQpfUris(
                        qpfSource.getSourceName(), startTime);

                if (qpfURIs != null && !qpfURIs.isEmpty()) {
                    qpfs.add(qpfURIs);
                    qpfSources.add(qpfSource);
                }
            }
        }

        int i = 0;
        smonitor.worked(25);
        SubMonitor qpfmonitor = smonitor.newChild(225);
        qpfmonitor.beginTask(null, qpfs.size() * PROGRESS_FACTOR);
        for (NavigableMap<Date, List<String>> qpfURIs : qpfs) {
            FFMPAggregateRecord qpfCache = null;
            source = qpfSources.get(i);

            String pdataKey = findQPFHomeDataKey(source);
            qpfCache = readAggregateRecord(source, pdataKey, resourceData.wfo);

            if (qpfCache != null) {
                this.ffmpMonitor.insertFFMPData(qpfCache, qpfURIs,
                        resourceData.siteKey, source.getSourceName());
            }

            doQpf(qpfURIs, product.getQpf(i),
                    qpfmonitor.newChild(PROGRESS_FACTOR));
            i++;
        }
        if (!this.shouldRun()) {
            return Status.CANCEL_STATUS;
        }

        // Virtual
        smonitor.subTask("Processing Virtual...");
        doVirtual(smonitor.newChild(200));

        // Guidance
        smonitor.subTask("Processing Guidance...");
        doGuidance(startTime, smonitor.newChild(200));

        System.out.println("Initial Load Job took: "
                + (System.currentTimeMillis() - t0));
        return Status.OK_STATUS;
    }

    /**
     * Loads the aggregate files
     * 
     * @param sourceName
     * @param huc
     * @param wfo
     * @return
     */
    private FFMPAggregateRecord readAggregateRecord(SourceXML source,
            String pdataKey, String wfo) {

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
     * Get the sourceSiteDataKey for this piece of data
     * 
     * @param source
     * @param pdataKey
     * @return
     */
    private String getSourceSiteDataKey(SourceXML source, String pdataKey) {
        return source.getSourceName() + "-" + resourceData.siteKey + "-"
                + pdataKey;
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
                File hdf5File = FFMPUtils.getHdf5File(resourceData.wfo,
                        sourceSiteDataKey);
                DataStoreFactory.getDataStore(hdf5File);

                return pdataKey;
            } catch (Exception e) {
                // not the right key, doesn't exist
                continue;
            }
        }

        return resourceData.siteKey;
    }

}
