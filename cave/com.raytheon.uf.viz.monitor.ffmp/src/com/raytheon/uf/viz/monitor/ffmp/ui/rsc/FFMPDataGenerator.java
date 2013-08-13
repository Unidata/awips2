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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData;

/**
 * FFMP Data generator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2009           dhladky     Initial creation
 * Jan 25, 2012 DR 13839  gzhang	  Use paintTime for QPF
 * Mar 01, 2013 DR 13228  gzhang      Get VGB county info for row name 
 * Feb 1,  2013 DR 1569   dhladky     Switched to using pypies records instead of files
 * Feb 19, 2013    1639   njensen      Replaced FFMPCacheRecord with FFMPRecord
 * feb 20, 2013    1635   dhladky     Fixed multi guidance displays
 * Feb 28, 2013    1729   dhladky     General enhancements for speed.
 * Apr 12, 2013    1902   mpduff      Code Cleanup.
 * Apr 15, 2013    1890   dhladky     Added another constant fix.
 * Apr 15, 2013    1911   dhladky     Fixed forced FFG for centered aggregates.
 * Apr 24, 2013    1946   mpduff      Fixed FFFG value for ALL when an aggregate is forced
 * Apr 26, 2013    1954   bsteffen    Minor code cleanup throughout FFMP.
 * May 07, 2013    1986   njensen     Removed unnecessary sort
 * May 10, 2013    1919   mpduff      Fixed problem with VGBs
 * May 22, 2013    1902   mpduff      Code cleanup.
 * Jun 11, 2013    2085   njensen     Extracted row creation to FFMPRowGenerator and
 *                                     multi-threaded row creation.
 * July 1, 2013    2155   dhladky     Fixed bug that created more rows than were actually needed.
 * Jul 15, 2013 2184        dhladky     Remove all HUC's for storage except ALL
 * Jul 16, 2013    2197   njensen     Use FFMPBasinData.hasAnyBasins() for efficiency
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPDataGenerator {
    private final FfmpTableConfig tableConfig;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataGenerator.class);

    protected final String siteKey;

    protected final Date paintRefTime;

    protected final Object centeredAggregationKey;

    protected final String huc;

    protected final double sliderTime;

    protected boolean isWorstCase = false;

    protected FFMPTemplates ft = null;

    protected FFMPResource resource = null;

    protected FFMPMonitor monitor = null;

    protected FFMPBasinData qpeBasin = null;

    protected FFMPBasinData qpfBasin = null;

    protected FFMPBasinData rateBasin = null;

    protected Map<String, FFMPBasinData> guidBasins = null;

    protected FFMPBasinData virtualBasin = null;

    protected Map<String, FFMPRecord> guidRecords = null;

    protected FFMPRecord baseRec = null;

    protected boolean isRate = false;

    protected long expirationTime = 0l;

    protected String[] cwaArr = null;

    protected Map<String, FFFGForceUtil> forceUtils = null;

    protected FfmpTableConfigData ffmpTableCfgData = null;

    private JobPool jobPool = new JobPool("Table Data Generation", 3, true,
            Job.INTERACTIVE);

    public FFMPDataGenerator(FFMPMonitor monitor, FFMPResource resource) {
        siteKey = resource.getSiteKey();
        paintRefTime = resource.getPaintTime().getRefTime();
        centeredAggregationKey = resource.centeredAggregationKey;
        huc = resource.getHuc();
        sliderTime = resource.getTime();
        isWorstCase = resource.isWorstCase();

        this.tableConfig = FfmpTableConfig.getInstance();
        this.resource = resource;
        this.monitor = monitor;
        this.ft = monitor.getTemplates(siteKey);
        SourceXML primarySource = resource.getResourceData()
                .getPrimarySourceXML();
        this.isRate = primarySource.isRate();
        this.expirationTime = primarySource.getExpirationMinutes(siteKey)
                * TimeUtil.MILLIS_PER_MINUTE;
        ffmpTableCfgData = tableConfig.getTableConfigData(siteKey);
    }

    /**
     * Generate the FFMP Data.
     * 
     * @return FFMPTableData object
     * @throws Exception
     */

    public FFMPTableData generateFFMPData() throws Exception {

        // You should always have at least a QPE data source
        FFMPTableData tData = null;
        // update the FFFGDataManager
        FFFGDataMgr.getUpdatedInstance();

        try {

            FIELDS field = getBaseField();

            if (field == null || baseRec == null) {

                return tData;
            }

            List<DomainXML> domains = resource.getDomains();

            if ((centeredAggregationKey == null) || huc.equals(FFMPRecord.ALL)) {

                if (huc.equals(FFMPRecord.ALL)) {

                    FFMPBasinData fbd = baseRec.getBasinData();
                    tData = new FFMPTableData(fbd.getBasins().size());

                    for (Long key : fbd.getBasins().keySet()) {

                        FFMPBasinMetaData fmdb = ft.getBasin(siteKey, key);

                        if (fmdb == null) {

                            continue;

                        }

                        for (DomainXML domain : domains) {

                            String cwa = domain.getCwa();

                            if ((cwa.equals(fmdb.getCwa()))
                                    || (domain.isPrimary() && fmdb
                                            .isPrimaryCwa())) {

                                try {
                                    setFFMPRow(fbd.get(key), tData, false, cwa);

                                } catch (Exception e) {

                                    statusHandler.handle(Priority.PROBLEM,
                                            "Couldn't create table row", e);
                                }

                                if (virtualBasin != null) {

                                    for (Long id : ft
                                            .getVirtualGageBasinLookupIds(
                                                    siteKey, key, huc,
                                                    resource.basinTableDlg
                                                            .getRowName())) {

                                        try {

                                            setFFMPRow(virtualBasin.get(id),
                                                    tData, true, cwa);

                                        } catch (Exception e) {

                                            statusHandler.handle(
                                                    Priority.PROBLEM,
                                                    "Couldn't create table row"
                                                            + e);

                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {

                    // Find all of the basins for this HUC level
                    List<Long> keyList = ft
                            .getHucKeyList(siteKey, huc, domains);
                    tData = new FFMPTableData(keyList.size());

                    for (Long key : keyList) {

                        List<Long> pfafs = ft.getAggregatePfafs(key, siteKey,
                                huc);
                        boolean isVGB = false;

                        if (ft.checkVGBsInAggregate(key, siteKey, huc)) {

                            isVGB = true;
                        }

                        if (!pfafs.isEmpty()) {

                            FFMPBasinMetaData fmdb = ft.getBasinInDomains(
                                    siteKey, domains, pfafs);

                            if (fmdb != null) {

                                try {

                                    FFMPBasin basin = new FFMPBasin(key, true);
                                    setFFMPRow(basin, tData, isVGB, null);

                                } catch (Exception e) {

                                    statusHandler.handle(Priority.PROBLEM,

                                    "Couldn't create table row", e);
                                }
                            }
                        }
                    }
                }
            }

            // show pfafs in aggregation

            else {

                FFMPBasinData fbd = baseRec.getBasinData();
                List<Long> centerAggPfafs = resource
                        .getCenteredAggregatePfafs();
                tData = new FFMPTableData(centerAggPfafs.size());

                for (Long key : centerAggPfafs) {

                    FFMPBasinMetaData fmdb = ft.getBasin(siteKey, key);

                    if (fmdb != null) {
                        for (DomainXML domain : domains) {
                            if ((domain.getCwa().equals(fmdb.getCwa()))
                                    || (domain.isPrimary() && fmdb
                                            .isPrimaryCwa())) {

                                setFFMPRow(fbd.get(key), tData, false, null);

                                if (virtualBasin != null) {

                                    // We *DO NOT* want all of the aggregate
                                    // VGB's,
                                    // just the one's for this individual basin.

                                    List<Long> virtuals = ft
                                            .getVirtualGageBasinLookupIds(
                                                    siteKey, key,
                                                    FFMPRecord.ALL,
                                                    resource.basinTableDlg
                                                            .getRowName());

                                    for (Long id : virtuals) {
                                        try {
                                            setFFMPRow(virtualBasin.get(id),
                                                    tData, true, null);

                                        } catch (Exception e) {

                                            statusHandler
                                                    .handle(Priority.PROBLEM,
                                                            "Couldn't create table row",
                                                            e);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

        } catch (Exception e) {

            statusHandler.handle(Priority.PROBLEM,
                    "Failed to load FFMP table data!", e);

        }

        // wait for all the rows to finish being created before continuing on

        long t0 = System.currentTimeMillis();
        jobPool.join();
        System.out.println("Waited on FFMP job pool for: "
                + (System.currentTimeMillis() - t0));

        return tData;

    }

    private void setFFMPRow(FFMPBasin cBasin, FFMPTableData tData,
            boolean isVGB, String domain) {
        FFMPRowGenerator rowTask = new FFMPRowGenerator(this, cBasin, tData,
                isVGB, domain);
        jobPool.schedule(rowTask);
    }

    /**
     * Gets the base field
     * 
     * @return
     * @throws VizException
     */
    private FIELDS getBaseField() {

        String dataKey = resource.getDataKey();
        ProductXML product = resource.getProduct();
        Date tableTime = resource.getTableTime();

        FIELDS field = null;

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(siteKey);
        String qpfType = ffmpTableCfgData.getQpfType();
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getProduct(siteKey);
        SourceXML qpfSource = productRun.getQpfSources(product, qpfType).get(0);

        FFMPConfig config = FFMPConfig.getInstance();
        String includedCWAs = config.getFFMPConfigData().getIncludedCWAs();
        cwaArr = includedCWAs.split(",");
        monitor.setQpfWindow(monitor.getTimeWindow(qpfSource, paintRefTime,
                siteKey));
        Date qpeTime = paintRefTime;
        if (resource.isSplit()) {
            // hack off the QPF duration for the table values of QPE (Split
            // Window)
            double duration = qpfSource.getDurationHour();
            qpeTime = new Date(
                    (long) (qpeTime.getTime() - (duration * TimeUtil.MILLIS_PER_HOUR)));
        }

        monitor.setQpeWindow(new FFMPTimeWindow(tableTime, qpeTime));
        FFMPRecord rateRecord = monitor.getRateRecord(product, siteKey,
                dataKey, product.getRate(), paintRefTime, true);
        FFMPRecord qpeRecord = monitor.getQPERecord(product, siteKey, dataKey,
                product.getQpe(), tableTime, true);
        FFMPRecord qpfRecord = monitor.getQPFRecord(product, siteKey, dataKey,
                null, paintRefTime, true);
        guidRecords = monitor.getGuidanceRecords(product, siteKey, tableTime,
                true);
        FFMPRecord virtualRecord = monitor.getVirtualRecord(product, siteKey,
                dataKey, product.getVirtual(), tableTime, true);

        try {
            if (rateRecord != null) {
                rateBasin = rateRecord.getBasinData();
                if (rateBasin.hasAnyBasins()) {
                    field = FIELDS.RATE;
                    baseRec = rateRecord;
                }
            }
            if (qpeRecord != null) {
                qpeBasin = qpeRecord.getBasinData();
                if (qpeBasin.hasAnyBasins()) {

                    field = FIELDS.QPE;
                    if (baseRec == null) {
                        baseRec = qpeRecord;
                    }
                }
            }
            if (qpfRecord != null) {
                qpfBasin = qpfRecord.getBasinData();
            }
            if (guidRecords != null) {
                guidBasins = new HashMap<String, FFMPBasinData>();
                for (String type : guidRecords.keySet()) {
                    if (guidRecords.get(type) != null) {
                        guidBasins.put(type, guidRecords.get(type)
                                .getBasinData());
                    } else {
                        guidBasins.put(type, null);
                    }
                }
            }
            if (virtualRecord != null) {
                virtualBasin = virtualRecord.getBasinData();
            }

            // Get interpolators
            HashMap<String, FFMPGuidanceInterpolation> interpolators = resource
                    .getGuidanceInterpolators();
            if ((forceUtils == null) || (forceUtils.isEmpty())) {
                forceUtils = new HashMap<String, FFFGForceUtil>();

                for (String guidType : interpolators.keySet()) {
                    FFFGForceUtil fu = new FFFGForceUtil(resource, guidType);
                    forceUtils.put(guidType, fu);
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "field Not Available");
        }

        return field;
    }

}
