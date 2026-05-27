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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceIngestConfigXML;
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
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPTableColumnXML;

/**
 * FFMP Data generator
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 20, 2009           dhladky   Initial creation
 * Jan 25, 2012  13839    gzhang    Use paintTime for QPF
 * Mar 01, 2013  13228    gzhang    Get VGB county info for row name
 * Feb 01, 2013  1569     dhladky   Switched to using pypies records instead of
 *                                  files
 * Feb 19, 2013  1639     njensen   Replaced FFMPCacheRecord with FFMPRecord
 * Feb 20, 2013  1635     dhladky   Fixed multi guidance displays
 * Feb 28, 2013  1729     dhladky   General enhancements for speed.
 * Apr 12, 2013  1902     mpduff    Code Cleanup.
 * Apr 15, 2013  1890     dhladky   Added another constant fix.
 * Apr 15, 2013  1911     dhladky   Fixed forced FFG for centered aggregates.
 * Apr 24, 2013  1946     mpduff    Fixed FFFG value for ALL when an aggregate
 *                                  is forced
 * Apr 26, 2013  1954     bsteffen  Minor code cleanup throughout FFMP.
 * May 07, 2013  1986     njensen   Removed unnecessary sort
 * May 10, 2013  1919     mpduff    Fixed problem with VGBs
 * May 22, 2013  1902     mpduff    Code cleanup.
 * Jun 11, 2013  2085     njensen   Extracted row creation to FFMPRowGenerator
 *                                  and multi-threaded row creation.
 * Jul 01, 2013  2155     dhladky   Fixed bug that created more rows than were
 *                                  actually needed.
 * Jul 15, 2013  2184     dhladky   Remove all HUC's for storage except ALL
 * Jul 16, 2013  2197     njensen   Use FFMPBasinData.hasAnyBasins() for
 *                                  efficiency
 * May 19, 2014  16096    gzhang    Fix QPFSCAN not showing M issue for
 *                                  different radar source.
 * Sep 24, 2015  4756     dhladky   Preserve ordering of data hash for guidance
 *                                  types.
 * May 15, 2017  11861    lshi      FFMP use of QPF in calculating QPE in Basin
 *                                  Table
 * Mar 07, 2018  6581     mduff     Code cleanup.
 * Jul 10, 2018  6655     njensen   Updated for single FFMPRunXML, removed
 *                                  unused M_LIST
 * Jul 16, 2018  6766     randerso  Code cleanup
 * Jul 30, 2018  6720     njensen   Update for changed method names
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPDataGenerator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataGenerator.class);

    private final FfmpTableConfig tableConfig;

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

    /**
     * A map of guidance source family to FFMPBasinData.
     * 
     * As of August of 2018, order of items is extremely important as it somehow
     * relates to indexing into the table data. TODO: Make FFMP more robust so
     * the order is not so important.
     */
    protected LinkedHashMap<String, FFMPBasinData> guidBasins = null;

    protected FFMPBasinData virtualBasin = null;

    /**
     * A map of guidance source family to FFMPRecord.
     * 
     * As of August of 2018, order of items is extremely important as it somehow
     * relates to indexing into the table data. TODO: Make FFMP more robust so
     * the order is not so important.
     */
    protected LinkedHashMap<String, FFMPRecord> guidRecords = null;

    protected FFMPRecord baseRec = null;

    protected boolean isRate = false;

    protected long expirationTime = 0l;

    protected String[] cwaArr = null;

    protected Map<String, FFFGForceUtil> forceUtils = null;

    protected FfmpTableConfigData ffmpTableCfgData = null;

    private JobPool jobPool = new JobPool("Table Data Generation", 3, true,
            Job.INTERACTIVE);

    private FFMPBasinData qpfBasinClone = null;

    public FFMPDataGenerator(FFMPMonitor monitor, FFMPResource resource,
            Date updateTime) {
        siteKey = resource.getSiteKey();
        paintRefTime = updateTime;
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
            List<List<Long>> huclistsAll = getOtherSiteQpfBasins(FFMPRecord.ALL,
                    domains);
            if ((centeredAggregationKey == null)
                    || huc.equals(FFMPRecord.ALL)) {

                if (huc.equals(FFMPRecord.ALL)) {

                    FFMPBasinData fbd = baseRec.getBasinData();
                    tData = new FFMPTableData(fbd.getBasins().size());

                    for (Long key : fbd.getBasins().keySet()) {
                        FFMPBasinMetaData fmdb = ft.getBasin(siteKey, key);

                        if (fmdb == null) {
                            continue;
                        }
                        this.filterOtherSiteHucs(huclistsAll, key, false);
                        for (DomainXML domain : domains) {

                            String cwa = domain.getCwa();

                            if ((cwa.equals(fmdb.getCwa()))
                                    || (domain.isPrimary()
                                            && fmdb.isPrimaryCwa())) {

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
                    List<Long> keyList = ft.getHucKeyList(siteKey, huc,
                            domains);
                    tData = new FFMPTableData(keyList.size());
                    List<List<Long>> huclists = getOtherSiteQpfBasins(huc,
                            domains);
                    for (Long key : keyList) {

                        List<Long> pfafs = ft.getAggregatePfafs(key, siteKey,
                                huc);
                        boolean isVGB = false;

                        if (ft.checkVGBsInAggregate(key, siteKey, huc)) {

                            isVGB = true;
                        }

                        if (!pfafs.isEmpty()) {

                            FFMPBasinMetaData fmdb = ft
                                    .getBasinInDomains(siteKey, domains, pfafs);

                            if (fmdb != null) {

                                try {
                                    this.filterOtherSiteHucs(huclists, key,
                                            true);
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
            } else {
                // show pfafs in aggregation
                FFMPBasinData fbd = baseRec.getBasinData();
                List<Long> centerAggPfafs = resource
                        .getCenteredAggregatePfafs();
                tData = new FFMPTableData(centerAggPfafs.size());

                for (Long key : centerAggPfafs) {
                    FFMPBasinMetaData fmdb = ft.getBasin(siteKey, key);

                    if (fmdb != null) {
                        for (DomainXML domain : domains) {
                            if ((domain.getCwa().equals(fmdb.getCwa()))
                                    || (domain.isPrimary()
                                            && fmdb.isPrimaryCwa())) {
                                this.filterOtherSiteHucs(huclistsAll, key,
                                        false);
                                setFFMPRow(fbd.get(key), tData, false, null);

                                if (virtualBasin != null) {
                                    /*
                                     * We *DO NOT* want all of the aggregate
                                     * VGBs, just the one's for this individual
                                     * basin.
                                     */
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
                                            statusHandler.handle(
                                                    Priority.PROBLEM,
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
        statusHandler.debug("Waited on FFMP job pool for: "
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
        String qpfDisplayName = ffmpTableCfgData.getQpfDisplayName();
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getProduct(siteKey);
        SourceXML qpfSource = productRun
                .getQpfSourcesByDisplayName(product, qpfDisplayName).get(0);

        FFMPConfig config = FFMPConfig.getInstance();
        String includedCWAs = config.getFFMPConfigData().getIncludedCWAs();
        cwaArr = includedCWAs.split(",");
        monitor.setQpfWindow(
                monitor.getTimeWindow(qpfSource, paintRefTime, siteKey));
        Date qpeTimeEnd = paintRefTime;
        Date qpeTimeBegin = tableTime;

        if (resource.isSplit()) {
            // hack off the QPF duration for the table values of QPE (Split
            // Window)
            double duration = qpfSource.getDurationHour();
            qpeTimeBegin = new Date((long) (qpeTimeBegin.getTime()
                    + (duration * TimeUtil.MILLIS_PER_HOUR)));

        }
        monitor.setQpeWindow(new FFMPTimeWindow(qpeTimeBegin, qpeTimeEnd));

        FFMPRecord rateRecord = monitor.getRateRecord(product, siteKey, dataKey,
                product.getRate(), paintRefTime, true);
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
                qpfBasinClone = qpfRecord.getBasinData();
            }
            if (guidRecords != null) {
                guidBasins = new LinkedHashMap<>();
                for (Entry<String, FFMPRecord> entry : guidRecords.entrySet()) {
                    String type = entry.getKey();
                    if (entry.getValue() != null) {
                        // type is coming through as source name FAMILY
                        guidBasins.put(type, entry.getValue().getBasinData());
                    } else {
                        guidBasins.put(type, null);
                    }
                }
            }
            if (virtualRecord != null) {
                virtualBasin = virtualRecord.getBasinData();
            }

            // Get interpolators
            Map<String, FFMPGuidanceInterpolation> interpolators = resource
                    .getGuidanceInterpolators();
            if ((forceUtils == null) || (forceUtils.isEmpty())) {
                forceUtils = new HashMap<>();

                for (String guidType : interpolators.keySet()) {
                    FFFGForceUtil fu = new FFFGForceUtil(resource, guidType);
                    forceUtils.put(guidType, fu);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Field " + field + " not available", e);
        }

        return field;
    }

    /**
     * ---------------------------------------------------------- Below is for
     * DR 16096 use FfmpTableConfigData.setQpfType() to find the QPF type, only
     * one at a time with Radio Button on a Basin Table.
     *
     * see getBaseField() for qpfsource/qpftyp and
     * FfmpTableConfigData.setQpfType()
     *
     * update code one step at a time. correct first; performance,hard-coding
     * second.
     *
     * @return Map<QPFSCAN type, ArrayList<datakey>> i.e.: <QPFSCANkccx, [kccx]
     *         list> / <QPFSCAN, [kakq,klwx] list>
     */
    public Map<String, List<String>> getQpfDataKeyMap() {
        Map<String, List<String>> map = new HashMap<>();

        FFMPRunXML rxml = FFMPRunConfigurationManager.getInstance()
                .getFFMPRunner();
        List<String> qpflist = getQpfTypes();

        List<SourceIngestConfigXML> sicList = rxml.getSourceIngests();
        for (SourceIngestConfigXML sic : sicList) {
            String sname = sic.getSourceName();

            for (String qpf : qpflist) {
                if (qpf.equalsIgnoreCase(sname)) {
                    map.put(sname, sic.getDataKey());
                }
            }
        }
        return map;
    }

    /**
     * Based on AttributesDlg.createAttributeControls(), used for adding a QPF
     * column. Only qpfs in table column are of interest
     *
     * @return
     */
    public List<String> getQpfTypes() {
        ProductXML prodXml = monitor.getProductXML(resource.getPrimarySource());
        FFMPRunConfigurationManager runManager = FFMPRunConfigurationManager
                .getInstance();
        ProductRunXML productRun = runManager.getProduct(resource.getSiteKey());
        return productRun.getQpfDisplayNames(prodXml);
    }

    /**
     * get displaying dataKeys of the displaying QPFSCAN 2014-01-01
     */
    public List<String> getDisplayingQpfDataKeys(String dqpf) {
        Map<String, List<String>> map = getQpfDataKeyMap();
        List<String> list = map.get(dqpf);

        return list == null ? new ArrayList<>() : list;
    }

    /**
     * filtering non-QPFSCAN basins/hucs
     */
    public List<List<Long>> getOtherSiteQpfBasins(String huc,
            List<DomainXML> domains) {
        String dqpf = getQpfType();
        List<List<Long>> huclist = new ArrayList<>();

        List<String> dataKeys = this.getDisplayingQpfDataKeys(dqpf);
        // mosaic QPFSCAN
        for (String site : dataKeys) {
            huclist.add(ft.getHucKeyList(site, huc, domains));
        }

        return huclist;
    }

    public void filterOtherSiteHucs(List<List<Long>> huclists, Long key,
            boolean isAggregate) {
        if (huclists == null || huclists.isEmpty()) {
            return;
        }

        if (isAggregate) {
            this.setHucLevelQpf(key);
            return;
        }

        // Only for non-aggregates; fix NO DATA shows 0.0
        try {
            if (Float.isNaN(resource.getBasin(key, FFMPRecord.FIELDS.QPF,
                    this.paintRefTime, false).getValue())) {
                setQPFMissing();
            } else {
                this.qpfBasin = this.qpfBasinClone;
            }
        } catch (Exception e) {
            statusHandler.info("FFMPResource.getBasin() Exception: "
                    + e.getLocalizedMessage(), e);
        }

    }

    /**
     * based on FFMPConfig.isSplit() and
     * AttributesDlg.createAttributeControls() @176/178 since Only using
     * AttrData causing all "M" when QPFSCAN column already on.
     */
    public String getQpfType() {
        String qCname = "";
        boolean qpfColOn = false;

        try {
            qCname = this.ffmpTableCfgData
                    .getTableColumnAttr(
                            ffmpTableCfgData.getTableColumnKeys()[3])
                    .getOriginalName().split("::")[0];
        } catch (Exception e) {
            statusHandler.warn("Error getting QPF type", e);
        }
        for (FFMPTableColumnXML tcXML : monitor.getConfig().getFFMPConfigData()
                .getTableColumnData()) {
            if (tcXML.getColumnName().compareTo(FIELDS.QPF.name()) == 0) {
                qpfColOn = tcXML.getDisplayedInTable();
            }
        }

        return qpfColOn ? qCname
                : monitor.getConfig().getAttrData().getQpfType();
    }

    // Utilize the fact FFMPRowGenerator set QPFSCAN M if qpfBasin null
    private void setQPFMissing() {
        this.qpfBasin = null;
    }

    /*
     * Loop through the HUC's basins to check if there are values not NaN then
     * set qpf; otherwise set the HUC level M. centeredAggregationKey NULL: not
     * a specific huc (COUNTY,HUC0,etc) clicked
     */
    private void setHucLevelQpf(Long key) {
        List<Long> list = this.monitor.getTemplates(this.siteKey)
                .getAggregatePfafs(key, this.siteKey, this.huc);
        boolean hasValue = false;

        for (Long bkey : list) {
            try {
                if (!Float.isNaN(resource.getBasin(bkey, FFMPRecord.FIELDS.QPF,
                        this.paintRefTime, false).getValue())) {
                    // one is enough
                    hasValue = true;
                    break;
                }
            } catch (VizException e) {
                statusHandler.info(
                        "FFMPDataGenerator Exception: " + e.getMessage(), e);
            }
        }

        qpfBasin = hasValue ? this.qpfBasinClone : null;
    }
}