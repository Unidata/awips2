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
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
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
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableCellData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableRowData;
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
 * Apr 15, 2013 1890      dhladky     Added another constant fix.
 * Apr 15, 2013 1911      dhladky     Fixed forced FFG for centered aggregates.
 * Apr 24, 2013 1946      mpduff      Fixed FFFG value for ALL when an aggregate is forced
 * Apr 26, 2013 1954      bsteffen    Minor code cleanup throughout FFMP.
 * May 7, 2013   1986     njensen     Removed unnecessary sort
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

    private static final String ALL = FFMPRecord.ALL;

    private static final String NA = "NA";

    private final String siteKey;

    private final Date paintRefTime;

    private final Object centeredAggregationKey;

    private final String huc;

    private final double sliderTime;

    private boolean isWorstCase = false;

    private FFMPTemplates ft = null;

    private FFMPResource resource = null;

    private FFMPMonitor monitor = null;

    private FFMPBasinData qpeBasin = null;

    private FFMPBasinData qpfBasin = null;

    private FFMPBasinData rateBasin = null;

    private Map<String, FFMPBasinData> guidBasins = null;

    private FFMPBasinData virtualBasin = null;

    private Map<String, FFMPRecord> guidRecords = null;

    private FFMPRecord baseRec = null;

    private boolean isRate = false;

    private long expirationTime = 0l;

    private String[] cwaArr = null;

    private Map<String, FFFGForceUtil> forceUtils = null;

    private FfmpTableConfigData ffmpTableCfgData = null;

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
        tData = new FFMPTableData();

        try {
            FIELDS field = getBaseField();

            if (field == null || baseRec == null) {
                return tData;
            }
            FFMPBasinData fbd = null;
            if (centeredAggregationKey != null) {
                fbd = baseRec.getBasinData(ALL);
            } else {
                fbd = baseRec.getBasinData(huc);
            }

            List<DomainXML> domains = resource.getDomains();
            if (!fbd.getBasins().isEmpty()) {
                if ((centeredAggregationKey == null) || huc.equals(ALL)) {
                    // System.out.println(fbd.getBasins().keySet().size()
                    // + " rows in the table");
                    for (Long key : fbd.getBasins().keySet()) {
                        if (huc.equals(ALL)) {
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
                                        setFFMPRow(fbd.get(key), tData, false,
                                                cwa);
                                    } catch (Exception e) {
                                        statusHandler
                                                .handle(Priority.PROBLEM,
                                                        "Couldn't create table row"
                                                                + e);
                                    }
                                    if (virtualBasin != null) {
                                        for (Long id : ft
                                                .getVirtualGageBasinLookupIds(
                                                        siteKey, key, huc,
                                                        resource.basinTableDlg
                                                                .getRowName())) {
                                            try {
                                                setFFMPRow(
                                                        virtualBasin.get(id),
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

                        } else {
                            /*
                             * make sure at least one basin in the agg is in the
                             * CWA
                             */

                            List<Long> pfafs = ft.getAggregatePfafs(key,
                                    siteKey, huc);

                            boolean isVGB = false;
                            if (ft.checkVGBsInAggregate(key, siteKey, huc)) {
                                isVGB = true;
                            }

                            if (!pfafs.isEmpty()) {

                                FFMPBasinMetaData fmdb = ft.getBasinInDomains(
                                        siteKey, domains, pfafs);

                                if (fmdb != null) {
                                    try {
                                        setFFMPRow(fbd.get(key), tData, isVGB,
                                                null);
                                    } catch (Exception e) {
                                        statusHandler
                                                .handle(Priority.PROBLEM,
                                                        "Couldn't create table row"
                                                                + e);
                                    }
                                }
                            }
                        }
                    }
                }
                // show pfafs in aggregation
                else {
                    for (Long key : resource.getCenteredAggregatePfafs()) {

                        FFMPBasinMetaData fmdb = ft.getBasin(siteKey, key);

                        if (fmdb != null) {
                            for (DomainXML domain : domains) {

                                if ((domain.getCwa().equals(fmdb.getCwa()))
                                        || (domain.isPrimary() && fmdb
                                                .isPrimaryCwa())) {

                                    setFFMPRow(fbd.get(key), tData, false, null);

                                    if (virtualBasin != null) {
                                        for (Long id : ft
                                                .getVirtualGageBasinLookupIds(
                                                        siteKey, key, huc,
                                                        resource.basinTableDlg
                                                                .getRowName())) {
                                            try {
                                                setFFMPRow(
                                                        virtualBasin.get(id),
                                                        tData, true, null);
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
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to load FFMP table data!", e);
        }
        return tData;
    }

    private void setFFMPRow(FFMPBasin cBasin, FFMPTableData tData,
            boolean isVGB, String domain) {
        try {
            String pfafToTest = null;
            if (cBasin instanceof FFMPVirtualGageBasin) {
                pfafToTest = ((FFMPVirtualGageBasin) cBasin).getLid();
            } else {
                pfafToTest = cBasin.getPfaf().toString();
            }
            if (tData.containsPfaf(pfafToTest)) {
                return;
            }
        } catch (Exception e) {
            return;
        }

        String displayName = "";
        String mouseOverText = "";

        FFMPTableRowData trd = new FFMPTableRowData(
                ffmpTableCfgData.getTableColumnKeys().length);

        Float guidance = Float.NaN;
        Float qpe = Float.NaN;
        Float rate = Float.NaN;
        Float qpf = Float.NaN;
        FIELDS rowField = FIELDS.NAME;

        if (isVGB) {
            rowField = FIELDS.VIRTUAL;
        }

        if (cBasin instanceof FFMPVirtualGageBasin) {
            rowField = FIELDS.VIRTUAL;

            FFMPVirtualGageBasin vgBasin = (FFMPVirtualGageBasin) cBasin;

            String lid = vgBasin.getLid();

            if (lid != null) {

                StringBuilder sb = new StringBuilder(lid);
                // in this special case it is actually the LID
                trd.setPfaf(lid);
                FFMPVirtualGageBasinMetaData fvgmbd = ft
                        .getVirtualGageBasinMetaData(siteKey, lid);
                FFMPBasinMetaData metabasin = ft.getBasin(siteKey,
                        fvgmbd.getParentPfaf());
                Long parentBasinPfaf = fvgmbd.getParentPfaf();

                mouseOverText = metabasin.getBasinId() + "\n" + lid + "-"
                        + fvgmbd.getName();

                if (!huc.equals(ALL)) {
                    sb.append("-").append(fvgmbd.getName());
                }

                trd.setTableCellData(0,
                        new FFMPTableCellData(rowField, sb.toString(),
                                mouseOverText));

                if (!isWorstCase || huc.equals(ALL)
                        || (centeredAggregationKey != null)) {

                    if (!cBasin.getValues().isEmpty()) {
                        rate = vgBasin.getValue(paintRefTime);
                        if (sliderTime > 0.00) {
                            FFMPTimeWindow window = monitor.getQpeWindow();
                            qpe = cBasin.getAccumValue(window.getAfterTime(),
                                    window.getBeforeTime(), expirationTime,
                                    isRate);
                        } else {
                            qpe = 0.0f;
                        }
                    }

                    trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
                            rate));
                    trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
                            qpe));

                    if (qpfBasin != null) {
                        FFMPBasin basin = qpfBasin.get(parentBasinPfaf);
                        if (basin != null) {
                            FFMPTimeWindow window = monitor.getQpfWindow();
                            qpf = basin.getAverageValue(window.getAfterTime(),
                                    window.getBeforeTime());
                        }
                    }
                    trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
                            qpf));

                    // run over each guidance type
                    int i = 0;
                    for (String guidType : guidBasins.keySet()) {
                        guidance = Float.NaN;

                        FFFGForceUtil forceUtil = forceUtils.get(guidType);
                        forceUtil.setSliderTime(sliderTime);

                        FFMPTableCellData guidCellData = getGuidanceCellData(
                                cBasin, domain, guidType, parentBasinPfaf);
                        if (guidCellData == null) {
                            guidCellData = new FFMPTableCellData(
                                    FIELDS.GUIDANCE, Float.NaN);
                        } else {
                            guidance = guidCellData.getValueAsFloat();
                        }
                        trd.setTableCellData(i + 4, guidCellData);

                        float ratioValue = Float.NaN;
                        float diffValue = Float.NaN;
                        // If guidance is NaN then it cannot be > 0
                        if (!qpe.isNaN() && (guidance > 0.0f)) {
                            ratioValue = FFMPUtils.getRatioValue(qpe, guidance);
                            diffValue = FFMPUtils.getDiffValue(qpe, guidance);
                        }
                        trd.setTableCellData(i + 5, new FFMPTableCellData(
                                FIELDS.RATIO, ratioValue));
                        trd.setTableCellData(i + 6, new FFMPTableCellData(
                                FIELDS.DIFF, diffValue));

                        i += 3;
                    }
                } else {
                    trd = getMaxValue(trd, cBasin);
                }

                tData.addDataRow(trd);

            }
        } else {
            displayName = getDisplayName(cBasin);

            if (displayName != null) {
                long cBasinPfaf = cBasin.getPfaf();
                String cBasinPfafStr = Long.toString(cBasinPfaf);
                StringBuilder sb = new StringBuilder(cBasinPfafStr);
                sb.append("\n").append(displayName);
                trd.setPfaf(cBasinPfafStr);
                trd.setTableCellData(0, new FFMPTableCellData(rowField,
                        displayName, sb.toString()));

                if (!isWorstCase || huc.equals(ALL)
                        || (centeredAggregationKey != null)) {
                    if (rateBasin != null) {
                        FFMPBasin basin = rateBasin.get(cBasinPfaf);
                        if (basin != null) {
                            rate = basin.getValue(paintRefTime);
                        }
                    }
                    trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
                            rate));

                    if (qpeBasin != null) {
                        FFMPBasin basin = qpeBasin.get(cBasinPfaf);
                        if (basin != null) {
                            FFMPTimeWindow window = monitor.getQpeWindow();
                            qpe = basin.getAccumValue(window.getAfterTime(),
                                    window.getBeforeTime(), expirationTime,
                                    isRate);
                        }
                    }

                    trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
                            qpe));

                    if (qpfBasin != null) {
                        FFMPBasin basin = qpfBasin.get(cBasinPfaf);
                        if (basin != null) {
                            FFMPTimeWindow window = monitor.getQpfWindow();
                            qpf = basin.getAverageValue(window.getAfterTime(),
                                    window.getBeforeTime());
                        }
                    }

                    trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
                            qpf));

                    // run over each guidance type
                    int i = 0;
                    for (String guidType : guidBasins.keySet()) {
                        guidance = Float.NaN;
                        FFFGForceUtil forceUtil = forceUtils.get(guidType);
                        forceUtil.setSliderTime(sliderTime);
                        FFMPTableCellData guidCellData = getGuidanceCellData(
                                cBasin, domain, guidType, cBasinPfaf);
                        if (guidCellData == null) {
                            // check for forcing even if no data are available
                            guidance = getForcedAvg(forceUtil, domain, cBasin,
                                    guidType);
                            boolean forced = !guidance.isNaN();
                            guidCellData = new FFMPTableCellData(
                                    FIELDS.GUIDANCE, guidance, forced);
                        } else {
                            guidance = guidCellData.getValueAsFloat();
                        }

                        trd.setTableCellData(i + 4, guidCellData);

                        float ratioValue = Float.NaN;
                        float diffValue = Float.NaN;
                        // If guidance is NaN then it cannot be > 0
                        if (!qpe.isNaN() && (guidance > 0.0f)) {
                            ratioValue = FFMPUtils.getRatioValue(qpe, guidance);
                            diffValue = FFMPUtils.getDiffValue(qpe, guidance);
                        }
                        trd.setTableCellData(i + 5, new FFMPTableCellData(
                                FIELDS.RATIO, ratioValue));
                        trd.setTableCellData(i + 6, new FFMPTableCellData(
                                FIELDS.DIFF, diffValue));

                        i += 3;
                    }
                } else {
                    trd = getMaxValue(trd, cBasin);
                }

                tData.addDataRow(trd);
            }
        }
    }

    private FFMPTableCellData getGuidanceCellData(FFMPBasin cBasin,
            String domain, String guidType, Long parentBasinPfaf) {
        long cBasinPfaf = cBasin.getPfaf();

        FFMPBasinData guidBasin = guidBasins.get(guidType);

        FFMPGuidanceBasin ffmpGuidBasin = null;
        if (guidBasin != null) {
            ffmpGuidBasin = (FFMPGuidanceBasin) guidBasin.get(cBasinPfaf);
        }

        if (ffmpGuidBasin == null) {
            return null;
        }
        List<Long> pfafList = Collections.emptyList();
        List<Long> forcedPfafs = Collections.emptyList();
        boolean forced = false;
        Float guidance = Float.NaN;
        FFFGForceUtil forceUtil = forceUtils.get(guidType);

        // If aggregate, get basins within the aggregate
        if (cBasin.getAggregated()) {
            if (domain == null) {
                pfafList = ft.getAggregatePfafs(cBasinPfaf, siteKey, huc);
            } else if (!domain.equals(NA)) {
                if (!huc.equals(ALL)) {
                    pfafList = ft.getAggregatePfafsByDomain(parentBasinPfaf,
                            siteKey, domain, huc);
                }
            } else {
                pfafList = ft.getAggregatePfafsByDomain(parentBasinPfaf,
                        siteKey, domain, huc);
                pfafList.add(ft.getAggregatedPfaf(cBasinPfaf, siteKey, huc));
            }
        }

        if (FFFGDataMgr.getInstance().isForcingConfigured()) {
            FFMPBasin parentBasin = cBasin;
            if (cBasinPfaf != parentBasinPfaf.longValue()) {
                parentBasin = baseRec.getBasinData(ALL).get(parentBasinPfaf);
            }
            forceUtil.calculateForcings(domain, ft, parentBasin);
            forcedPfafs = forceUtil.getForcedPfafList();
            forced = forceUtil.isForced();
        }

        if (!forcedPfafs.isEmpty() || !pfafList.isEmpty()
                && centeredAggregationKey == null) {
            FFMPBasinData basinData = guidRecords.get(guidType).getBasinData(
                    ALL);
            guidance = basinData.getAverageGuidanceValue(pfafList, resource
                    .getGuidanceInterpolators().get(guidType), guidance,
                    forcedPfafs, resource.getGuidSourceExpiration(guidType));
            forced = !forcedPfafs.isEmpty();
        } else {
            guidance = resource.getGuidanceValue(ffmpGuidBasin, paintRefTime,
                    guidType);
            if (guidance < 0.0f) {
                guidance = Float.NaN;
            }
        }

        return new FFMPTableCellData(FIELDS.GUIDANCE, guidance, forced);
    }

    private float getForcedAvg(FFFGForceUtil forceUtil, String domain,
            FFMPBasin cBasin, String guidType) {
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        List<Long> forcedPfafs;
        List<Long> pfafList = new ArrayList<Long>();
        float guidance = Float.NaN;

        boolean forced = false;
        if (fdm.isForcingConfigured()) {
            forceUtil.calculateForcings(domain, ft, cBasin);
            forcedPfafs = forceUtil.getForcedPfafList();
            forced = forceUtil.isForced();
            if (!forced) {
                return Float.NaN;
            }
        } else {
            return Float.NaN;
        }

        if (cBasin.getAggregated()) {
            if (domain == null) {
                pfafList = ft.getAggregatePfafs(cBasin.getPfaf(), siteKey, huc);
            } else if (!domain.equals(NA)) {
                if (!huc.equals(ALL)) {
                    pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
                            siteKey, domain, huc);
                }
            } else {
                pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
                        siteKey, domain, huc);
                pfafList.add(ft.getAggregatedPfaf(cBasin.getPfaf(), siteKey,
                        huc));
            }
        }

        if (!isWorstCase || huc.equals(ALL) || (centeredAggregationKey != null)) {
            if (((forcedPfafs.size() > 1)) || forced) {
                // Calculate an average
                guidance = forceUtil.getAvgForcedValue(pfafList, forcedPfafs,
                        resource.getGuidanceInterpolators().get(guidType),
                        resource.getGuidSourceExpiration(guidType), ft);
                // } else if (forcedPfafs.size() > 1) {
                // guidance = forceUtil.getAvgForcedValue(pfafList,
                // forcedPfafs,
                // resource.getGuidanceInterpolators().get(guidType),
                // resource.getGuidSourceExpiration(), ft);
                // forced = true;
            }
        } else {
            // TODO Calculate a max value

        }

        return guidance;
    }

    /**
     * Regular basin display name
     * 
     * @param basin
     * @return
     */
    private String getDisplayName(FFMPBasin basin) {
        String name = null;

        try {
            if (huc.equals(ALL) || (centeredAggregationKey != null)) {
                name = ft.getBasin(siteKey, basin.getPfaf()).getStreamName();
            }
            // aggregations
            else {

                ArrayList<Long> pfafs = ft.getAggregatePfafs(basin.getPfaf(),
                        siteKey, huc);
                if (!pfafs.isEmpty()) {
                    if (huc.equals(FFMPRecord.COUNTY)) {
                        name = ft.getCountyStateName(siteKey, basin.getPfaf());
                    } else {
                        for (int i = 0; i < pfafs.size(); i++) {
                            if (ft.getBasin(siteKey, pfafs.get(0)).getHucName() != null) {
                                name = ft.getBasin(siteKey, pfafs.get(0))
                                        .getHucName();
                                break;
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "No display name for basin.."
                    + basin.getPfaf());
        }
        return name;
    }

    private FFMPTableRowData getMaxValue(FFMPTableRowData trd, FFMPBasin cBasin) {
        ArrayList<DomainXML> domainList = FFMPRunConfigurationManager
                .getInstance().getDomains();
        ArrayList<DomainXML> activeDomains = new ArrayList<DomainXML>();
        for (DomainXML domainXml : domainList) {
            for (String cwa : cwaArr) {
                if (domainXml.getCwa().equalsIgnoreCase(cwa)) {
                    activeDomains.add(domainXml);
                    break;
                }
            }
        }

        ArrayList<Long> pfafs = ft.getAggregatePfafs(cBasin.getPfaf(), siteKey,
                huc, activeDomains);
        trd.setPfaf(cBasin.getPfaf().toString());
        Float qpe = Float.NaN;
        Float guidance = Float.NaN;
        Float rate = Float.NaN;
        Float qpf = Float.NaN;
        Float ratioValue = Float.NaN;
        Float diffValue = Float.NaN;

        if (cBasin instanceof FFMPVirtualGageBasin) {
            if (!pfafs.isEmpty()) {
                if (virtualBasin != null) {
                    rate = virtualBasin.get(cBasin.getPfaf()).getValue(
                            paintRefTime);

                    if (sliderTime > 0.00) {
                        qpe = virtualBasin.get(cBasin.getPfaf()).getAccumValue(
                                monitor.getQpeWindow().getAfterTime(),
                                monitor.getQpeWindow().getBeforeTime(),
                                expirationTime, isRate);
                    } else {
                        qpe = 0.0f;
                    }
                }
                trd.setTableCellData(1,
                        new FFMPTableCellData(FIELDS.RATE, rate));

                trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE, qpe));

                if (qpfBasin != null) {
                    qpf = new Float(qpfBasin.get(cBasin.getPfaf()).getMaxValue(
                            monitor.getQpfWindow().getAfterTime(),
                            monitor.getQpfWindow().getBeforeTime()))
                            .floatValue();
                }

                trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF, qpf));

                processGuidance(trd, cBasin, pfafs, qpe);
            }
        } else {
            // Not Virtual
            if (!pfafs.isEmpty()) {
                if (rateBasin != null) {
                    rate = rateBasin.getMaxValue(pfafs, paintRefTime);
                }
                trd.setTableCellData(1,
                        new FFMPTableCellData(FIELDS.RATE, rate));

                if (qpeBasin != null) {
                    qpe = qpeBasin.getAccumMaxValue(pfafs, monitor
                            .getQpeWindow().getBeforeTime(), monitor
                            .getQpeWindow().getAfterTime(), expirationTime,
                            isRate);
                }
                trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE, qpe));

                if (qpfBasin != null) {
                    qpf = qpfBasin.getAverageMaxValue(pfafs, monitor
                            .getQpfWindow().getAfterTime(), monitor
                            .getQpfWindow().getBeforeTime());

                    // qpf = getQPFValue(true, new Long(0l), pfafs);/* DR13839
                    // */
                }
                trd.setTableCellData(3,
                        new FFMPTableCellData(FIELDS.QPF, qpf.floatValue()));

                // run over each guidance type
                int i = 0;
                for (String guidType : guidBasins.keySet()) {
                    guidance = Float.NaN;
                    diffValue = Float.NaN;
                    ratioValue = Float.NaN;

                    FFFGForceUtil forceUtil = forceUtils.get(guidType);
                    forceUtil.setSliderTime(sliderTime);

                    FFMPBasinData guidBasin = guidBasins.get(guidType);

                    List<Long> pfafList = new ArrayList<Long>();
                    if ((guidBasin != null)
                            && (!guidBasin.getBasins().isEmpty())) {
                        if (cBasin.getAggregated()) {
                            pfafList = ft.getAggregatePfafs(cBasin.getPfaf(),
                                    siteKey, huc);
                            pfafList.add(ft.getAggregatedPfaf(cBasin.getPfaf(),
                                    siteKey, huc));
                        }

                        boolean forced = false;
                        List<Long> forcedPfafs = new ArrayList<Long>();
                        FFFGDataMgr fdm = FFFGDataMgr.getInstance();

                        if (fdm.isForcingConfigured()) {
                            forceUtil.calculateForcings(pfafList, ft, cBasin);
                            forcedPfafs = forceUtil.getForcedPfafList();
                            forced = forceUtil.isForced();
                        }

                        if (!forced) {
                            if ((forcedPfafs != null)
                                    && (!forcedPfafs.isEmpty())) {
                                forced = true;
                            }
                        }

                        if (isWorstCase) {
                            guidance = guidRecords
                                    .get(guidType)
                                    .getBasinData(ALL)
                                    .getMaxGuidanceValue(
                                            pfafs,
                                            resource.getGuidanceInterpolators()
                                                    .get(guidType),
                                            resource.getGuidSourceExpiration(guidType),
                                            cBasin.getPfaf());
                        } else {
                            FFMPGuidanceBasin basin = (FFMPGuidanceBasin) guidRecords
                                    .get(guidType).getBasinData(huc)
                                    .get(cBasin.getPfaf());
                            guidance = resource.getGuidanceValue(basin, monitor
                                    .getQpeWindow().getBeforeTime(), guidType);
                        }

                        trd.setTableCellData(i + 4, new FFMPTableCellData(
                                FIELDS.GUIDANCE, guidance, forced));
                    } else {
                        trd.setTableCellData(i + 4, new FFMPTableCellData(
                                FIELDS.GUIDANCE, Float.NaN));
                    }

                    // If guidance is NaN then it cannot be > 0
                    if (!qpe.isNaN() && (guidance > 0.0f)) {

                        List<Float> qpes = qpeBasin.getAccumValues(pfafs,
                                monitor.getQpeWindow().getAfterTime(), monitor
                                        .getQpeWindow().getBeforeTime(),
                                expirationTime, isRate);
                        List<Float> guids = null;
                        if (guidBasin != null) {
                            guids = guidBasin.getGuidanceValues(pfafs, resource
                                    .getGuidanceInterpolators().get(guidType),
                                    resource.getGuidSourceExpiration(guidType));
                        }

                        if ((!qpes.isEmpty())
                                && ((guids != null) && (!guids.isEmpty()))) {
                            ratioValue = FFMPUtils
                                    .getMaxRatioValue(qpes, guids);
                            diffValue = FFMPUtils.getMaxDiffValue(qpes, guids);
                        }
                        trd.setTableCellData(i + 5, new FFMPTableCellData(
                                FIELDS.RATIO, ratioValue));
                        trd.setTableCellData(i + 6, new FFMPTableCellData(
                                FIELDS.DIFF, diffValue));
                    } else {
                        trd.setTableCellData(i + 5, new FFMPTableCellData(
                                FIELDS.RATIO, Float.NaN));
                        trd.setTableCellData(i + 6, new FFMPTableCellData(
                                FIELDS.DIFF, Float.NaN));
                    }

                    i += 3;
                }

            } else {
                if ((rateBasin != null)
                        && (rateBasin.get(cBasin.getPfaf()) != null)) {
                    rate = rateBasin.get(cBasin.getPfaf()).getValue(
                            paintRefTime);
                }
                trd.setTableCellData(1,
                        new FFMPTableCellData(FIELDS.RATE, rate));

                if ((qpeBasin != null)
                        && (qpeBasin.get(cBasin.getPfaf()) != null)) {
                    qpe = qpeBasin.get(cBasin.getPfaf()).getAccumValue(
                            monitor.getQpeWindow().getAfterTime(),
                            monitor.getQpeWindow().getBeforeTime(),
                            expirationTime, isRate);
                }
                trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE, qpe));

                if ((qpfBasin != null)
                        && (qpfBasin.get(cBasin.getPfaf()) != null)) {
                    qpf = new Float(qpfBasin.get(cBasin.getPfaf()).getMaxValue(
                            monitor.getQpfWindow().getAfterTime(),
                            monitor.getQpfWindow().getBeforeTime()))
                            .floatValue();
                }
                trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF, qpf));

                processGuidance(trd, cBasin, pfafs, qpe);
            }

        }

        return trd;
    }

    /**
     * @param trd
     * @param cBasin
     * @param pfafs
     * @param qpe
     */
    private void processGuidance(FFMPTableRowData trd, FFMPBasin cBasin,
            ArrayList<Long> pfafs, Float qpe) {
        Float guidance;
        Float ratioValue;
        Float diffValue;
        int i = 0;
        for (String guidType : guidBasins.keySet()) {
            guidance = Float.NaN;
            diffValue = Float.NaN;
            ratioValue = Float.NaN;

            FFFGForceUtil forceUtil = forceUtils.get(guidType);
            forceUtil.setSliderTime(sliderTime);

            FFMPBasinData guidBasin = guidBasins.get(guidType);

            if (guidBasin != null) {

                FFMPGuidanceBasin basin = ((FFMPGuidanceBasin) guidBasin
                        .get(cBasin.getPfaf()));
                guidance = resource.getGuidanceValue(basin, monitor
                        .getQpeWindow().getBeforeTime(), guidType);

                if (guidance < 0.0f) {
                    guidance = Float.NaN;
                }

                forceUtil.calculateForcings(pfafs, ft, cBasin);

                List<Long> forcedPfafs = forceUtil.getForcedPfafList();
                boolean forced = forceUtil.isForced();

                if (!forced) {
                    if ((forcedPfafs != null) && (!forcedPfafs.isEmpty())) {
                        forced = true;
                    }
                }

                trd.setTableCellData(i + 4, new FFMPTableCellData(
                        FIELDS.GUIDANCE, guidance, forced));
            } else {
                trd.setTableCellData(i + 4, new FFMPTableCellData(
                        FIELDS.GUIDANCE, Float.NaN));
            }

            // If guidance is NaN then it cannot be > 0
            if (!qpe.isNaN() && (guidance > 0.0f)) {
                ratioValue = FFMPUtils.getRatioValue(qpe, guidance);
                diffValue = FFMPUtils.getDiffValue(qpe, guidance);
            }
            trd.setTableCellData(i + 5, new FFMPTableCellData(FIELDS.RATIO,
                    ratioValue));
            trd.setTableCellData(i + 6, new FFMPTableCellData(FIELDS.DIFF,
                    diffValue));

            i += 3;
        }
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
        String localHuc = null;

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

        if (isWorstCase || (centeredAggregationKey != null)) {
            // make sure that "ALL" is loaded
            localHuc = ALL;
        } else {
            localHuc = huc;
        }

        FFMPRecord rateRecord = monitor.getRateRecord(product, siteKey,
                dataKey, product.getRate(), paintRefTime, localHuc, true);
        FFMPRecord qpeRecord = monitor.getQPERecord(product, siteKey, dataKey,
                product.getQpe(), tableTime, localHuc, true);
        FFMPRecord qpfRecord = monitor.getQPFRecord(product, siteKey, dataKey,
                null, paintRefTime, localHuc, true);
        guidRecords = monitor.getGuidanceRecords(product, siteKey, tableTime,
                localHuc, true);
        FFMPRecord virtualRecord = null;
        if (localHuc.equals(ALL)) {
            virtualRecord = monitor.getVirtualRecord(product, siteKey, dataKey,
                    product.getVirtual(), tableTime, localHuc, true);
        }

        try {
            if (rateRecord != null) {
                rateBasin = rateRecord.getBasinData(localHuc);
                if (!rateBasin.getBasins().isEmpty()) {
                    field = FIELDS.RATE;
                    baseRec = rateRecord;
                }
            }
            if (qpeRecord != null) {
                qpeBasin = qpeRecord.getBasinData(localHuc);
                if (!qpeBasin.getBasins().isEmpty()) {
                    field = FIELDS.QPE;
                    if (baseRec == null) {
                        baseRec = qpeRecord;
                    }
                }
            }
            if (qpfRecord != null) {
                qpfBasin = qpfRecord.getBasinData(localHuc);
            }
            if (guidRecords != null) {
                guidBasins = new HashMap<String, FFMPBasinData>();
                for (String type : guidRecords.keySet()) {
                    if (guidRecords.get(type) != null) {
                        guidBasins.put(type, guidRecords.get(type)
                                .getBasinData(localHuc));
                    } else {
                        guidBasins.put(type, null);
                    }
                }
            }
            if (virtualRecord != null) {
                virtualBasin = virtualRecord.getBasinData(localHuc);
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
