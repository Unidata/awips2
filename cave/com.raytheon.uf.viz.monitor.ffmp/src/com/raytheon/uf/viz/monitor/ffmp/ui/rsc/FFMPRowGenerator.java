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
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableCellData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableRowData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData;

/**
 * Generates an FFMPTableRowData for the parameters specified and adds them to
 * the table data. This class was created by separating out some of the logic of
 * FFMPDataGenerator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2013 2085       njensen     Initial creation
 * Jul 15, 2013 2184       dhladky     Remove all HUC's for storage except ALL
 * Apr 30, 2014 2060       njensen     Safety checks for null guidance
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FFMPRowGenerator implements Runnable {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPRowGenerator.class);

    private static final String NA = "NA";

    // values specific to this row
    private FFMPBasin cBasin;

    private FFMPTableData tData;

    private boolean isVGB;

    private String domain;

    // values shared between all rows
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

    protected FFMPRecord baseRec = null;

    private boolean isRate = false;

    private long expirationTime = 0l;

    private String[] cwaArr = null;

    private Map<String, FFFGForceUtil> forceUtils = null;

    private FfmpTableConfigData ffmpTableCfgData = null;

    public FFMPRowGenerator(FFMPDataGenerator parent, FFMPBasin cBasin,
            FFMPTableData tData, boolean isVGB, String domain) {
        // these are the values related to this specific row
        this.cBasin = cBasin;
        this.tData = tData;
        this.isVGB = isVGB;
        this.domain = domain;

        // these are the values shared between all rows
        this.siteKey = parent.siteKey;
        this.paintRefTime = parent.paintRefTime;
        this.centeredAggregationKey = parent.centeredAggregationKey;
        this.huc = parent.huc;
        this.sliderTime = parent.sliderTime;
        this.isWorstCase = parent.isWorstCase;
        this.ft = parent.ft;
        this.resource = parent.resource;
        this.monitor = parent.monitor;
        this.qpeBasin = parent.qpeBasin;
        this.qpfBasin = parent.qpfBasin;
        this.rateBasin = parent.rateBasin;
        this.guidBasins = parent.guidBasins;
        this.virtualBasin = parent.virtualBasin;
        this.guidRecords = parent.guidRecords;
        this.baseRec = parent.baseRec;
        this.isRate = parent.isRate;
        this.expirationTime = parent.expirationTime;
        this.cwaArr = parent.cwaArr;
        this.forceUtils = parent.forceUtils;
        this.ffmpTableCfgData = parent.ffmpTableCfgData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
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

                if (!huc.equals(FFMPRecord.ALL)) {
                    sb.append("-").append(fvgmbd.getName());
                }

                trd.setTableCellData(0,
                        new FFMPTableCellData(rowField, sb.toString(),
                                mouseOverText));

                if (huc.equals(FFMPRecord.ALL)
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

                        FFMPTableCellData guidCellData = getGuidanceCellData(
                                cBasin, domain, guidType, parentBasinPfaf);
                        if (guidCellData == null) {
                            // check for forcing even if no data are available
                            guidance = getForcedAvg(domain, cBasin, guidType);
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
                } else if (isWorstCase) {
                    trd = getMaxValue(trd, cBasin);
                } else {
                    // general Aggregate HUC processing
                    ArrayList<Long> pfafs = ft.getAggregatePfafs(
                            cBasin.getPfaf(), siteKey, domain);

                    if (!cBasin.getValues().isEmpty()) {
                        rate = vgBasin.getAverageValue(paintRefTime,
                                expirationTime);
                        if (sliderTime > 0.00) {
                            FFMPTimeWindow window = monitor.getQpeWindow();
                            qpeBasin.getAccumAverageValue(pfafs,
                                    window.getAfterTime(),
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
                        FFMPTimeWindow window = monitor.getQpfWindow();
                        qpf = qpfBasin.getAverageValue(pfafs,
                                window.getAfterTime(), window.getBeforeTime());
                    }
                    trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
                            qpf));

                    // run over each guidance type
                    int i = 0;
                    for (String guidType : guidBasins.keySet()) {
                        guidance = Float.NaN;

                        FFMPTableCellData guidCellData = getGuidanceCellData(
                                cBasin, domain, guidType, parentBasinPfaf);
                        if (guidCellData == null) {
                            // check for forcing even if no data are available
                            guidance = getForcedAvg(domain, cBasin, guidType);
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
                }

                trd.setSortCallback(tData);
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

                if (!isWorstCase || huc.equals(FFMPRecord.ALL)
                        || (centeredAggregationKey != null)) {

                    ArrayList<Long> pfafs = null;

                    if (cBasin.getAggregated()) {
                        pfafs = ft.getAggregatePfafs(cBasin.getPfaf(), siteKey,
                                huc);
                    }

                    if (rateBasin != null) {
                        if (cBasin.getAggregated()) {
                            rate = rateBasin.getAverageValue(pfafs,
                                    paintRefTime);
                        } else {
                            FFMPBasin basin = rateBasin.get(cBasinPfaf);
                            if (basin != null) {
                                rate = basin.getValue(paintRefTime);
                            }
                        }

                    }
                    trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
                            rate));

                    if (qpeBasin != null) {

                        FFMPTimeWindow window = monitor.getQpeWindow();

                        if (cBasin.getAggregated()) {
                            qpe = qpeBasin.getAccumAverageValue(pfafs,
                                    window.getAfterTime(),
                                    window.getBeforeTime(), expirationTime,
                                    isRate);
                        } else {
                            FFMPBasin basin = qpeBasin.get(cBasinPfaf);
                            if (basin != null) {
                                qpe = basin.getAccumValue(
                                        window.getAfterTime(),
                                        window.getBeforeTime(), expirationTime,
                                        isRate);
                            }
                        }
                    }

                    trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
                            qpe));

                    if (qpfBasin != null) {
                        FFMPTimeWindow window = monitor.getQpfWindow();
                        if (cBasin.getAggregated()) {
                            qpf = qpfBasin.getAverageValue(pfafs,
                                    window.getAfterTime(),
                                    window.getBeforeTime());
                        } else {
                            FFMPBasin basin = qpfBasin.get(cBasinPfaf);
                            if (basin != null) {

                                qpf = basin.getAverageValue(
                                        window.getAfterTime(),
                                        window.getBeforeTime());
                            }
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
                            guidance = getForcedAvg(domain, cBasin, guidType);
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

                trd.setSortCallback(tData);
                tData.addDataRow(trd);
            }
        }
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
            if (huc.equals(FFMPRecord.ALL) || (centeredAggregationKey != null)) {
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

                    rate = virtualBasin.getMaxValue(pfafs, paintRefTime);

                    if (sliderTime > 0.00) {
                        qpe = virtualBasin.getAccumMaxValue(pfafs, monitor
                                .getQpeWindow().getAfterTime(), monitor
                                .getQpeWindow().getBeforeTime(),
                                expirationTime, isRate);
                    } else {
                        qpe = 0.0f;
                    }
                }
                trd.setTableCellData(1,
                        new FFMPTableCellData(FIELDS.RATE, rate));

                trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE, qpe));

                if (qpfBasin != null) {
                    qpf = qpfBasin.getMaxValue(pfafs, monitor.getQpfWindow()
                            .getAfterTime(), monitor.getQpfWindow()
                            .getBeforeTime());
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
                        ForceUtilResult forceResult = forceUtil
                                .calculateForcings(pfafList, ft, cBasin);
                        forcedPfafs = forceResult.getForcedPfafList();
                        forced = forceResult.isForced();
                    }

                    if (!forced) {
                        if ((forcedPfafs != null) && (!forcedPfafs.isEmpty())) {
                            forced = true;
                        }
                    }
                    if ((guidBasin != null)
                            && (!guidBasin.getBasins().isEmpty())) {

                        if (isWorstCase) {
                            guidance = guidRecords
                                    .get(guidType)
                                    .getBasinData()
                                    .getMaxGuidanceValue(
                                            pfafs,
                                            resource.getGuidanceInterpolators()
                                                    .get(guidType),
                                            resource.getGuidSourceExpiration(guidType),
                                            cBasin.getPfaf());
                        } else {

                            guidance = guidRecords
                                    .get(guidType)
                                    .getBasinData()
                                    .getAverageGuidanceValue(
                                            pfafs,
                                            resource.getGuidanceInterpolators()
                                                    .get(guidType),
                                            resource.getGuidSourceExpiration(guidType));

                        }

                        trd.setTableCellData(i + 4, new FFMPTableCellData(
                                FIELDS.GUIDANCE, guidance, forced));
                    } else {
                        if (forced) {
                            // Recalculate guidance using the forced value(s)
                            guidance = forceUtil.getMaxForcedValue(
                                    pfafList,
                                    forcedPfafs,
                                    resource.getGuidanceInterpolators().get(
                                            guidType), resource
                                            .getGuidSourceExpiration(guidType),
                                    ft);
                        }

                        trd.setTableCellData(i + 4, new FFMPTableCellData(
                                FIELDS.GUIDANCE, guidance, forced));
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
                        } else if (forced) {
                            guids = forceUtil.getForcedGuidValues(
                                    pfafList,
                                    forcedPfafs,
                                    resource.getGuidanceInterpolators().get(
                                            guidType), resource
                                            .getGuidSourceExpiration(guidType),
                                    ft);
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

                ForceUtilResult forceResult = forceUtil.calculateForcings(
                        pfafs, ft, cBasin);

                List<Long> forcedPfafs = forceResult.getForcedPfafList();
                boolean forced = forceResult.isForced();

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

    private float getForcedAvg(String domain, FFMPBasin cBasin, String guidType) {
        FFFGForceUtil forceUtil = forceUtils.get(guidType);
        forceUtil.setSliderTime(sliderTime);
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        List<Long> forcedPfafs;
        List<Long> pfafList = new ArrayList<Long>();
        float guidance = Float.NaN;

        boolean forced = false;
        if (fdm.isForcingConfigured()) {
            ForceUtilResult forceResult = forceUtil.calculateForcings(domain,
                    ft, cBasin);
            forcedPfafs = forceResult.getForcedPfafList();
            forced = forceResult.isForced();
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
                if (!huc.equals(FFMPRecord.ALL)) {
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

        if (!isWorstCase || huc.equals(FFMPRecord.ALL)
                || (centeredAggregationKey != null)) {
            if (((forcedPfafs.size() > 1)) || forced) {
                // Calculate an average
                guidance = forceUtil.getAvgForcedValue(pfafList, forcedPfafs,
                        resource.getGuidanceInterpolators().get(guidType),
                        resource.getGuidSourceExpiration(guidType), ft);
            }
        } else {
            // TODO Calculate a max value

        }

        return guidance;
    }

    private FFMPTableCellData getGuidanceCellData(FFMPBasin cBasin,
            String domain, String guidType, Long parentBasinPfaf) {
        long cBasinPfaf = cBasin.getPfaf();
        List<Long> pfafList = Collections.emptyList();
        List<Long> forcedPfafs = Collections.emptyList();
        boolean forced = false;
        Float guidance = Float.NaN;
        FFFGForceUtil forceUtil = forceUtils.get(guidType);
        forceUtil.setSliderTime(sliderTime);

        // If aggregate, get basins within the aggregate
        if (cBasin.getAggregated()) {
            if (domain == null) {
                pfafList = ft.getAggregatePfafs(cBasinPfaf, siteKey, huc);
            } else if (!domain.equals(NA)) {
                if (!huc.equals(FFMPRecord.ALL)) {
                    pfafList = ft.getAggregatePfafsByDomain(parentBasinPfaf,
                            siteKey, domain, huc);
                }
            } else {
                pfafList = ft.getAggregatePfafsByDomain(parentBasinPfaf,
                        siteKey, domain, huc);
                pfafList.add(ft.getAggregatedPfaf(cBasinPfaf, siteKey, huc));
            }
        } else {
            pfafList = new ArrayList<Long>();
            pfafList.add(cBasinPfaf);
        }

        if (FFFGDataMgr.getInstance().isForcingConfigured()) {
            FFMPBasin parentBasin = cBasin;
            if (cBasinPfaf != parentBasinPfaf.longValue()) {
                parentBasin = baseRec.getBasinData().get(parentBasinPfaf);
            }
            ForceUtilResult forceResult = forceUtil.calculateForcings(domain,
                    ft, parentBasin);
            forcedPfafs = forceResult.getForcedPfafList();
            forced = forceResult.isForced();
        }

        FFMPRecord grec = guidRecords.get(guidType);
        if (grec != null) {
            if (!forcedPfafs.isEmpty() || forced || !pfafList.isEmpty()) {
                // Recalculate guidance using the forced value(s)
                guidance = grec.getBasinData().getAverageGuidanceValue(
                        pfafList,
                        resource.getGuidanceInterpolators().get(guidType),
                        guidance, forcedPfafs,
                        resource.getGuidSourceExpiration(guidType));
            } else {
                FFMPGuidanceBasin ffmpGuidBasin = (FFMPGuidanceBasin) grec
                        .getBasinData().get(cBasinPfaf);
                guidance = resource.getGuidanceValue(ffmpGuidBasin,
                        paintRefTime, guidType);

                if (guidance < 0.0f) {
                    guidance = Float.NaN;
                }
            }
        }

        return new FFMPTableCellData(FIELDS.GUIDANCE, guidance, forced);
    }

}
