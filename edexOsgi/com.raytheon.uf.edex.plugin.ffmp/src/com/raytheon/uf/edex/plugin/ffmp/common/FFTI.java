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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGap;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFTIDataManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.FFTIAttributeXML;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;

/**
 * FFTI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFTI implements Runnable {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTI.class);

    private static final transient IUFStatusHandler monitorHandler = UFStatus
            .getMonitorHandler(FFTI.class);

    private FFMPConfig config = null;

    private FFTIDataManager fdm = null;

    private FFMPGenerator ffmpgen = null;

    private FFMPTemplates templates = null;

    private DecimalFormat formatter = null;

    private Priority messagePriority = Priority.INFO;

    private StringBuffer alertMessage = new StringBuffer();

    private ArrayList<FFTIAlertData> alertDataArray;

    public FFTI(FFMPGenerator ffmpgen) {
        this.ffmpgen = ffmpgen;
        this.config = ffmpgen.config;
        this.fdm = ffmpgen.config.fdm;
        this.templates = ffmpgen.template;

        this.formatter = new DecimalFormat();
        formatter.setMaximumFractionDigits(2);
        formatter.setMinimumIntegerDigits(1);

        alertDataArray = new ArrayList<FFTIAlertData>();
    }

    /**
     * After processing sources, evaluate those sources against the FFTI config.
     */
    public void evaluateFFTI() {

        alertDataArray.clear();
        boolean isAlert = processSettings();

        if (isAlert) {
            alertMessage.append(createAlertMessages());
            monitorHandler.handle(messagePriority, alertMessage.toString());
        }
        // Debug
        // statusHandler.handle(Priority.INFO, alertMessage.toString());

    }

    private boolean processSettings() {
        ArrayList<FFTISourceXML> removeSources = new ArrayList<FFTISourceXML>();
        boolean isAlert = false;

        for (FFTISettingXML setting : fdm.getSettingList()) {
            FFTIAttributeXML attribute = setting.getAttribute();

            if (attribute.getAttributeName().equals(
                    FFTIAttributeXML.ATTRIBUTE.ACCUM.getAttribute())) {
                removeSources = processAccumulation(setting);
            } else if (attribute.getAttributeName().equals(
                    FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())
                    || attribute.getAttributeName().equals(
                            FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {
                for (FFTISourceXML source : ffmpgen.getFFTISources()) {
                    for (String sourceName : source.getDisplayNameList()) {
                        if (sourceName != null) {
                            if (setting.getQpeSource().getDisplayNameList() != null) {
                                for (String dispName : setting.getQpeSource()
                                        .getDisplayNameList()) {
                                    if (sourceName.equals(dispName)) {
                                        processRatioDiff(dispName, setting
                                                .getGuidSource()
                                                .getDisplayNameList(),
                                                source.getDurationHour(),
                                                attribute, setting
                                                        .getGuidSource()
                                                        .getDurationHour());
                                        isAlert = true;
                                    }
                                }
                            }
                            if (setting.getQpfSource().getDisplayNameList() != null) {
                                for (String dispName : setting.getQpfSource()
                                        .getDisplayNameList()) {
                                    if (sourceName.equals(dispName)) {
                                        processRatioDiff(dispName, setting
                                                .getGuidSource()
                                                .getDisplayNameList(),
                                                source.getDurationHour(),
                                                attribute, setting
                                                        .getGuidSource()
                                                        .getDurationHour());
                                        isAlert = true;
                                    }
                                }
                            }
                        }
                    }

                    removeSources.add(source);
                }
            }

            // done processing settings, release processing thread
            ffmpgen.getFFTISources().removeAll(removeSources);
        }

        return isAlert;
    }

    public ArrayList<FFTISourceXML> processAccumulation(FFTISettingXML setting) {
        ArrayList<FFTISourceXML> removeSources = new ArrayList<FFTISourceXML>();
        FFTISourceXML guidSource = setting.getGuidSource();
        FFTISourceXML qpeSource = setting.getQpeSource();
        FFTISourceXML qpfSource = setting.getQpfSource();
        ArrayList<FFTIAccum> accumList = new ArrayList<FFTIAccum>();

        if (guidSource != null) {
            if (guidSource.getDisplayNameList() != null) {
                accumList.addAll(processAccumList(setting.getGuidSource()
                        .getDurationHour(), guidSource.getDisplayNameList()));
                removeSources.add(guidSource);
            }
        }

        if (qpeSource != null) {
            if (qpeSource.getDisplayNameList() != null) {
                accumList.addAll(processAccumList(setting.getQpeSource()
                        .getDurationHour(), qpeSource.getDisplayNameList()));
                removeSources.add(qpeSource);
            }
        }

        if (qpfSource != null) {
            if (qpfSource.getDisplayNameList() != null) {
                accumList.addAll(processAccumList(setting.getQpfSource()
                        .getDurationHour(), qpfSource.getDisplayNameList()));
                removeSources.add(qpfSource);
            }
        }

        processAccumAlert(setting, accumList);

        return removeSources;
    }

    private List<FFTIAccum> processAccumList(double duration,
            ArrayList<String> displayNames) {

        List<FFTIAccum> accumList = new ArrayList<FFTIAccum>();
        String fftiSourceKey = null;
        String fftiSiteKey = null;
        SourceXML source = null;
        FFTIAccum accum;
        FFTIAccum worstAccum = new FFTIAccum();
        worstAccum.setAccumulation(0.0);

        for (String displayName : displayNames) {
            String[] fftiKeys = displayName.split("-");
            // monolithic processing
            if (fftiKeys.length == 1) {

                source = ffmpgen.getSourceConfig().getSourceByDisplayName(
                        displayName);
                ArrayList<String> sites = getSites(source);
                FFTIAccum[] accums = new FFTIAccum[sites.size()];
                // process all pieces of the mosaic
                for (int i = 0; i < sites.size(); i++) {
                    accums[i] = getAccumulationForSite(displayName,
                            sites.get(i), duration);
                }

                // find the highest for the entire mosaic
                accum = new FFTIAccum();
                accum.setAccumulation(0.0);

                for (int j = 0; j < accums.length; j++) {

                    if (accum.getAccumulation() <= accums[j].getAccumulation()) {
                        accum.setAccumulation(accums[j].getAccumulation());
                    }

                    if (accum.getGap() <= accums[j].getGap()) {
                        accum.setGap(accums[j].getGap());
                    }

                    accum.setName(accums[j].getName());
                }

                accum.setUnit(source.getUnit());
                statusHandler.handle(
                        Priority.INFO,
                        "FFTI mosaic ACCUM: " + source.getSourceName() + " "
                                + accum.getAccumulation() + " gap: "
                                + accum.getGap());
                accumList.add(accum);
            } else {
                fftiSiteKey = fftiKeys[0];
                fftiSourceKey = fftiKeys[1];

                source = ffmpgen.getSourceConfig().getSourceByDisplayName(
                        fftiSourceKey);

                accum = getAccumulationForSite(fftiSourceKey, fftiSiteKey,
                        duration);

                accum.setUnit(source.getUnit());
                statusHandler.handle(Priority.INFO,
                        "FFTI ACCUM: " + source.getSourceName() + " "
                                + fftiSiteKey + " " + accum.getAccumulation()
                                + " gap: " + accum.getGap());

                accumList.add(accum);
            }
        }

        return accumList;
    }

    /**
     * ratio/diff processor
     * 
     * @param displayName
     * @param duration
     * @param attribute
     */
    public void processRatioDiff(String displayName,
            ArrayList<String> guidDisplayNames, double duration,
            FFTIAttributeXML attribute, double guidDuration) {

        String[] qKeys = displayName.split("-");
        SourceXML qSourceXML = null;

        try {
            // monolithic processing
            if (qKeys.length == 1) {

                Float val = 0.0f;
                Double gap = 0.0;
                String unit = null;

                qSourceXML = ffmpgen.getSourceConfig().getSourceByDisplayName(
                        displayName);
                unit = qSourceXML.getUnit();
                ArrayList<String> sites = getSites(qSourceXML);

                for (String site : sites) {

                    FFTIRatioDiff values = getRatioAndDiffForSite(
                            qSourceXML.getSourceName(), site,
                            guidDisplayNames.get(0), duration);

                    Float tempVal = Float.NaN;

                    if (values != null) {

                        if (attribute.getAttributeName()
                                .equals(FFTIAttributeXML.ATTRIBUTE.RATIO
                                        .getAttribute())) {

                            tempVal = FFMPUtils.getMaxRatioValue(
                                    values.getQpes(), values.getGuids());
                        } else if (attribute.getAttributeName().equals(
                                FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {

                            tempVal = FFMPUtils.getMaxDiffValue(
                                    values.getQpes(), values.getGuids());
                        }

                        if ((tempVal != Float.NaN)
                                && (values.getGap() != Double.NaN)) {
                            if (Math.abs(tempVal) > val) {
                                val = tempVal;
                                gap = values.getGap();
                            }
                        }
                    }
                }

                if ((val != Float.NaN) && (gap != Double.NaN)) {

                    statusHandler
                            .handle(Priority.INFO,
                                    "FFTI mosaic Ratio/DIFF: " + displayName
                                            + " " + guidDisplayNames.get(0)
                                            + " " + " duration: " + duration
                                            + " val: " + val);

                    processAlert(attribute, displayName, duration, val, unit,
                            gap, guidDisplayNames.get(0), guidDuration);
                }

            } else {

                Float val = 0.0f;
                Double gap = 0.0;
                String unit = null;

                String qSiteKey = qKeys[0];
                String qSourceKey = qKeys[1];

                qSourceXML = ffmpgen.fscm.getSourceByDisplayName(qSourceKey);
                unit = qSourceXML.getUnit();

                FFTIRatioDiff values = getRatioAndDiffForSite(
                        qSourceXML.getSourceName(), qSiteKey,
                        guidDisplayNames.get(0), duration);

                if (values != null) {

                    if (attribute.getAttributeName().equals(
                            FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())) {

                        val = FFMPUtils.getMaxRatioValue(values.getQpes(),
                                values.getGuids());
                    } else if (attribute.getAttributeName().equals(
                            FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {

                        val = FFMPUtils.getMaxDiffValue(values.getQpes(),
                                values.getGuids());
                    }

                    gap = values.getGap();

                    if ((!val.isNaN()) && (!gap.isNaN())) {

                        statusHandler.handle(Priority.INFO, "FFTI Ratio/DIFF: "
                                + displayName + " " + guidDisplayNames.get(0)
                                + " " + " duration: " + duration + " val: "
                                + val);

                        processAlert(attribute, displayName, duration, val,
                                unit, gap, guidDisplayNames.get(0),
                                guidDuration);
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(
                    Priority.ERROR,
                    "Failed to evaluate Ratio/Diff. "
                            + attribute.getAttributeName() + ": " + displayName
                            + "\n" + e);
            e.printStackTrace();
        }
    }

    private void processAccumAlert(FFTISettingXML setting,
            ArrayList<FFTIAccum> accumList) {
        FFTIAttributeXML attribute = setting.getAttribute();
        double value = -9999;
        String unit = null;
        String displayName = null;
        double gapVal;

        try {
            double yellow = attribute.getYellowThrshld();
            double red = attribute.getRedThrshld();

            for (FFTIAccum accum : accumList) {
                value = accum.getAccumulation();
                unit = accum.getUnit();
                displayName = accum.getName();
                gapVal = accum.getGap();

                String sourceDisplayName = null;
                if (displayName.contains("-")) {
                    sourceDisplayName = displayName.split("-")[1];
                } else {
                    sourceDisplayName = displayName;
                }

                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSourceByDisplayName(sourceDisplayName);

                String sourceType = source.getSourceType();

                // Set up the alert data object
                FFTIAlertData fad = new FFTIAlertData();

                if (sourceType
                        .equalsIgnoreCase(SOURCE_TYPE.QPE.getSourceType())) {
                    fad.addSource(SOURCE_TYPE.QPE, displayName);
                    fad.addDuration(SOURCE_TYPE.QPE, setting.getQpeSource()
                            .getDurationHour());
                } else if (sourceType.equalsIgnoreCase(SOURCE_TYPE.QPF
                        .getSourceType())) {
                    fad.addSource(SOURCE_TYPE.QPF, displayName);
                    fad.addDuration(SOURCE_TYPE.QPF, setting.getQpfSource()
                            .getDurationHour());
                }

                fad.setAttributeName(attribute.getAttributeName());
                fad.setDisplayName(displayName);
                fad.setUnit(unit);
                fad.setValue(formatter.format(value));
                fad.setGap(getGapString(gapVal));

                if ((value >= yellow) && (value < red)) {
                    fad.setPriority(String.valueOf(UFStatus.Priority.PROBLEM
                            .ordinal()));

                    if (messagePriority.ordinal() > Priority.PROBLEM.ordinal()) {
                        messagePriority = UFStatus.Priority.PROBLEM;
                    }
                } else if (value >= red) {
                    fad.setPriority(String
                            .valueOf(UFStatus.Priority.SIGNIFICANT.ordinal()));
                    if (messagePriority.ordinal() > UFStatus.Priority.SIGNIFICANT
                            .ordinal()) {
                        messagePriority = UFStatus.Priority.SIGNIFICANT;
                    }
                } else {
                    fad.setPriority(String.valueOf(UFStatus.Priority.EVENTA
                            .ordinal()));
                    if (messagePriority.ordinal() > UFStatus.Priority.EVENTA
                            .ordinal()) {
                        messagePriority = UFStatus.Priority.EVENTA;
                    }
                }

                alertDataArray.add(fad);
            }
        } catch (Exception e) {
            e.printStackTrace();
            statusHandler
                    .handle(Priority.ERROR, "failed to transmit FFTI alert. "
                            + attribute.getAttributeName() + " Value: " + value);
        }
    }

    /**
     * Process alert message for alertviz
     * 
     * @param attribute
     * @param fftiSource
     * @param value
     */
    private void processAlert(FFTIAttributeXML attribute, String displayName,
            double duration, double value, String unit, double gapVal,
            String guidDisplayName, double guidDuration) {

        try {

            double yellow = attribute.getYellowThrshld();
            double red = attribute.getRedThrshld();

            if (attribute.getAttributeName().equals(
                    FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())) {
                yellow = yellow * 100.0;
                red = red * 100.0;
                unit = "%";
            }

            if ((value >= yellow) && (value < red)) {
                addAlertData(attribute.getAttributeName(), displayName, value,
                        duration, gapVal, UFStatus.Priority.PROBLEM.ordinal(),
                        unit, guidDisplayName, guidDuration);
                if (messagePriority.ordinal() > Priority.PROBLEM.ordinal()) {
                    messagePriority = UFStatus.Priority.PROBLEM;
                }
            } else if (value >= red) {
                addAlertData(attribute.getAttributeName(), displayName, value,
                        duration, gapVal,
                        UFStatus.Priority.SIGNIFICANT.ordinal(), unit,
                        guidDisplayName, guidDuration);
                if (messagePriority.ordinal() > UFStatus.Priority.SIGNIFICANT
                        .ordinal()) {
                    messagePriority = UFStatus.Priority.SIGNIFICANT;
                }
            } else {
                addAlertData(attribute.getAttributeName(), displayName, value,
                        duration, gapVal, UFStatus.Priority.EVENTA.ordinal(),
                        unit, guidDisplayName, guidDuration);
                if (messagePriority.ordinal() > UFStatus.Priority.EVENTA
                        .ordinal()) {
                    messagePriority = UFStatus.Priority.EVENTA;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            statusHandler
                    .handle(Priority.ERROR, "failed to transmit FFTI alert. "
                            + attribute.getAttributeName() + " Value: " + value);
        }
    }

    /**
     * Format each setting output
     * 
     * @param attributeName
     * @param displayName
     * @param value
     * @param duration
     * @param gapVal
     * @param priority
     * @param unit
     */
    private void addAlertData(String attributeName, String displayName,
            double value, double duration, double gapVal, int priority,
            String unit, String guidDisplayName, double guidDuration) {
        String sourceDisplayName = null;

        if (displayName.contains("-")) {
            sourceDisplayName = displayName.split("-")[1];
        } else {
            sourceDisplayName = displayName;
        }

        SourceXML source = FFMPSourceConfigurationManager.getInstance()
                .getSourceByDisplayName(sourceDisplayName);

        String sourceType = source.getSourceType();

        FFTIAlertData fad = new FFTIAlertData();
        if (sourceType.equalsIgnoreCase(SOURCE_TYPE.QPE.getSourceType())) {
            fad.addSource(SOURCE_TYPE.QPE, displayName);
            fad.addDuration(SOURCE_TYPE.QPE, duration);
        } else if (sourceType.equalsIgnoreCase(SOURCE_TYPE.QPF.getSourceType())) {
            fad.addSource(SOURCE_TYPE.QPF, displayName);
            fad.addDuration(SOURCE_TYPE.QPF, duration);
        }

        fad.setAttributeName(attributeName);
        fad.setDisplayName(displayName);
        fad.setUnit(unit);
        fad.setValue(formatter.format(value));
        fad.setGap(getGapString(gapVal));
        fad.setPriority(new Integer(priority).toString());
        fad.addDuration(SOURCE_TYPE.GUIDANCE, guidDuration);
        fad.addSource(SOURCE_TYPE.GUIDANCE, guidDisplayName);

        alertDataArray.add(fad);
    }

    @Override
    public void run() {
        try {
            if (ffmpgen.logger.isDebugEnabled()) {
                ffmpgen.logger.debug("FFTI: evaluating sources ");
            }
            evaluateFFTI();
            if (ffmpgen.logger.isDebugEnabled()) {
                ffmpgen.logger.debug("FFTI: finsihed evaluating sources");
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "failed to evaluate FFTI. "
                    + e);
            e.printStackTrace();
        } finally {
            ffmpgen.fftiSources.clear();
            ffmpgen.fftiDone = true;
            // reset messagePriority
            messagePriority = Priority.INFO;
        }
    }

    /**
     * Create the alert string
     * 
     * @return The alert message
     */
    private String createAlertMessages() {
        String qpeSource = "";
        String qpfSource = "";
        String guidSource = "";
        String line;

        for (FFTIAlertData fad : alertDataArray) {
            if ((fad.getSource(SOURCE_TYPE.QPE) != null)
                    && (fad.getSource(SOURCE_TYPE.QPE).length() > 0)) {
                qpeSource = fad.getDuration(SOURCE_TYPE.QPE) + "hr "
                        + fad.getSource(SOURCE_TYPE.QPE);
            } else {
                qpeSource = "";
            }

            fad.addSourceDisplay(SOURCE_TYPE.QPE, qpeSource);

            if ((fad.getSource(SOURCE_TYPE.QPF) != null)
                    && (fad.getSource(SOURCE_TYPE.QPF).length() > 0)) {
                qpfSource = fad.getDuration(SOURCE_TYPE.QPF) + "hr "
                        + fad.getSource(SOURCE_TYPE.QPF);
            } else {
                qpfSource = "";
            }

            fad.addSourceDisplay(SOURCE_TYPE.QPF, qpfSource);

            if ((fad.getSource(SOURCE_TYPE.GUIDANCE) != null)
                    && (fad.getSource(SOURCE_TYPE.GUIDANCE).length() > 0)) {
                guidSource = fad.getDuration(SOURCE_TYPE.GUIDANCE) + "hr "
                        + fad.getSource(SOURCE_TYPE.GUIDANCE);
            } else {
                guidSource = "";
            }

            fad.addSourceDisplay(SOURCE_TYPE.GUIDANCE, guidSource);
        }
        int[] sourceLengths = getSourceLengths();

        StringBuilder sb = new StringBuilder();

        sb.append("FFMP update time: " + config.getDate() + "\n");
        sb.append("CWA: ");
        for (String cwa : fdm.getCwaList()) {
            sb.append(cwa + " ");
        }
        sb.append("\n");

        String lineFmt = "%-5s %-" + sourceLengths[0] + "s %-"
                + sourceLengths[1] + "s %-" + sourceLengths[2]
                + "s %6s %7s %-3s %-6s";

        // String headerFmt = typeFmt + "  " + sourceFmt + "  %5s " + unitFmt
        // + " %8s %8s %s\n";

        String headerStr = String.format(lineFmt, "Type", "QPE", "QPF",
                "GUID  ", "Value", "Unit", "Pri", "GAP");

        sb.append(headerStr + "\n");

        for (int i = 0; i < headerStr.length(); i++) {
            sb.append("-");
        }
        sb.append("\n");

        for (FFTIAlertData fad : alertDataArray) {
            line = String.format(lineFmt, fad.getAttributeName(),
                    fad.getSourceDisplay(SOURCE_TYPE.QPE),
                    fad.getSourceDisplay(SOURCE_TYPE.QPF),
                    fad.getSourceDisplay(SOURCE_TYPE.GUIDANCE), fad.getValue(),
                    fad.getUnit(), "(" + fad.getPriority() + ")", fad.getGap());
            sb.append(line + "\n");
        }

        return sb.toString();
    }

    /**
     * Gets the gap value
     * 
     * @param gap
     * @return
     */
    private String getGapString(double gap) {
        if (gap == 0.0) {
            return "NO GAP";
        } else {
            return formatter.format(gap);
        }
    }

    /**
     * Gets a list of sites this source is used in
     * 
     * @param source
     * @return
     */
    private ArrayList<String> getSites(SourceXML source) {

        ArrayList<String> sites = new ArrayList<String>();

        FFMPRunXML runner = ffmpgen.getRunConfig().getRunner(config.getCWA());

        for (ProductRunXML productRun : runner.getProducts()) {
            SourceXML primarySource = ffmpgen.getSourceConfig().getSource(
                    productRun.getProductName());
            ProductXML product = ffmpgen.getSourceConfig().getProduct(
                    primarySource.getSourceName());

            if (product.containsSource(source.getSourceName())) {
                sites.add(productRun.getProductKey());
            }
        }

        return sites;
    }

    /**
     * Get value for an individual piece of the puzzle
     * 
     * @param fftiSourceKey
     * @param fftiSiteKey
     * @param duration
     * @return
     */
    private FFTIAccum getAccumulationForSite(String fftiSourceKey,
            String fftiSiteKey, double duration) {
        SourceXML ffmpSource = ffmpgen.getSourceConfig()
                .getSourceByDisplayName(fftiSourceKey);

        FFTIAccum accumulator = null;
        long cur = config.getDate().getTime();
        long timeBack = (long) (duration * 3600 * 1000);
        Date backDate = new Date(cur - timeBack);
        long expirationTime = ffmpSource.getExpirationMinutes(fftiSiteKey) * 60 * 1000;

        FFMPDataContainer fdc = null;

        if (ffmpSource.isMosaic()) {
            fdc = ffmpgen.getFFMPDataContainer(ffmpSource.getDisplayName());
        } else {
            fdc = ffmpgen.getFFMPDataContainer(fftiSiteKey + "-"
                    + ffmpSource.getDisplayName());
        }
        // go over the list of CWAs gathering the pfaf list
        ArrayList<Long> pfafs = new ArrayList<Long>();
        ArrayList<String> cwaList = fdm.getCwaList();

        Double gap = getGap(fdc, ffmpSource, duration, fftiSiteKey);

        if (gap != Double.NaN) {

            accumulator = new FFTIAccum();
            if (ffmpSource.isMosaic()) {
                accumulator.setName(ffmpSource.getDisplayName());
            } else {
                accumulator.setName(fftiSiteKey + "-" + fftiSourceKey);
            }
            for (String cwa : cwaList) {
                for (Long key : fdc.getBasinData("ALL").getBasins().keySet()) {

                    FFMPBasinMetaData basin = templates.getBasin(key);
                    if ((basin != null) && (basin.getCwa() != null)) {
                        if (basin.getCwa().equals(cwa)) {
                            pfafs.add(key);
                        }
                    }
                }
            }

            double amount = fdc.getMaxValue(pfafs, backDate, config.getDate(),
                    expirationTime, false);
            //System.out.println("Amount "+accumulator.getName()+ " : "+amount);
            // max value for monitored area
            accumulator.setAccumulation(amount);

            accumulator.setGap(gap);
        }

        return accumulator;
    }

    private FFTIRatioDiff getRatioAndDiffForSite(String qSourceKey,
            String qSiteKey, String ffgType, double duration) {

        FFTIRatioDiff values = null;
        SourceXML ffmpQSource = ffmpgen.fscm.getSourceByDisplayName(qSourceKey);

        if (ffmpQSource == null) {
            ffmpQSource = ffmpgen.fscm.getSource(qSourceKey);
        }

        // make sure we have data
        Date ffgBackDate = new Date(config.getDate().getTime()
                - (3600 * 1000 * 24));
        String primarySource = ffmpgen.fscm.getPrimarySource(ffmpQSource);
        ProductXML product = ffmpgen.fscm.getProduct(primarySource);

        FFMPDataContainer guidContainer = ffmpgen.getFFMPDataContainer(ffgType);
        long guidSourceExpiration = 0l;

        for (SourceXML iguidSource : product.getGuidanceSourcesByType(ffgType)) {

            if (guidSourceExpiration == 0l) {
                guidSourceExpiration = iguidSource
                        .getExpirationMinutes(qSiteKey) * 60 * 1000;
            }

            if (!guidContainer.containsKey(iguidSource.getSourceName())) {

                guidContainer = FFTIProcessor.populateDataContainer(
                        guidContainer, templates, null, ffgBackDate,
                        config.getDate(), config.getCWA(), iguidSource,
                        qSiteKey);
            }
        }

        // if still nothing, punt!
        if (guidContainer.size() == 0) {

            statusHandler.handle(Priority.PROBLEM,
                    "FFTI: No guidance sources available for " + qSiteKey + " "
                            + qSourceKey + " " + " comparison.");
            return null;
        }

        FFMPDataContainer qpeContainer = null;

        if (ffmpQSource.isMosaic()) {
            qpeContainer = ffmpgen.getFFMPDataContainer(ffmpQSource
                    .getDisplayName());
        } else {
            qpeContainer = ffmpgen.getFFMPDataContainer(qSiteKey + "-"
                    + ffmpQSource.getSourceName());
        }

        // go over the list of CWAs gathering the pfaf list
        ArrayList<Long> pfafs = new ArrayList<Long>();
        ArrayList<String> cwaList = fdm.getCwaList();
        for (String cwa : cwaList) {
            for (Long key : qpeContainer.getBasinData("ALL").getBasins()
                    .keySet()) {
                FFMPBasinMetaData basin = templates.getBasin(key);
                if ((basin != null) && (basin.getCwa() != null)) {
                    if (basin.getCwa().equals(cwa)) {
                        pfafs.add(key);
                    }
                }
            }
        }

        long cur = config.getDate().getTime();
        long timeBack = (long) (duration * 3600 * 1000);
        Date backDate = new Date(cur - timeBack);
        long expirationTime = ffmpQSource.getExpirationMinutes(qSiteKey) * 60 * 1000;

        Double gap = getGap(qpeContainer, ffmpQSource, duration, qSiteKey);

        if (gap != Double.NaN) {

            ArrayList<Float> qpes = qpeContainer.getBasinData("ALL")
                    .getAccumValues(pfafs, backDate, config.getDate(),
                            expirationTime, false);

            FFMPGuidanceInterpolation interpolator = new FFMPGuidanceInterpolation(
                    ffmpgen.fscm, product, ffmpgen.frcm.getRunner(
                            config.getCWA()).getProduct(qSiteKey),
                    primarySource, ffgType, qSiteKey);
            interpolator.setInterpolationSources(duration);

            ArrayList<Float> guids = guidContainer.getBasinData("ALL")
                    .getGuidanceValues(pfafs, interpolator,
                            guidSourceExpiration);

            values = new FFTIRatioDiff(qpes, guids, gap);

        }

        return values;
    }

    /**
     * gets the gap
     * 
     * @param qpeContainer
     * @param ffmpQSource
     * @param duration
     * @return
     */
    private Double getGap(FFMPDataContainer qpeContainer,
            SourceXML ffmpQSource, double duration, String qSiteKey) {

        long cur = config.getDate().getTime();
        long timeBack = (long) (duration * 3600 * 1000);
        Date backDate = new Date(cur - timeBack);
        long expirationTime = ffmpQSource.getExpirationMinutes(qSiteKey) * 60 * 1000;
        Double gapVal = 0.0;

        if (qpeContainer.getOrderedTimes(backDate) != null) {

            gapVal = 0.0;
            ArrayList<FFMPGap> gaps = FFMPGap.getGaps(
                    qpeContainer.getOrderedTimes(backDate), expirationTime,
                    backDate, config.getDate());
            for (FFMPGap gap : gaps) {
                gapVal += gap.getGap();
            }
            gapVal = gapVal / 60;
        }

        return gapVal;
    }

    /**
     * Get the source name lengths for QPE, QPF, and Guidance.
     * 
     * @return int[] of lenghts for QPE, QPF, Guidance
     */
    private int[] getSourceLengths() {
        // QPE, QPF, GUID
        int[] sourceLengths = new int[3];
        sourceLengths[0] = 4;
        sourceLengths[1] = 4;
        sourceLengths[2] = 4;

        for (FFTIAlertData fad : alertDataArray) {
            if (fad.getSourceDisplay(SOURCE_TYPE.QPE).length() > sourceLengths[0]) {
                sourceLengths[0] = fad.getSourceDisplay(SOURCE_TYPE.QPE)
                        .length();
            }

            if (fad.getSourceDisplay(SOURCE_TYPE.QPF).length() > sourceLengths[1]) {
                sourceLengths[1] = fad.getSourceDisplay(SOURCE_TYPE.QPF)
                        .length();
            }

            if (fad.getSourceDisplay(SOURCE_TYPE.GUIDANCE).length() > sourceLengths[2]) {
                sourceLengths[2] = fad.getSourceDisplay(SOURCE_TYPE.GUIDANCE)
                        .length();
            }
        }

        return sourceLengths;
    }
}