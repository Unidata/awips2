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

import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGap;
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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;

/**
 * FFTI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2011            dhladky      Initial creation
 * July 13, 2012           dhladky      Revamped to help memory
 * 02/01/13  1569          D. Hladky    Added constants
 * 02/25/13  1660          D. Hladky    Moved FFTI processing to help with mosaic memory usage
 * 01/10/14     2359       njensen      Fix exception logging
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

    private DecimalFormat formatter = null;

    private Priority messagePriority = Priority.INFO;

    private StringBuffer alertMessage = new StringBuffer();

    private ArrayList<FFTIAlertData> alertDataArray;

    public FFTI(FFMPGenerator ffmpgen) {
        this.ffmpgen = ffmpgen;
        this.config = ffmpgen.config;
        this.fdm = ffmpgen.config.fdm;
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

            // monolithic processing for mosaic sources
            if (fftiKeys.length == 1) {

                source = ffmpgen.getSourceConfig().getSourceByDisplayName(
                        displayName);
                ArrayList<String> sites = getSites(source);
                ArrayList<FFTIAccum> accums = new ArrayList<FFTIAccum>();

                // process all pieces of the mosaic
                for (int i = 0; i < sites.size(); i++) {

                    String dataKey = sites.get(i);

                    for (int j = 0; j < sites.size(); j++) {

                        FFTIAccum faccum = ffmpgen.getAccumulationForSite(
                                displayName, sites.get(j), dataKey, duration,
                                source.getUnit());

                        if (faccum != null) {
                            accums.add(faccum);
                        }
                    }
                }

                // find the highest for the entire mosaic
                accum = new FFTIAccum();
                accum.setAccumulation(0.0);

                for (FFTIAccum faccum : accums) {

                    if (accum.getAccumulation() <= faccum.getAccumulation()) {
                        accum.setAccumulation(faccum.getAccumulation());
                    }

                    if (accum.getGap() <= faccum.getGap()) {
                        accum.setGap(faccum.getGap());
                    }

                    accum.setName(faccum.getName());
                    accum.setUnit(faccum.getUnit());
                }

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

                accum = ffmpgen.getAccumulationForSite(fftiSourceKey,
                        fftiSiteKey, fftiSiteKey, duration, source.getUnit());

                if (accum != null) {
                    accum.setUnit(source.getUnit());
                    statusHandler.handle(
                            Priority.INFO,
                            "FFTI ACCUM: " + source.getSourceName() + " "
                                    + fftiSiteKey + " "
                                    + accum.getAccumulation() + " gap: "
                                    + accum.getGap());

                    accumList.add(accum);
                }
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

                    FFTIRatioDiff values = ffmpgen.getRatioAndDiffForSite(
                            qSourceXML.getSourceName(), site,
                            guidDisplayNames.get(0), duration, unit);

                    Float tempVal = Float.NaN;

                    if (values != null) {

                        if (attribute.getAttributeName()
                                .equals(FFTIAttributeXML.ATTRIBUTE.RATIO
                                        .getAttribute())) {

                            if (values.getGuids() != null
                                    && values.getQpes() != null) {
                                tempVal = FFMPUtils.getMaxRatioValue(
                                        values.getQpes(), values.getGuids());
                            }
                        } else if (attribute.getAttributeName().equals(
                                FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {

                            if (values.getGuids() != null
                                    && values.getQpes() != null) {
                                tempVal = FFMPUtils.getMaxDiffValue(
                                        values.getQpes(), values.getGuids());
                            }
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

                FFTIRatioDiff values = ffmpgen.getRatioAndDiffForSite(
                        qSourceXML.getSourceName(), qSiteKey,
                        guidDisplayNames.get(0), duration, unit);

                if (values != null) {

                    if (attribute.getAttributeName().equals(
                            FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())) {

                        if (values.getGuids() != null
                                && values.getQpes() != null) {
                            val = FFMPUtils.getMaxRatioValue(values.getQpes(),
                                    values.getGuids());
                        }

                    } else if (attribute.getAttributeName().equals(
                            FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {

                        if (values.getGuids() != null
                                && values.getQpes() != null) {
                            val = FFMPUtils.getMaxDiffValue(values.getQpes(),
                                    values.getGuids());
                        }
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
            statusHandler
                    .handle(Priority.ERROR,
                            "Failed to evaluate Ratio/Diff. "
                                    + attribute.getAttributeName() + ": "
                                    + displayName, e);
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
                if (accum != null && accum.getName() != null) {
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

                    SourceXML source = FFMPSourceConfigurationManager
                            .getInstance().getSourceByDisplayName(
                                    sourceDisplayName);

                    String sourceType = source.getSourceType();

                    // Set up the alert data object
                    FFTIAlertData fad = new FFTIAlertData();

                    if (sourceType.equalsIgnoreCase(SOURCE_TYPE.QPE
                            .getSourceType())) {
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
                        fad.setPriority(String
                                .valueOf(UFStatus.Priority.PROBLEM.ordinal()));

                        if (messagePriority.ordinal() > Priority.PROBLEM
                                .ordinal()) {
                            messagePriority = UFStatus.Priority.PROBLEM;
                        }
                    } else if (value >= red) {
                        fad.setPriority(String
                                .valueOf(UFStatus.Priority.SIGNIFICANT
                                        .ordinal()));
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
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "failed to transmit FFTI alert. "
                                    + attribute.getAttributeName() + " Value: "
                                    + value, e);
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
            statusHandler
                    .handle(Priority.ERROR,
                            "failed to transmit FFTI alert. "
                                    + attribute.getAttributeName() + " Value: "
                                    + value, e);
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

        String displayStr = null;
        if (attributeName.equals(FFTIAttributeXML.ATTRIBUTE.RATIO
                .getAttribute())) {
            displayStr = String.valueOf(Math.round(value));
        } else {
            displayStr = String.format("%1.2f", value);
        }

        fad.setValue(displayStr);
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
            statusHandler
                    .handle(Priority.ERROR, "failed to evaluate FFTI. ", e);
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
     * gets the gap
     * 
     * @param qpeContainer
     * @param ffmpQSource
     * @param duration
     * @return
     */
    public static Double getGap(FFMPDataContainer qpeContainer,
            SourceXML ffmpQSource, Date curdate, double duration,
            String qSiteKey) {

        long timeBack = (long) (duration * TimeUtil.MILLIS_PER_HOUR);
        Date backDate = new Date(curdate.getTime() - timeBack);
        long expirationTime = ffmpQSource.getExpirationMinutes(qSiteKey);
        Double gapVal = 0.0;

        if (qpeContainer.getOrderedTimes(backDate) != null) {

            gapVal = 0.0;
            List<FFMPGap> gaps = FFMPGap.getGaps(
                    qpeContainer.getOrderedTimes(backDate), expirationTime,
                    backDate, curdate);
            for (FFMPGap gap : gaps) {
                gapVal += gap.getGap();
            }

            gapVal = gapVal / TimeUtil.MINUTES_PER_HOUR;
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
