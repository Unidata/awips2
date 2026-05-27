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
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGap;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFTIException;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;
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
 * Apr 01, 2011            dhladky     Initial creation
 * Jul 13, 2012            dhladky     Revamped to help memory
 * 02/01/13     1569       D. Hladky   Added constants
 * 02/25/13     1660       D. Hladky   Moved FFTI processing to help with mosaic memory usage
 * 01/10/14     2359       njensen     Fix exception logging
 * Jun 11, 2018 6560       njensen     Major cleanup
 * Jul 30, 2018 6720       njensen     Update for changed method names
 * Aug 06, 2018 6633       dgilling    Fix format of createAlertMessages to
 *                                     better match A1.
 * Aug 14, 2018 6720       njensen     Use simplified enums
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFTI implements Runnable {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTI.class);

    private static final IUFStatusHandler monitorHandler = UFStatus
            .getMonitorHandler(FFTI.class);

    private static final String ALERT_MESSAGE_FORMAT = "  %-6s %-20s %-20s %-20s %6s %-3s %-3s -%25s ";

    private FFMPConfig config = null;

    private FFTIDataManager fdm = null;

    private FFMPGenerator ffmpgen = null;

    private DecimalFormat formatter = null;

    private Priority messagePriority = Priority.INFO;

    private List<FFTIAlertData> alertDataArray;

    public FFTI(FFMPGenerator ffmpgen) {
        this.ffmpgen = ffmpgen;
        this.config = ffmpgen.config;
        this.fdm = ffmpgen.config.fdm;
        this.formatter = new DecimalFormat();
        formatter.setMaximumFractionDigits(2);
        formatter.setMinimumIntegerDigits(1);

        alertDataArray = new ArrayList<>();
    }

    /**
     * After processing sources, evaluate those sources against the FFTI config.
     *
     * @throws FFTIException
     */
    public void evaluateFFTI() throws FFTIException {
        alertDataArray.clear();
        boolean isAlert = processSettings();

        if (isAlert) {
            String alertMessage = createAlertMessages();
            monitorHandler.handle(messagePriority, alertMessage);
        }
    }

    private boolean processSettings() throws FFTIException {
        List<FFTISourceXML> removeSources = new ArrayList<>();
        boolean isAlert = false;

        for (FFTISettingXML setting : fdm.getSettingList()) {
            FFTIAttributeXML attribute = setting.getAttribute();

            String attributeName = attribute.getAttributeName();
            if (attributeName
                    .equals(FFTIAttributeXML.ATTRIBUTE.ACCUM.getAttribute())) {
                removeSources = processAccumulation(setting);
            } else if (attributeName
                    .equals(FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())
                    || attributeName.equals(
                            FFTIAttributeXML.ATTRIBUTE.DIFF.getAttribute())) {
                for (FFTISourceXML source : ffmpgen.getFFTISources()) {
                    for (String sourceName : source.getDisplayNameList()) {
                        if (sourceName != null) {
                            List<List<String>> displayNameLists = new ArrayList<>(
                                    2);
                            displayNameLists.add(setting.getQpeSource()
                                    .getDisplayNameList());
                            displayNameLists.add(setting.getQpfSource()
                                    .getDisplayNameList());

                            for (List<String> dispNameList : displayNameLists) {
                                if (dispNameList != null) {
                                    for (String dispName : dispNameList) {
                                        if (sourceName.equals(dispName)) {
                                            processRatioDiff(dispName, setting
                                                    .getGuidSource()
                                                    .getDisplayNameList()
                                                    .get(0),
                                                    source.getDurationHour(),
                                                    attribute,
                                                    setting.getGuidSource()
                                                            .getDurationHour());
                                            isAlert = true;
                                        }
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

    private List<FFTISourceXML> processAccumulation(FFTISettingXML setting)
            throws FFTIException {
        List<FFTISourceXML> removeSources = new ArrayList<>();
        List<FFTISourceXML> sourcesToProcess = new ArrayList<>(3);
        List<FFTIAccum> accumList = new ArrayList<>();

        sourcesToProcess.add(setting.getGuidSource());
        sourcesToProcess.add(setting.getQpeSource());
        sourcesToProcess.add(setting.getQpfSource());

        for (FFTISourceXML source : sourcesToProcess) {
            if (source != null) {
                List<String> displayNameList = source.getDisplayNameList();
                if (displayNameList != null) {
                    accumList.addAll(processAccumList(source.getDurationHour(),
                            displayNameList));
                    removeSources.add(source);
                }
            }
        }

        processAccumAlert(setting, accumList);

        return removeSources;
    }

    private List<FFTIAccum> processAccumList(double duration,
            List<String> displayNames) throws FFTIException {
        List<FFTIAccum> accumList = new ArrayList<>();
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
                source = ffmpgen.getSourceConfig()
                        .getSourceByDisplayName(displayName);
                List<String> sites = getSites(source);
                List<FFTIAccum> accums = new ArrayList<>();

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

                statusHandler.handle(Priority.INFO,
                        "FFTI mosaic ACCUM: " + source.getSourceName() + " "
                                + accum.getAccumulation() + " gap: "
                                + accum.getGap());
                accumList.add(accum);
            } else {
                fftiSiteKey = fftiKeys[0];
                fftiSourceKey = fftiKeys[1];

                source = ffmpgen.getSourceConfig()
                        .getSourceByDisplayName(fftiSourceKey);

                accum = ffmpgen.getAccumulationForSite(fftiSourceKey,
                        fftiSiteKey, fftiSiteKey, duration, source.getUnit());

                if (accum != null) {
                    accum.setUnit(source.getUnit());
                    statusHandler.handle(Priority.INFO,
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
     * @param guidDisplayName
     * @param duration
     * @param attribute
     * @param guidDuration
     */
    private void processRatioDiff(String displayName, String guidDisplayName,
            double duration, FFTIAttributeXML attribute, double guidDuration) {
        String[] qKeys = displayName.split("-");
        SourceXML qSourceXML = null;

        Float val = 0.0f;
        Double gap = 0.0;
        String unit = null;

        String qSourceKey = null;
        if (qKeys.length == 1) {
            qSourceKey = displayName;
        } else {
            qSourceKey = qKeys[1];
        }
        qSourceXML = ffmpgen.getSourceConfig()
                .getSourceByDisplayName(qSourceKey);
        unit = qSourceXML.getUnit();

        List<String> qSiteKeys = null;
        if (qKeys.length == 1) {
            qSiteKeys = getSites(qSourceXML);
        } else {
            qSiteKeys = new ArrayList<>(1);
            qSiteKeys.add(qKeys[0]);
        }

        try {
            for (String qSiteKey : qSiteKeys) {
                FFTIRatioDiff values = ffmpgen.getRatioAndDiffForSite(
                        qSourceXML, qSiteKey, guidDisplayName, duration, unit);

                Float tempVal = Float.NaN;
                if (values != null) {
                    List<Float> qpes = values.getQpes();
                    List<Float> guids = values.getGuids();

                    if (qpes != null && guids != null) {
                        String attributeName = attribute.getAttributeName();
                        if (attributeName
                                .equals(FFTIAttributeXML.ATTRIBUTE.RATIO
                                        .getAttribute())) {
                            tempVal = FFMPUtils.getMaxRatioValue(qpes,
                                    guids);
                        } else if (attributeName
                                .equals(FFTIAttributeXML.ATTRIBUTE.DIFF
                                        .getAttribute())) {
                            tempVal = FFMPUtils.getMaxDiffValue(qpes,
                                    guids);
                        }
                    }

                    if (!Float.isNaN(tempVal)
                            && (!Double.isNaN(values.getGap()))) {
                        if (Math.abs(tempVal) > val) {
                            val = tempVal;
                            gap = values.getGap();
                        }
                    }
                }
            }

            statusHandler.handle(Priority.INFO,
                    "FFTI mosaic Ratio/DIFF: " + displayName + " "
                            + guidDisplayName + " " + " duration: " + duration
                            + " val: " + val);

            processAlert(attribute, displayName, duration, val, unit, gap,
                    guidDisplayName, guidDuration);
        } catch (FFTIException e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to evaluate Ratio/Diff. "
                            + attribute.getAttributeName() + ": " + displayName,
                    e);
        }
    }

    private void processAccumAlert(FFTISettingXML setting,
            List<FFTIAccum> accumList) {
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
                            .getInstance()
                            .getSourceByDisplayName(sourceDisplayName);

                    SourceType sourceType = source.getSourceType();

                    // Set up the alert data object
                    FFTIAlertData fad = new FFTIAlertData();

                    if (sourceType == SourceType.QPE) {
                        fad.addSource(SourceType.QPE, displayName);
                        fad.addDuration(SourceType.QPE,
                                setting.getQpeSource().getDurationHour());
                    } else if (sourceType == SourceType.QPF) {
                        fad.addSource(SourceType.QPF, displayName);
                        fad.addDuration(SourceType.QPF,
                                setting.getQpfSource().getDurationHour());
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
                        fad.setPriority(String.valueOf(
                                UFStatus.Priority.SIGNIFICANT.ordinal()));
                        if (messagePriority
                                .ordinal() > UFStatus.Priority.SIGNIFICANT
                                        .ordinal()) {
                            messagePriority = UFStatus.Priority.SIGNIFICANT;
                        }
                    } else {
                        fad.setPriority(String
                                .valueOf(UFStatus.Priority.EVENTA.ordinal()));
                        if (messagePriority.ordinal() > UFStatus.Priority.EVENTA
                                .ordinal()) {
                            messagePriority = UFStatus.Priority.EVENTA;
                        }
                    }

                    alertDataArray.add(fad);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to transmit FFTI alert. "
                            + attribute.getAttributeName() + " Value: " + value,
                    e);
        }
    }

    /**
     * Process alert message for alertviz
     *
     * @param attribute
     * @param displayName
     * @param duration
     * @param value
     * @param unit
     * @param gapVal
     * @param guidDisplayName
     * @param guidDuration
     */
    private void processAlert(FFTIAttributeXML attribute, String displayName,
            double duration, double value, String unit, double gapVal,
            String guidDisplayName, double guidDuration) {
        try {
            double yellow = attribute.getYellowThrshld();
            double red = attribute.getRedThrshld();

            if (attribute.getAttributeName()
                    .equals(FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())) {
                yellow = yellow * 100.0;
                red = red * 100.0;
                unit = "%";
            }

            FFTIAlertData fad = buildAlertData(attribute.getAttributeName(),
                    displayName, value, duration, gapVal, unit, guidDisplayName,
                    guidDuration);
            if ((value >= yellow) && (value < red)) {
                fad.setPriority(
                        Integer.toString(UFStatus.Priority.PROBLEM.ordinal()));
                alertDataArray.add(fad);
                if (messagePriority.ordinal() > Priority.PROBLEM.ordinal()) {
                    messagePriority = UFStatus.Priority.PROBLEM;
                }
            } else if (value >= red) {
                fad.setPriority(Integer
                        .toString(UFStatus.Priority.SIGNIFICANT.ordinal()));
                alertDataArray.add(fad);
                if (messagePriority.ordinal() > UFStatus.Priority.SIGNIFICANT
                        .ordinal()) {
                    messagePriority = UFStatus.Priority.SIGNIFICANT;
                }
            } else {
                fad.setPriority(
                        Integer.toString(UFStatus.Priority.EVENTA.ordinal()));
                alertDataArray.add(fad);
                if (messagePriority.ordinal() > UFStatus.Priority.EVENTA
                        .ordinal()) {
                    messagePriority = UFStatus.Priority.EVENTA;
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to build FFTI alert. "
                            + attribute.getAttributeName() + " Value: " + value,
                    e);
        }
    }

    /**
     * Setup an FFTIAlertData. This method will not set priority.
     *
     * @param attributeName
     * @param displayName
     * @param value
     * @param duration
     * @param gapVal
     * @param unit
     * @param guidDisplayName
     * @param guidDuration
     */
    private FFTIAlertData buildAlertData(String attributeName,
            String displayName, double value, double duration, double gapVal,
            String unit, String guidDisplayName, double guidDuration) {
        String sourceDisplayName = null;
        if (displayName.contains("-")) {
            sourceDisplayName = displayName.split("-")[1];
        } else {
            sourceDisplayName = displayName;
        }

        SourceXML source = FFMPSourceConfigurationManager.getInstance()
                .getSourceByDisplayName(sourceDisplayName);
        SourceType sourceType = source.getSourceType();

        FFTIAlertData fad = new FFTIAlertData();
        if (sourceType == SourceType.QPE) {
            fad.addSource(SourceType.QPE, displayName);
            fad.addDuration(SourceType.QPE, duration);
        } else if (sourceType == SourceType.QPF) {
            fad.addSource(SourceType.QPF, displayName);
            fad.addDuration(SourceType.QPF, duration);
        }

        fad.setAttributeName(attributeName);
        fad.setDisplayName(displayName);
        fad.setUnit(unit);

        String displayStr = null;
        if (attributeName
                .equals(FFTIAttributeXML.ATTRIBUTE.RATIO.getAttribute())) {
            displayStr = String.valueOf(Math.round(value));
        } else {
            displayStr = String.format("%1.2f", value);
        }

        fad.setValue(displayStr);
        fad.setGap(getGapString(gapVal));
        fad.addDuration(SourceType.GUIDANCE, guidDuration);
        fad.addSource(SourceType.GUIDANCE, guidDisplayName);

        return fad;
    }

    @Override
    public void run() {
        try {
            if (ffmpgen.logger.isDebugEnabled()) {
                ffmpgen.logger.debug("FFTI: evaluating sources ");
            }
            evaluateFFTI();
            if (ffmpgen.logger.isDebugEnabled()) {
                ffmpgen.logger.debug("FFTI: finished evaluating sources");
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Failed to evaluate FFTI", e);
        } finally {
            ffmpgen.fftiSources.clear();
            ffmpgen.fftiDone = true;
            // reset messagePriority
            messagePriority = Priority.INFO;
        }
    }

    /**
     * Create the alert string.
     * <p>
     * <p>
     * Format will be similar to the following:
     * <p>
     *
     * <pre>
     *                                 FFMP monitor update time: Aug 14 18 23:53:00 GMT
     *                                               for LWX
     *                                                 ***
     * Type   QPE                  QPF                  Guid                  Value Uni Pri
     * ------ -------------------- -------------------- -------------------- ------ --- ---
     * Accum      1hr kdix                                                     0.00 in. (4) !!DATA GAP!! 0.10 hrs
     * Accum      3hr klwx             1hr QPFSCAN                             3.17 in. (1)
     * Ratio      1hr kdix                                  1hr RFCFFG            0 %   (4)
     * Diff       1hr klwx                                  1hr RFCFFG         0.15 in. (3)
     * </pre>
     *
     * @return The alert message
     */
    private String createAlertMessages() {
        StringBuilder message = new StringBuilder("FFMP monitor update time: ")
                .append(config.getDate()).append('\n');
        message.append("for ");
        message.append(String.join(" ", fdm.getCwaList()));
        message.append('\n');
        message.append("***").append('\n');
        message.append(String.format(ALERT_MESSAGE_FORMAT, "Type", "QPE", "QPF",
                "Guid", "Value", "Uni", "Pri", StringUtils.EMPTY)).append('\n');
        message.append(String.format(ALERT_MESSAGE_FORMAT,
                StringUtils.repeat('-', 6), StringUtils.repeat('-', 20),
                StringUtils.repeat('-', 20), StringUtils.repeat('-', 20),
                StringUtils.repeat('-', 6), StringUtils.repeat('-', 3),
                StringUtils.repeat('-', 3), StringUtils.EMPTY)).append('\n');

        for (FFTIAlertData fad : alertDataArray) {
            message.append(format4Monitor(fad)).append('\n');
        }

        return message.toString();
    }

    private String format4Monitor(FFTIAlertData alertData) {
        String type = StringUtils.left(alertData.getAttributeName(), 6);

        String source = alertData.getSource(SourceType.QPE);
        source = (StringUtils.isNotEmpty(source))
                ? getString4Dur(alertData.getDuration(SourceType.QPE)) + source
                : StringUtils.EMPTY;
        String qpe = StringUtils.left(source, 20);

        source = alertData.getSource(SourceType.QPF);
        source = (StringUtils.isNotEmpty(source))
                ? getString4Dur(alertData.getDuration(SourceType.QPF)) + source
                : StringUtils.EMPTY;
        String qpf = StringUtils.left(source, 20);

        source = alertData.getSource(SourceType.GUIDANCE);
        source = (StringUtils.isNotEmpty(source))
                ? getString4Dur(alertData.getDuration(SourceType.GUIDANCE))
                        + source
                : StringUtils.EMPTY;
        String guid = StringUtils.left(source, 20);

        String value = StringUtils.left(alertData.getValue(), 6);

        String units = StringUtils.EMPTY;
        switch (alertData.getAttributeName()) {
        case "Accum":
        case "Diff":
            units = "in.";
            break;
        case "Ratio":
            units = "%";
            break;
        default:
            units = StringUtils.EMPTY;
            break;
        }

        String priority = StringUtils.left("(" + alertData.getPriority() + ")",
                3);

        String gap = StringUtils.left(alertData.getGap(), 25);

        return String.format(ALERT_MESSAGE_FORMAT, type, qpe, qpf, guid, value,
                units, priority, gap);
    }

    private String getString4Dur(double fval) {
        int ival = (int) (fval * 100);

        int precision;
        if (ival % 100 == 0) {
            precision = 0;
        } else if (ival % 50 == 0) {
            precision = 1;
        } else {
            precision = 2;
        }

        NumberFormat formatter = NumberFormat.getInstance();
        formatter.setMinimumIntegerDigits(1);
        formatter.setMaximumFractionDigits(precision);

        return String.format("%8shr ", formatter.format(fval));
    }

    /**
     * Gets the gap value
     *
     * @param gap
     * @return
     */
    private String getGapString(double gap) {
        if (gap > 0) {
            return "!!DATA GAP!! " + formatter.format(gap) + " hrs";
        }

        return StringUtils.EMPTY;
    }

    /**
     * Gets a list of sites this source is used in
     *
     * @param source
     * @return
     */
    private List<String> getSites(SourceXML source) {
        List<String> sites = new ArrayList<>();

        FFMPRunXML runner = ffmpgen.getRunConfig().getRunner(config.getCWA());

        for (ProductRunXML productRun : runner.getProducts()) {
            SourceXML primarySource = ffmpgen.getSourceConfig()
                    .getSource(productRun.getProductName());
            ProductXML product = ffmpgen.getSourceConfig()
                    .getProductByPrimarySourceName(
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
     * @param curdate
     * @param duration
     * @param qSiteKey
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
}
