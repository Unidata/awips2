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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.AlertalarmvalId;
import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.util.QualityCodeUtil;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.edex.plugin.mpe.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.conversion.RocCheckerInterceptor;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSObservationDbDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.AlertalarmvalDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.LocdatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.rocchecker.RocCheckerConfig.ROC_QUALITY;

/**
 * Java implementation of roc_checker. RocChecker will be triggered by the alarm
 * whfs cron (for now) because it was originally executed by the overall
 * run_alarm_whfs script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2016 5590       bkowal      Initial creation
 * Jun 24, 2016 5699       bkowal      Implemented data retrieval and ROC verification.
 * Jun 29, 2016 5699       bkowal      Implemented alert/alarm creation and obs quality code updates.
 * Jul 12, 2016 5669       bkowal      Relocated Roc Checker Config to a less generic location.
 * Nov 10, 2016 5999       bkowal      Retrieve partial data limits records using: getObsLimits.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RocChecker {

    private static enum RocStatus {
        ROC_OKAY, ROC_ALERT, ROC_BAD, ROC_ALARM
    }

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static Map<String, ObservationTable> lookupObservationMap;

    private final JAXBManager jaxbManager;

    private static final String CONFIG_FILE = "mpeProc"
            + IPathManager.SEPARATOR + "RocChecker.xml";

    private static final String DATA_LIMIT_DATE_FORMAT = "MM-dd";

    private static final String ALARM_LOG_DATE_FORMAT = "YYYY-MM-dd HH:mm:ss";

    private static final ThreadLocal<SimpleDateFormat> dataLimitDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATA_LIMIT_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private static final ThreadLocal<SimpleDateFormat> alarmLogDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(ALARM_LOG_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private static final String LOG_QC_ALARM_VAL_FORMAT = "  %s [%+.2f = (%.2f - %.2f) / %d mins]";

    private static final String ROC_LT_ZERO_FORMAT = "roc < 0.0"
            + LOG_QC_ALARM_VAL_FORMAT;

    private static final String ROC_GTR_QC_LOG_FORMAT = "roc >= qc"
            + LOG_QC_ALARM_VAL_FORMAT;

    private static final String ROC_GTR_ALARM_LOG_FORMAT = "roc >= alarm"
            + LOG_QC_ALARM_VAL_FORMAT;

    private static final String ROC_GTR_ALERT_LOG_FORMAT = "roc >= alert"
            + LOG_QC_ALARM_VAL_FORMAT;

    private final Object configLock = new Object();

    private RocCheckerRunConfiguration rocCheckerRunConfiguration;

    public RocChecker() {
        lookupObservationMap = new HashMap<>(ObservationTable.values().length,
                1.0f);
        for (ObservationTable observation : ObservationTable.values()) {
            lookupObservationMap.put(observation.name(), observation);
        }
        try {
            jaxbManager = new JAXBManager(RocCheckerRunConfiguration.class);
        } catch (JAXBException e) {
            throw new RuntimeException(
                    "Failed to instantiate the JAXB Manager.", e);
        }
    }

    public void execute() {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        logger.info("RocChecker has started ...");
        try {
            RocCheckerInputs inputs = gatherInputs();
            if (inputs.generationPossible()) {
                produceOutputs(inputs);
            }
        } catch (Exception e) {
            timer.stop();
            logger.info("RocChecker has failed to finished in "
                    + TimeUtil.prettyDuration(timer.getElapsedTime()) + ".", e);
            return;
        }
        timer.stop();
        logger.info("RocChecker has successfully finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    private RocCheckerInputs gatherInputs() throws Exception {
        synchronized (configLock) {
            if (rocCheckerRunConfiguration == null) {
                readRocCheckerConfig();
            }

            if (rocCheckerRunConfiguration.isEmptyConfig()) {
                logger.info(
                        "No observation tables have been specified in Roc Checker configuration: {}. Exiting.",
                        CONFIG_FILE);
                return new RocCheckerInputs();
            }

            List<RocCheckerObservationData> observationDataList = new ArrayList<>();
            for (RocCheckerConfig config : rocCheckerRunConfiguration
                    .getConfigs()) {
                if (!validateConfig(config)) {
                    continue;
                }
                RocCheckerObservationData rocCheckerObservationData = retrieveObservations(
                        config);
                if (!rocCheckerObservationData.observationsFound()) {
                    continue;
                }
                observationDataList.add(rocCheckerObservationData);
            }

            if (observationDataList.isEmpty()) {
                /*
                 * No observations were found at all for the current run.
                 */
                return new RocCheckerInputs();
            }

            RocCheckerInputs inputs = new RocCheckerInputs();
            inputs.setObservationData(observationDataList);

            /*
             * Retrieve the data limits provided that {@link Observation}s were
             * found. After analysis, it was determined that for now retrieving
             * all available records is the best solution. It would be possible
             * to retrieve certain data limit records based on the lid and pe
             * values associated with the observation; but, most of the time the
             * subset of data limit records retrieved would either be exactly
             * equal to or close to equal to the full set. Additionally, all
             * records will also be retrieved every run rather than once during
             * initialization and then again as updates are made because there
             * currently is not a way to listen for updates to the data limits
             * records.
             */
            inputs.setDataLimitLookupMap(retrieveDataLimits());
            inputs.setLocationDataLimitLookupMap(retrieveLocDataLimits());

            return inputs;
        }
    }

    private void produceOutputs(RocCheckerInputs inputs) throws Exception {
        for (RocCheckerObservationData obsData : inputs.getObservationData()) {
            logger.info(
                    "Performing Rate of Change check for: {} on {} observation(s) ...",
                    obsData.getConfig().toString(),
                    obsData.getTotalObservationCount());
            for (ObservationKey key : obsData.getObservationLookupMap()
                    .keySet()) {
                List<Observation> obsList = obsData.getObservationLookupMap()
                        .get(key);
                logger.info(
                        "Analyzing {} observation(s) with attributes: {} ...",
                        obsList.size(), key.toString());
                ObsLimits obsLimits = lookupDataLimits(
                        inputs.getLocationDataLimitLookupMap(),
                        inputs.getDataLimitLookupMap(),
                        obsList.iterator().next());
                /*
                 * Only actually check the data if at least one data limit has
                 * been defined.
                 */
                final String lid = key.getLid();
                final String pe = key.getPe();
                if (obsLimits == null || !obsLimits.oneLimitDefined()) {
                    logger.info(
                            "{}: {} - Skipping ROC Check of observation(s) because no data limit(s) have been defined.",
                            lid, pe);
                } else {
                    logger.info("{}: {} - Found data limit: {}.", lid, pe,
                            obsLimits.toString());
                    rocCheck(obsList, obsLimits, lid, pe, obsData.getConfig());
                }
            }
        }
    }

    /**
     * Determines which data limits (if any) should apply to the specified
     * {@link Observation}.
     * 
     * @param locationDataLimitLookupMap
     *            a {@link Map} of available default data limits
     * @param dataLimitLookupMap
     *            a {@link Map} of available location-specific data limits
     * @param observation
     *            the specified {@link Observation}
     * @return the data limits that were found in the form of an
     *         {@link ObsLimits}; {code null} if not data limits could be found
     * @throws Exception
     */
    private ObsLimits lookupDataLimits(
            final Map<LocDataLimitKey, List<Locdatalimits>> locationDataLimitLookupMap,
            final Map<DataLimitKey, List<Datalimits>> dataLimitLookupMap,
            final Observation observation) throws Exception {
        /*
         * First check for location-specific data limits.
         */
        LocDataLimitKey locDataLimitKey = new LocDataLimitKey(
                observation.getLid(), observation.getPe(),
                observation.getDur());
        List<Locdatalimits> locDataLimitList = locationDataLimitLookupMap
                .get(locDataLimitKey);
        if (locDataLimitList != null) {
            /*
             * Determine if the current observation is within the time range of
             * a location-based data limit.
             */
            for (Locdatalimits locdatalimits : locDataLimitList) {
                if (withinDataLimitTimeRange(observation.getObstime(),
                        locdatalimits.getId().getMonthdaystart(),
                        locdatalimits.getMonthdayend())) {
                    return new ObsLimits(locdatalimits);
                }
            }
        }

        /*
         * When no location-specific data limits are found, check the default
         * data limits.
         */
        DataLimitKey dataLimitKey = new DataLimitKey(observation.getPe(),
                observation.getDur());
        List<Datalimits> dataLimitList = dataLimitLookupMap.get(dataLimitKey);
        if (dataLimitList != null) {
            /*
             * Determine if the current observation is within the time range of
             * a data limit.
             */
            for (Datalimits datalimits : dataLimitList) {
                if (withinDataLimitTimeRange(observation.getObstime(),
                        datalimits.getId().getMonthdaystart(),
                        datalimits.getMonthdayend())) {
                    return new ObsLimits(datalimits);
                }
            }
        }

        return null;
    }

    /**
     * Performs the Rate of Change verification for the specified
     * {@link Observation}s.
     * 
     * @param obsList
     *            the specified {@link Observation}s as a {@link List}
     * @param obsLimits
     *            the data limits to use in the rate of change verification
     * @param lid
     *            the lid associated with the specified observations
     * @param pe
     *            the pe associated with the specified observations
     * @param config
     *            the configuration that has triggered this Rate of Change
     *            verification
     */
    private void rocCheck(final List<Observation> obsList,
            final ObsLimits obsLimits, final String lid, final String pe,
            final RocCheckerConfig config) {
        Double previousValue = null;
        Calendar previousObsTime = null;
        RocStatus rocStatus = RocStatus.ROC_OKAY;
        boolean passed = true;

        RocCheckerInterceptor rocCheckerInterceptor = new RocCheckerInterceptor();
        for (Observation obs : obsList) {
            /*
             * Do not use the missing values or extremum values.
             */
            final boolean useValue = ShefConstants.Z.equals(obs.getExtremum())
                    && obs.getValue() != null;
            if (useValue && previousValue != null) {
                /*
                 * Same date/time?
                 */
                if (previousObsTime.getTime().equals(obs.getObstime())) {
                    /*
                     * Determine if the values are equal.
                     */
                    if (!previousValue.equals(obs.getValue())) {
                        /*
                         * The values are not equivalent despite the fact that
                         * the readings are for the same date/time.
                         */
                        passed = false;
                        logger.warn(
                                "{}: {} - Two values with the same date/time: {} are not equal - previous = {}; current = {}.",
                                lid, pe, previousObsTime.getTime().toString(),
                                previousValue, obs.getValue());
                    }
                } else {
                    /*
                     * Calculate the Rate of Change.
                     */
                    /*
                     * Currently assuming that the observations will be in
                     * ascending order by lid, pe, and date/time. Provided that
                     * is the case, the elapsed time should never be a negative
                     * value.
                     */
                    final long elapsedTimeSeconds = (obs.getObstime().getTime()
                            - previousObsTime.getTimeInMillis())
                            / TimeUtil.MILLIS_PER_SECOND;
                    /*
                     * numberOfMinutes purely exists for logging purposes in the
                     * case that an alarm is triggered.
                     */
                    final int numberOfMinutes = (int) (elapsedTimeSeconds
                            / TimeUtil.SECONDS_PER_MINUTE);
                    final double valueChange = obs.getValue().doubleValue()
                            - previousValue.doubleValue();

                    /*
                     * Calculate the rate of change in units/hour.
                     */
                    final double rateOfChange = (valueChange
                            / (double) elapsedTimeSeconds)
                            * TimeUtil.SECONDS_PER_HOUR;

                    /*
                     * Reset the ROC Status.
                     */
                    rocStatus = RocStatus.ROC_OKAY;

                    if (isROCExceedMax(obsLimits.getThresholdROCMax(),
                            rateOfChange, obs.getPe(), lid, obs.getObstime(),
                            numberOfMinutes, obs.getValue(), previousValue)) {
                        passed = false;
                        rocStatus = RocStatus.ROC_BAD;

                        /*
                         * if the QC_ROC check reflects failure yet the database
                         * reflects success, set the subject quality control
                         * code in the database to failure.
                         */
                        if (obs.getQualityCode() != null && QualityCodeUtil
                                .checkQcCode(QualityCodeUtil.QC_ROC_PASSED,
                                        obs.getQualityCode())) {
                            /*
                             * Update the quality code to failure.
                             */
                            final long updatedQualityCode = QualityCodeUtil
                                    .setQcCode(QualityCodeUtil.QC_ROC_FAILED,
                                            obs.getQualityCode());
                            AbstractIHFSObservationDbDao<?, ?> dao = getObsDao(
                                    config.getTable());
                            if (dao == null) {
                                /*
                                 * There should never be a possibility of dao
                                 * being NULL at this point due to previous
                                 * validation that should have been completed.
                                 */
                                throw new IllegalStateException(
                                        "Failed to find a dao for table: "
                                                + config.getTable()
                                                + ". The associated configuration should have never made it past the validation phase.");
                            }
                            if (!AppsDefaultsConversionWrapper
                                    .parallelExecEnabled()) {
                                Object id = null;
                                try {
                                    id = dao.updateObservationQualityCode(obs,
                                            updatedQualityCode);
                                } catch (Exception e) {
                                    logger.error(
                                            "Failed to update the quality code for Observation: "
                                                    + obs.toString()
                                                    + " associated with Table: "
                                                    + config.getTable() + ".",
                                            e);
                                }
                                if (id == null) {
                                    logger.error(
                                            "Unable to update the quality code for Observation: {} associated with Table: {}. Failed to find an associated record.",
                                            obs.toString(), config.getTable());
                                } else {
                                    logger.info(
                                            "Successfully update quality code for: {}. Update: {} -> {}.",
                                            id.toString(), obs.getQualityCode(),
                                            updatedQualityCode);
                                    new RocCheckerInterceptor().intercept(obs,
                                            updatedQualityCode,
                                            config.getTable());
                                }
                            } else {
                                rocCheckerInterceptor.intercept(obs,
                                        updatedQualityCode, config.getTable());
                            }
                        }
                    } else {
                        /*
                         * QC_ROC check passed or was not performed. Check
                         * against Alarm and possibly the Alert roc_limits.
                         */
                        if (isROCAlarm(obsLimits.getThresholdAlarmROCLimit(),
                                rateOfChange, lid, obs.getObstime(),
                                numberOfMinutes, obs.getValue(),
                                previousValue)) {
                            passed = false;
                            rocStatus = RocStatus.ROC_ALARM;

                            createAlertOrAlarm(obs, rateOfChange, rocStatus);
                            return;
                        }

                        if (rocStatus == RocStatus.ROC_OKAY && isROCAlert(
                                obsLimits.getThresholdAlertROCLimit(),
                                rateOfChange, lid, obs.getObstime(),
                                numberOfMinutes, obs.getValue(),
                                previousValue)) {
                            passed = false;
                            rocStatus = RocStatus.ROC_ALERT;

                            createAlertOrAlarm(obs, rateOfChange, rocStatus);
                        }
                    }
                }
            }

            if (rocStatus != RocStatus.ROC_BAD) {
                if (useValue) {
                    previousValue = obs.getValue();
                    previousObsTime = TimeUtil.newGmtCalendar(obs.getObstime());
                }
            }
        }

        if (passed) {
            /*
             * All rate of changes were deemed to be valid and within the
             * expected range.
             */
            logger.info("{}: {} - PASSED.", lid, pe);
        }
    }

    private void createAlertOrAlarm(final Observation observation,
            final double rateOfChange, final RocStatus rocStatus) {
        boolean saveToDisk = AppsDefaultsConversionWrapper
                .parallelExecEnabled();

        AlertalarmvalDao dao = new AlertalarmvalDao();
        AlertalarmvalId id = new AlertalarmvalId();
        id.setLid(observation.getLid());
        id.setPe(observation.getPe());
        id.setDur((short) observation.getDur());
        id.setTs(observation.getTs());
        id.setExtremum(observation.getExtremum());
        id.setProbability((float) MpeConstants.MISSING_VALUE);
        id.setValidtime(observation.getObstime());
        id.setBasistime(TimeUtil.newEpochCalendar().getTime());
        if (rocStatus == RocStatus.ROC_ALARM) {
            id.setAaCateg(RocCheckerConstants.ROC_AA_CATEG_ALARM);
        } else if (rocStatus == RocStatus.ROC_ALERT) {
            id.setAaCateg(RocCheckerConstants.ROC_AA_CATEG_ALERT);
        } else {
            id.setAaCateg(RocCheckerConstants.ROC_AA_CATEG_UNDEF);
        }
        id.setAaCheck(RocCheckerConstants.ROC_AA_CHECK);
        /*
         * Determine if any records already exist based on id.
         */
        Alertalarmval duplicate = dao.retrieveById(id);
        if (duplicate != null && !saveToDisk) {
            /*
             * A record already exists for this alert/alarm. TODO: remove
             * override save flag from condition. When save to disk is set that
             * indicates that another process is completing the same steps in
             * parallel; so, there is a high probability there will be duplicate
             * alert/alarms. So, we want to ensure that they will still be
             * intercepted.
             */
            return;
        }

        Alertalarmval alertalarmval = new Alertalarmval(id);
        alertalarmval.setValue(observation.getValue());
        alertalarmval.setSupplValue(rateOfChange);
        alertalarmval.setShefQualCode(observation.getShefQualCode());
        alertalarmval.setQualityCode(observation.getQualityCode());
        alertalarmval.setRevision((short) observation.getRevision());
        alertalarmval.setProductId(observation.getProductId());
        alertalarmval.setProducttime(observation.getProducttime());
        alertalarmval.setPostingtime(TimeUtil.newDate());

        if (!saveToDisk) {
            /*
             * Leave action time set to null here.
             */
            try {
                dao.persist(alertalarmval);
                logger.info("Successfully created Alertalarmval with id: {}.",
                        id.toString());
            } catch (Exception e) {
                logger.error("Failed to create Alertalarmval with id: "
                        + id.toString() + ".", e);
            }
        } else {
            new RocCheckerInterceptor().intercept(alertalarmval);
        }
    }

    /**
     * Determines if the calculated Rate of Change exceeds the ROC Max threshold
     * (when specified). A warning will be logged if the calculated Rate of
     * Change is found to be outside the ROC Max threshold. Also includes a
     * special case that ensures that the calculated Rate of Change is positive
     * when the pe is 'PC'.
     * 
     * @param thresholdROCMax
     *            the ROC max threshold that has been set for the current lid
     *            and pe
     * @param rateOfChange
     *            the calculated rate of change
     * @param pe
     *            the pe that the Rate of Change has been calculated for
     * @param lid
     *            the lid that the Rate of Change has been calculated for
     * @param obsTime
     *            the date/time of the observation
     * @param numberOfMinutes
     *            only exists for logging purposes
     * @param currentValue
     *            the value associated with the current observation
     * @param previousValue
     *            the value associated with the previous observation
     * @return {@code true} if the calculated rate of change exceeds the
     *         threshold; {@code false}, otherwise.
     */
    private boolean isROCExceedMax(final Double thresholdROCMax,
            final double rateOfChange, final String pe, final String lid,
            final Date obsTime, final int numberOfMinutes,
            final double currentValue, final double previousValue) {
        if (thresholdROCMax == null) {
            return false;
        }

        if ((Math.abs(rateOfChange)
                * ObsLimits.LIMIT_MULTIPLIER) >= (thresholdROCMax.doubleValue()
                        * ObsLimits.LIMIT_MULTIPLIER)) {
            logger.warn("{}: {}", lid,
                    String.format(ROC_GTR_QC_LOG_FORMAT,
                            alarmLogDF.get().format(obsTime), rateOfChange,
                            currentValue, previousValue, numberOfMinutes));
            return true;
        }

        if (CommonHydroConstants.PC.equals(pe) && rateOfChange <= 0) {
            logger.warn("{}: {}", lid,
                    String.format(ROC_LT_ZERO_FORMAT,
                            alarmLogDF.get().format(obsTime), rateOfChange,
                            currentValue, previousValue, numberOfMinutes));
            return true;
        }

        return false;
    }

    /**
     * Determines if the calculated Rate of Change exceeds the ROC Alarm Limit
     * threshold (when specified). A warning will be logged if the calculated
     * Rate of Change is found to be outside the ROC Alarm Limit threshold.
     * 
     * @param thresholdAlarmROCLimit
     *            the ROC Alarm Limit threshold that has been set for the
     *            current lid and pe
     * @param rateOfChange
     *            the calculated rate of change
     * @param lid
     *            the lid that the Rate of Change has been calculated for
     * @param obsTime
     *            the date/time of the observation
     * @param numberOfMinutes
     *            only exists for logging purposes
     * @param currentValue
     *            the value associated with the current observation
     * @param previousValue
     *            the value associated with the previous observation
     * @return {@code true} if the calculated rate of change exceeds the
     *         threshold; {@code false}, otherwise.
     */
    private boolean isROCAlarm(final Double thresholdAlarmROCLimit,
            final double rateOfChange, final String lid, final Date obsTime,
            final int numberOfMinutes, final double currentValue,
            final double previousValue) {
        if (thresholdAlarmROCLimit == null) {
            return false;
        }

        if ((Math.abs(rateOfChange)
                * ObsLimits.LIMIT_MULTIPLIER) >= (thresholdAlarmROCLimit
                        .doubleValue() * ObsLimits.LIMIT_MULTIPLIER)) {
            logger.warn("{}: {}", lid,
                    String.format(ROC_GTR_ALARM_LOG_FORMAT,
                            alarmLogDF.get().format(obsTime), rateOfChange,
                            currentValue, previousValue, numberOfMinutes));
            return true;
        }

        return false;
    }

    /**
     * Determines if the calculated Rate of Change exceeds the ROC Alert Limit
     * threshold (when specified). A warning will be logged if the calculated
     * Rate of Change is found to be outside the ROC Alarm Limit threshold.
     * 
     * @param thresholdAlarmROCLimit
     *            the ROC Alarm Limit threshold that has been set for the
     *            current lid and pe
     * @param rateOfChange
     *            the calculated rate of change
     * @param lid
     *            the lid that the Rate of Change has been calculated for
     * @param obsTime
     *            the date/time of the observation
     * @param numberOfMinutes
     *            only exists for logging purposes
     * @param currentValue
     *            the value associated with the current observation
     * @param previousValue
     *            the value associated with the previous observation
     * @return {@code true} if the calculated rate of change exceeds the
     *         threshold; {@code false}, otherwise.
     */
    private boolean isROCAlert(final Double thresholdAlertROCLimit,
            final double rateOfChange, final String lid, final Date obsTime,
            final int numberOfMinutes, final double currentValue,
            final double previousValue) {
        if (thresholdAlertROCLimit == null) {
            return false;
        }

        if ((Math.abs(rateOfChange)
                * ObsLimits.LIMIT_MULTIPLIER) >= (thresholdAlertROCLimit
                        .doubleValue() * ObsLimits.LIMIT_MULTIPLIER)) {
            logger.warn("{}: {}", lid,
                    String.format(ROC_GTR_ALERT_LOG_FORMAT,
                            alarmLogDF.get().format(obsTime), rateOfChange,
                            currentValue, previousValue, numberOfMinutes));
            return true;
        }

        return false;
    }

    /**
     * Determines if the specified observation date/time is within the data
     * limits formatted (MM-dd) date range.
     * 
     * @param obstime
     *            the specified observation date/time
     * @param monthdaystart
     *            the data limits formatted start of the date range
     * @param monthdayend
     *            the data limits formatted end of the date range
     * @return {@code true} when the specified observation date/time is within
     *         the range; {code false}, otherwise.
     * @throws Exception
     */
    private boolean withinDataLimitTimeRange(final Date obstime,
            final String monthdaystart, final String monthdayend)
                    throws Exception {
        Date parsedLimitStart = null;
        Date parsedLimitEnd = null;
        try {
            parsedLimitStart = dataLimitDF.get().parse(monthdaystart);
        } catch (ParseException e) {
            throw new RocCheckerFailedException(
                    "Failed to parse data limit date: " + monthdaystart + ".");
        }

        try {
            parsedLimitEnd = dataLimitDF.get().parse(monthdayend);
        } catch (ParseException e) {
            throw new RocCheckerFailedException(
                    "Failed to parse data limit date: " + monthdayend + ".");
        }

        /*
         * Most of {@link Date} is deprecated; so, it is easier to work with
         * {@link Calendar}.
         */
        Calendar limitStart = TimeUtil.newGmtCalendar(parsedLimitStart);
        Calendar limitEnd = TimeUtil.newGmtCalendar(parsedLimitEnd);
        Calendar obsCompareTime = TimeUtil.newGmtCalendar(obstime);

        /*
         * Only the month/day are important for this comparison. So, set all
         * other fields to the minimum value.
         */
        TimeUtil.minCalendarFields(limitStart, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(limitEnd, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(obsCompareTime, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);

        /*
         * The specified obs time must be within (not before, not after) the
         * limit start date and end date.
         */
        if (obsCompareTime.before(limitStart)
                || obsCompareTime.after(limitEnd)) {
            return false;
        }

        return true;
    }

    /**
     * Retrieves the {@link Observation}s for the specified
     * {@link RocCheckerConfig}.
     * 
     * @param config
     *            the specified {@link RocCheckerConfig}
     */
    private RocCheckerObservationData retrieveObservations(
            RocCheckerConfig config) {
        logger.info("Processing Roc Checker configuration: {} ...",
                config.toString());

        /*
         * Determine which table to interact with. This should never return null
         * due to the configuration validation that was previously completed.
         * Used for logging purposes.
         */
        ObservationTable observationTable = lookupObservationMap
                .get(config.getTable());
        /*
         * Based on the table, retrieve the associated {@link
         * AbstractIHFSObservationDbDao}.
         */
        AbstractIHFSObservationDbDao<?, ?> dao = getObsDao(config.getTable());
        if (dao == null) {
            logger.error(
                    "Failed to find an observation dao for entity: {}. Skipping configuration: {}.",
                    observationTable.getEntityClass(), config.toString());
            return new RocCheckerObservationData(config);
        }

        /*
         * The end time of the data retrieval range will be based on some number
         * of hours (potentially 0) subtracted from the current time.
         */
        Calendar endTime = TimeUtil.newGmtCalendar();
        TimeUtil.minCalendarFields(endTime, Calendar.MILLISECOND);
        endTime.add(Calendar.HOUR, -config.getEndHours());

        /*
         * The start time of the data retrieval range will be based on some
         * number of hours (must be > 0) subtracted from the end time.
         */
        Calendar startTime = TimeUtil.newCalendar(endTime);
        startTime.add(Calendar.HOUR, -config.getStartHours());

        /*
         * Determine the quality code.
         */
        String dataQualityLogMsg = "GOOD DATA ONLY, NO QUESTIONABLE DATA";
        int qualityCode = QualityCodeUtil.GOOD_QUESTIONABLE_THRESHOLD;
        if (config.getQuality() == ROC_QUALITY.GQ) {
            dataQualityLogMsg = "GOOD AND QUESTIONABLE DATA";
            qualityCode = QualityCodeUtil.QUESTIONABLE_BAD_THRESHOLD;
        }
        logger.info(
                "Retrieving observation data from Table: {} with quality: {} for time period: {} to {} ...",
                observationTable.name(), dataQualityLogMsg,
                startTime.getTime().toString(), endTime.getTime().toString());

        List<Observation> observationRecords = Collections.emptyList();
        if (config.isNoConstraintsConfig()) {
            observationRecords = dao.getObservations(startTime, endTime,
                    qualityCode, MpeConstants.MISSING_VALUE);
        } else if (config.isOnlyLidConstraintsConfig()) {
            observationRecords = dao.getObservationsForLid(startTime, endTime,
                    qualityCode, MpeConstants.MISSING_VALUE,
                    config.getRocCheckerConstraints().getLids());
        } else if (config.isOnlyPEConstraintConfig()) {
            observationRecords = dao.getObservationsForPE(startTime, endTime,
                    qualityCode, MpeConstants.MISSING_VALUE,
                    config.getRocCheckerConstraints().getPes());
        } else if (config.isLidAndPEConstraintConfig()) {
            observationRecords = dao.getObservationsForLidAndPE(startTime,
                    endTime, qualityCode, MpeConstants.MISSING_VALUE,
                    config.getRocCheckerConstraints().getLids(),
                    config.getRocCheckerConstraints().getPes());
        }

        logger.info("Retrieved {} observation record(s).",
                observationRecords.size());
        Map<ObservationKey, List<Observation>> observationLookupMap = Collections
                .emptyMap();
        if (!observationRecords.isEmpty()) {
            /*
             * pre-group the observation records by: lid, pe, obstime
             */
            observationLookupMap = new LinkedHashMap<>(
                    observationRecords.size(), 1.0f);
            for (Observation observation : observationRecords) {
                ObservationKey key = new ObservationKey(observation.getLid(),
                        observation.getPe());
                if (!observationLookupMap.containsKey(key)) {
                    observationLookupMap.put(key,
                            new LinkedList<Observation>());
                }
                observationLookupMap.get(key).add(observation);
            }
        }

        return new RocCheckerObservationData(startTime, endTime, config,
                observationLookupMap, observationRecords.size());
    }

    /**
     * Retrieves any available {@link Datalimits} record(s).
     * 
     * @return a mapping of the {@link Datalimits} record(s) that were retrieved
     *         to {@link DataLimitKey}s.
     */
    private Map<DataLimitKey, List<Datalimits>> retrieveDataLimits() {
        logger.info("Retrieving data limits records ...");
        List<Datalimits> records = new DatalimitsDao().getObsLimits();
        logger.info("Retrieved {} data limits record(s).", records.size());

        if (records.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<DataLimitKey, List<Datalimits>> dataLimitLookupMap = new HashMap<>(
                records.size(), 1.0f);
        for (Datalimits record : records) {
            DataLimitKey key = new DataLimitKey(record.getId().getPe(),
                    record.getId().getDur());
            if (!dataLimitLookupMap.containsKey(key)) {
                dataLimitLookupMap.put(key, new ArrayList<Datalimits>(1));
            }
            dataLimitLookupMap.get(key).add(record);
        }
        return dataLimitLookupMap;
    }

    /**
     * Retrieves any available {@link Locdatalimits} record(s).
     * 
     * @return a mapping of the {@link Locdatalimits} record(s) that were
     *         retrieved to {@link LocDataLimitKey}s.
     */
    private Map<LocDataLimitKey, List<Locdatalimits>> retrieveLocDataLimits() {
        logger.info("Retrieving location data limits records ...");
        List<Locdatalimits> records = new LocdatalimitsDao().getObsLimits();
        logger.info("Retrieved {} location data limits record(s).",
                records.size());

        if (records.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<LocDataLimitKey, List<Locdatalimits>> locationDataLimitLookupMap = new HashMap<>(
                records.size(), 1.0f);
        for (Locdatalimits record : records) {
            LocDataLimitKey key = new LocDataLimitKey(record.getId().getLid(),
                    record.getId().getPe(), record.getId().getDur());
            if (!locationDataLimitLookupMap.containsKey(key)) {
                locationDataLimitLookupMap.put(key,
                        new ArrayList<Locdatalimits>(1));
            }
            locationDataLimitLookupMap.get(key).add(record);
        }
        return locationDataLimitLookupMap;
    }

    private AbstractIHFSObservationDbDao<?, ?> getObsDao(final String table) {
        /*
         * Determine which table to interact with. This should never return null
         * due to the configuration validation that was previously completed.
         */
        ObservationTable observationTable = lookupObservationMap.get(table);
        if (observationTable == null) {
            return null;
        }

        /*
         * Based on the table, retrieve the associated {@link
         * AbstractIHFSObservationDbDao}.
         */
        AbstractIHFSObservationDbDao<?, ?> dao = ObservationDaoLookupManager
                .getInstance()
                .lookupObservationDao(observationTable.getEntityClass());
        return dao;
    }

    /**
     * Validates the contents of the specified {@link RocCheckerConfig} before
     * attempting to use the configuration. Any validation problems will be
     * logged.
     * 
     * @param config
     *            the specified {@link RocCheckerConfig}
     * @return {@code true}, if the configuration is valid; {@code false},
     *         otherwise.
     */
    private boolean validateConfig(RocCheckerConfig config) {
        /*
         * Verify that the table referenced in the config is recognized.
         */
        if (!lookupObservationMap.containsKey(config.getTable())) {
            logger.error(
                    "Roc Checker Config: {} references an invalid Table: {}. Skipping config.",
                    config.toString(), config.getTable());
            return false;
        }

        /*
         * Verify that the quality (if specified) in the config is recognized.
         */
        if (config.getQuality() == null) {
            final String allowedQualities = StringUtils
                    .arrayToCommaDelimitedString(ROC_QUALITY.values());
            logger.error(
                    "Roc Checker Config: {} includes an invalid quality field; quality (if specified) must be one of: {}. Skipping config.",
                    config.toString(), allowedQualities);
            return false;
        }

        /*
         * Verify that the end hours (if specified) in the config are >= 0.
         */
        if (config.getEndHours() < 0) {
            logger.error(
                    "Roc Checker Config: {} includes an invalid endHours field: {}; endHours must be >= 0. Skipping config.",
                    config.toString(), config.getEndHours());
            return false;
        }

        /*
         * Verify that the start hours specified in the config are > 0.
         */
        if (config.getStartHours() <= 0) {
            logger.error(
                    "Roc Checker Config: {} includes an invalid startHours field: {}; startHours must be > 0. Skipping config.",
                    config.toString(), config.getStartHours());

            return false;
        }

        return true;
    }

    /**
     * Reads the Roc Checker configuration from localization.
     * 
     * @throws Exception
     */
    private void readRocCheckerConfig() throws Exception {
        logger.info(
                "Configuring Roc Checker. Reading localization file: {} ...",
                CONFIG_FILE);
        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile localizationFile = pathManager
                .getStaticLocalizationFile(CONFIG_FILE);
        if (localizationFile == null) {
            throw new RocCheckerFailedException(
                    "Failed to configure Roc Checker. Unable to find expected localization file: "
                            + CONFIG_FILE + ".");
        }

        try (InputStream is = localizationFile.openInputStream()) {
            rocCheckerRunConfiguration = jaxbManager.unmarshalFromInputStream(
                    RocCheckerRunConfiguration.class, is);
        } catch (Exception e) {
            throw new RocCheckerFailedException(
                    "Failed to configure Roc Checker. Failed to read localization file: "
                            + CONFIG_FILE + ".");
        }

        logger.info("Successfully read localization file: {}.", CONFIG_FILE);
    }
}