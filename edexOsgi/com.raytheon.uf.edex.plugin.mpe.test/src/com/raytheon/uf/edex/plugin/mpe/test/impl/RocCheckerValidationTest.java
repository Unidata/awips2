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
package com.raytheon.uf.edex.plugin.mpe.test.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import com.raytheon.uf.common.dataplugin.shef.data.IObservation;
import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.edex.plugin.mpe.rocchecker.ObservationDaoLookupManager;
import com.raytheon.uf.edex.plugin.mpe.rocchecker.ObservationTable;
import com.raytheon.uf.edex.plugin.mpe.test.ValidationTestException;
import com.raytheon.uf.edex.plugin.mpe.test.core.IValidationTest;
import com.raytheon.uf.edex.plugin.mpe.conversion.RocCheckerInterceptor;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.InterceptConstants;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.RocInterceptedAlertalarmval;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.RocInterceptedObservation;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSObservationDbDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.AlertalarmvalDao;

/**
 * Validation tests for Roc Checker.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class RocCheckerValidationTest implements IValidationTest {

    private static enum ValidationMode {
        OBS("Obs"), ALERT_ALARM("Alert/Alarm");

        private final String displayText;

        private ValidationMode(String displayText) {
            this.displayText = displayText;
        }

        public String getDisplayText() {
            return displayText;
        }
    }

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /*
     * Copied from Roc Checker. However, this class is only temporary.
     */
    private static Map<String, ObservationTable> lookupObservationMap;

    private static JAXBManager jaxbManager;

    private final AlertalarmvalDao alertalarmvalDao = new AlertalarmvalDao();

    public RocCheckerValidationTest() {
        if (lookupObservationMap == null) {
            lookupObservationMap = new HashMap<>(
                    ObservationTable.values().length, 1.0f);
            for (ObservationTable observation : ObservationTable.values()) {
                lookupObservationMap.put(observation.name(), observation);
            }
        }
    }

    @Override
    public String getTestName() {
        return "Roc Checker";
    }

    @Override
    public void executeValidationTest(Calendar validationDateTime)
            throws ValidationTestException {
        if (jaxbManager == null) {
            try {
                jaxbManager = new JAXBManager(RocInterceptedObservation.class,
                        RocInterceptedAlertalarmval.class);
            } catch (JAXBException e) {
                throw new ValidationTestException(
                        "Failed to initialize the JAXBManager.", e);
            }
        }

        /*
         * Two types of validation must occur: 1) validate any observations that
         * had quality code updates (this will not necessarily return 100%
         * because end users may fix such records prior to the validation run)
         * 2) validate any alerts/alarms that have been generated
         */
        Path interceptDataDirectory = null;
        try {
            interceptDataDirectory = InterceptConstants
                    .getInterceptDataDirectory();
        } catch (Exception e) {
            throw new ValidationTestException(
                    "Failed to determine the intercept data directory.", e);
        }

        final Path interceptObsDataDirectory = interceptDataDirectory.resolve(
                RocCheckerInterceptor.INTERCEPT_COMPONENT_DIR).resolve(
                RocCheckerInterceptor.INTERCEPT_OBS_DIR);
        validate(interceptObsDataDirectory, ValidationMode.OBS);
        final Path interceptAlertAlarmPath = interceptDataDirectory.resolve(
                RocCheckerInterceptor.INTERCEPT_COMPONENT_DIR).resolve(
                RocCheckerInterceptor.INTERCEPT_ALERT_ALARM_DIR);
        validate(interceptAlertAlarmPath, ValidationMode.ALERT_ALARM);
    }

    private void validate(final Path dataPath, final ValidationMode mode) {
        logger.info("Validating {} ...", mode.getDisplayText());
        if (!Files.exists(dataPath)) {
            logger.info(
                    "Skipping {} validation test. The {} validation directory: {} does not exist.",
                    mode.getDisplayText(), mode.getDisplayText(),
                    dataPath.toString());
            return;
        }

        Collection<File> validationFiles = FileUtils.listFiles(
                dataPath.toFile(), null, false);
        if (CollectionUtils.isEmpty(validationFiles)) {
            logger.info(
                    "Skipping {} validation test. No {} validation files exist.",
                    mode.getDisplayText(), mode.getDisplayText(),
                    dataPath.toString());
            return;
        }

        logger.info("Validating {} {} validation files.",
                validationFiles.size(), mode.getDisplayText());
        for (File file : validationFiles) {
            try {
                boolean result = false;
                switch (mode) {
                case OBS:
                    result = validateObs(file);
                    break;
                case ALERT_ALARM:
                    result = validateAlertAlarm(file);
                    break;
                }
                if (result) {
                    logger.info(
                            "{} validation file: {} has PASSED validation.",
                            mode.getDisplayText(), file.getAbsolutePath());
                } else {
                    logger.warn(
                            "{} validation file: {} has FAILED validation.",
                            mode.getDisplayText(), file.getAbsolutePath());
                }
            } catch (Exception e) {
                logger.error("Failed to validate " + mode.getDisplayText()
                        + " validation file: " + file.getAbsolutePath() + ".",
                        e);
            } finally {
                try {
                    /*
                     * Cleanup the validation files.
                     */
                    Files.deleteIfExists(file.toPath());
                } catch (IOException e) {
                    logger.error(
                            "Failed to delete vadiation file: "
                                    + file.getAbsolutePath() + ".", e);
                }
            }
        }
        logger.info("{} validation has concluded.", mode.getDisplayText());
    }

    private boolean validateObs(final File obsValidationFile) throws Exception {
        /*
         * Read the validation file.
         */
        RocInterceptedObservation intercepted = jaxbManager
                .unmarshalFromXmlFile(RocInterceptedObservation.class,
                        obsValidationFile);
        AbstractIHFSObservationDbDao<?, ?> dao = getObsDao(intercepted
                .getTable());
        if (dao == null) {
            logger.warn("Validation Failed: Unable to find an Obs Dao for table: "
                    + intercepted.getTable() + ".");
            return false;
        }

        final IObservation retrieved = dao.retrieveById(intercepted);
        if (retrieved == null) {
            logger.warn("Validation Failed: Failed to find an Observation associated with the validation Observation.");
            return false;
        }

        if (!intercepted.getQualityCode().equals(retrieved.getQualityCode())) {
            logger.warn("Validation Failed: The validation Obs quality code does not match the retrieved Obs quality code.");
        }

        return true;
    }

    private boolean validateAlertAlarm(final File alertAlarmValidationFile)
            throws Exception {
        /*
         * Read the validation file.
         */
        RocInterceptedAlertalarmval intercept = jaxbManager
                .unmarshalFromXmlFile(RocInterceptedAlertalarmval.class,
                        alertAlarmValidationFile);
        Alertalarmval alertalarmval = RocInterceptedAlertalarmval
                .convertAlertAlarm(intercept);
        Alertalarmval retrievedAlertalarmval = alertalarmvalDao
                .retrieveById(alertalarmval.getId());
        /*
         * Clear posting time before the comparison because it is based on the
         * current time when the record was created. Clear action time. Action
         * time is initially set to null by RocChecker; but, it can and will be
         * updated by another part of the system if some "action" occurs.
         */
        alertalarmval.setPostingtime(null);
        alertalarmval.setActionTime(null);
        if (retrievedAlertalarmval != null) {
            retrievedAlertalarmval.setPostingtime(null);
            retrievedAlertalarmval.setActionTime(null);
        }
        if (alertalarmval.equals(retrievedAlertalarmval)) {
            return true;
        }

        logger.warn("Validation Failed: The validation Alert/Alarm does not match the retrieved Alert/Alarm.");

        return false;
    }

    /*
     * Copied from Roc Checker. But, this class is only temporary.
     */
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
                .getInstance().lookupObservationDao(
                        observationTable.getEntityClass());
        return dao;
    }
}