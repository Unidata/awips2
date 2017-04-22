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
package com.raytheon.uf.edex.plugin.mpe.conversion;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.InterceptConstants;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.RocInterceptedAlertalarmval;
import com.raytheon.uf.edex.plugin.mpe.conversion.data.RocInterceptedObservation;

/**
 * ROC Checker only produces database records. So, there are no actual output
 * files to compare. In parallel mode, we do not want ROC Checker to directly
 * interact with the database. So, ROC Checker will give information that would
 * normally be stored in the database to this class where it can temporarily be
 * persisted as XML to the file system for later automated comparison. TODO: the
 * entire package that this class is a part of should be removed when the
 * decision has been made to use the converted mpe applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2016 5699       bkowal      Initial creation
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RocCheckerInterceptor {

    public static final String INTERCEPT_COMPONENT_DIR = "roc_checker";

    public static final String INTERCEPT_OBS_DIR = "obs";

    public static final String INTERCEPT_ALERT_ALARM_DIR = "alertalarm";

    private static final int MAX_FILE_CONFLICT_RES_ATTMPT = 1000;

    private static JAXBManager jaxbManager;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public RocCheckerInterceptor() {
        if (jaxbManager == null) {
            try {
                jaxbManager = new JAXBManager(RocInterceptedObservation.class,
                        RocInterceptedAlertalarmval.class);
            } catch (JAXBException e) {
                logger.error("Failed to initialize the JAXBManager.", e);
            }
        }
    }

    public void intercept(final Observation observation,
            final long updatedQualityCode, final String table) {
        if (jaxbManager == null
                || !AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            return;
        }

        final Path outputFilePath = getOutputFilePath(INTERCEPT_OBS_DIR);
        if (outputFilePath == null) {
            return;
        }

        /*
         * Prepare the JAXB-able observation.
         */
        final RocInterceptedObservation interceptedObservation = new RocInterceptedObservation(
                observation, table, updatedQualityCode);
        /*
         * Write the observation. The written files should only exist for
         * slightly over an hour assuming that there is not extended EDEX
         * downtime.
         */
        writeInterceptDataFile(outputFilePath, interceptedObservation);
    }

    public void intercept(final Alertalarmval alertalarmval) {
        if (jaxbManager == null
                || !AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            return;
        }

        final Path outputFilePath = getOutputFilePath(INTERCEPT_ALERT_ALARM_DIR);
        if (outputFilePath == null) {
            return;
        }

        /*
         * Prepare the JAXB-able alert/alarm.
         */
        final RocInterceptedAlertalarmval rocInterceptedAlertalarmval = new RocInterceptedAlertalarmval(
                alertalarmval);
        /*
         * Write the alert/alarm. The written files should only exist for
         * slightly over an hour assuming that there is not extended EDEX
         * downtime.
         */
        writeInterceptDataFile(outputFilePath, rocInterceptedAlertalarmval);
    }

    private Path getOutputFilePath(final String dataTypeDir) {
        Path dataRootPath = null;
        try {
            dataRootPath = InterceptConstants.getInterceptDataDirectory();
        } catch (Exception e) {
            logger.error("Failed to determine the intercept data directory.", e);
            return null;
        }

        final Path dataPath = dataRootPath.resolve(INTERCEPT_COMPONENT_DIR)
                .resolve(dataTypeDir);
        if (!Files.exists(dataPath)) {
            try {
                Files.createDirectories(dataPath);
            } catch (IOException e) {
                logger.error("Failed to create the intercept data directory: "
                        + dataPath.toString() + ".", e);
            }
        }

        Path dataFilePath = dataPath.resolve(Long.toString(System
                .currentTimeMillis()) + ".xml");
        if (Files.exists(dataFilePath)) {
            boolean attemptNameConflictResolution = true;
            int uniqueNameIndex = 0;
            while (attemptNameConflictResolution) {
                dataFilePath = dataPath.resolve(Long.toString(System
                        .currentTimeMillis())
                        + "_"
                        + Integer.toString(uniqueNameIndex) + ".xml");
                if (!Files.exists(dataFilePath)) {
                    /*
                     * Generated a file with a unique name.
                     */
                    attemptNameConflictResolution = false;
                } else {
                    if (uniqueNameIndex <= MAX_FILE_CONFLICT_RES_ATTMPT) {
                        ++uniqueNameIndex;
                    } else {
                        /*
                         * Failed to generate a file with a unique name.
                         */
                        attemptNameConflictResolution = false;
                        logger.warn("Failed to generate a unique intercept file name. Intercept file: "
                                + dataFilePath.toString()
                                + " will be overwritten.");
                    }
                }
            }
        }
        return dataFilePath;
    }

    private void writeInterceptDataFile(final Path outputFilePath, Object object) {
        try {
            jaxbManager.marshalToXmlFile(object, outputFilePath);
            logger.info("Successfully wrote intercept verification file: "
                    + outputFilePath.toString() + ".");
        } catch (SerializationException e) {
            logger.error("Failed to write intercept verification file: "
                    + outputFilePath.toString() + ".", e);
        }
    }
}