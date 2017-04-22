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
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.commons.io.FileUtils;

import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasTableFile;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasTableHeader;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasTableIOException;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasTableRow;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasmesgenConstants;
import com.raytheon.uf.edex.plugin.mpe.conversion.BiasTableInterceptor;
import com.raytheon.uf.edex.plugin.mpe.test.ValidationTestException;
import com.raytheon.uf.edex.plugin.mpe.test.core.IValidationTest;

/**
 * Validation tests for Biasmesgen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2016  5576       bkowal      Initial creation
 * Jun 16, 2016 5576       bkowal      Do not compare bias julian time in the header.
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasmesgenValidationTest implements IValidationTest {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /*
     * Due to the nature of bias table files, files generated for validation
     * will be allowed to remain for up to (2 * 60) minutes before validation
     * will be declared a failure for a given file.
     */
    private static final long BIAS_TABLE_GRACE_PERIOD = TimeUtil.MILLIS_PER_HOUR * 2;

    private static final int GENERATION_ID_GROUP = 1;

    private static final String BIAS_TABLE_REGEX = "(\\d{3}\\.\\d{3}\\.\\d{2}\\.\\d\\.\\d{4})\\d{2}";

    private static final Pattern biasTablePattern = Pattern
            .compile(BIAS_TABLE_REGEX);

    private static final String BIAS_TABLE_VALIDATE_REGEX = BIAS_TABLE_REGEX
            + BiasTableInterceptor.VALIDATE_EXT;

    private static final Pattern biasTableValidatePattern = Pattern
            .compile(BIAS_TABLE_VALIDATE_REGEX);

    public BiasmesgenValidationTest() {
    }

    @Override
    public String getTestName() {
        return "Biasmesgen";
    }

    @Override
    public void executeValidationTest(final Calendar validationDateTime)
            throws ValidationTestException {
        /*
         * The Biasmesgen test ignores the specified validation date/time and
         * instead attempts to validate all available data.
         */
        /*
         * Retrieve the bias table directory.
         */
        Path biasOutputRootPath = null;
        try {
            biasOutputRootPath = AppsDefaultsConversionWrapper
                    .getPathForToken(BiasmesgenConstants.AppsDefaults.BIAS_MSG_DIR);
        } catch (Exception e) {
            throw new ValidationTestException(
                    "Failed to retrieve the directory path associated with "
                            + AppsDefaults.NAME + " property: "
                            + BiasmesgenConstants.AppsDefaults.BIAS_MSG_DIR
                            + ".", e);
        }
        logger.info("Bias Output Root Directory: {}",
                biasOutputRootPath.toString());

        /*
         * Determine which files, if any, are available in the bias output root
         * directory.
         */
        Collection<File> biasTableFiles = FileUtils.listFiles(
                biasOutputRootPath.toFile(), null, false);
        if (biasTableFiles.isEmpty()) {
            /*
             * No bias tables to process.
             */
            logger.info(
                    "No bias table files were discovered for validation for verification tests: {}.",
                    validationDateTime.getTime().toString());
            return;
        }

        /*
         * Segregate the bias table files into files that exist for validation
         * and generated files. Discard any unrecognized files. Maps of bias id
         * (extracted by the regex declared up above) to the associated
         * validation file and the associated generation file.
         */
        final Map<String, List<Path>> validationBiasTableMap = new HashMap<>(
                biasTableFiles.size(), 1.0f);
        final Map<String, List<Path>> generatedBiasTableMap = new HashMap<>(
                biasTableFiles.size(), 1.0f);
        final List<Path> unrecognizedFilesList = new ArrayList<>(
                biasTableFiles.size());
        for (File biasTableFile : biasTableFiles) {
            final String fileName = biasTableFile.getName();
            /*
             * Verify the file is recognized.
             */
            Matcher matcher = biasTablePattern.matcher(fileName);
            boolean maybeValidation = false;
            if (!matcher.matches()) {
                // Maybe it is a validation bias table file?
                matcher = biasTableValidatePattern.matcher(fileName);
                maybeValidation = true;
            }

            if (!matcher.matches()) {
                unrecognizedFilesList.add(biasTableFile.toPath());
                continue;
            }

            /*
             * Determine if the validation grace period has already passed for
             * the file. If it has delete the file. This check will verify the
             * number of files that will be validated to ensure that a
             * significant number does not accumulate during EDEX downtime. This
             * is based on a worse case scenario. The best case scenario is that
             * there will only be a maximum of two files for every radar that a
             * particular site is responsible for within a single hour.
             */
            final long lastModifiedTime = biasTableFile.lastModified();
            if (validationDateTime.getTimeInMillis() >= (lastModifiedTime + BIAS_TABLE_GRACE_PERIOD)) {
                logger.error("Failed to validate bias table file: "
                        + biasTableFile.getAbsolutePath()
                        + ". The validation period has expired.");
                try {
                    Files.deleteIfExists(biasTableFile.toPath());
                } catch (IOException e) {
                    logger.error("Failed to purge expired bias table file: "
                            + biasTableFile.getAbsolutePath() + ".", e);
                }
                continue;
            }

            final String biasIdKey = matcher.group(GENERATION_ID_GROUP);
            if (!validationBiasTableMap.containsKey(biasIdKey)) {
                validationBiasTableMap.put(biasIdKey, new ArrayList<Path>(1));
            }
            if (!generatedBiasTableMap.containsKey(biasIdKey)) {
                generatedBiasTableMap.put(biasIdKey, new ArrayList<Path>(1));
            }

            if (maybeValidation) {
                /*
                 * Based on the match, this is a bias table validation file.
                 */
                validationBiasTableMap.get(biasIdKey).add(
                        biasTableFile.toPath());
                logger.info(
                        "Found validation bias table file: {} with bias id: {}.",
                        biasTableFile.toString(), biasIdKey);
            } else {
                generatedBiasTableMap.get(biasIdKey)
                        .add(biasTableFile.toPath());
                logger.info(
                        "Found generated bias table file: {} with bias id: {}.",
                        biasTableFile.toString(), biasIdKey);
            }
        }

        /*
         * Purge any unrecognized files.
         */
        if (!unrecognizedFilesList.isEmpty()) {
            for (Path path : unrecognizedFilesList) {
                try {
                    Files.deleteIfExists(path);
                    logger.info("Purged unrecongized bias table file: {}.",
                            path.toString());
                } catch (IOException e) {
                    logger.error(
                            "Failed to purge unrecognized bias table file: "
                                    + path.toString() + ".", e);
                }
            }
        }

        /*
         * The comparison will be based on the validation side of the
         * relationship. So, verify that validation bias table files are
         * available.
         */
        if (validationBiasTableMap.isEmpty()) {
            logger.info(
                    "No validation bias tables were found for verification tests: {}.",
                    validationDateTime.getTime().toString());
            return;
        }

        for (String biasId : validationBiasTableMap.keySet()) {
            /*
             * Retrieve any associated validation bias table files.
             */
            List<Path> validationFiles = validationBiasTableMap.get(biasId);

            /*
             * Retrieve any associated generated bias table files.
             */
            List<Path> generatedFiles = generatedBiasTableMap.get(biasId);

            /*
             * There should be a one-to-one match. If not, there may be a
             * problem that requires investigation. Probably not worth loading
             * all possible files into memory to attempt to identify potential
             * matches.
             */
            if (validationFiles.size() != 1 || generatedFiles.size() != 1) {
                logger.error(
                        "Found multiple (or no) potential bias table matches for: {}; generated count = {}, validation count = {}. Skipping validation.",
                        biasId, generatedFiles.size(), validationFiles.size());
                continue;
            }

            /*
             * Prepare to read the files to complete the bias table comparison.
             */
            final Path validationTablePath = validationFiles.iterator().next();
            final Path generatedTablePath = generatedFiles.iterator().next();

            /*
             * Read the bias table files.
             */
            BiasTableFile validationBiasTable = null;
            BiasTableFile generatedBiasTable = null;
            try {
                validationBiasTable = BiasTableFile
                        .loadBiasTable(validationTablePath);
                logger.info(
                        "Successfully read validation bias table file: {}.",
                        validationTablePath.toString());
            } catch (BiasTableIOException e) {
                logger.error("Failed to read validation bias table file: "
                        + validationTablePath.toString() + ".", e);
                continue;
            }
            try {
                generatedBiasTable = BiasTableFile
                        .loadBiasTable(generatedTablePath);
                logger.info("Successfully read generated bias table file: {}.",
                        generatedTablePath.toString());
            } catch (BiasTableIOException e) {
                logger.error("Failed to read generated bias table file: "
                        + generatedTablePath.toString() + ".", e);
                continue;
            }

            /*
             * Compare the headers. However, the entire header will not be
             * compared because the generation date/time are incorporated into
             * the header. Validate everything with the exception of the bias
             * table "Julian" time.
             */
            BiasTableHeader validateHeader = validationBiasTable.getHeader();
            BiasTableHeader generateHeader = generatedBiasTable.getHeader();
            if (validateHeader.getMessageCode() == generateHeader
                    .getMessageCode()) {
                logger.info("Successfully validated the Bias Table header - Message Codes.");
            } else {
                logger.error("Bias Table header - Message Codes do not match.");
            }
            if (validateHeader.getMessageDate() == generateHeader
                    .getMessageDate()) {
                logger.info("Successfully validated the Bias Table header - Message Dates.");
            } else {
                logger.error("Bias Table header - Message Dates do not match.");
            }
            if (validateHeader.getMessageLength() == generateHeader
                    .getMessageLength()) {
                logger.info("Successfully validated the Bias Table header - Message Lengths.");
            } else {
                logger.error("Bias Table header - Message Lengths do not match.");
            }
            if (validateHeader.getSourceId() == generateHeader.getSourceId()) {
                logger.info("Successfully validated the Bias Table header - Source Ids.");
            } else {
                logger.error("Bias Table header - Source Ids do not match.");
            }
            if (validateHeader.getDestinationId() == generateHeader
                    .getDestinationId()) {
                logger.info("Successfully validated the Bias Table header - Destination Ids.");
            } else {
                logger.error("Bias Table header - Destination Ids do not match.");
            }
            if (validateHeader.getNumberBlocks() == generateHeader
                    .getNumberBlocks()) {
                logger.info("Successfully validated the Bias Table header - Number Blocks.");
            } else {
                logger.error("Bias Table header - Number Blocks do not match.");
            }

            /*
             * Compare the sub-headers.
             */
            if (validationBiasTable.getSubHeader().equals(
                    generatedBiasTable.getSubHeader())) {
                logger.info("Successfully validated the Bias Table subheaders.");
            } else {
                logger.error("Bias Table subheaders do not match.");
            }

            /*
             * Compare the observation date.
             */
            if (validationBiasTable.getObservationDate().equals(
                    generatedBiasTable.getObservationDate())) {
                logger.info("Successfully validated the Bias Table observation date.");
            } else {
                logger.error("Bias Table observation dates do not match.");
            }

            /*
             * Skip the generation date. There is no guarantee that both files
             * would have been created at the same exact time.
             */

            /*
             * Verify that the tables include the same number of rows.
             */
            if (validationBiasTable.getNumberOfRows() != generatedBiasTable
                    .getNumberOfRows()) {
                logger.error("Bias Tables do not include the same number of rows.");
                /*
                 * No point in validating the contents of the tables if they do
                 * not have the same number of rows.
                 */
                continue;
            }
            /*
             * Validate the contents of each row.
             */
            for (int i = 0; i < validationBiasTable.getNumberOfRows(); i++) {
                BiasTableRow validationRow = validationBiasTable.getBiasTable()
                        .get(i);
                BiasTableRow generatedRow = generatedBiasTable.getBiasTable()
                        .get(i);
                if (validationRow.equals(generatedRow)) {
                    logger.info("Successfully validated bias table row {}.",
                            (i + 1));
                } else {
                    logger.error("Row {} of the Bias Tables do not match.",
                            (i + 1));
                }
            }

            /*
             * All verification has been finished. The files can now be purged.
             */
            try {
                Files.deleteIfExists(validationTablePath);
                logger.info("Purged validation bias table file: {}.",
                        validationTablePath.toString());
            } catch (IOException e) {
                logger.error("Failed to purge validation bias table file: "
                        + validationTablePath.toString() + ".", e);
            }
            try {
                Files.deleteIfExists(generatedTablePath);
                logger.info("Purged generated bias table file: {}.",
                        generatedTablePath.toString());
            } catch (IOException e) {
                logger.error("Failed to purge generated bias table file: "
                        + generatedTablePath.toString() + ".", e);
            }
        }
    }
}