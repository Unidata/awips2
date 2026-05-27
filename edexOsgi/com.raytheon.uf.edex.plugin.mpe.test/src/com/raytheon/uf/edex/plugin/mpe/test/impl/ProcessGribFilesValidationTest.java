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

import java.util.Calendar;
import java.util.Iterator;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.gribit2.grib.BinaryDataSection;
import com.raytheon.uf.common.mpe.gribit2.grib.BitMapSection;
import com.raytheon.uf.common.mpe.gribit2.grib.GribFile;
import com.raytheon.uf.common.mpe.gribit2.grib.GridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.IndicatorSection;
import com.raytheon.uf.common.mpe.gribit2.grib.InvalidGribException;
import com.raytheon.uf.common.mpe.gribit2.grib.LambertConformalGridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.LatLonAndGaussianAndStaggeredGridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.MercatorGridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.PolarStereographicGridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.ProductDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.Table11Flags;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsConfigLoader;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsLoadException;
import com.raytheon.uf.edex.plugin.mpe.apps.RequiredTokenMissingException;
import com.raytheon.uf.edex.plugin.mpe.test.ValidationTestException;
import com.raytheon.uf.edex.plugin.mpe.test.config.ProcessGribFilesTestConfig;
import com.raytheon.uf.edex.plugin.mpe.test.core.IValidationTest;

/**
 * Validation tests for the Process Grib Files Runner.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2016  4628       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class ProcessGribFilesValidationTest implements IValidationTest {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Override
    public String getTestName() {
        return "Process Grib Files";
    }

    @Override
    public void executeValidationTest(Calendar validationDateTime)
            throws ValidationTestException {
        /*
         * Configure the validation test.
         */
        ProcessGribFilesTestConfig config = new ProcessGribFilesTestConfig();
        try {
            AppsDefaultsConfigLoader.populateFromAppsDefaults(config);
        } catch (RequiredTokenMissingException | AppsDefaultsLoadException e) {
            throw new ValidationTestException(
                    "Failed to configure the Process Grib Files Validation Test for run: "
                            + validationDateTime.getTime().toString() + ".",
                    e);
        }

        /*
         * Retrieve the alternate, parallel version of the grib directories of
         * interest.
         */
        final Path parallelGribPath;
        try {
            parallelGribPath = AppsDefaultsConversionWrapper
                    .getPathForToken(MpeConstants.AppsDefaults.MPE_GRIB_DIR);
        } catch (AppsDefaultsPathException e) {
            throw new ValidationTestException(
                    "Failed to retrieve the directory path associated with "
                            + AppsDefaults.NAME + " property: "
                            + MpeConstants.AppsDefaults.MPE_GRIB_DIR + ".",
                    e);
        }
        final Path parallelQpeGribSbnDirPath;
        try {
            parallelQpeGribSbnDirPath = AppsDefaultsConversionWrapper
                    .getPathForToken(
                            MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR);
        } catch (AppsDefaultsPathException e) {
            throw new ValidationTestException(
                    "Failed to retrieve the directory path associated with "
                            + AppsDefaults.NAME + " property: "
                            + MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR
                            + ".",
                    e);
        }

        /*
         * Check for grib files that need to be compared.
         */
        logger.info("Validating generated grib files in: {} ...",
                parallelGribPath.toString());
        validateGribFiles(parallelGribPath, config.getGribPath());

        /*
         * Check for qpe sbn grib files that need to be compared.
         */
        validateGribFiles(parallelQpeGribSbnDirPath,
                config.getQpeGribSbnDirPath());
    }

    private void validateGribFiles(final Path parallelGribPath,
            final Path gribPath) {
        logger.info("Validating generated grib files in directory: {} ...",
                parallelGribPath.toString());
        int count = 0;
        Iterator<File> fileIterator = FileUtils.iterateFiles(
                parallelGribPath.toFile(), new String[] { "grib" }, false);
        while (fileIterator.hasNext()) {
            final Path parallelValidationPath = fileIterator.next().toPath();
            ++count;
            logger.info("Validating grib file: {} ...",
                    parallelValidationPath.toString());
            /*
             * Attempt to find the associated grib file generated by the Fortran
             * version of gribit.
             */
            final Path validationPath = gribPath
                    .resolve(parallelValidationPath.getFileName());
            if (!Files.exists(validationPath)) {
                logger.error(
                        "Unable to find the expected legacy-generated grib file: {} to use for comparison. Failed to validate grib file: {}.",
                        validationPath.toString(),
                        parallelValidationPath.toString());
                continue;
            }

            /*
             * Read the original and parallel grib files.
             */
            final GribFile validationGribFile;
            try {
                validationGribFile = GribFile.loadGribFile(validationPath);
            } catch (InvalidGribException e) {
                logger.error("Failed to load the legacy generated grib file: "
                        + validationPath.toString()
                        + ". Failed to validate grib file: "
                        + parallelValidationPath.toString() + ".", e);
                continue;
            }
            final GribFile parallelValidationGribFile;
            try {
                parallelValidationGribFile = GribFile
                        .loadGribFile(parallelValidationPath);
            } catch (InvalidGribException e) {
                logger.error("Failed to load the Java generated grib file: "
                        + parallelValidationPath.toString()
                        + ". Failed to validate grib file: "
                        + parallelValidationPath.toString() + ".", e);
                continue;
            }
            logger.info(
                    "Successfully loaded legacy generated grib file: {} and Java generated grib file: {} for comparison.",
                    validationPath.toString(),
                    parallelValidationPath.toString());
            final Boolean binaryCompare = compareGribBinary(validationPath,
                    parallelValidationPath);
            if (Boolean.FALSE.equals(binaryCompare)) {
                /*
                 * If the files are not binarily equivalent, perform a detailed
                 * comparison to identify the differences.
                 */
                compareGrib(validationGribFile, parallelValidationGribFile);
            } else if (Boolean.TRUE.equals(binaryCompare)) {
                /*
                 * The files are binarily equivalent.
                 */
                logger.info(
                        "Successfully verified the binary equivalence of grib files: {} and {}.",
                        validationPath.toString(),
                        parallelValidationPath.toString());
            }
            cleanupValidationGrib(parallelValidationPath);
        }
        logger.info(
                "Finished validating {} generated grib file(s) in directory: {}.",
                count, parallelGribPath.toString());
    }

    /**
     * Attempts a binary comparison of the two specified grib files.
     * 
     * @param compare1
     *            the {@link Path} to the first specified grib file
     * @param compare2
     *            the {@link Path} to the second specified grib file
     * @return {@link Boolean#TRUE} if the files are equivalent,
     *         {@link Boolean#FALSE} if the files are not equivalent, or
     *         {@code null} in the case that the specified files could not be
     *         compared.
     */
    private Boolean compareGribBinary(final Path compare1,
            final Path compare2) {
        /*
         * Read the byte contents of the files.
         */
        final byte[] compare1Bytes;
        final byte[] compare2Bytes;

        try {
            compare1Bytes = Files.readAllBytes(compare1);
        } catch (IOException e) {
            logger.error(
                    "Failed to read grib file: " + compare1.toString() + ".",
                    e);
            return null;
        }
        try {
            compare2Bytes = Files.readAllBytes(compare2);
        } catch (IOException e) {
            logger.error(
                    "Failed to read grib file: " + compare2.toString() + ".",
                    e);
            return null;
        }

        if (compare1Bytes.length != compare2Bytes.length) {
            return Boolean.FALSE;
        }

        for (int i = 0; i < compare1Bytes.length; i++) {
            if (compare1Bytes[i] != compare2Bytes[i]) {
                return Boolean.FALSE;
            }
        }

        return Boolean.TRUE;
    }

    private void compareGrib(final GribFile compare1, final GribFile compare2) {
        /*
         * Compare the file headers.
         */
        if (compare1.getHeader().equals(compare2.getHeader())) {
            logger.info("Successfully validated the GRIB headers.");
        } else {
            logger.error("GRIB headers do not match.");
        }

        /*
         * Compare the indicator sections.
         */
        compareIndicatorSections(compare1.getIs(), compare2.getIs());

        /*
         * Compare the product definition sections.
         */
        compareProductDefinitionSections(compare1.getPds(), compare2.getPds());

        /*
         * Compare the grid definition sections.
         */
        compareGridDefinitionSections(compare1.getGds(), compare2.getGds());

        /*
         * Compare the bitmap sections.
         */
        compareBitMapSections(compare1.getBms(), compare2.getBms());

        /*
         * Compare the binary data sections.
         */
        compareBinaryDataSections(compare1.getBds(), compare2.getBds());

    }

    private void compareIndicatorSections(final IndicatorSection compare1,
            final IndicatorSection compare2) {
        /*
         * Compare the total grib size.
         */
        if (compare1.getTotalGribSize() == compare2.getTotalGribSize()) {
            logger.info(
                    "Successfully validated the Grib Indicator Section - Total Grib Size.");
        } else {
            logger.error(
                    "Grib Indicator Section - Total Grib Sizes do not match.");
        }
    }

    private void compareProductDefinitionSections(
            final ProductDefinitionSection compare1,
            final ProductDefinitionSection compare2) {
        /*
         * Compare the number bytes.
         */
        if (compare1.getNumberBytes() == compare2.getNumberBytes()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Number Bytes.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Number Bytes do not match.");
        }

        /*
         * Compare the param table version num.
         */
        if (compare1.getParamTableVersionNum() == compare2
                .getParamTableVersionNum()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Param Table Version Num.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Param Table Version Nums do not match.");
        }

        /*
         * Compare the originating center.
         */
        if (compare1.getOriginatingCenter() == compare2
                .getOriginatingCenter()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Originating Center.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Originating Centers do not match.");
        }

        /*
         * Compare the model id.
         */
        if (compare1.getModelId() == compare2.getModelId()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Model Id.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Model Ids do not match.");
        }

        /*
         * Compare the grid.
         */
        if (compare1.getGrid() == compare2.getGrid()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Grid.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Grids do not match.");
        }

        /*
         * Compare the include GDS flag.
         */
        if (compare1.getIncludeGDS().equals(compare2.getIncludeGDS())) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Include GDS.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Include GDS flags do not match.");
        }

        /*
         * Compare the include BMS flag.
         */
        if (compare1.getIncludeBMS().equals(compare2.getIncludeBMS())) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Include BMS.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Include BMS flags do not match.");
        }

        /*
         * Compare the param unit indicator.
         */
        if (compare1.getParamUnitIndicator() == compare2
                .getParamUnitIndicator()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Param Unit Indicator.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Param Unit Indicators do not match.");
        }

        /*
         * Compare the type level indicator.
         */
        if (compare1.getTypeLevelIndicator() == compare2
                .getTypeLevelIndicator()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Type Level Indicator.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Type Level Indicators do not match.");
        }

        /*
         * Compare the level 1 value.
         */
        if (compare1.getLevelValue1() == compare2.getLevelValue2()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Level 1 Value.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Level 1 Values do not match.");
        }

        /*
         * Compare the level 2 value.
         */
        if (compare1.getLevelValue2() == compare2.getLevelValue2()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Level 2 Value.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Level 2 Values do not match.");
        }

        /*
         * Compare the year.
         */
        if (compare1.getYear() == compare2.getYear()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Year.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Years do not match.");
        }

        /*
         * Compare the month.
         */
        if (compare1.getMonth() == compare2.getMonth()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Month.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Months do not match.");
        }

        /*
         * Compare the day.
         */
        if (compare1.getDay() == compare2.getDay()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Day.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Days do not match.");
        }

        /*
         * Compare the hour.
         */
        if (compare1.getHour() == compare2.getHour()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Hour.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Hours do not match.");
        }

        /*
         * Compare the minute.
         */
        if (compare1.getMinute() == compare2.getMinute()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Minute.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Minutes do not match.");
        }

        /*
         * Compare the forecast time unit.
         */
        if (compare1.getForecastTimeUnit() == compare2.getForecastTimeUnit()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Forecast Time Unit.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Forecast Time Units do not match.");
        }

        /*
         * Compare the p1 period of time.
         */
        if (compare1.getPeriodOfTimeP1() == compare2.getPeriodOfTimeP1()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - P1 Period of Time.");
        } else {
            logger.error(
                    "Grib Product Definition Section - P1 Period of Times do not match.");
        }

        /*
         * Compare the p2 period of time.
         */
        if (compare1.getPeriodOfTimeP2() == compare2.getPeriodOfTimeP2()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - P2 Period of Time.");
        } else {
            logger.error(
                    "Grib Product Definition Section - P2 Period of Times do not match.");
        }

        /*
         * Compare the time range indicator.
         */
        if (compare1.getTimeRangerIndicator() == compare2
                .getTimeRangerIndicator()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Time Range Indicator.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Time Range Indicators do not match.");
        }

        /*
         * Compare the included number.
         */
        if (compare1.getIncludedNumber() == compare2.getIncludedNumber()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Included Number.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Included Numbers do not match.");
        }

        /*
         * Compare the average missing number.
         */
        if (compare1.getAvgMissingNumber() == compare2.getAvgMissingNumber()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Average Missing Number.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Average Missing Numbers do not match.");
        }

        /*
         * Compare the century.
         */
        if (compare1.getCentury() == compare2.getCentury()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Century.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Centuries do not match.");
        }

        /*
         * Compare the sub center.
         */
        if (compare1.getSubCenter() == compare2.getSubCenter()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Sub Center.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Sub Centers do not match.");
        }

        /*
         * Compare the grib set sub center flag.
         */
        if (compare1.getGribSetSubCenter() == compare2.getGribSetSubCenter()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Grib Set Sub Center.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Grib Set Sub Center flags do not match.");
        }

        /*
         * Compare the decimal scale factor.
         */
        if (compare1.getDecimalScaleFactor() == compare2
                .getDecimalScaleFactor()) {
            logger.info(
                    "Successfully validated the Grib Product Definition Section - Decimal Scale Factor.");
        } else {
            logger.error(
                    "Grib Product Definition Section - Decimal Scale Factors do not match.");
        }
    }

    private void compareGridDefinitionSections(
            final GridDefinitionSection<?> compare1,
            final GridDefinitionSection<?> compare2) {
        if (compare1 == null && compare2 == null) {
            /*
             * Grid Definition Section was not written to the file. Nothing to
             * compare.
             */
            return;
        }

        if (compare1 != null && compare2 != null) {
            /*
             * Compare the number bytes.
             */
            if (compare1.getNumberBytes() == compare2.getNumberBytes()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Number Bytes.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Number Bytes do not match.");
            }

            /*
             * Compare the number of vertical coordinates.
             */
            if (compare1.getNumVertCoords() == compare2.getNumVertCoords()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Number Vertical Coordinates.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Number Vertical Coordinates do not match.");
            }

            /*
             * Compare the PV, PL OR 255 flag
             */
            if (compare1.getPvPL255() == compare2.getPvPL255()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - PV, PL, OR 255.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - PV, PL OR 255 flags do not match.");
            }

            /*
             * Compare the data representation type
             */
            if (compare1.getDataRepresentationType() == compare2
                    .getDataRepresentationType()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Data Representation Type.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Data Representation Types do not match.");
            }

            /*
             * Compare the number of X points
             */
            if (compare1.getNx() == compare2.getNx()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Number X Points.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Number X Points do not match.");
            }

            /*
             * Compare the number of Y points
             */
            if (compare1.getNy() == compare2.getNy()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Number Y Points.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Number Y Points do not match.");
            }

            /*
             * Compare the origin latitude
             */
            if (compare1.getOriginLat() == compare2.getOriginLat()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Origin Latitude.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Origin Latitudes do not match.");
            }

            /*
             * Compare the origin longitude
             */
            if (compare1.getOriginLon() == compare2.getOriginLon()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Origin Longitude.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Origin Longitudes do not match.");
            }

            /*
             * Compare the resolution component flag
             */
            if (compare1.getResolutionComponentFlag() == compare2
                    .getResolutionComponentFlag()) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Resolution Component Flag.");
            } else {
                logger.error(
                        "Grib Grid Definition Section - Resolution Component Flags do not match.");
            }

            if (compare1 instanceof LambertConformalGridDefinitionSection
                    && compare2 instanceof LambertConformalGridDefinitionSection) {
                compareGridDefinitionSection(
                        (LambertConformalGridDefinitionSection) compare1,
                        (LambertConformalGridDefinitionSection) compare2);
            } else if (compare1 instanceof LatLonAndGaussianAndStaggeredGridDefinitionSection
                    && compare2 instanceof LatLonAndGaussianAndStaggeredGridDefinitionSection) {
                compareGridDefinitionSection(
                        (LatLonAndGaussianAndStaggeredGridDefinitionSection) compare1,
                        (LatLonAndGaussianAndStaggeredGridDefinitionSection) compare2);
            } else if (compare1 instanceof MercatorGridDefinitionSection
                    && compare2 instanceof MercatorGridDefinitionSection) {
                compareGridDefinitionSection(
                        (MercatorGridDefinitionSection) compare1,
                        (MercatorGridDefinitionSection) compare2);
            } else if (compare1 instanceof PolarStereographicGridDefinitionSection
                    && compare2 instanceof PolarStereographicGridDefinitionSection) {
                compareGridDefinition(
                        (PolarStereographicGridDefinitionSection) compare1,
                        (PolarStereographicGridDefinitionSection) compare2);
            } else {
                logger.error(
                        "Grib Grid Definition Sections do not match - different types of grid definitions.");
            }
        } else {
            logger.error("Grid Definition Sections do not match.");
        }
    }

    private void compareGridDefinitionSection(
            LambertConformalGridDefinitionSection compare1,
            LambertConformalGridDefinitionSection compare2) {
        /*
         * Compare longitude of meridian parallel to the x-axis
         */
        if (compare1.getLonMeridianParallelX() == compare2
                .getLonMeridianParallelX()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Longitude Meridian Parallel X.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Longitude Meridian Parallel X do not match.");
        }

        /*
         * Compare grid length x
         */
        if (compare1.getGridLengthX() == compare2.getGridLengthX()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Grid Length X.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Grid Length X do not match.");
        }

        /*
         * Compare grid length y
         */
        if (compare1.getGridLengthY() == compare2.getGridLengthY()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Grid Length Y.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Grid Length Y do not match.");
        }

        /*
         * Compare the projection center flag
         */
        if (compare1.getProjectionCenterFlag() == compare2
                .getProjectionCenterFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Projection Center Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Projection Center Flags do not match.");
        }

        /*
         * Compare the scanning mode flag
         */
        if (compare1.getScanningModeFlag() == compare2.getScanningModeFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Scanning Mode Flag.");
        } else {
            logger.error("Grib Grid Definition Section - Scanning Mode Flag.");
        }

        /*
         * Compare the first latitude cone cut
         */
        if (compare1.getFirstLatConeCut() == compare2.getFirstLatConeCut()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - First Latitude Cone Cut.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - First Latitude Cone Cut do not match.");
        }

        /*
         * Compare the second latitude
         */
        if (compare1.getSecondLat() == compare2.getSecondLat()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Second Latitude.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Second Latitudes do not match.");
        }

        /*
         * Compare the south pole latitude
         */
        if (compare1.getLatSouthPole() == compare2.getLatSouthPole()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - South Pole Latitude.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - South Pole Latitudes do not match.");
        }

        /*
         * Compare the south pole longitude
         */
        if (compare1.getLonSouthPole() == compare2.getLonSouthPole()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - South Pole Longitude.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - South Pole Longitudes do not match.");
        }
    }

    private void compareGridDefinitionSection(
            LatLonAndGaussianAndStaggeredGridDefinitionSection compare1,
            LatLonAndGaussianAndStaggeredGridDefinitionSection compare2) {
        /*
         * Compare the ninth value
         */
        if (compare1.getNinthValue() == compare2.getNinthValue()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Ninth Values.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Ninth Values do not match.");
        }

        /*
         * Compare the tenth value
         */
        if (compare1.getTenthValue() == compare2.getTenthValue()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Tenth Values.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Tenth Values do not match.");
        }

        /*
         * Compare the eleventh value
         */
        if (compare1.getEleventhValue() == compare2.getEleventhValue()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Eleventh Values.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Eleventh Values do not match.");
        }

        /*
         * Compare the twelfth value
         */
        if (compare1.getTwelfthValue() == compare2.getTwelfthValue()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Twelfth Values.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Twelfth Values do not match.");
        }

        /*
         * Compare the scanning mode flag
         */
        if (compare1.getScanningModeFlag() == compare2.getScanningModeFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Scanning Mode Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Scanning Mode Flags do not match.");
        }

        /*
         * Compare the number points
         */
        if ((compare1.getNumberPoints() != null
                && compare2.getNumberPoints() != null)
                && (compare1.getNumberPoints().length == compare2
                        .getNumberPoints().length)) {
            boolean numberPointsMatch = true;
            int[] numberPoints1 = compare1.getNumberPoints();
            int[] numberPoints2 = compare2.getNumberPoints();
            for (int i = 0; i < numberPoints1.length; i++) {
                if (numberPoints1[i] != numberPoints2[i]) {
                    logger.error(
                            "Grib Grid Definition Section - Number Points do not match.");
                    numberPointsMatch = false;
                    break;
                }
            }

            if (numberPointsMatch) {
                logger.info(
                        "Successfully validated the Grib Grid Definition Section - Number Points.");
            }
        } else if (compare1.getNumberPoints() == null
                && compare2.getNumberPoints() == null) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Number Points.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Number Points do not match.");
        }
    }

    private void compareGridDefinitionSection(
            MercatorGridDefinitionSection compare1,
            MercatorGridDefinitionSection compare2) {
        /*
         * Compare the lat project cylinder intersect
         */
        if (compare1.getLatProjCylinderIntersect() == compare2
                .getLatProjCylinderIntersect()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Scanning Mode Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Lat Project Cylinder Intersects do not match.");
        }

        /*
         * Compare the scanning mode flag
         */
        if (compare1.getScanningModeFlag() == compare2.getScanningModeFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Scanning Mode Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Scanning Mode Flags do not match.");
        }
    }

    private void compareGridDefinition(
            PolarStereographicGridDefinitionSection compare1,
            PolarStereographicGridDefinitionSection compare2) {
        /*
         * Compare the longitude meridian parallel X
         */
        if (compare1.getLonMeridianParallelX() == compare2
                .getLonMeridianParallelX()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Longitude Meridian Parallel X.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Longitude Meridian Parallel X do not match.");
        }

        /*
         * Compare grid length x
         */
        if (compare1.getGridLengthX() == compare2.getGridLengthX()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Grid Length X.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Grid Length X do not match.");
        }

        /*
         * Compare grid length y
         */
        if (compare1.getGridLengthY() == compare2.getGridLengthY()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Grid Length Y.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Grid Length Y do not match.");
        }

        /*
         * Compare the projection center flag
         */
        if (compare1.getProjectionCenterFlag() == compare2
                .getProjectionCenterFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Projection Center Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Projection Center Flags do not match.");
        }

        /*
         * Compare the scanning mode flag
         */
        if (compare1.getScanningModeFlag() == compare2.getScanningModeFlag()) {
            logger.info(
                    "Successfully validated the Grib Grid Definition Section - Scanning Mode Flag.");
        } else {
            logger.error(
                    "Grib Grid Definition Section - Scanning Mode Flags do not match.");
        }
    }

    private void compareBitMapSections(final BitMapSection compare1,
            final BitMapSection compare2) {
        if (compare1 == null && compare2 == null) {
            /*
             * BitMap Section was not written to the file. Nothing to compare.
             */
            return;
        }

        /*
         * Verify that the BitMap Section has been written to both files.
         */
        if (compare1 != null && compare2 != null) {
            /*
             * Compare the number of bytes.
             */
            if (compare1.getNumberBytes() == compare2.getNumberBytes()) {
                logger.info(
                        "Successfully validated the Grib Bit Map Definition Section - Number Bytes.");
            } else {
                logger.error(
                        "Grib Bit Map Definition Section - Number Bytes do not match.");
            }

            /*
             * Compare the ib flags.
             */
            if (compare1.getIbFlag().equals(compare2.getIbFlag())) {
                logger.info(
                        "Successfully validated the Grib Bit Map Definition Section - IB Flag.");
            } else {
                logger.error(
                        "Grib Bit Map Definition Section - IB Flags do not match.");
            }

            /*
             * Compare the bitmaps.
             */
            if (compare1.getBitMap().length == compare2.getBitMap().length) {
                boolean bitMapMatch = true;
                int[] bitMap1 = compare1.getBitMap();
                int[] bitMap2 = compare2.getBitMap();
                for (int i = 0; i < bitMap1.length; i++) {
                    if (bitMap1[i] != bitMap2[i]) {
                        logger.error(
                                "Grib Bit Map Definition Section - Bit Maps do not match.");
                        bitMapMatch = false;
                        break;
                    }
                }

                if (bitMapMatch) {
                    logger.info(
                            "Successfully validated the Grib Bit Map Definition Section - Bit Map.");
                }
            } else {
                logger.error(
                        "Grib Bit Map Definition Section - Bit Maps do not match.");
            }
        } else {
            logger.error(
                    "Grib Bit Map Definition Section - BitMap Sections do not match.");
        }
    }

    private void compareBinaryDataSections(final BinaryDataSection compare1,
            final BinaryDataSection compare2) {
        /*
         * Compare the number of bytes.
         */
        if (compare1.getNumberBytes() == compare2.getNumberBytes()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Number Bytes.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Number Bytes do not match.");
        }

        /*
         * Compare the number of fill bits.
         */
        if (compare1.getNumberFillBits() == compare2.getNumberFillBits()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Number Fill Bits.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Number Fill Bits do not match.");
        }

        /*
         * Compare the scale factor.
         */
        if (compare1.getScaleFactor() == compare2.getScaleFactor()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Scale Factor.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Scale Factors do not match.");
        }

        /*
         * Compare the packed bits.
         */
        if (compare1.getBitsToPack() == compare2.getBitsToPack()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Packed Bits.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Packed Bits do not match.");
        }

        /*
         * Compare the packed data.
         */
        if (compare1.getPackedData().length == compare2
                .getPackedData().length) {
            boolean packedDataMatch = true;
            short[] packedData1 = compare1.getPackedData();
            short[] packedData2 = compare2.getPackedData();
            for (int i = 0; i < packedData1.length; i++) {
                if (packedData1[i] != packedData2[i]) {
                    logger.error(
                            "Grib Binary Data Section - Packed Data do not match.");
                    packedDataMatch = false;
                    break;
                }
            }
            if (packedDataMatch) {
                logger.info(
                        "Successfully validated the Grib Binary Data Section - Packed Data.");
            }
        } else {
            logger.error(
                    "Grib Binary Data Section - Packed Data do not match.");
        }

        compareTable11Flags(compare1.getTable11Flags(),
                compare2.getTable11Flags());
    }

    private void compareTable11Flags(final Table11Flags compare1,
            final Table11Flags compare2) {
        /*
         * data flag
         */
        if (compare1.getData() == compare2.getData()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - data.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Data flags do not match.");
        }

        /*
         * packing flag
         */
        if (compare1.getPacking() == compare2.getPacking()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - packing.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Packing flags do not match.");
        }

        /*
         * original data type flag
         */
        if (compare1.getOriginalDataType() == compare2.getOriginalDataType()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - original data type.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Original Data Type flags do not match.");
        }

        /*
         * octet 14 flag
         */
        if (compare1.getOctet14() == compare2.getOctet14()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - octet 14.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Octet 14 flags do not match.");
        }

        /*
         * grid point type flag
         */
        if (compare1.getGridPointType() == compare2.getGridPointType()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - grid point type.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Grid Point Type flags do not match.");
        }

        /*
         * secondary bit map flag
         */
        if (compare1.getSecondaryBitMaps() == compare2.getSecondaryBitMaps()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - secondary bit map.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Secondary Bit Map flags do not match.");
        }

        /*
         * second order values width flag
         */
        if (compare1.getSecondOrderValuesWidth() == compare2
                .getSecondOrderValuesWidth()) {
            logger.info(
                    "Successfully validated the Grib Binary Data Section - Table 11 Flags - second order values width.");
        } else {
            logger.error(
                    "Grib Binary Data Section - Table 11 Flags - Second Order Values Width flags do not match.");
        }
    }

    private void cleanupValidationGrib(final Path gribPath) {
        try {
            logger.info("Removing validation grib file: {} ...",
                    gribPath.toString());
            Files.delete(gribPath);
        } catch (IOException e) {
            logger.error("Failed to delete validation grib file: "
                    + gribPath.toString() + ".", e);
        }
    }
}