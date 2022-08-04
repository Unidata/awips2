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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.awt.Rectangle;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Calendar;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;

import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.XmrgFile.XmrgHeader;
import com.raytheon.uf.edex.plugin.mpe.CommonMPEUtils;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.grib.HPEGribFileRunnerFactory;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.nc.MosaicNetcdfFileWrapper;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.gif.HpeGifImageGenerator;
import org.locationtech.jts.geom.Coordinate;

/**
 * Common utilities for HPE Field Gen.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2016 5631       bkowal      Initial creation
 * Sep 19, 2016 5631       bkowal      Added {@link #buildCategoryName(int)} and
 *                                     {@link #convertDoubleArray(int, int, double[][], double, int, int, double[][])}.
 * Sep 27, 2016 5631       bkowal      Added {@link #interpolate2DFloatArray(float[][], float[][], float, float, float, float[][])}.
 * Oct 05, 2016 5631       bkowal      Added {@link #convertLatLonToScaledHrap(double, double, double)},
 *                                     and {@link #convertFloatArray(int, int, float[][], float, int, int, float[][])}.
 * Oct 13, 2016 5631       bkowal      Added support for writing xmrg mosaic files. Ideally methods will later
 *                                     be relocated to an abstraction.
 * Oct 18, 2016  5631      bkowal      Utilize {@link HpeGifImageGenerator} for gif image generation.                                   
 * Oct 26, 2016 5631       bkowal      Prepared for NetCDF file generation using {@link MosaicNetcdfFileWrapper}.                                    
 * Oct 19, 2016 5631       bkowal      Updated to use {@link HPEGribFileRunnerFactory}.
 * Jan 04, 2016 5631       bkowal      Temporarily disabled grib file output until it can be
 *                                     updated to use the correct directories for inputs.
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPEFieldgenUtils {

    private static final String ALT_GRIB_FILE_FMT = "%s.grib";

    private static final String ALT_NETCDF_FILE_FMT = "%s%s.nc";

    private static final String ALT_GIF_FILE_FMT = "%s%s.gif";

    protected HPEFieldgenUtils() {
    }

    /**
     * Converts the specified latitude and longitude to scaled hrap based on the
     * specified {@link HrapGridFactor}. Based on:
     * hpe_fieldgen/TEXT/empe_utils.c.
     * 
     * @param lon
     *            the specified longitude
     * @param lat
     *            the specified latitude
     * @param hrapGridFactor
     *            the specified {@link HrapGridFactor}
     * @return the scaled hrap as a {@link Coordinate}
     */
    public static Coordinate convertLatLonToScaledHrap(final double lon,
            final double lat, final HrapGridFactor hrapGridFactor) {
        if (hrapGridFactor == null) {
            throw new IllegalArgumentException(
                    "Required argument 'hrapGridFactor' cannot be NULL.");
        }
        return convertLatLonToScaledHrap(lon, lat,
                (double) hrapGridFactor.getNum());
    }

    /**
     * Converts the specified latitude and longitude to scaled hrap based on the
     * specified factor. Based on: hpe_fieldgen/TEXT/empe_utils.c.
     * 
     * @param lon
     *            the specified longitude
     * @param lat
     *            the specified latitude
     * @param factor
     *            the specified factor
     * @return the scaled hrap as a {@link Coordinate}
     */
    public static Coordinate convertLatLonToScaledHrap(final double lon,
            final double lat, final double factor) {
        final Coordinate hrapCoord = CommonMPEUtils
                .convertLatLonToHrapByReference(lon, lat);
        hrapCoord.x *= factor;
        hrapCoord.y *= factor;
        return hrapCoord;
    }

    /**
     * Builds the category name based on the specified time value. Based on:
     * 'buildCategoryName' in:
     * 
     * @param timeValue
     *            the specified time value
     * @return the category name that was built
     * @throws HPEFieldGenConfigurationException
     */
    public static String buildCategoryName(final int timeValue)
            throws HPEFieldGenConfigurationException {
        if (timeValue <= 99) {
            /*
             * The category is minute if the duration is less than 99 minutes.
             */
            return String.format("M%02d", timeValue);
        } else {
            /*
             * The category is hour if the duration is greater than 99 minutes,
             * but less than 99 hours.
             */
            int catHours = timeValue / 60;
            if (catHours <= 99) {
                return String.format("H%02d", catHours);
            } else {
                /*
                 * The category is day if the duration is greater than 99 hour,
                 * but less than 99 days.
                 */
                int catDays = catHours / 24;
                if (catDays <= 99) {
                    /*
                     * The category is day if the duration is greater than 99
                     * hour, but less than 99 days.
                     */
                    return String.format("D%02d", catDays);
                } else {
                    throw new HPEFieldGenConfigurationException(
                            "Failed to build a category name. An invalid time value: "
                                    + timeValue + " has been specified.");
                }
            }
        }
    }

    /**
     * Expands or diminishes the specified source array into a larger or smaller
     * specified destination array respectively. Both the source and destination
     * arrays must be two-dimensional double arrays. Based on:
     * 'convertDoubleArray' in hpe_fieldgen/TEXT/empe_utils.c.
     * 
     * @param numberRowsOrigin
     *            number of rows to use in the source array
     * @param numberColsOrigin
     *            number of columns to use in the source array
     * @param source
     *            the specified source array
     * @param defaultValue
     *            default value to place in the destination
     * @param numberRowsDest
     *            number of rows to use in the destination array
     * @param numberColsDest
     *            number of columns to use in the destination array
     * @param destination
     *            the specified destination array; will contain the results of
     *            the operation
     */
    public static void convertDoubleArray(final int numberRowsOrigin,
            final int numberColsOrigin, final double[][] source,
            final double defaultValue, final int numberRowsDest,
            final int numberColsDest, final double[][] destination) {
        if (source == null) {
            throw new IllegalArgumentException(
                    "Required argument 'source' cannot be NULL.");
        }
        if (destination == null) {
            throw new IllegalArgumentException(
                    "Required argument 'target' cannot be NULL.");
        }
        if ((numberRowsOrigin <= numberRowsDest)
                && (numberColsOrigin <= numberColsDest)) {
            /*
             * Expand the array.
             */
            int rowFactor = numberRowsDest / numberRowsOrigin;
            int columnFactor = numberColsDest / numberColsOrigin;

            int rowRemainder = numberRowsDest % numberRowsOrigin;
            int columnRemainder = numberColsDest % numberColsOrigin;

            for (int i = 0; i < numberRowsOrigin; i++) {
                for (int j = 0; j < numberColsOrigin; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            destination[indexX][indexY] = source[i][j];
                        }
                    }
                }
            }

            if (rowRemainder > 0) {
                for (int i = (numberRowsDest
                        - rowRemainder); i < numberRowsDest; i++) {
                    /*
                     * Initialize any remaining rows to the default.
                     */
                    // Determine how many columns within the row need to be
                    // initialized.
                    if (columnRemainder == 0) {
                        Arrays.fill(destination[i], defaultValue);
                    } else {
                        Arrays.fill(destination[i],
                                (numberColsDest - columnRemainder),
                                numberColsDest, defaultValue);
                    }
                }
            } else if (columnRemainder > 0) {
                /*
                 * Initialize any remaining columns to the default.
                 */
                for (int i = 0; i < numberRowsDest; i++) {
                    Arrays.fill(destination[i],
                            (numberColsDest - columnRemainder), numberColsDest,
                            defaultValue);
                }
            }
        } else if ((numberRowsOrigin > numberRowsDest)
                && (numberColsOrigin > numberColsDest)) {
            /*
             * diminish the array
             */
            int count[][] = new int[numberRowsDest][numberColsDest];
            int rowFactor = numberRowsOrigin / numberRowsDest;
            int columnFactor = numberColsOrigin / numberColsDest;
            /*
             * Arrays automatically initialized to 0, no initialization step
             * required.
             */

            /*
             * summarize for the target array
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            if (Math.abs(source[indexX][indexY]
                                    - defaultValue) > 0.001) {
                                destination[i][j] += source[indexX][indexY];
                                count[i][j]++;
                            }
                        }
                    }
                }
            }

            /*
             * Average for the target array if the count value greater than 0,
             * otherwise, set it to missing.
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    if (count[i][j] > 0) {
                        destination[i][j] /= count[i][j];
                    } else {
                        destination[i][j] = defaultValue;
                    }
                }
            }
        }
    }

    /**
     * Expands or diminishes the specified source array into a larger or smaller
     * specified destination array respectively. Both the source and destination
     * arrays must be two-dimensional float arrays. Based on:
     * 'convertFloatArray' in hpe_fieldgen/TEXT/empe_utils.c.
     * 
     * @param numberRowsOrigin
     *            number of rows to use in the source array
     * @param numberColsOrigin
     *            number of columns to use in the source array
     * @param source
     *            the specified source array
     * @param defaultValue
     *            default value to place in the destination
     * @param numberRowsDest
     *            number of rows to use in the destination array
     * @param numberColsDest
     *            number of columns to use in the destination array
     * @param destination
     *            the specified destination array; will contain the results of
     *            the operation
     */
    public static void convertFloatArray(final int numberRowsOrigin,
            final int numberColsOrigin, final float[][] source,
            final float defaultValue, final int numberRowsDest,
            final int numberColsDest, final float[][] destination) {
        if (source == null) {
            throw new IllegalArgumentException(
                    "Required argument 'source' cannot be NULL.");
        }
        if (destination == null) {
            throw new IllegalArgumentException(
                    "Required argument 'target' cannot be NULL.");
        }
        if ((numberRowsOrigin <= numberRowsDest)
                && (numberColsOrigin <= numberColsDest)) {
            /*
             * Expand the array.
             */
            int rowFactor = numberRowsDest / numberRowsOrigin;
            int columnFactor = numberColsDest / numberColsOrigin;

            int rowRemainder = numberRowsDest % numberRowsOrigin;
            int columnRemainder = numberColsDest % numberColsOrigin;

            for (int i = 0; i < numberRowsOrigin; i++) {
                for (int j = 0; j < numberColsOrigin; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            destination[indexX][indexY] = source[i][j];
                        }
                    }
                }
            }

            if (rowRemainder > 0) {
                for (int i = (numberRowsDest
                        - rowRemainder); i < numberRowsDest; i++) {
                    /*
                     * Initialize any remaining rows to the default.
                     */
                    // Determine how many columns within the row need to be
                    // initialized.
                    if (columnRemainder == 0) {
                        Arrays.fill(destination[i], defaultValue);
                    } else {
                        Arrays.fill(destination[i],
                                (numberColsDest - columnRemainder),
                                numberColsDest, defaultValue);
                    }
                }
            } else if (columnRemainder > 0) {
                /*
                 * Initialize any remaining columns to the default.
                 */
                for (int i = 0; i < numberRowsDest; i++) {
                    Arrays.fill(destination[i],
                            (numberColsDest - columnRemainder), numberColsDest,
                            defaultValue);
                }
            }
        } else if ((numberRowsOrigin > numberRowsDest)
                && (numberColsOrigin > numberColsDest)) {
            /*
             * diminish the array
             */
            int count[][] = new int[numberRowsDest][numberColsDest];
            int rowFactor = numberRowsOrigin / numberRowsDest;
            int columnFactor = numberColsOrigin / numberColsDest;
            /*
             * Arrays automatically initialized to 0, no initialization step
             * required.
             */

            /*
             * summarize for the target array
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            if (Math.abs(source[indexX][indexY]
                                    - defaultValue) > 0.001) {
                                destination[i][j] += source[indexX][indexY];
                                count[i][j]++;
                            }
                        }
                    }
                }
            }

            /*
             * Average for the target array if the count value greater than 0,
             * otherwise, set it to missing.
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    if (count[i][j] > 0) {
                        destination[i][j] /= count[i][j];
                    } else {
                        destination[i][j] = defaultValue;
                    }
                }
            }
        }
    }

    /**
     * Expands or diminishes the specified source array into a larger or smaller
     * specified destination array respectively. Both the source and destination
     * arrays must be two-dimensional short arrays. Based on:
     * 'convertShortArray' in hpe_fieldgen/TEXT/empe_utils.c.
     * 
     * @param numberRowsOrigin
     *            number of rows to use in the source array
     * @param numberColsOrigin
     *            number of columns to use in the source array
     * @param source
     *            the specified source array
     * @param defaultValue
     *            default value to place in the destination
     * @param numberRowsDest
     *            number of rows to use in the destination array
     * @param numberColsDest
     *            number of columns to use in the destination array
     * @param destination
     *            the specified destination array; will contain the results of
     *            the operation
     */
    public static void convertShortArray(final int numberRowsOrigin,
            final int numberColsOrigin, final short[][] source,
            final short defaultValue, final int numberRowsDest,
            final int numberColsDest, final short[][] destination) {
        if (source == null) {
            throw new IllegalArgumentException(
                    "Required argument 'source' cannot be NULL.");
        }
        if (destination == null) {
            throw new IllegalArgumentException(
                    "Required argument 'target' cannot be NULL.");
        }
        if ((numberRowsOrigin <= numberRowsDest)
                && (numberColsOrigin <= numberColsDest)) {
            /*
             * Expand the array.
             */
            int rowFactor = numberRowsDest / numberRowsOrigin;
            int columnFactor = numberColsDest / numberColsOrigin;

            int rowRemainder = numberRowsDest % numberRowsOrigin;
            int columnRemainder = numberColsDest % numberColsOrigin;

            for (int i = 0; i < numberRowsOrigin; i++) {
                for (int j = 0; j < numberColsOrigin; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            destination[indexX][indexY] = source[i][j];
                        }
                    }
                }
            }

            if (rowRemainder > 0) {
                for (int i = (numberRowsDest
                        - rowRemainder); i < numberRowsDest; i++) {
                    /*
                     * Initialize any remaining rows to the default.
                     */
                    // Determine how many columns within the row need to be
                    // initialized.
                    if (columnRemainder == 0) {
                        Arrays.fill(destination[i], defaultValue);
                    } else {
                        Arrays.fill(destination[i],
                                (numberColsDest - columnRemainder),
                                numberColsDest, defaultValue);
                    }
                }
            } else if (columnRemainder > 0) {
                /*
                 * Initialize any remaining columns to the default.
                 */
                for (int i = 0; i < numberRowsDest; i++) {
                    Arrays.fill(destination[i],
                            (numberColsDest - columnRemainder), numberColsDest,
                            defaultValue);
                }
            }
        } else if ((numberRowsOrigin > numberRowsDest)
                && (numberColsOrigin > numberColsDest)) {
            /*
             * diminish the array
             */
            int count[][] = new int[numberRowsDest][numberColsDest];
            int rowFactor = numberRowsOrigin / numberRowsDest;
            int columnFactor = numberColsOrigin / numberColsDest;
            /*
             * Arrays automatically initialized to 0, no initialization step
             * required.
             */

            /*
             * summarize for the target array
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    for (int k = 0; k < rowFactor; k++) {
                        for (int m = 0; m < columnFactor; m++) {
                            int indexX = i * rowFactor + k;
                            int indexY = j * columnFactor + m;
                            if (Math.abs(source[indexX][indexY]
                                    - defaultValue) > 0.001) {
                                destination[i][j] += source[indexX][indexY];
                                count[i][j]++;
                            }
                        }
                    }
                }
            }

            /*
             * Average for the target array if the count value greater than 0,
             * otherwise, set it to missing.
             */
            for (int i = 0; i < numberRowsDest; i++) {
                for (int j = 0; j < numberColsDest; j++) {
                    if (count[i][j] > 0) {
                        destination[i][j] /= count[i][j];
                    } else {
                        destination[i][j] = defaultValue;
                    }
                }
            }
        }
    }

    /**
     * Interpolates the two specified 2-dimensional float arrays into a single
     * 2-dimensional float array specified as the destination.
     * 
     * @param array1
     *            the first of the specified two dimensional arrays to
     *            interpolate
     * @param array2
     *            the second of the specified two dimensional arrays to
     *            interpolate
     * @param missingValue
     *            default missing value to place in the array under certain
     *            conditions
     * @param weight1
     *            weight value of the first array that is specified
     * @param weight2
     *            weight value of the second array that is specified
     * @param destination
     *            the specified destination two-dimensional float array
     */
    public static void interpolate2DFloatArray(final float[][] array1,
            final float[][] array2, final float missingValue,
            final float weight1, final float weight2,
            final float[][] destination) {
        if ((weight1 + weight2) < 0.001) {
            for (int i = 0; i < destination.length; i++) {
                Arrays.fill(destination[i], missingValue);
            }
            return;
        }

        final float factor1 = weight1 / (weight1 + weight2);
        final float factor2 = weight2 / (weight1 + weight2);

        for (int i = 0; i < destination.length; i++) {
            for (int j = 0; j < destination[i].length; j++) {
                if ((Math.abs(array1[i][j] - missingValue) > 0.001)
                        && (Math.abs(array2[i][j] - missingValue) > 0.001)) {
                    destination[i][j] = (array1[i][j] * factor1)
                            + (array2[i][j] * factor2);
                } else {
                    destination[i][j] = missingValue;
                }
            }
        }
    }

    /*
     * Based on: hpe_fieldgen/TEXT/write_array.c.
     */
    public static void writeXmrg(final Rectangle geoGridData,
            final double factor, final boolean replaceMissing,
            final double[][] mosaic, final Path xmrgPath,
            final Calendar runDateTime, final String userId,
            final String processFlag) throws Exception {
        /*
         * Determine the size of the output mosaic.
         */
        final int numColumns = (int) geoGridData.width;
        final int numRows = (int) geoGridData.height;
        final short[] xmrgMosaic = new short[numRows * numColumns];

        for (int i = 0; i < numRows; i++) {
            for (int j = 0; j < numColumns; j++) {
                short value = (short) (mosaic[i][j] * factor + 0.5);
                if (replaceMissing && (value < 0)) {
                    value = 0;
                }
                final int index = i * numColumns + j;
                xmrgMosaic[index] = value;
            }
        }

        XmrgFile xmrgFile = new XmrgFile();
        final Calendar xmrgDateTime = TimeUtil.newCalendar(runDateTime);
        // 0 seconds and milliseconds
        TimeUtil.minCalendarFields(xmrgDateTime, Calendar.SECOND,
                Calendar.MILLISECOND);

        XmrgHeader xmrgHeader = new XmrgHeader();
        xmrgHeader.setSaveDate(xmrgDateTime.getTime());
        xmrgHeader.setValidDate(xmrgDateTime.getTime());
        xmrgHeader.setUserId(userId);
        xmrgHeader.setProcessFlag(processFlag);

        xmrgFile.setHeader(xmrgHeader);
        xmrgFile.setHrapExtent(geoGridData);
        xmrgFile.setData(xmrgMosaic);

        xmrgFile.save(xmrgPath.toFile(),
                FilePermissionConstants.POSIX_FILE_SET);
    }

    /**
     * Will generate gifs, netcdf files, and jpegs using the specified mosaic
     * data in accordance with Apps_defaults configuration (jpeg generation is
     * not currently supported). Based on:
     * hpe_fieldgen/TEXT/write_formatted_xmrg.c.
     * 
     * @param geoGridData
     *            global HRAP lowerleft-corner bin and dimension and dimension
     *            of the RFC estimation domain
     * @param alternateFormatFlags
     *            identifies which alternate file formats to generate as well as
     *            where the alternate files should be saved
     * @param mosaic
     *            the specified mosaic data
     * @param xmrgPath
     *            the {@link Path} to the xmrg file that was generated for the
     *            current mosaic
     * @param processFlag
     *            {@link String}-based identifier identifying the type of mosaic
     *            data
     * @param geoFileData
     *            contains geographic information about various entities within
     *            the United States.
     * @param netCDFDirProperty_tmp
     *            the name of the netcdf output directory property for the
     *            current mosaic. This argument is only required for as long as
     *            parallel execution is implemented. (TODO: remove this argument
     *            when parallel execution is eliminated)
     * @param gifDirProperty_tmp
     *            the name of the gif output directory property for the current
     *            mosaic. This argument is only required for as long as parallel
     *            execution is implemented. (TODO: remove this argument when
     *            parallel execution is eliminated)
     * @param logger
     *            {@link Logger} associated with the current mosaic generator
     */
    public static void convertXmrgMultiFormat(final Rectangle geoGridData,
            final IXmrgAlternateFormatFlags alternateFormatFlags,
            final double[][] mosaic, final Path xmrgPath,
            final String processFlag, final GeoData geoFileData,
            final String formattedXmrgDate, final double autoGraphicScale,
            final String netCDFDirProperty_tmp, final String gifDirProperty_tmp,
            final Logger logger) {
        /*
         * The name of the xmrg file is used to determine names of the files
         * generated for the alternate formats.
         */
        final String xmrgFileName = xmrgPath.getFileName().toString();
        if (Boolean.TRUE.equals(alternateFormatFlags.getSaveGRIB())) {
            /*
             * Determine the name of the grib file that will be generated.
             */
            final String gribOutputName = String.format(ALT_GRIB_FILE_FMT,
                    xmrgFileName);
            try {
                // TODO: uncomment when the input file retrieval is updated to
                // utilize the parallel directory.
                /*
                 * HPEGribFileRunnerFactory.getInstance()
                 * .lookupRunner(gribOutputName, processFlag, xmrgFileName)
                 * .execute();
                 */
            } catch (Exception e) {
                logger.error("Unable to convert xmrg file: " + xmrgFileName
                        + " -> grib file: " + gribOutputName + ".", e);
            }
        }

        if (Boolean.TRUE.equals(alternateFormatFlags.getSaveNetCDF())) {
            /*
             * Create and save a netcdf file.
             */

            // Build the NetCDF file name.
            String prefix = (alternateFormatFlags.getNetCDFId() == null)
                    ? StringUtils.EMPTY
                    : alternateFormatFlags.getNetCDFId().trim();
            final String netCDFFileName = String.format(ALT_NETCDF_FILE_FMT,
                    prefix, xmrgFileName);
            Path netCDFPath = null;
            if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
                try {
                    netCDFPath = AppsDefaultsConversionWrapper
                            .getPathForToken(netCDFDirProperty_tmp)
                            .resolve(netCDFFileName);
                } catch (AppsDefaultsPathException e) {
                    logger.error(
                            "Failed to retrieve the NetCDF Output directory for "
                                    + AppsDefaults.NAME + " token: "
                                    + netCDFDirProperty_tmp
                                    + ". Skipping NetCDF generation.",
                            e);
                }
            } else {
                netCDFPath = alternateFormatFlags.getNetCDFPath()
                        .resolve(netCDFFileName);
            }

            // TODO: uncomment when NetCDF Java library path issues have been
            // resolved.
            /*
             * MosaicNetcdfFileWrapper mosaicNetcdfFileWrapper = new
             * MosaicNetcdfFileWrapper( mosaic, processFlag, geoGridData,
             * formattedXmrgDate); mosaicNetcdfFileWrapper.generate(netCDFPath);
             */
        }

        if (Boolean.TRUE.equals(alternateFormatFlags.getSaveGIF())
                && geoFileData != null) {
            /*
             * Create and save a gif file.
             */

            // Build the GIF file name.
            String prefix = (alternateFormatFlags.getGifId() == null)
                    ? StringUtils.EMPTY
                    : alternateFormatFlags.getGifId().trim();
            final String gifFileName = String.format(ALT_GIF_FILE_FMT, prefix,
                    xmrgFileName);
            Path gifPath = null;
            if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
                try {
                    gifPath = AppsDefaultsConversionWrapper
                            .getPathForToken(gifDirProperty_tmp)
                            .resolve(gifFileName);
                } catch (AppsDefaultsPathException e) {
                    logger.error(
                            "Failed to retrieve the GIF Output directory for "
                                    + AppsDefaults.NAME + " token: "
                                    + netCDFDirProperty_tmp
                                    + ". Skipping GIF generation.",
                            e);
                }
            } else {
                gifPath = alternateFormatFlags.getGifPath()
                        .resolve(gifFileName);
            }

            HpeGifImageGenerator gifGenerator = new HpeGifImageGenerator(
                    geoFileData, geoGridData, mosaic, formattedXmrgDate,
                    autoGraphicScale);
            gifGenerator.generate(gifPath);
        }

        if (Boolean.TRUE.equals(alternateFormatFlags.getSaveJPEG())) {
            logger.info(
                    "JPEG support has not yet been implemented. The recommendation is to use GIF.");
        }
    }
}