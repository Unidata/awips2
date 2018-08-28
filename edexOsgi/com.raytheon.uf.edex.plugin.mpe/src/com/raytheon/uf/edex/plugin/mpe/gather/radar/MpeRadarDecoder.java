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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Base decoder for radar products.
 *
 * Ported from the common code of decode_dhr_dsp/TEXT/write_decoded_dhr.c,
 * decode_dhr_dsp/TEXT/write_decoded_dsp.c,
 * hydroapps/precip_proc/bin/Run_DecodeDHR, and
 * hydroapps/precip_proc/bin/Run_DecodeDSP
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 01, 2016 5588       nabowle     Initial creation
 * Dec 16, 2016 5588       nabowle     Throw exceptions for invalid azimuth and range.
 * Jul 19, 2018 5588       mapeters    Account for legacy/java decode paths using separate
 *                                     gather dirs
 *
 * </pre>
 *
 * @author nabowle
 */

public abstract class MpeRadarDecoder<T extends MpeRadarFile<?>> {

    private static final SingleTypeJAXBManager<MpeRadarDecoderConfig> JAXB = SingleTypeJAXBManager
            .createWithoutException(MpeRadarDecoderConfig.class);

    private volatile boolean initialized;

    private String appsDefaultProductDir;

    private String appsDefaultGridDir;

    private volatile Boolean writeToDb = false;

    private String configFile;

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Constructor.
     *
     * @param productType
     *            The radar product type.
     * @param appsDefaultProductDir
     *            The token to lookup the product dir.
     * @param appsDefaultGridDir
     *            The token to lookup the grid dir.
     */
    public MpeRadarDecoder(String productType, String appsDefaultProductDir,
            String appsDefaultGridDir) {
        super();
        this.appsDefaultProductDir = appsDefaultProductDir;
        this.appsDefaultGridDir = appsDefaultGridDir;
        this.configFile = "mpeProc" + IPathManager.SEPARATOR + "decode"
                + productType + ".xml";
    }

    /**
     * Decodes every valid Radar File in the product directory and writes the
     * grid conversion of each file to the grid directory.
     *
     * If writeToDb is true, the decoded radar and adaptable parameters will be
     * written to the database.
     *
     * Port of hydroapps/precip_proc/bin/Run_DecodeDHR and
     * hydroapps/precip_proc/bin/Run_DecodeDSP.
     *
     * @throws MpeRadarDecodeException
     */
    public void execute() throws MpeRadarDecodeException {
        if (!this.initialized) {
            synchronized (this) {
                if (!this.initialized) {
                    readLocalizationFile();
                    watchLocalizationFile();
                    this.initialized = true;
                }
            }
        }
        try {
            // fail fast for all files if there is an issue with the grid dir.
            AppsDefaultsConversionWrapper
                    .getPathForToken(this.appsDefaultGridDir);
        } catch (AppsDefaultsPathException e) {
            throw new MpeRadarDecodeException(
                    "Unable to retrieve the grid dir.", e);
        }

        Path prodDir = AppsDefaultsConversionWrapper
                .getPathForTokenWithoutCreating(this.appsDefaultProductDir);
        if (!Files.exists(prodDir)) {
            throw new MpeRadarDecodeException(
                    "Unable to retrieve the product dir.");
        }

        File productDir = prodDir.toFile();
        File[] files = productDir.listFiles();
        for (File file : files) {
            if (!file.isFile()) {
                // skip non-files
                continue;
            }
            Path path = file.toPath();

            T radarFile;
            try {
                radarFile = loadRadarFile(path,
                        MpeRadarDecodeConstants.DEFAULT_BUILD_VERSION);
            } catch (MpeRadarInputException e) {
                logger.info(
                        "{} will be deleted because it is not a valid radar file.",
                        path, e);
                file.delete();
                continue;
            }

            float[][] data = convertRadarFile(radarFile);
            try {
                writeRadarGridFile(radarFile, data);
            } catch (IOException | AppsDefaultsPathException e) {
                logger.warn(
                        "Could not write grid file. {} will not be deleted.",
                        path, e);
                continue;
            }

            if (this.writeToDb) {
                writeRadarRecords(radarFile);
            }

            // Successfully processed, delete the temp file
            file.delete();
        }
    }

    /**
     * Get the grid file to write to.
     *
     * @param file
     * @return
     * @throws AppsDefaultsPathException
     */
    protected File getGridFile(T file) throws AppsDefaultsPathException {
        File gridDir = new File(AppsDefaultsConversionWrapper
                .getPathForToken(this.appsDefaultGridDir).toString());
        File outputFile = new File(gridDir, file.getGridFilename());
        return outputFile;
    }

    /**
     * Reads the radar localization file. If the file cannot be read, the
     * current configuration state will be unchanged.
     */
    private void readLocalizationFile() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile localizationFile = pathManager
                .getStaticLocalizationFile(this.configFile);

        if (null == localizationFile) {
            logger.warn("{} does not exist.", this.configFile);
            return;
        }

        try (InputStream is = localizationFile.openInputStream()) {
            MpeRadarDecoderConfig config = JAXB.unmarshalFromInputStream(is);
            this.writeToDb = config.getWriteToDB();
            logger.debug("Configuration from {} has been read.",
                    this.configFile);
        } catch (IOException | SerializationException
                | LocalizationException e) {
            logger.warn("{} could not be read.", this.configFile, e);
        }
    }

    /**
     * Adds a localization path observer for the config file to catch live
     * config changes.
     */
    private void watchLocalizationFile() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        pathManager.addLocalizationPathObserver(this.configFile, (file) -> {
            if (file.getPath().equals(this.configFile)) {
                readLocalizationFile();
            }
        });
    }

    /**
     * Convert the radar file to quarterly HRAP grid.
     *
     * The radar data is normalized and then the values at each Azimuth/Range
     * coordinate is binned into the respective quarter HRAP grid coordinate,
     * and the average for each quarter HRAP grid coordinate is computed.
     *
     * Ported from the common code of decode_dhr_dsp/TEXT/write_decoded_dhr.c
     * and decode_dhr_dsp/TEXT/write_decoded_dsp.c.
     *
     * @param radarFile
     */
    public float[][] convertRadarFile(T radarFile)
            throws MpeRadarDecodeException {
        float[][] data = normalizeData(radarFile);
        /*
         * build lookup table for conversion between quarter HRAP(I,J) and radar
         * polar coordinate system(azimuth, range)
         */
        int[][] quarterHrapToRadarAzimuth = new int[MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP];
        int[][] quarterHrapToRadarRange = new int[MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP];
        MpeRadarProductDescription productDesc = radarFile.getDescription();
        MpeRadarDecodeUtils.buildLookupTable(productDesc.getLatitude(),
                productDesc.getLongitude(), quarterHrapToRadarAzimuth,
                quarterHrapToRadarRange);
        int index;
        int[][] numBin1km = new int[MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP];
        float[][] sumRad1km = new float[MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP];
        for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
            for (int j = 0; j < MpeRadarDecodeConstants.MAX_JHRAP; j++) {
                index = MpeRadarDecodeConstants.MAX_JHRAP - (j + 1);

                int azimuth = quarterHrapToRadarAzimuth[i][j];

                if (azimuth > MpeRadarDecodeConstants.MAX_AZIMUTH) {
                    azimuth = MpeRadarDecodeConstants.MAX_AZIMUTH;
                }

                int range = quarterHrapToRadarRange[i][j];

                if ((range != MpeRadarDecodeConstants.BEYOND_RANGE)
                        && (azimuth != MpeRadarDecodeConstants.BEYOND_RANGE)) {
                    if (azimuth < 1
                            || azimuth > MpeRadarDecodeConstants.MAX_AZIMUTH) {
                        throw new MpeRadarDecodeException(String.format(
                                "Azimuth angle (%d, %d) out of range at [%d][%d]",
                                azimuth, range, j, i));
                    }

                    if (range < 1
                            || range > MpeRadarDecodeConstants.MAX_RANGE) {
                        throw new MpeRadarDecodeException(String.format(
                                "Range (%d, %d) out of range at [%d][%d]",
                                azimuth, range, j, i));
                    }

                    numBin1km[i][index]++;
                    sumRad1km[i][index] += data[azimuth - 1][range - 1];
                }
            }
        }

        float[][] radar1km = new float[MpeRadarDecodeConstants.MAX_JHRAP][MpeRadarDecodeConstants.MAX_IHRAP];
        for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
            for (int j = 0; j < MpeRadarDecodeConstants.MAX_JHRAP; j++) {
                if (numBin1km[i][j] > 0) {
                    // Note: i/j index order reversed intentionally
                    radar1km[j][i] = sumRad1km[i][j] / numBin1km[i][j];
                } else {
                    radar1km[j][i] = MpeRadarDecodeConstants.DEFAULT_RADAR_1KM;
                }
            }
        }

        return radar1km;
    }

    /**
     * Write the radar grid file to disk.
     *
     * @param file
     *            The original radar file.
     * @param data
     *            The quarterly hrap grid data.
     * @throws IOException
     * @throws AppsDefaultsPathException
     */
    protected abstract void writeRadarGridFile(T file, float[][] data)
            throws IOException, AppsDefaultsPathException;

    /**
     * Normalize the radar data into a MpeRadarDecodeConstants.MAX_AZIMUTH x
     * MpeRadarDecodeConstants.MAX_RANGE array in the desired units.
     *
     * @param radarFile
     * @return
     */
    protected abstract float[][] normalizeData(T radarFile);

    /**
     * Load the radar file.
     *
     * @param path
     *            The path to the radar file.
     * @param buildVersion
     *            The build version to use.
     * @return
     */
    protected abstract T loadRadarFile(Path path, int buildVersion)
            throws MpeRadarInputException;

    /**
     * Write the radar records to the database.
     *
     * @param radarFile
     */
    protected abstract void writeRadarRecords(T radarFile);

}
