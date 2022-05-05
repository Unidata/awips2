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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResult;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsConfigLoader;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsLoadException;
import com.raytheon.uf.edex.plugin.mpe.apps.RequiredTokenMissingException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DAARadarResultDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwradarresultDao;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.AbstractGriddedRadarReader;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.DHRGriddedRadarReader;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.DPRGriddedRadarReader;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldGenInputs;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenConstants;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenUtils;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERadarMosaic;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERunConfiguration;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.RadarLocRecord;
import com.raytheon.uf.edex.plugin.mpe.precip.GageData;
import org.locationtech.jts.geom.Coordinate;

/**
 * Algorithm for DHRMOSAIC generation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2016 5631       bkowal      Initial creation
 * Oct 05, 2016 5631       bkowal      Update to gather the needed radar and to
 *                                     generate the mean field bias.
 * Oct 11, 2016 5631       bkowal      Implemented file output and preparation for
 *                                     result data record creation.
 * Oct 18, 2016 5631       bkowal      Provide additional information to support
 *                                     gif alternate file format creation.                                    
 * Oct 19, 2016 5631       bkowal      Provide the process flag to the utility that
 *                                     generates alternate mosaic outputs.
 *
 * </pre>
 *
 * @author bkowal
 */

public class DHRMosaicGenerator {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String MISBIN_FILE_FMT = "misbin.%s";

    private static final String XMRG_FILE_DATE_FMT = "yyyyMMddHHmm";

    private static final String XMRG_FILE_NAME_FMT = "DHRMOSAIC%sz";

    private static final String HEIGHT_FILE_NAME_FMT = "DHRHEIGHT%sz";

    private static final String INDEX_FILE_NAME_FMT = "DHRINDEX%sz";

    private static final String MPE_USER = "SAN";

    private final DAARadarResultDao daaRadarResultDao = new DAARadarResultDao();

    private final RwradarresultDao rwradarresultDao = new RwradarresultDao();

    private final DateFormat xmrgNameDF = new SimpleDateFormat(
            XMRG_FILE_DATE_FMT);

    private final HPERunConfiguration config;

    private final HPEFieldGenInputs inputs;

    private final DHRMosaicAppsConfig appsConfig;

    public DHRMosaicGenerator(final HPERunConfiguration config,
            final HPEFieldGenInputs inputs)
                    throws MosaicGenerationFailedException {
        this.config = config;
        this.inputs = inputs;
        appsConfig = new DHRMosaicAppsConfig();
        try {
            AppsDefaultsConfigLoader.populateFromAppsDefaults(appsConfig);
        } catch (RequiredTokenMissingException | AppsDefaultsLoadException e) {
            throw new MosaicGenerationFailedException(HPERadarMosaic.DHRMOSAIC,
                    "Failed to load the Apps Defaults configuration.", e);
        }
    }

    public void generateMosaic(final int[][] id) throws Exception {
        logger.info("Start {} calculation at: {} ...",
                HPERadarMosaic.DHRMOSAIC.name(),
                TimeUtil.newGmtCalendar().getTime().toString());

        final int radarRows = DPAConstants.NUM_DPA_ROWS
                * config.getConfig().getHrapGridFactor().getNum();
        final int radarCols = DPAConstants.NUM_DPA_COLS
                * config.getConfig().getHrapGridFactor().getNum();
        final float[][] currentRadar = new float[radarRows][radarCols];
        final short[][] currentMiscBins = new short[radarRows][radarCols];

        /*
         * Read from RWRadarResult table for SinglePol or DAARadarResult table
         * for DualPol.
         */
        Map<String, RadarResult> radarResultsMap = Collections.emptyMap();
        if (!config.getConfig().isDualPolOn()) {
            radarResultsMap = retrieveRadarResults();
        } else {
            radarResultsMap = retrieveDAARadarResults();
        }

        /*
         * Read the misbin information if the token hpe_load_misbin is ON,
         * otherwise the misbin will be default value(1).
         */
        Map<String, short[]> radarMisBinsMap = Collections.emptyMap();
        if (config.getConfig().isLoadMisbin()) {
            radarMisBinsMap = readRadarMissingBins();
        }

        /*
         * Iterate through all radars.
         */
        int radarIndex = 1;
        final double[][] height = new double[(int) config.getGeoGridData()
                .getHeight()][(int) config.getGeoGridData().getWidth()];
        for (int i = 0; i < height.length; i++) {
            Arrays.fill(height[i], HPEFieldgenConstants.HEIGHT_DEFAULT);
        }
        final double[][] mosaic = new double[(int) config.getGeoGridData()
                .getHeight()][(int) config.getGeoGridData().getWidth()];
        for (int i = 0; i < mosaic.length; i++) {
            Arrays.fill(mosaic[i], MpeConstants.MOSAIC_DEFAULT);
        }
        final List<String> sortedRadarIds = new ArrayList<>(
                inputs.getRadarLocMap().keySet());
        Collections.sort(sortedRadarIds);
        for (String radarId : sortedRadarIds) {
            logger.info("Completing {} calculation for Radar {} ...",
                    HPERadarMosaic.DHRMOSAIC.name(), radarId);
            final ITimer radarGenTimer = TimeUtil.getTimer();
            radarGenTimer.start();

            /*
             * initialize the edit bias and ignore radar flags
             */
            boolean ignoreRadar = false;
            boolean editBias = false;
            double editBiasValue = 0.0;

            /*
             * adjust the edit bias and ignore radar flags based on the {@link
             * RadarResult} (when applicable).
             */
            final RadarResult radarResult = radarResultsMap.get(radarId);
            if (radarResult != null) {
                ignoreRadar = radarResult.isIgnoreRadar();
                editBias = radarResult.isEditBias();
                editBiasValue = radarResult.getBias();
            }

            /*
             * Read in gridded radar data, count number of available radars, an
             * "ignored" radar is considered "not available"
             */
            AbstractGriddedRadarReader<?> griddedRadarReader = null;
            String productInUse = null;
            if (config.getConfig().isDualPolOn()) {
                productInUse = "dual pol product DPR";
                griddedRadarReader = new DPRGriddedRadarReader(
                        appsConfig.getDprGridPath(), config);
            } else {
                productInUse = "single pol product DHR";
                griddedRadarReader = new DHRGriddedRadarReader(
                        appsConfig.getDhrGridPath(), config);
            }

            final float[][] griddedRadarData = griddedRadarReader
                    .retrieveData(radarId, ignoreRadar);
            if (griddedRadarData != null) {
                logger.info("{} is used for radar: {}", productInUse, radarId);
            }

            /*
             * TODO: radar available? - this is just a counter. So, need to wait
             * to see where it is used.
             */

            /*
             * Convert radar array to current hrap grid
             */
            HPEFieldgenUtils.convertFloatArray(griddedRadarData.length,
                    griddedRadarData[0].length, griddedRadarData,
                    MpeConstants.RADAR_DEFAULT, radarRows, radarCols,
                    currentRadar);

            /*
             * pick up the misbin for current running radar and convert it to
             * current hrap grid
             */
            short[] radarMisBin = radarMisBinsMap.get(radarId);
            if (radarMisBin == null) {
                /*
                 * Use the default misbin value.
                 */
                for (int i = 0; i < currentMiscBins.length; i++) {
                    Arrays.fill(currentMiscBins[i],
                            HPEFieldgenConstants.MISBIN_DEFAULT);
                }
            } else {
                final short[][] hrapMiscBins = new short[DPAConstants.NUM_DPA_ROWS][DPAConstants.NUM_DPA_COLS];
                for (int j = 0; j < DPAConstants.NUM_DPA_ROWS; j++) {
                    for (int k = 0; k < DPAConstants.NUM_DPA_COLS; k++) {
                        int index = j * DPAConstants.NUM_DPA_COLS + k;
                        hrapMiscBins[j][k] = radarMisBin[index];
                    }
                }
                HPEFieldgenUtils.convertShortArray(hrapMiscBins.length,
                        hrapMiscBins[0].length, hrapMiscBins,
                        HPEFieldgenConstants.MISBIN_DEFAULT, radarRows,
                        radarCols, currentMiscBins);
            }

            /*
             * Get mean field bias if it has been enabled.
             */
            CalculatedMeanBias calculatedMeanBias = new CalculatedMeanBias();
            if (config.isDhrMeanFieldBias()) {
                RadarLocRecord radarLocRecord = inputs.getRadarLocMap()
                        .get(radarId);
                if (radarLocRecord == null) {
                    throw new MosaicGenerationFailedException(
                            HPERadarMosaic.DHRMOSAIC,
                            "Unable to find a Radar Location for radar: "
                                    + radarId + ".");
                }
                GageData gageData = inputs.getGageDataMap()
                        .get(config.getRunDateTime());
                logger.info("Retrieving the Mean Field Bias.");
                final MeanFieldBiasCalculator meanFieldBiasCalculator = new MeanFieldBiasCalculator(
                        radarId, radarLocRecord, radarRows, radarCols,
                        currentMiscBins, currentRadar, gageData, config,
                        HPERadarMosaic.DHRMOSAIC);
                calculatedMeanBias = meanFieldBiasCalculator
                        .retrieveMeanFieldBias();

                if (editBias) {
                    logger.info("Edited Bias {} will be used for radar {}.",
                            editBiasValue, radarId);
                    calculatedMeanBias.setMeanBias(editBiasValue);
                }
            }

            logger.info("Mean Field Bias is {} for radar {}.",
                    calculatedMeanBias.getMeanBias(), radarId);

            /*
             * Execute the mosaicking algorithm.
             */
            createMosaic(inputs.getRadarLocMap().get(radarId), radarRows,
                    radarCols, currentRadar, currentMiscBins, height, mosaic,
                    id, radarIndex);
            radarGenTimer.stop();
            logger.info("Finished {} calculation for Radar {} (duration = {}).",
                    HPERadarMosaic.DHRMOSAIC.name(), radarId,
                    TimeUtil.prettyDuration(radarGenTimer.getElapsedTime()));

            ++radarIndex;
        }

        /*
         * Write out gridded data in xmrg format to flat files.
         */
        final String formattedXmrgDate = xmrgNameDF
                .format(config.getRunDateTime().getTime());

        final ITimer writeTimer = TimeUtil.getTimer();
        logger.info("Writing {} fields to flat files ...",
                HPERadarMosaic.DHRMOSAIC.name());
        writeTimer.start();

        final Path dhrMosaicPath = AppsDefaultsConversionWrapper
                .getPathForToken(AppsDefaultsDirKeys.HPE_DHRMOSAIC_DIR);
        final Path dhrMosaicXmrgPath = dhrMosaicPath
                .resolve(String.format(XMRG_FILE_NAME_FMT, formattedXmrgDate));
        try {
            HPEFieldgenUtils.writeXmrg(config.getGeoGridData(),
                    MpeConstants.FACTOR_PRECIP, false, mosaic,
                    dhrMosaicXmrgPath, config.getRunDateTime(), MPE_USER,
                    HPEFieldgenConstants.DHR_PROC_FLAG);
            logger.info("Successfully wrote {} xmrg file: {}.",
                    HPERadarMosaic.DHRMOSAIC.name(),
                    dhrMosaicXmrgPath.toString());
        } catch (Exception e) {
            throw new MosaicGenerationFailedException(HPERadarMosaic.DHRMOSAIC,
                    "Failed to write xmrg file: " + dhrMosaicXmrgPath + ".", e);
        }

        HPEFieldgenUtils.convertXmrgMultiFormat(config.getGeoGridData(),
                appsConfig, mosaic, dhrMosaicXmrgPath,
                HPEFieldgenConstants.DHR_PROC_FLAG, config.getGeoFileData(),
                formattedXmrgDate, config.getConfig().getAutoGraphicScale(),
                HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_NETCDF_DIR,
                HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_GIF_DIR, logger);

        /*
         * The potential exists to apply mpe polygons to the mosaic data at this
         * point. However, it is currently set as a pre-compiler directive,
         * APPLY_POLYGON, that is set to false (disabled) in:
         * hpe_fieldgen/TEXT/empe_constants.h.
         */

        if (Boolean.TRUE.equals(appsConfig.getSaveHeight())) {
            if (appsConfig.getHeightPath() == null) {
                logger.error(
                        "Skipping generation of the height xmrg file because no destination directory has been specified. A destination directory can be specified using the {} token: {}.",
                        AppsDefaults.NAME,
                        HPEFieldgenConstants.AppsDefaults.HPE_DHRHEIGHT_DIR);
            } else {
                /*
                 * Determine the name of the height xmrg file.
                 */
                Path heightPath;
                if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
                    try {
                        heightPath = AppsDefaultsConversionWrapper
                                .getPathForToken(
                                        HPEFieldgenConstants.AppsDefaults.HPE_DHRHEIGHT_DIR)
                                .resolve(String.format(HEIGHT_FILE_NAME_FMT,
                                        formattedXmrgDate));
                    } catch (AppsDefaultsPathException e) {
                        heightPath = null;
                        logger.error(
                                "Failed to retrieve the Height Output directory for "
                                        + AppsDefaults.NAME + " token: "
                                        + HPEFieldgenConstants.AppsDefaults.HPE_DHRHEIGHT_DIR
                                        + ". Skipping Height XMRG generation.",
                                e);
                    }
                } else {
                    heightPath = appsConfig.getHeightPath().resolve(String
                            .format(HEIGHT_FILE_NAME_FMT, formattedXmrgDate));
                }
                if (heightPath != null) {
                    try {
                        HPEFieldgenUtils.writeXmrg(config.getGeoGridData(),
                                MpeConstants.FACTOR_OTHER, false, mosaic,
                                heightPath, config.getRunDateTime(), MPE_USER,
                                HPEFieldgenConstants.DHR_PROC_FLAG);
                        logger.info(
                                "Successfully wrote {} height xmrg file: {}.",
                                HPERadarMosaic.DHRMOSAIC.name(),
                                heightPath.toString());
                    } catch (Exception e) {
                        logger.error("Failed to write {} height xmrg file: {}.",
                                HPERadarMosaic.DHRMOSAIC.name(),
                                heightPath.toString());
                    }
                }
            }
        }

        if (Boolean.TRUE.equals(appsConfig.getSaveDHRIndex())) {
            if (appsConfig.getDhrIndexPath() == null) {
                logger.error(
                        "Skipping generation of the index xmrg file because no destination directory has been specified. A destination directory can be specified using the {} token: {}.",
                        AppsDefaults.NAME,
                        HPEFieldgenConstants.AppsDefaults.HPE_DHRINDEX_DIR);
            } else {
                /*
                 * Determine the name of the height xmrg file.
                 */
                Path indexPath;
                if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
                    try {
                        indexPath = AppsDefaultsConversionWrapper
                                .getPathForToken(
                                        HPEFieldgenConstants.AppsDefaults.HPE_DHRINDEX_DIR)
                                .resolve(String.format(HEIGHT_FILE_NAME_FMT,
                                        formattedXmrgDate));
                    } catch (AppsDefaultsPathException e) {
                        indexPath = null;
                        logger.error(
                                "Failed to retrieve the Index Output directory for "
                                        + AppsDefaults.NAME + " token: "
                                        + HPEFieldgenConstants.AppsDefaults.HPE_DHRINDEX_DIR
                                        + ". Skipping Height XMRG generation.",
                                e);
                    }
                } else {
                    indexPath = appsConfig.getDhrIndexPath().resolve(String
                            .format(INDEX_FILE_NAME_FMT, formattedXmrgDate));
                }
                if (indexPath != null) {
                    final double[][] indexMosaic = new double[id.length][id[0].length];
                    for (int i = 0; i < id.length; i++) {
                        for (int j = 0; j < id[i].length; j++) {
                            indexMosaic[i][j] = (double) id[i][j];
                        }
                    }
                    try {
                        HPEFieldgenUtils.writeXmrg(config.getGeoGridData(),
                                MpeConstants.FACTOR_OTHER, false, indexMosaic,
                                indexPath, config.getRunDateTime(), MPE_USER,
                                HPEFieldgenConstants.DHR_PROC_FLAG);
                        logger.info(
                                "Successfully wrote {} index xmrg file: {}.",
                                HPERadarMosaic.DHRMOSAIC.name(),
                                indexPath.toString());
                    } catch (Exception e) {
                        logger.error("Failed to write {} index xmrg file: {}.",
                                HPERadarMosaic.DHRMOSAIC.name(),
                                indexPath.toString());
                    }
                }
            }
        }

        writeTimer.stop();
        logger.info("Finished writing {} fields to flat files (duration = {}).",
                HPERadarMosaic.DHRMOSAIC.name(),
                TimeUtil.prettyDuration(writeTimer.getElapsedTime()));

        writeTimer.reset();
        logger.info("Updating the HPERadarResult table for {} ...",
                HPERadarMosaic.DHRMOSAIC.name());
        writeTimer.start();

        /*
         * TODO: implement. Will be a separate changeset due to the fact that
         * significant modifications will need to be made to the
         * com.raytheon.uf.edex.plugin.hpe plugin as well as users of that
         * plugin because it is still utilizing Hibernate hbm files without any
         * JPA annotations.
         */

        logger.info(
                "Finished updating the HPERadarResult table for {} (duration = {}).",
                HPERadarMosaic.DHRMOSAIC.name(),
                TimeUtil.prettyDuration(writeTimer.getElapsedTime()));
        writeTimer.stop();

    }

    /*
     * Based on: hpe_fieldgen/TEXT/read_radarresult.c
     */
    private Map<String, RadarResult> retrieveRadarResults() {
        Calendar runDatimeTime = TimeUtil.newCalendar(config.getRunDateTime());
        /*
         * Zero out seconds (and milliseconds).
         */
        TimeUtil.minCalendarFields(runDatimeTime, Calendar.SECOND,
                Calendar.MILLISECOND);
        List<Rwradarresult> radarResults = rwradarresultDao
                .selectByObsTimeOrderedByRadId(runDatimeTime);
        if (radarResults.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<String, RadarResult> radarResultsMap = new HashMap<>(
                radarResults.size(), 1.0f);
        radarResults.forEach((r) -> radarResultsMap.put(r.getId().getRadid(),
                new RadarResult(r)));
        return radarResultsMap;
    }

    /*
     * Based on: hpe_fieldgen/TEXT/read_daaradarresult.c
     */
    private Map<String, RadarResult> retrieveDAARadarResults() {
        Calendar runDateTimeHour = TimeUtil
                .newCalendar(config.getRunDateTime());
        /*
         * Only care about the hour in the time field. So, 0 everything else
         * out.
         */
        TimeUtil.minCalendarFields(runDateTimeHour, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);
        List<DAARadarResult> radarResults = daaRadarResultDao
                .selectByObsTimeOrderedByRadId(runDateTimeHour);
        if (radarResults.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<String, RadarResult> radarResultsMap = new HashMap<>(
                radarResults.size(), 1.0f);
        radarResults.forEach((r) -> radarResultsMap.put(r.getId().getRadid(),
                new RadarResult(r)));
        return radarResultsMap;
    }

    /*
     * Based on: hpe_fieldgen/TEXT/read_misc.c
     */
    private Map<String, short[]> readRadarMissingBins() {
        final Map<String, short[]> radarMisBinsMap = new HashMap<>(
                inputs.getRadarLocMap().keySet().size(), 1.0f);
        for (String radarId : inputs.getRadarLocMap().keySet()) {
            short[] misbins = new short[DPAConstants.NUM_DPA_ELEMENTS];
            final Path radarMisbinPath = appsConfig.getMpeMisbinPath()
                    .resolve(String.format(MISBIN_FILE_FMT, radarId));
            logger.info("Loading Radar Missing Bin file: {} ...",
                    radarMisbinPath.toString());
            if (!Files.exists(radarMisbinPath)) {
                logger.warn(
                        "Unable to find Radar Missing Bin file: {}. Using default array of 1s.",
                        radarMisbinPath.toString());
                Arrays.fill(misbins, (short) 1);
                radarMisBinsMap.put(radarId, misbins);
                continue;
            }

            byte[] misbinBytes;
            try {
                misbinBytes = Files.readAllBytes(radarMisbinPath);
            } catch (IOException e) {
                logger.warn("Failed to read Radar Missing Bin file: "
                        + radarMisbinPath.toString()
                        + ". Using default array of 1s.", e);
                Arrays.fill(misbins, (short) 1);
                radarMisBinsMap.put(radarId, misbins);
                continue;
            }

            /*
             * Verify that the number of bytes read is enough to fill the
             * destination + 8 (number of data bytes are encoded at both the
             * beginning and the end of the data array).
             */
            if ((misbinBytes.length - 8) != (misbins.length * 2)) {
                /*
                 * Insufficient file structure.
                 */
                logger.warn(
                        "Radar Missing Bin file: {} may be corrupt or incomplete. Only read {} bytes; expected to read {} bytes. Using default array of 1s.",
                        radarMisbinPath.toString(), misbinBytes.length,
                        ((misbins.length * 2) + 8));
                Arrays.fill(misbins, (short) 1);
                radarMisBinsMap.put(radarId, misbins);
                continue;
            }

            ByteBuffer byteBuffer = ByteBuffer.wrap(misbinBytes);
            byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
            /*
             * Skip the first four bytes - contain the number of bytes of data
             * in the file.
             */
            byteBuffer.position(4);
            byteBuffer.asShortBuffer().get(misbins);
            radarMisBinsMap.put(radarId, misbins);
        }

        return radarMisBinsMap;
    }

    /*
     * Based on: hpe_fieldgen/TEXT/create_dhrmosaic.c
     */
    private void createMosaic(final RadarLocRecord radarLocRecord,
            final int dhrRows, final int dhrCols, final float[][] radar,
            final short[][] miscBin, final double[][] height,
            final double[][] mosaic, final int[][] id, final int index) {
        /*
         * locate hrap pixel that radar is in
         */
        final double factor = (double) dhrRows
                / (double) DPAConstants.NUM_DPA_ROWS;
        Coordinate latLonCoord = HPEFieldgenUtils.convertLatLonToScaledHrap(
                radarLocRecord.getLongitude(), radarLocRecord.getLatitude(),
                factor);
        int intCol = (int) latLonCoord.x;
        int intRow = (int) latLonCoord.y;

        /*
         * calculate starting hrap coordinates
         */
        for (int i = 0; i < dhrRows; i++) {
            for (int j = 0; j < dhrRows; j++) {
                /*
                 * check radar pixel to make sure it exists, this check ensures
                 * that the height array is dynamic and varies with radar
                 * availability.
                 */
                if (radar[i][j] < 0.0) {
                    continue;
                }

                /*
                 * compute location of radar pixel on national hrap grid, then
                 * check to make sure this point is in rfc area.
                 */
                int tmpRow = intRow - dhrRows / 2 + i;
                int tmpCol = intCol - dhrCols / 2 + j;

                /*
                 * Retrieve the pixel height.
                 */
                double pixelHeight = inputs.getRadarBeamHeight()[i][j];
                pixelHeight += radarLocRecord.getElevation();

                /*
                 * check to make sure pixel height is less than that on mosaic.
                 * 
                 * convert x,y coordinates to local coordinates of DHRMosaic and
                 * MHeight array
                 */
                tmpCol -= config.getGeoGridData().x;
                tmpRow -= config.getGeoGridData().y;

                if ((tmpCol < 0) || (tmpRow < 0)) {
                    continue;
                }

                if ((tmpCol >= config.getGeoGridData().width)
                        || (tmpRow >= config.getGeoGridData().height)) {
                    continue;
                }

                if (pixelHeight < height[tmpRow][tmpCol]) {
                    /*
                     * check to make sure pixel is not a miscBinArray additional
                     * check for fake miscBinArray data.
                     */
                    if (miscBin[i][j] == 1) {
                        mosaic[tmpRow][tmpCol] = (double) radar[i][j];
                        id[tmpRow][tmpCol] = index;
                        height[tmpRow][tmpCol] = pixelHeight;
                    }
                }
            }
        }
    }
}