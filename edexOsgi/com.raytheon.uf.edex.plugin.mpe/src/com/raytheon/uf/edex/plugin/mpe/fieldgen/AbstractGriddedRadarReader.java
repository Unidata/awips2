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
package com.raytheon.uf.edex.plugin.mpe.fieldgen;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.IGriddedRadarRecord;
import com.raytheon.uf.common.mpe.constants.DHRConstants;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.CPlusPlusSizeConstants;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.IGriddedRadarRetrievalDao;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenUtils;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERadarMosaic;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERunConfiguration;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic.MosaicGenerationFailedException;

/**
 * Abstraction of the gridded radar retrieval process used by HPE Field Gen.
 * Based on: hpe_fieldgen/TEXT/read_dpr_data.c,
 * hpe_fieldgen/TEXT/read_decoded_dpr.c, hpe_fieldgen/TEXT/read_dpr_radar.c,
 * hpe_fieldgen/TEXT/read_dhr_data.c, hpe_fieldgen/TEXT/read_decoded_dhr.c,
 * hpe_fieldgen/TEXT/read_dhr_radar.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2016 5631       bkowal      Initial creation
 * Oct 11, 2016 5631       bkowal      Read the radar data in little endian byte order.
 *
 * </pre>
 *
 * @author bkowal
 */

public abstract class AbstractGriddedRadarReader<T extends IGriddedRadarRecord> {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    private final IGriddedRadarRetrievalDao<T> dao;

    private final Path decodedRadarPath;

    private final HPERunConfiguration runConfig;

    public AbstractGriddedRadarReader(final IGriddedRadarRetrievalDao<T> dao,
            final Path decodedRadarPath, final HPERunConfiguration runConfig) {
        if (dao == null) {
            throw new IllegalArgumentException(
                    "Required argument 'dao' cannot be NULL.");
        }
        if (decodedRadarPath == null) {
            throw new IllegalArgumentException(
                    "Required argument 'decodedRadarPath' cannot be NULL.");
        }
        if (runConfig == null) {
            throw new IllegalArgumentException(
                    "Required argument 'runConfig' cannot be NULL.");
        }
        this.dao = dao;
        this.decodedRadarPath = decodedRadarPath;
        this.runConfig = runConfig;
    }

    public float[][] retrieveData(final String radarId,
            final boolean ignoreRadar) throws Exception {
        if (ignoreRadar) {
            logger.info(
                    "Single Pol DHR radar: {} marked as ignore. Halting data retrieval.",
                    radarId);
            return null;
        }

        GriddedRadarMatch griddedRadarMatch = retrieveRadarDataRecords(radarId,
                ignoreRadar);
        final float[][] radar = new float[DHRConstants.NUM_DHR_ROWS][DHRConstants.NUM_DHR_COLS];
        if (griddedRadarMatch == null) {
            /*
             * No Match Found.
             */
            logger.info(
                    "No radar data was found within the current time range for Radar: {} for the current run date/time: {}.",
                    radarId, runConfig.getRunDateTime().getTime().toString());
            /*
             * Provide the default radar data.
             */
            for (int i = 0; i < DHRConstants.NUM_DHR_ROWS; i++) {
                Arrays.fill(radar[i], MpeConstants.RADAR_DEFAULT);
            }
            return radar;
        }

        if (griddedRadarMatch instanceof GriddedRadarMatchBounds) {
            /*
             * GriddedRadarMatchBounds indicates that an exact match was not
             * found and that radar records with an obs time before and/or after
             * the current date/time were found.
             */
            GriddedRadarMatchBounds matchBounds = (GriddedRadarMatchBounds) griddedRadarMatch;
            if (matchBounds.isPastMatch() && matchBounds.isFutureMatch()) {
                /*
                 * Found two products before and after and run date/time. Need
                 * to load both radar products and interpolate them.
                 */
                logger.info(
                        "Using radar data before the runtime within {}, filename: {}.",
                        TimeUtil.prettyDuration(
                                ((GriddedRadarMatchBounds) griddedRadarMatch)
                                        .getOffsetPast()),
                        matchBounds.getGridFilename());
                final float[][] decodedPastRadarData = readDecodedRadarData(
                        matchBounds.getGridFilename());
                logger.info(
                        "Using radar data after the runtime within {}, filename: {}.",
                        TimeUtil.prettyDuration(
                                ((GriddedRadarMatchBounds) griddedRadarMatch)
                                        .getOffsetFuture()),
                        matchBounds.getGridFilenameFuture());
                final float[][] decodedFutureRadarData = readDecodedRadarData(
                        matchBounds.getGridFilenameFuture());
                HPEFieldgenUtils.interpolate2DFloatArray(decodedPastRadarData,
                        decodedFutureRadarData, MpeConstants.RADAR_DEFAULT,
                        (float) matchBounds.getOffsetPast(),
                        (float) matchBounds.getOffsetFuture(), radar);
            } else if (matchBounds.isPastMatch() && matchBounds
                    .getOffsetPast() < (runConfig.getConfig().getDhrWindow()
                            * TimeUtil.MILLIS_PER_MINUTE)) {
                /*
                 * Only a radar record (within the current DHR window) with an
                 * obs time before the current run date/time was found.
                 */
                logger.info(
                        "No radar data after the run time. Using the latest record before the run time within {}, filename: {}.",
                        TimeUtil.prettyDuration(
                                ((GriddedRadarMatchBounds) griddedRadarMatch)
                                        .getOffsetPast()),
                        griddedRadarMatch.getGridFilename());
                /*
                 * Read the decoded radar file and complete any adjustments.
                 */
                final float[][] decodedRadarData = readDecodedRadarData(
                        matchBounds.getGridFilename());
                adjustGriddedRadarData(decodedRadarData, radar);
            }
        } else {
            /* Exact Match */
            final float[][] decodedRadarData = readDecodedRadarData(
                    griddedRadarMatch.getGridFilename());
            adjustGriddedRadarData(decodedRadarData, radar);
        }

        return radar;
    }

    private GriddedRadarMatch retrieveRadarDataRecords(final String radarId,
            final boolean ignoreRadar) {
        final int dhrWindow = runConfig.getConfig().getDhrWindow();
        final Calendar startObsTime = TimeUtil
                .newCalendar(runConfig.getRunDateTime());
        startObsTime.add(Calendar.MINUTE, -dhrWindow);
        final Calendar endObsTime = TimeUtil
                .newCalendar(runConfig.getRunDateTime());
        endObsTime.add(Calendar.MINUTE, dhrWindow);

        List<T> radarRecordList = dao.selectByRadIdBetweenObsTime(radarId,
                startObsTime, endObsTime);
        if (radarRecordList.isEmpty()) {
            return null;
        }

        GriddedRadarMatchBounds griddedRadarMatchBounds = null;
        for (T radarRecord : radarRecordList) {
            if (radarRecord.getGridFilename() == null) {
                /*
                 * Skip the incomplete record.
                 */
                logger.info(
                        "No grid file name is yet associated with radar record: {}. Skipping record.",
                        radarRecord.toString());
                continue;
            }

            final Calendar radarObsTime = TimeUtil
                    .newCalendar(radarRecord.getObsTime());
            if (radarObsTime.equals(runConfig.getRunDateTime())) {
                /*
                 * Found an exact match for the run date/time.
                 */

                /*
                 * No point in checking other records when an exact match is
                 * found.
                 */
                return new GriddedRadarMatch(radarRecord.getMeanFieldBias(),
                        radarRecord.getGridFilename());
            } else if (radarObsTime.before(runConfig.getRunDateTime())) {
                /*
                 * radar obstime occurs before the run date/time.
                 */
                if (griddedRadarMatchBounds == null) {
                    griddedRadarMatchBounds = new GriddedRadarMatchBounds();
                }
                final long offset = runConfig.getRunDateTime().getTimeInMillis()
                        - radarObsTime.getTimeInMillis();
                if (griddedRadarMatchBounds.isPastCloserToPresent(offset)) {
                    griddedRadarMatchBounds
                            .setMeanFieldBias(radarRecord.getMeanFieldBias());
                    griddedRadarMatchBounds
                            .setGridFilename(radarRecord.getGridFilename());
                    griddedRadarMatchBounds.setOffsetPast(offset);
                }
            } else {
                /*
                 * radar obstime occurs after the run date/time.
                 */
                if (griddedRadarMatchBounds == null) {
                    griddedRadarMatchBounds = new GriddedRadarMatchBounds();
                }
                final long offset = radarObsTime.getTimeInMillis()
                        - runConfig.getRunDateTime().getTimeInMillis();
                if (griddedRadarMatchBounds.isFutureCloserToPresent(offset)) {
                    griddedRadarMatchBounds.setMeanFieldBiasFuture(
                            radarRecord.getMeanFieldBias());
                    griddedRadarMatchBounds.setGridFilenameFuture(
                            radarRecord.getGridFilename());
                    griddedRadarMatchBounds.setOffsetFuture(offset);
                }
            }
        }

        return griddedRadarMatchBounds;
    }

    private float[][] readDecodedRadarData(final String gridFilename)
            throws MosaicGenerationFailedException {
        final Path decodedRadarDataPath = decodedRadarPath
                .resolve(gridFilename);
        if (!Files.exists(decodedRadarDataPath)) {
            throw new MosaicGenerationFailedException(HPERadarMosaic.DHRMOSAIC,
                    "Unable to find the expected Decoded Radar file: "
                            + decodedRadarDataPath.toString() + ".");
        }
        logger.info("Loading Decoded Radar file: {} ...",
                decodedRadarDataPath.toString());
        final byte[] decodedDPRBytes;
        try {
            decodedDPRBytes = Files.readAllBytes(decodedRadarDataPath);
        } catch (IOException e) {
            throw new MosaicGenerationFailedException(HPERadarMosaic.DHRMOSAIC,
                    "Failed to read Decoded Radar file: "
                            + decodedRadarDataPath.toString() + ".");
        }

        /*
         * byte array should include enough data to support 6 integers and
         * enough floats to fill a num dhr rows by num dhr columns array.
         */
        final int expectedSize = (6 * CPlusPlusSizeConstants.SIZE_OF_INT)
                + (DHRConstants.NUM_DHR_ELEMENTS
                        * CPlusPlusSizeConstants.SIZE_OF_FLOAT);
        if (decodedDPRBytes.length != expectedSize) {
            throw new MosaicGenerationFailedException(HPERadarMosaic.DHRMOSAIC,
                    "Decoded Radar file: " + decodedRadarDataPath.toString()
                            + " is incomplete. Read " + decodedDPRBytes.length
                            + " bytes; expected to read " + expectedSize
                            + " bytes.");
        }

        final float[][] dprData = new float[DHRConstants.NUM_DHR_ROWS][DHRConstants.NUM_DHR_COLS];
        ByteBuffer byteBuffer = ByteBuffer.wrap(decodedDPRBytes);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        /*
         * skip the first 6 size of integer bytes.
         */
        byteBuffer.position(6 * CPlusPlusSizeConstants.SIZE_OF_INT);
        final FloatBuffer floatBuffer = byteBuffer.asFloatBuffer();
        for (int i = 0; i < DHRConstants.NUM_DHR_ROWS; i++) {
            floatBuffer.get(dprData[i]);
        }

        return dprData;
    }

    /**
     * Ensures that any negative radar values are replaced by the default and
     * that any radar values less than {@link MpeConstants#ACC_MIN} are replaced
     * by 0. All other radar values are copied over as is. Based on a for-loop
     * that was copy-pasted twice in: hpe_fieldgen/TEXT/read_dpr_data.c.
     * 
     * @param source
     *            contains the data that will need to be adjusted / altered
     * @param dest
     *            contains the result of the adjustment
     */
    private void adjustGriddedRadarData(final float[][] source,
            final float[][] dest) {
        for (int i = 0; i < DHRConstants.NUM_DHR_ROWS; i++) {
            for (int j = 0; j < DHRConstants.NUM_DHR_COLS; j++) {
                if (source[i][j] < 0.0) {
                    dest[i][j] = MpeConstants.RADAR_DEFAULT;
                } else if (source[i][j] < MpeConstants.ACC_MIN) {
                    dest[i][j] = 0.0f;
                } else {
                    dest[i][j] = source[i][j];
                }
            }
        }
    }
}