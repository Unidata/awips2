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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.DAABiasDyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasdyn;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DAABiasDynDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwbiasdynDao;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenUtils;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERadarMosaic;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERunConfiguration;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.RadarLocRecord;
import com.raytheon.uf.edex.plugin.mpe.precip.GageData;
import com.raytheon.uf.edex.plugin.mpe.precip.PrecipDataRecord;
import org.locationtech.jts.geom.Coordinate;

/**
 * Retrieves the mean field bias for each radar. Based on:
 * hpe_fieldgen/TEXT/get_mean_bias.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class MeanFieldBiasCalculator {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final RwbiasdynDao rwbiasdynDao = new RwbiasdynDao();

    private final RadarlocDao radarlocDao = new RadarlocDao();

    private final DAABiasDynDao dAABiasDynDao = new DAABiasDynDao();

    private final String radarId;

    private final RadarLocRecord radarLocRecord;

    private final int gridRows;

    private final int gridCols;

    private final short[][] radarMiscBins;

    private final float[][] radar;

    private final GageData gageData;

    private final HPERunConfiguration config;

    private final HPERadarMosaic mosaic;

    public MeanFieldBiasCalculator(final String radarId,
            final RadarLocRecord radarLocRecord, final int gridRows,
            final int gridCols, final short[][] radarMiscBins,
            final float[][] radar, final GageData gageData,
            final HPERunConfiguration config, final HPERadarMosaic mosaic) {
        this.radarId = radarId;
        this.radarLocRecord = radarLocRecord;
        this.gridRows = gridRows;
        this.gridCols = gridCols;
        this.radarMiscBins = radarMiscBins;
        this.radar = radar;
        this.gageData = gageData;
        this.config = config;
        this.mosaic = mosaic;
    }

    /**
     * Determines and calculates the mean field bias.
     * 
     * @return the calculated mean field bias as a {@link CalculatedMeanBias}
     * @throws MosaicGenerationFailedException
     */
    public CalculatedMeanBias retrieveMeanFieldBias()
            throws MosaicGenerationFailedException {
        /*
         * Determine how many gage pairs there will be (provided that gage data
         * actually is available - only at the top of the hour).
         */
        int pairSize = 0;
        List<GageRadarPair> gageRadarPairs = Collections.emptyList();
        if (gageData != null) {
            /*
             * The number of gage pairs is equivalent to the number of gages
             * (excluding pseudo gages)
             */
            pairSize = gageData.getGagePrecip().size();
            gageRadarPairs = new ArrayList<>(pairSize);
            pairGageRadar(gageRadarPairs);
        }
        logger.info("Calculating Mean Bias with lag {} ...",
                config.getConfig().getRfcBiasLag());
        return calculateMeanBias(gageRadarPairs);
    }

    /**
     * Attempts to pair gages with a radar. Based on:
     * hpe_fieldgen/TEXT/pair_gage_radar.c.
     * 
     * @param gageRadarPairs
     *            the results of the pairing attempt as a {@link List} of
     *            {@link GageRadarPair}s.
     */
    private void pairGageRadar(final List<GageRadarPair> gageRadarPairs) {
        final double minGrValueBias = config.getRwbiasstat()
                .getMinGrValueBias();

        /*
         * get scaled hrap coordinates of radar
         */
        final double factor = (double) gridRows
                / (double) DPAConstants.NUM_DPA_ROWS;
        final Coordinate scaledHrapCoords = HPEFieldgenUtils
                .convertLatLonToScaledHrap(radarLocRecord.getLongitude(),
                        radarLocRecord.getLatitude(), factor);

        /*
         * find lower left corner of grid_rows * grid_cols radar grid in
         * national hrap coordinates
         */
        int hrapX = (int) (scaledHrapCoords.x - gridRows / 2);
        int hrapY = (int) (scaledHrapCoords.y - gridRows / 2);

        /*
         * find lower left corner of 131x131 radar grid in local coordinate
         * which start at array location (0,0) in lower left
         */
        final Rectangle geoData = config.getGeoGridData();
        int localHrapX = hrapX - geoData.x;
        int localHrapY = hrapY - geoData.y;

        /*
         * Iterate through all non-pseudo gages
         */
        for (PrecipDataRecord precip : gageData.getGagePrecip()) {
            if (precip.getValue() <= minGrValueBias) {
                continue;
            }

            int tmpHrapX = precip.getX() - localHrapX;
            int tmpHrapY = precip.getY() - localHrapY;

            if ((tmpHrapX < 0) || (tmpHrapX >= gridRows) || (tmpHrapY < 0)
                    || (tmpHrapY >= gridCols)) {
                continue;
            }

            if (radarMiscBins[tmpHrapX][tmpHrapY] == 0) {
                continue;
            }

            if ((double) radar[tmpHrapX][tmpHrapY] <= minGrValueBias) {
                continue;
            }

            /*
             * match collocated radar rainfall and exclude pseudo gages from
             * consideration
             */

            GageRadarPair pair = new GageRadarPair();
            pair.setLid(precip.getId());
            pair.setHrapX(precip.getX());
            pair.setHrapY(precip.getY());
            pair.setGageValue(precip.getValue());
            pair.setRadarValue((double) radar[tmpHrapX][tmpHrapY]);
            gageRadarPairs.add(pair);
        }

        /*
         * log the number of positive gage-radar pairs
         */
        if (gageRadarPairs.isEmpty()) {
            logger.info("There are no additional gage-radar pairs.");
        } else {
            for (GageRadarPair pair : gageRadarPairs) {
                logger.info("Found gage-radar pair: {}", pair.toString());
            }
        }
    }

    /**
     * Gathers necessary inputs and executes the mean field bias calculation.
     * Based on: hpe_fieldgen/TEXT/calculate_mean_bias.c.
     * 
     * @param gageRadarPairs
     *            a {@link List} of {@link GageRadarPair}s to use in the
     *            calculation. Can be empty.
     * @return the calculated mean field bias as a {@link CalculatedMeanBias}
     * @throws MosaicGenerationFailedException
     */
    private CalculatedMeanBias calculateMeanBias(
            final List<GageRadarPair> gageRadarPairs)
                    throws MosaicGenerationFailedException {
        final int numSpan = config.getRwbiasstat().getNumSpan();

        /*
         * read state variables for current hour from RWBiasDyn table. if record
         * not found for current hour, then look back lag_cut hours for a
         * record.
         */

        final Map<Short, BiasMemSpanValue> biasMemSpanValueMap = retrieveBiasdyn();
        double biasLong = biasMemSpanValueMap.get((short) (numSpan - 1))
                .getBias();

        /*
         * check gage/radar pairs for quality control,
         */
        if (!performQCGageRadarPairs(gageRadarPairs, biasLong)) {
            logger.info("Gage/Radar Pairs QC was not completed.");
        }

        /*
         * compute the bias for each mem_span value when blnComputeBias == 1. In
         * the C++ code, blnComputeBias is hard-coded to 0 on line 106 of:
         * hpe_fieldgen/TEXT/calculate_mean_bias.c. The one instance where it
         * could be set to 1 (true) is commented out on lines 158 - 164.
         */

        /*
         * compute the best bias.
         */
        double meanBias = 1.0;
        double memSpanBias = 0.0;
        final int npairBiasSelect = config.getRwbiasstat().getNpairBiasSelect();

        /*
         * prepare the mem span lookup. Must have 10 elements because there are
         * 10 mem span values.
         */
        final double[] memSpan = new double[10];
        memSpan[0] = config.getRwbiasstat().getMemSpan1();
        memSpan[1] = config.getRwbiasstat().getMemSpan2();
        memSpan[2] = config.getRwbiasstat().getMemSpan3();
        memSpan[3] = config.getRwbiasstat().getMemSpan4();
        memSpan[4] = config.getRwbiasstat().getMemSpan5();
        memSpan[5] = config.getRwbiasstat().getMemSpan6();
        memSpan[6] = config.getRwbiasstat().getMemSpan7();
        memSpan[7] = config.getRwbiasstat().getMemSpan8();
        memSpan[8] = config.getRwbiasstat().getMemSpan9();
        memSpan[9] = config.getRwbiasstat().getMemSpan10();

        for (int i = 0; i < numSpan; i++) {
            BiasMemSpanValue biasMemSpanValue = biasMemSpanValueMap
                    .get((short) i);
            if (biasMemSpanValue.getNumPairs() > npairBiasSelect) {
                meanBias = biasMemSpanValue.getBias();
                memSpanBias = memSpan[i];
                break;
            }
        }

        return new CalculatedMeanBias(meanBias, memSpanBias);
    }

    /**
     * Retrieves either {@link Rwbiasdyn} or {@link DAABiasDyn} records that are
     * then converted to {@link BiasMemSpanValue}. The determination of which
     * records to retrieve is based on a variety of inputs including (but, not
     * limited to): the various configuration flags, the current run date/time
     * hour, and the radar that data is being retrieved for. Based on:
     * hpe_fieldgen/TEXT/read_rwbiasdyn2.c.
     * 
     * @return a {@link Map} of Memory Span Indicators (short) to the generated
     *         {@link BiasMemSpanValue}.
     * @throws MosaicGenerationFailedException
     */
    private Map<Short, BiasMemSpanValue> retrieveBiasdyn()
            throws MosaicGenerationFailedException {
        final int rfcBiasLag = config.getConfig().getRfcBiasLag();
        final String biasSource = config.getConfig().getDefaultBiasSource();
        final String siteId = config.getConfig().getFxaLocalSite();
        final int lagCut = config.getRwbiasstat().getLagCut();

        logger.info("hpe rfc bias lag is: {}.", rfcBiasLag);
        logger.info("hpe bias source is: {}", biasSource);

        final Calendar runDateTimeHour = TimeUtil
                .newCalendar(config.getRunDateTime());
        TimeUtil.minCalendarFields(runDateTimeHour, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);

        final Map<Short, BiasMemSpanValue> biasMemSpanValueMap = new HashMap<>(
                config.getRwbiasstat().getNumSpan(), 1.0f);
        final Calendar startDateTime = TimeUtil.newCalendar(runDateTimeHour);
        if ("local".equals(biasSource.toString())) {
            /*
             * If the bias source is LOCAL, pick up bias value based on the
             * FXA_LOCAL_SITE.
             */
            startDateTime.add(Calendar.HOUR_OF_DAY, -lagCut);
            if (config.getConfig().isDualPolOn()) {
                logger.info(
                        "Loading DP LOCAL MFB bias from DAABiasDyn table for siteID: {}, lag time = {}",
                        siteId, lagCut);
                retrieveDAABiasDyn(biasMemSpanValueMap,
                        TimeUtil.newCalendar(runDateTimeHour), lagCut, siteId);
            } else {
                logger.info(
                        "Loading SP LOCAL MFB bias from RWBiasDyn table for siteID: {}, lag time = {}",
                        siteId, lagCut);
                retrieveRwbiasdyn(biasMemSpanValueMap,
                        TimeUtil.newCalendar(runDateTimeHour), lagCut, siteId);
            }
        } else {
            /*
             * retrieve the office ID from RadarLoc table
             */
            final Set<String> radarIds = new HashSet<>(1, 1.0f);
            radarIds.add(radarId);
            List<Radarloc> radarlocList = radarlocDao
                    .getNumAndOfficeForRadarIds(radarIds);
            if (radarlocList.isEmpty()) {
                throw new MosaicGenerationFailedException(mosaic,
                        "Could not find office id for radar: " + radarId + ".");
            }
            Radarloc radarloc = radarlocList.iterator().next();
            final String officeId = radarloc.getOfficeId();
            logger.info("Found office id: {} for radar: {}.", officeId,
                    radarId);

            /*
             * Pick up the bias value based on the radar's office ID. If there
             * is no record found, check if the office ID is the same as the
             * FXA_LOCAL_SITE, if not, then use the FXA_LOCAL_SITE as office ID
             * to pick up the bias value again.
             */
            if (siteId.equals(officeId)) {
                /*
                 * Will only want the hour field included in the date/time
                 * query.
                 */
                startDateTime.add(Calendar.HOUR_OF_DAY, -lagCut);
                if (config.getConfig().isDualPolOn()) {
                    logger.info(
                            "Loading DP RFC MFB bias from DAABiasDyn table for office: {}, lag time = {} ...",
                            officeId, lagCut);
                    retrieveDAABiasDyn(biasMemSpanValueMap,
                            TimeUtil.newCalendar(runDateTimeHour), lagCut,
                            officeId);
                } else {
                    logger.info(
                            "Loading SP RFC MFB bias from the RWBiasDyn table for office: {}, lag time = {} ...",
                            officeId, lagCut);
                    retrieveRwbiasdyn(biasMemSpanValueMap,
                            TimeUtil.newCalendar(runDateTimeHour), lagCut,
                            officeId);
                }
            } else {
                /*
                 * if the office ID != FXA_LOCAL_SITE, then use the
                 * HPE_RFC_BIAS_LAG_TOKEN value to pick up the bias value.
                 */
                startDateTime.add(Calendar.HOUR_OF_DAY, -rfcBiasLag);
                if (config.getConfig().isDualPolOn()) {
                    logger.info(
                            "Loading DP RFC MFB from the DAABiasDyn table for office: {}, lag time = {} ...",
                            officeId, rfcBiasLag);
                    retrieveDAABiasDyn(biasMemSpanValueMap,
                            TimeUtil.newCalendar(runDateTimeHour), rfcBiasLag,
                            officeId);
                    if (biasMemSpanValueMap.isEmpty()) {
                        logger.info(
                                "Cannot load SP RFC MFB from the DAABiasDyn table for office: {}. Loading SP LOCAL MFB for site: {} ...",
                                officeId, siteId);
                        retrieveDAABiasDyn(biasMemSpanValueMap,
                                TimeUtil.newCalendar(runDateTimeHour),
                                rfcBiasLag, siteId);
                    }
                } else {
                    logger.info(
                            "Loading SP RFC MFB from the RWBiasDyn table for office: {}, lag time = {} ...",
                            officeId, rfcBiasLag);
                    retrieveRwbiasdyn(biasMemSpanValueMap,
                            TimeUtil.newCalendar(runDateTimeHour), rfcBiasLag,
                            officeId);
                    if (biasMemSpanValueMap.isEmpty()) {
                        logger.info(
                                "Cannot load SP RFC MFB from the RWBiasDyn table for office: {}. Loading SP LOCAL MFB for site: {} ...",
                                officeId, siteId);
                        retrieveRwbiasdyn(biasMemSpanValueMap,
                                TimeUtil.newCalendar(runDateTimeHour),
                                rfcBiasLag, siteId);
                    }
                }
            }
        }

        long lag = 0;
        if (biasMemSpanValueMap.isEmpty()) {
            logger.info(
                    "Failed to find Bias Mem Span Values in the Rwbiasdyn/DAABiasDyn tables between date/time {} and current run date/time {}. Reinitializing state variables.",
                    startDateTime.getTime().toString(),
                    runDateTimeHour.getTime().toString());
            for (int i = 0; i < config.getRwbiasstat().getNumSpan(); i++) {
                BiasMemSpanValue biasMemSpanValue = new BiasMemSpanValue();
                biasMemSpanValue.setMemspanInd((short) i);
                biasMemSpanValue.setNumPairs(0.0);
                biasMemSpanValue.setSumgag(0.0);
                biasMemSpanValue.setSumrad(0.0);
                biasMemSpanValue.setBias(1.0);
                biasMemSpanValueMap.put((short) i, biasMemSpanValue);

                logger.info(
                        "For memspan ind: {}, using default state variable: {}.",
                        i, biasMemSpanValue.toString());
            }
        } else {
            /*
             * Determine if older state variables are being used.
             */
            final long lagTimeDiffMillis = runDateTimeHour.getTimeInMillis()
                    - biasMemSpanValueMap.values().iterator().next()
                            .getObsTime().getTimeInMillis();
            lag = lagTimeDiffMillis / TimeUtil.MILLIS_PER_HOUR;
            if (lag > 1 && lag < lagCut) {
                logger.info("Using state variables from: {} hours ago.", lag);
            }
            for (BiasMemSpanValue biasMemSpanValue : biasMemSpanValueMap
                    .values()) {
                logger.info("For memspan ind: {}, using state variable: {}.",
                        biasMemSpanValue.getMemspanInd(),
                        biasMemSpanValue.toString());
            }
        }

        return biasMemSpanValueMap;
    }

    /**
     * Retrieves the {@link Rwbiasdyn} records converted to
     * {@link BiasMemSpanValue}s for the current configuration, current run
     * date/time hour, and radar. Based on the: 'read_rwbiasdyn2' function in:
     * hpe_fieldgen/TEXT/read_rwbiasdyn2.c.
     * 
     * @param biasMemSpanValueMap
     *            {@link Map} to place the generated {@link BiasMemSpanValue} in
     * @param runDateTimeHour
     *            the current run date/time hour
     * @param lagHours
     *            maximum number of hours to backtrack when attempting to find
     *            usable {@link Rwbiasdyn} records
     * @param officeId
     *            the id of the office associated with the radar
     */
    private void retrieveRwbiasdyn(
            final Map<Short, BiasMemSpanValue> biasMemSpanValueMap,
            final Calendar runDateTimeHour, final int lagHours,
            final String officeId) {
        for (int i = 0; i < lagHours; i++) {
            runDateTimeHour.add(Calendar.HOUR_OF_DAY, -1);
            List<Rwbiasdyn> rwbiasdynList = rwbiasdynDao.getForRadarAndObsTime(
                    runDateTimeHour.getTime(), radarId, officeId);
            if (rwbiasdynList.isEmpty()) {
                continue;
            }

            for (Rwbiasdyn rwbiasdyn : rwbiasdynList) {
                BiasMemSpanValue biasMemSpanValue = new BiasMemSpanValue();
                biasMemSpanValue
                        .setMemspanInd(rwbiasdyn.getId().getMemspanInd());
                biasMemSpanValue.setNumPairs(rwbiasdyn.getNumpairs());
                biasMemSpanValue.setSumgag(rwbiasdyn.getSumgag());
                biasMemSpanValue.setSumrad(rwbiasdyn.getSumrad());
                biasMemSpanValue.setBias(rwbiasdyn.getBias());
                biasMemSpanValue.setObsTime(
                        TimeUtil.newCalendar(rwbiasdyn.getId().getObstime()));
                biasMemSpanValueMap.put(rwbiasdyn.getId().getMemspanInd(),
                        biasMemSpanValue);
            }
            break;
        }
    }

    /**
     * Retrieves the {@link DAABiasDyn} records converted to
     * {@link BiasMemSpanValue}s for the current configuration, current run
     * date/time hour, and radar. Based on:
     * hpe_fieldgen/TEXT/read_daabiasdyn_table.c.
     * 
     * @param biasMemSpanValueMap
     *            {@link Map} to place the generated {@link BiasMemSpanValue} in
     * @param runDateTimeHour
     *            the current run date/time hour
     * @param lagHours
     *            maximum number of hours to backtrack when attempting to find
     *            usable {@link DAABiasDyn} records
     * @param officeId
     *            the id of the office associated with the radar
     */
    private void retrieveDAABiasDyn(
            final Map<Short, BiasMemSpanValue> biasMemSpanValueMap,
            final Calendar runDateTimeHour, final int lagHours,
            final String officeId) {
        for (int i = 0; i < lagHours; i++) {
            runDateTimeHour.add(Calendar.HOUR_OF_DAY, -1);
            List<DAABiasDyn> dAABiasDynList = dAABiasDynDao
                    .selectForRadIdWithinTimeRange(radarId, officeId,
                            runDateTimeHour);
            if (dAABiasDynList.isEmpty()) {
                continue;
            }

            for (DAABiasDyn dAABiasDyn : dAABiasDynList) {
                BiasMemSpanValue biasMemSpanValue = new BiasMemSpanValue();
                biasMemSpanValue
                        .setMemspanInd(dAABiasDyn.getId().getMemspanInd());
                biasMemSpanValue.setNumPairs(dAABiasDyn.getNumpairs());
                biasMemSpanValue.setSumgag(dAABiasDyn.getSumgag());
                biasMemSpanValue.setSumrad(dAABiasDyn.getSumgag());
                biasMemSpanValue.setBias(dAABiasDyn.getBias());
                biasMemSpanValue.setObsTime(
                        TimeUtil.newCalendar(dAABiasDyn.getId().getObstime()));
                biasMemSpanValueMap.put(dAABiasDyn.getId().getMemspanInd(),
                        biasMemSpanValue);
            }
            break;
        }
    }

    /**
     * Perform quality control of mean field bias gage-radar pairs. Any
     * gage-radar pairs that are some number of standard deviations away from
     * the mean will be discarded. Based on:
     * hpe_fieldgen/TEXT/gage_radar_pairs_qc.c.
     * 
     * @param gageRadarPairs
     *            {@link List} of [@link GageRadarPair}s to perform quality
     *            control on
     * @param biasLong
     *            the bias value at the longest memory span
     * @return {@code true} if the QC check was successfully performed;
     *         {@code false}, otherwise.
     */
    private boolean performQCGageRadarPairs(
            final List<GageRadarPair> gageRadarPairs, final double biasLong) {
        if (gageRadarPairs.isEmpty()) {
            // no qc to perform, there are no gage-radar pairs.
            return true;
        }

        /*
         * number of gage/radar pairs needed to do standard dev test
         */
        final int requiredPairsStdDevTest = 5;

        /*
         * if there are >= npair_stddev_test number of positive gage/radar
         * pairs, then fit a gage value vs radar value regression line and throw
         * out any gage/radar pair which is more than istd_cut standard
         * deviations away from the line.
         * 
         * if there are < npair_stddev_test number of positive gage/radar pairs,
         * then assume a regression line with slope = longest term bias value if
         * the gage value differs by more than gage_cut_value inches from the
         * point on the line corresponding to the radar value, then throw out
         * the gage/radar pair
         */
        final int npair = gageRadarPairs.size();
        if (npair >= requiredPairsStdDevTest) {
            /*
             * standard cutoff - cutoff error standard deviation
             */
            final int standardCutoff = config.getRwbiasstat().getStdCut();

            /*
             * option -- 1 for one-parameter regression (i.e., slope-only), 2
             * for two-parameter regression (i.e., slope and intercept)
             */
            final int option = config.getRwbiasstat().getBiasQcOpt();

            /*
             * fit two/one-parameter least-square regression line
             */
            double sumGage = 0.0;
            double sumRadar = 0.0;
            double sumGageRadar = 0.0;
            double sumRadar2 = 0.0;
            for (GageRadarPair gageRadarPair : gageRadarPairs) {
                sumGage += gageRadarPair.getGageValue();
                sumRadar += gageRadarPair.getRadarValue();
                sumGageRadar += gageRadarPair.getGageValue()
                        * gageRadarPair.getRadarValue();
                sumRadar2 += gageRadarPair.getRadarValue()
                        * gageRadarPair.getRadarValue();
            }

            double slope = 0.0;
            if (option == 1) {
                slope = sumGageRadar / sumRadar2;
            } else {
                slope = (npair * sumGageRadar - sumGage * sumRadar)
                        / (npair * sumRadar2 - sumRadar * sumRadar);
            }

            /*
             * only continue if the slope is positive.
             */
            if (slope <= 0) {
                return false;
            }

            double b = 0.0;
            // no change for b when option is 1.
            if (option != 1) {
                b = (sumGage - sumRadar * slope) / npair;
            }

            /*
             * compute error statistics
             */
            double sume1 = 0.0;
            double sume2 = 0.0;
            for (GageRadarPair gageRadarPair : gageRadarPairs) {
                final double error = gageRadarPair.getGageValue()
                        - slope * gageRadarPair.getRadarValue() - b;
                sume1 += error;
                sume2 += error * error;
            }

            double ave = sume1 / (double) npair;
            double var = sume2 / (npair - 1)
                    - sume1 * sume1 / (npair * (npair - 1));

            if (var <= 0.0) {
                return false;
            }

            double std = Math.sqrt(var);

            /*
             * standardize errors and cull pairs
             */
            Iterator<GageRadarPair> itr = gageRadarPairs.iterator();
            while (itr.hasNext()) {
                GageRadarPair gageRadarPair = itr.next();
                double error = gageRadarPair.getGageValue()
                        - slope * gageRadarPair.getRadarValue() - b;
                error = (error - ave) / std;
                if (!(Math.abs(error) <= standardCutoff)) {
                    logger.info(
                            "{} has been excluded because > {} standard deviation from the mean.",
                            gageRadarPair.toString(), standardCutoff);
                    itr.remove();
                }
            }
        } else {
            /*
             * for case when number of gage/radar pairs < npair_stddev_test,
             * gage value difference from regression line (units = inches)
             */
            final double gageCutValue = 3.0;

            double slope = biasLong;
            double b = 0.0;

            Iterator<GageRadarPair> itr = gageRadarPairs.iterator();
            while (itr.hasNext()) {
                GageRadarPair gageRadarPair = itr.next();
                double error = gageRadarPair.getGageValue()
                        - slope * gageRadarPair.getRadarValue() - b;
                if (!(Math.abs(error) <= gageCutValue)) {
                    logger.info(
                            "{} has been excluded because > {} standard deviation from the mean.",
                            gageRadarPair.toString(), gageCutValue);
                    itr.remove();
                }
            }
        }

        return true;
    }
}