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

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.awt.Rectangle;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwparams;
import com.raytheon.uf.common.dataplugin.shef.tables.RwparamsId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwresult;
import com.raytheon.uf.common.dataplugin.shef.tables.RwresultId;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsConfigLoader;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsLoadException;
import com.raytheon.uf.edex.plugin.mpe.apps.RequiredTokenMissingException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwbiasstatDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwparamsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwresultDao;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.RadarBeamHeightFile;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic.DHRMosaicGenerator;
import com.raytheon.uf.edex.plugin.mpe.geo.BinaryGeoDataFile;
import com.raytheon.uf.edex.plugin.mpe.geo.MpeGageLocationsGeoDataAsciiFile;
import com.raytheon.uf.edex.plugin.mpe.geo.TownGeoDataAsciiFile;
import com.raytheon.uf.edex.plugin.mpe.precip.GageData;
import com.raytheon.uf.edex.plugin.mpe.precip.PrecipDataRecord;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;

/**
 * The High-resolution Precipitation Estimator (HPE). Based on:
 * hpe_fieldgen/TEXT/main_empe_fieldgen.c.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2016 5631       bkowal      Initial creation
 * Aug 31, 2016 5631       bkowal      Load the remaining configuration. Prepare to
 *                                     load inputs.
 * Sep 13, 2016 5631       bkowal      Additional data gathering and config packaging.
 * Sep 19, 2016 5631       bkowal      Gather remaining inputs. Prepare to produce outputs.
 * Sep 27, 2016 5631       bkowal      Started implementation of mosaic generation.
 * Oct 11, 2016 5631       bkowal      Defined the id array which will be used across all
 *                                     mosaic generation algorithms.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class HPEFieldgen {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final RwparamsDao rwparamsDao = new RwparamsDao();

    private static final RwbiasstatDao rwbiasstatDao = new RwbiasstatDao();

    private static final int DEFAULT_RUN_HOURS = 1;

    private static final String HPE_GEN_SEPARATOR = ",";

    private static final String RFC_BOUNDARY_BIN_FILE = "rfc_boundary.bin";

    private static final String RFC_STATE_BIN_FILE = "state.bin";

    public HPEFieldgen() {
    }

    public void execute() {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        logger.info("HPE Field Gen has started ...");
        logger.info("HPE Precip Processing -- Run: {}",
                TimeUtil.newCalendar().getTime().toString());

        try {
            final HPERunConfiguration runConfig = configureRun();
            final HPEFieldGenInputs inputs = gatherInputs(runConfig);
            produceOutputs(runConfig, inputs);
        } catch (Exception e) {
            timer.stop();
            logger.error("HPE Field Gen has failed to finished in "
                    + TimeUtil.prettyDuration(timer.getElapsedTime()) + ".", e);
            return;
        }

        timer.stop();
        logger.info("HPE Field Gen has successfully finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    private HPERunConfiguration configureRun() throws Exception {
        HPERunConfiguration runConfig = new HPERunConfiguration();
        HPEFieldGenConfig config = readAppsDefaultsConfig();
        runConfig.setConfig(config);

        /*
         * Initialize the date value with current date. Always set the seconds
         * to 0.
         */
        final Calendar runDateTime = TimeUtil.newGmtCalendar();
        TimeUtil.minCalendarFields(runDateTime, Calendar.SECOND,
                Calendar.MILLISECOND);
        /*
         * adjust the run time by diminishing the lag time.
         */
        runDateTime.add(Calendar.MINUTE, -config.getTimeLag());
        runConfig.setRunDateTime(runDateTime);
        /*
         * Determine how many hours of data should be analyzed. For now the
         * default will just be used, as it is in the existing native code
         * baseline. However, this could also be made configurable via a
         * localization file because it can also (optionally) be specified as a
         * command-line argument to the hpe fieldgen program.
         */
        final int runHours = DEFAULT_RUN_HOURS;
        runConfig.setRunHours(runHours);

        /*
         * determine which generation types the mean field bias will need to be
         * calculated for.
         */
        final Set<HPERadarMosaic> productsToGenerate = determineProductsToGenerate(
                config.getBaseMosaic(), config.getBestMosaic());
        runConfig.setProductsToGenerate(productsToGenerate);

        /*
         * handle the special case flags now that all products that need to be
         * generated are known.
         */

        /*
         * Need to calculate the mean field bias when ebmosaic is ON
         */
        boolean calculateMeanFieldBias = productsToGenerate
                .contains(HPERadarMosaic.EBMOSAIC);
        runConfig.setCalculateMeanFieldBias(calculateMeanFieldBias);

        /*
         * Need to compute the dhrmosaic when bdhrmosaic is ON
         */
        boolean dhrMeanFieldBias = productsToGenerate
                .contains(HPERadarMosaic.BDHRMOSAIC);
        runConfig.setDhrMeanFieldBias(dhrMeanFieldBias);

        /*
         * Need to read the prism data when one of the following is ON: mmosiac,
         * mlmosaic, gageonly
         */
        boolean retrievePrismData = (productsToGenerate
                .contains(HPERadarMosaic.MMOSAIC)
                || productsToGenerate.contains(HPERadarMosaic.MLMOSAIC)
                || productsToGenerate.contains(HPERadarMosaic.GAGEONLY));
        runConfig.setRetrievePrismData(retrievePrismData);
        logger.info("Retrieve prism data: {}", retrievePrismData);

        /*
         * read the geo grid data.
         */
        Rectangle geoGridData = readGeoGridData(config.getHrapGridFactor());
        logger.info(
                "Loaded geographic data: hrap_x={}, hrap_y={}, num_cols={}, num_rows={}.",
                geoGridData.x, geoGridData.y, geoGridData.width,
                geoGridData.height);
        runConfig.setGeoGridData(geoGridData);

        /*
         * read the overlays
         */
        final GeoData geoFileData = readGeoFileData(config.getHrapGridFactor(),
                config.getGeoBinPath(), config.getGeoAsciiPath(),
                config.getGageLocationPath());
        runConfig.setGeoFileData(geoFileData);

        /*
         * read static parameters from database for running HPE Field Gen.
         */
        /*
         * read rwparams - based on: hpe_fieldgen/TEXT/read_rwparams.c
         */
        List<Rwparams> rwparamsList = rwparamsDao.getRwparamsRecords();
        if (rwparamsList.isEmpty()) {
            throw new HPEFieldGenConfigurationException(
                    "No Rwparams are present in the ihfs database.");
        }
        /*
         * Just use the first.
         */
        Rwparams rwparams = rwparamsList.iterator().next();
        runConfig.setRwparams(rwparams);

        /*
         * Previously the C++ code would iterate through all returned rows and
         * set certain fields to {@code null} based on the value of an indicator
         * variable that was extracted by the cursor
         * (https://www.ibm.com/support
         * /knowledgecenter/ssw_ibm_i_61/rzajp/rzajpindvar.htm). However, the
         * Java code is capable of retrieving null values as null values.
         */

        /*
         * read rwbiasstat - based on: hpe_fieldgen/TEXT/read_rwbiasstat.c.
         * Note: the legacy code was written to handle multiple records.
         * However, that is unnecessary because the record is being retrieved by
         * the primary key.
         */
        Rwbiasstat rwbiasstat = rwbiasstatDao
                .getForOffice(config.getFxaLocalSite());
        runConfig.setRwbiasstat(rwbiasstat);

        /*
         * Log configuration information. Based on:
         * /rary.ohd.pproc/src/hpe_fieldgen/TEXT/write_params.c.
         */
        logger.info("***** BEGIN: SYSTEM PARAMETERS *****");
        logger.info("dhr window={} (minutes)", config.getDhrWindow());
        logger.info("dsp window={} (minutes)", config.getDspWindow());
        logger.info("dsp duration={} (minutes)", config.getDspDuration());
        logger.info("xmrgdtform={}", config.getDateFormat().getText());
        logger.info("***** END: SYSTEM PARAMETERS *****");

        logger.info(
                "***** BEGIN: MEAN FIELD BIAS CALCULATION PARAMETERS *****");
        logger.info("min_gr_value_bias={}", rwbiasstat.getMinGrValueBias());
        logger.info("npair_bias_select={}", rwbiasstat.getNpairBiasSelect());
        logger.info("npair_svar_update={}", rwbiasstat.getNpairSvarUpdate());
        logger.info("std_cut={}", rwbiasstat.getStdCut());
        logger.info("lag_cut={}", rwbiasstat.getLagCut());
        logger.info("init_span={}", rwbiasstat.getInitSpan());
        logger.info("bias_qc_opt={}", rwbiasstat.getBiasQcOpt());
        logger.info(
                "memory span values: memSpan1={}, memSpan2={}, memSpan3={}, memSpan4={}, memSpan5={}, memSpan6={}, memSpan7={}, memSpan8={}, memSpan9={}, memSpan10={}",
                rwbiasstat.getMemSpan1(), rwbiasstat.getMemSpan2(),
                rwbiasstat.getMemSpan3(), rwbiasstat.getMemSpan4(),
                rwbiasstat.getMemSpan5(), rwbiasstat.getMemSpan6(),
                rwbiasstat.getMemSpan7(), rwbiasstat.getMemSpan8(),
                rwbiasstat.getMemSpan9(), rwbiasstat.getMemSpan10());
        logger.info("***** END: MEAN FIELD BIAS CALCULATION PARAMETERS *****");

        logger.info("***** BEGIN: MULTISENSOR FIELD ANALYSIS PARAMETERS *****");
        logger.info("rw_min_rain={}", rwparams.getId().getRwMinRain());
        logger.info("rw_sep_dist={}", rwparams.getId().getRwSepDist());
        logger.info("rw_lag0_ind_corr={}", rwparams.getId().getRwLag0IndCorr());
        logger.info("num_near_gages={}", rwparams.getId().getNumNearGages());
        logger.info("num_near_rad_bins={}",
                rwparams.getId().getNumNearRadBins());
        logger.info("def_cond_var_rad={}", rwparams.getId().getDefCondVarRad());
        logger.info("def_ind_corr_scl={}", rwparams.getId().getDefIndCorrScl());
        logger.info("min_ind_corr_scl={}", rwparams.getId().getMinIndCorrScl());
        logger.info("min_cond_corr_scl={}",
                rwparams.getId().getMinCondCorrScl());
        logger.info("max_ind_corr_scl={}", rwparams.getId().getMaxIndCorrScl());
        logger.info("max_cond_corr_scl={}",
                rwparams.getId().getMaxCondCorrScl());
        logger.info("nearest-neighbor search method = {}",
                (rwparams.getId()
                        .getNnSrchMethod() == RwparamsId.DOUBLE_HEAP_SORTING_SEARCH_IND)
                                ? RwparamsId.DOUBLE_HEAP_SORTING_SEARCH
                                : RwparamsId.SPIRAL_SEARCH);
        logger.info("***** END: MULTISENSOR FIELD ANALYSIS PARAMETERS *****");
        return runConfig;
    }

    private HPEFieldGenInputs gatherInputs(final HPERunConfiguration runConfig)
            throws HPEFieldGenConfigurationException {
        final HPEFieldGenConfig config = runConfig.getConfig();
        final HPEFieldGenInputs inputs = new HPEFieldGenInputs();
        /*
         * Read the Gage Data if it is the top of the hour.
         */
        if (config.getDspDuration() == 60
                && runConfig.getRunDateTime().get(Calendar.MINUTE) == 0) {
            /*
             * It is the top of the hour.
             */
            final GageDataManager gageDataManager = new GageDataManager(
                    runConfig);
            final Map<Calendar, GageData> gageDataMap = gageDataManager
                    .retrieveGageData();
            inputs.setGageDataMap(gageDataMap);
        }

        /*
         * Read in radar identifiers, lat/lon and heights.
         */
        final Map<String, RadarLocRecord> radarLocMap = retrieveRadarsInUseLocations();
        logger.info("Found {} active radar locations.", radarLocMap.size());
        inputs.setRadarLocMap(radarLocMap);

        /*
         * Build the category name based on dsp_duration.
         */
        final String categoryName = HPEFieldgenUtils
                .buildCategoryName(config.getDspDuration());
        logger.info("Generated category name: {}.", categoryName);
        inputs.setCategoryName(categoryName);

        /*
         * for multiple hours case: set the first run time to the earliest time.
         */
        runConfig.getRunDateTime().add(Calendar.HOUR_OF_DAY,
                -(runConfig.getRunHours() - 1));

        // Perform the load of the radar beam height.
        final double[][] radarBeamHeight;
        try {
            radarBeamHeight = RadarBeamHeightFile.readFileAtLocation(
                    config.getBeamHeightPath(), config.getHrapGridFactor());
            inputs.setRadarBeamHeight(radarBeamHeight);
        } catch (Exception e) {
            throw new HPEFieldGenConfigurationException(
                    "Failed to load the Radar Beam Height file: "
                            + config.getBeamHeightPath().resolve(
                                    RadarBeamHeightFile.FILE_NAME)
                            + ".",
                    e);
        }
        return inputs;
    }

    private void produceOutputs(final HPERunConfiguration runConfig,
            final HPEFieldGenInputs inputs) {
        for (int i = 0; i < runConfig.getRunHours(); i++) {
            boolean radarProcessed = false;

            /*
             * Lookup the Gage Sizes.
             */
            final Map<Calendar, GageData> gageDataMap = inputs.getGageDataMap();
            int gageSize = 0;
            int gageSizeP3 = 0;

            short[] iug = null;
            short[] ivg = null;
            float[] zg = null;

            if (!gageDataMap.isEmpty()) {
                final GageData gageData = gageDataMap
                        .get(runConfig.getRunDateTime());
                gageSize = gageData.getTotalGageCount();
                gageSizeP3 = gageData.getTotalGageP3Count();

                iug = new short[gageSize];
                ivg = new short[gageSize];
                zg = new float[gageSize];
                List<PrecipDataRecord> gagePrecip = gageData.getGagePrecip();
                for (int j = 0; j < gageSize; j++) {
                    PrecipDataRecord precip = gagePrecip.get(i);
                    iug[j] = (short) precip.getX();
                    ivg[i] = (short) precip.getY();
                    zg[j] = (float) precip.getValue();
                }
            }

            logger.info("Begin HPE Field Gen MOSAIC generation for: {}",
                    runConfig.getRunDateTime().getTime().toString());

            /*
             * Read prism data if required.
             */
            if (runConfig.isRetrievePrismData()) {
                // TODO: finish
            }

            /*
             * for this hour, check the autosave flag in the rwresult table.
             * this will indicate to subsequent modules whether or not to
             * overwrite the best estimate qpe.
             */
            boolean overwriteBestEst = checkAutoSave(
                    runConfig.getConfig().getRfcName(),
                    runConfig.getRunDateTime());

            /*
             * run mosaic functions based on mosaic status value.
             * 
             * there are 4 products that are fully tested and delivered for
             * ob83: dhrmosaic, bdhrmosaic, ermosaic and ebmosaic
             * 
             * and there are 5 more products are developed but not fully tested
             * and therefore not delivered in ob83.
             */
            List<HPERadarMosaic> productsToGenerate = new ArrayList<>(
                    runConfig.getProductsToGenerate());
            Collections.sort(productsToGenerate,
                    new HPERadarMosaicDependencyComparator());
            final int[][] id = new int[(int) runConfig.getGeoGridData()
                    .getHeight()][(int) runConfig.getGeoGridData().getWidth()];
            /*
             * The C++ code originally pre-initialized the id array to the
             * ID_DEFAULT constant. However, due to the fact that the ID_DEFAULT
             * constant is 0, the array is already pre-initialized in Java.
             */
            for (HPERadarMosaic hpeRadarMosaic : productsToGenerate) {
                logger.info(
                        "Generation for: {} with dependencies count: {} ...",
                        hpeRadarMosaic.name(),
                        hpeRadarMosaic.getDependencies().length);
                if (HPERadarMosaic.DHRMOSAIC == hpeRadarMosaic) {
                    try {
                        final DHRMosaicGenerator generator = new DHRMosaicGenerator(
                                runConfig, inputs);
                        generator.generateMosaic(id);
                    } catch (Exception e) {
                        logger.error("Failed to generate radar mosaic: "
                                + hpeRadarMosaic.name() + ".", e);
                        /*
                         * Note: this will have repercussions if there are radar
                         * mosaics that depend on this radar mosaic.
                         */
                    }
                }
            }
        }
    }

    /**
     * Checks the {@link Rwresult} for the specified rfc and observation
     * date/time to determine whether or not the best estimate qpe can be
     * overwritten. Based on: hpe_fieldgen/TEXT/check_autosave.c.
     * 
     * @param rfc
     *            the specified rfc
     * @param obsTime
     *            the specified observation date/time
     * @return {@code true} when the best estimate qpe can be overwritten;
     *         {@code false}, otherwise.
     */
    private boolean checkAutoSave(final String rfc, final Calendar obsTime) {
        RwresultId id = new RwresultId();
        id.setRfc(rfc);
        id.setObstime(obsTime.getTime());

        RwresultDao dao = new RwresultDao();
        Rwresult rwresult = dao.retrieveById(id);
        if (rwresult == null) {
            return false;
        }

        if ("F".equals(rwresult.getAutoSave())) {
            return true;
        }

        return false;
    }

    /**
     * Retrieves any {@link Radarloc}s indicated to be in use and converts them
     * to a {@link Map} of {@link RadarLocRecord}s. Based on:
     * hpe_fieldgen/TEXT/read_radarloc.c.
     * 
     * @return the generated {@link Map} of {@link RadarLocRecord}s
     */
    private Map<String, RadarLocRecord> retrieveRadarsInUseLocations() {
        final RadarlocDao radarlocDao = new RadarlocDao();
        List<Radarloc> radarsInUse = radarlocDao.getInUseRadarlocs();
        if (radarsInUse.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<String, RadarLocRecord> radarLocMap = new HashMap<>(
                radarsInUse.size(), 1.0f);
        radarsInUse.stream().forEach(
                (r) -> radarLocMap.put(r.getRadid(), new RadarLocRecord(r)));
        return radarLocMap;
    }

    /**
     * Returns the {@link HPERadarMosaic} associated with the specified mosaic
     * name.
     * 
     * @param mosaicName
     *            the specified mosaic name
     * @param appsToken
     *            the Apps_defaults token that the specified mosaic name was
     *            read from
     * @return the associated {@link HPERadarMosaic}
     * @throws HPEFieldGenConfigurationException
     */
    private HPERadarMosaic lookupRadarMosaicByName(final String mosaicName,
            final String appsToken) throws HPEFieldGenConfigurationException {
        HPERadarMosaic mosaic = HPERadarMosaic.lookupRadarMosaic(mosaicName);
        if (mosaic == null) {
            throw new HPEFieldGenConfigurationException(
                    "An invalid value has been specified for token: "
                            + HPEFieldgenConstants.AppsDefaults.HPE_BASE_RADAR_MOSAIC
                            + " in " + AppsDefaults.NAME
                            + ". The specified value is not associated with a recognized radar mosaic.");
        }

        return mosaic;
    }

    /**
     * Checks Apps_defaults to determine which mosaic products need to be
     * generated in addition to the specified base and best mosaics. Based on
     * the get_empe_product_state function in
     * hpe_fieldgen/TEXT/get_empe_product_state.c.
     * 
     * @param baseMosaic
     *            the specified base mosaic
     * @param bestMosaic
     *            the specified best mosaic
     * @return a {@link Set} containing the {@link HPERadarMosaic}s to generate
     *         products for
     */
    private Set<HPERadarMosaic> determineProductsToGenerate(
            final HPERadarMosaic baseMosaic, final HPERadarMosaic bestMosaic) {
        final Set<HPERadarMosaic> productsToGenerate = new HashSet<>(
                HPERadarMosaic.values().length, 1.0f);
        /*
         * Always generate the best and base product(s). Base products should
         * not have any dependencies.
         */
        productsToGenerate.add(baseMosaic);
        productsToGenerate.add(bestMosaic);
        /*
         * The base mosaic should not have any dependencies. However, the best
         * mosaic may. Ensure that they are accounted for.
         */
        addMosaicDependenciesToGeneration(productsToGenerate, bestMosaic);

        String generateListString = AppsDefaults.getInstance().getToken(
                HPEFieldgenConstants.AppsDefaults.HPE_GENERATE_LIST, null);
        generateListString = (generateListString == null) ? null
                : generateListString.trim();
        if (generateListString == null || generateListString.isEmpty()) {
            logger.info(
                    "No additional radar mosaics have been specified for generation in {} for token: {}.",
                    AppsDefaults.NAME,
                    HPEFieldgenConstants.AppsDefaults.HPE_GENERATE_LIST);
        } else {
            /*
             * Add any valid radar mosaics found in the generation list to the
             * collection of products to generate.
             */
            /*
             * Iterate through the list of potential radar mosaics to generate.
             */
            for (String potentialMosaic : generateListString
                    .split(HPE_GEN_SEPARATOR)) {
                potentialMosaic = potentialMosaic.trim();
                try {
                    HPERadarMosaic mosaic = lookupRadarMosaicByName(
                            potentialMosaic,
                            HPEFieldgenConstants.AppsDefaults.HPE_GENERATE_LIST);
                    productsToGenerate.add(mosaic);
                    addMosaicDependenciesToGeneration(productsToGenerate,
                            mosaic);
                } catch (HPEFieldGenConfigurationException e) {
                    logger.warn(
                            potentialMosaic
                                    + " is not a recognized radar mosaic. Skipping.",
                            e);
                }
            }
        }

        return productsToGenerate;
    }

    /**
     * Add any dependencies associated with the specified {@link HPERadarMosaic}
     * to the existing specified {@link Set} of products to generate.
     * 
     * @param productsToGenerate
     *            the specified {@link Set} of products to generate
     * @param mosaic
     *            the specified {@link HPERadarMosaic}
     */
    private void addMosaicDependenciesToGeneration(
            final Set<HPERadarMosaic> productsToGenerate,
            final HPERadarMosaic mosaic) {
        if (mosaic.getDependencies().length == 0) {
            return;
        }
        for (HPERadarMosaic dependency : mosaic.getDependencies()) {
            productsToGenerate.add(dependency);
            addMosaicDependenciesToGeneration(productsToGenerate, dependency);
        }
    }

    /**
     * Reads any needed Apps Defaults properties into a
     * {@link HPEFieldGenConfig}. Based on: hpe_fieldgen/TEXT/read_params.c.
     * 
     * @return the generated {@link HPEFieldGenConfig}.
     */
    private HPEFieldGenConfig readAppsDefaultsConfig()
            throws HPEFieldGenConfigurationException {
        /*
         * Skipping the OS information retrieval. It is unlikely (as of August
         * 2016), the OS will ever evaluate to "hp-ux" again.
         */

        /*
         * Skipping the database name retrieval. That is handled internally by
         * EDEX.
         */

        HPEFieldGenConfig config = new HPEFieldGenConfig();
        try {
            AppsDefaultsConfigLoader.populateFromAppsDefaults(config);
        } catch (RequiredTokenMissingException | AppsDefaultsLoadException e) {
            throw new HPEFieldGenConfigurationException(
                    "Failed to configure HPE Field Gen.", e);
        }

        return config;
    }

    /**
     * Reads the geo data and adjusts it based on the specified
     * {@link HrapGridFactor}. Based on: hpe_fieldgen/TEXT/read_geo_data.c.
     * 
     * @param hrapGridFactor
     *            the specified {@link HrapGridFactor}
     * @return the geo data that was read as a {@link Rectangle}
     */
    private Rectangle readGeoGridData(final HrapGridFactor hrapGridFactor)
            throws HPEFieldGenConfigurationException {
        Rectangle geoData = null;
        try {
            geoData = HRAPCoordinates.getHRAPCoordinates();
        } catch (NumberFormatException | IOException e) {
            throw new HPEFieldGenConfigurationException(
                    "Failed to read the Geo Data.", e);
        }

        /*
         * Update the geo data based on the HRAP Grid Factor.
         */
        geoData.x *= hrapGridFactor.getNum();
        geoData.y *= hrapGridFactor.getNum();
        geoData.width *= hrapGridFactor.getNum();
        geoData.height *= hrapGridFactor.getNum();

        return geoData;
    }

    /**
     * Reads the ascii and binary geo data files. Packages all geo information
     * that is read into a {@link GeoData}. Based on: function rfcw_load_static
     * in hpe_fieldgen/TEXT/save_gif.c, hpe_fieldgen/TEXT/get_loc_latlon.c.
     * 
     * @param hrapGridFactor
     *            optional {@link HrapGridFactor} used to scale the town geo
     *            data
     * @param geoBinPath
     *            {@link Path} to the binary geo data files
     * @param geoAsciiPath
     *            {@link Path} to the ascii geo data files
     * @return the generated {@link GeoData}
     * @throws HPEFieldGenConfigurationException
     */
    private GeoData readGeoFileData(final HrapGridFactor hrapGridFactor,
            final Path geoBinPath, final Path geoAsciiPath,
            final Path gageLocationPath)
                    throws HPEFieldGenConfigurationException {
        final Path rfcBoundaryPath = geoBinPath.resolve(RFC_BOUNDARY_BIN_FILE);
        final Path statePath = geoBinPath.resolve(RFC_STATE_BIN_FILE);
        final Path townPath = geoAsciiPath
                .resolve(TownGeoDataAsciiFile.FILE_NAME);
        final Path gageLocationAsciiGeoPath = gageLocationPath
                .resolve(MpeGageLocationsGeoDataAsciiFile.FILE_NAME);

        BinaryGeoDataFile rfcBoundaryOverlay = null;
        BinaryGeoDataFile stateOverlay = null;
        TownGeoDataAsciiFile townGeoData = null;
        MpeGageLocationsGeoDataAsciiFile gageLocationsGeoData = null;
        try {
            logger.info("Using rfc boundary overlay file: {} ...",
                    rfcBoundaryPath.toString());
            rfcBoundaryOverlay = BinaryGeoDataFile.loadFile(rfcBoundaryPath);
            logger.info("Using state boundary overlay file: {} ...",
                    statePath.toString());
            stateOverlay = BinaryGeoDataFile.loadFile(statePath);
            logger.info("Using rfc town overlay file: {} ...",
                    townPath.toString());
            townGeoData = TownGeoDataAsciiFile.loadFile(townPath,
                    hrapGridFactor);
            logger.info("Using gage locations geo data file: {} ...",
                    gageLocationAsciiGeoPath.toString());
            gageLocationsGeoData = MpeGageLocationsGeoDataAsciiFile
                    .loadFile(gageLocationAsciiGeoPath);
            return new GeoData(rfcBoundaryOverlay, stateOverlay, townGeoData,
                    gageLocationsGeoData);
        } catch (IOException e) {
            logger.error("Failed to read the Overlay Data.", e);
            return null;
        }
    }
}