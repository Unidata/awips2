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
package com.raytheon.uf.edex.plugin.mpe.gather;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.ParseException;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;

/**
 * Replacement for the DHRgather and DSPgather scripts. Those scripts do not
 * perform any file copying or moving (all commands that originally handled it
 * have previously been commented out in the scripts). HPEDhrSrv already handles
 * writing and/or moving the radar products it supports.
 *
 * This script has been setup to scan the DHR or DSP product directory for new
 * files that have been added since its last run. The parameters will be checked
 * in each file that is scanned to determine if the file includes precipitation
 * data.
 *
 * Any time a DHR product that included precipitation was found by the legacy
 * script, it would start and manage and monitor an external process for
 * decoding of both DHR and DSP files. This external process would be allowed to
 * run for a maximum of 20 minutes unless additional precipitation data was
 * discovered. This caused DHR/DSP products to be decoded within that 20-minute
 * window regardless of whether or not they had precipitation. However, this was
 * done only for stability reasons (so that the decoding process wasn't
 * starting/stopping continuously), as radar products without precipitation
 * don't actually need to be decoded. Thus, this is not needed now that the
 * process is on a camel route, so the 20-minute window and decoding products
 * without precipitation have been removed in this implementation.
 *
 * Also, in the legacy implementation, only DHR products were checked for
 * precipitation in order to determine when decoding of both DHR and DSP files
 * should occur. With this new implementation, they are each checked separately
 * to avoid timing issues and ensure that radar products with precipitation are
 * correctly decoded.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2018 5588       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public abstract class MpeRadarGather {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    protected final String productType;

    protected final String gatherDirToken;

    protected final String clusterLockName;

    protected final String clusterLockDetails;

    protected MpeRadarGather(String productType, String gatherDir) {
        this.productType = productType;
        this.gatherDirToken = gatherDir;
        this.clusterLockName = productType
                + MpeRadarGatherConstants.CLUSTER_LOCK_NAME_SUFFIX;
        this.clusterLockDetails = productType
                + MpeRadarGatherConstants.CLUSTER_LOCK_DETAILS_SUFFIX;
    }

    public boolean precipExists() {
        final Date currentRunDate = TimeUtil.newDate();
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        boolean precipFound = false;
        logger.info("{} Gather has started ...", productType);
        if (shouldExecute()) {
            try {
                precipFound = gather(currentRunDate);
            } catch (Exception e) {
                logger.warn(productType + " Gather failed.", e);
            }
        }
        timer.stop();
        logger.info("{} Gather has successfully finished in {}.", productType,
                TimeUtil.prettyDuration(timer.getElapsedTime()));
        return precipFound;
    }

    /**
     * Determines if the Java version of MPE Radar Gather should be allowed to
     * run/execute; dependent on whether or not parallel execution mode has been
     * enabled.
     *
     * @return
     */
    public boolean executeAllowed() {
        return AppsDefaultsConversionWrapper.parallelExecEnabled();
    }

    /**
     * MPE Radar Gather should only be executed when there is one or more radar
     * in use. This method will determine if there is a sufficient number of
     * radars in use.
     *
     * @return {@code true}, if there are a sufficient number of radars in use.
     *         {@code false}, otherwise.
     */
    private boolean shouldExecute() {
        final List<Radarloc> radarsInUse = new RadarlocDao()
                .getInUseRadarlocs();
        if (radarsInUse.isEmpty()) {
            logger.info("No radars are in use. Halting {} Gather.",
                    productType);
            return false;
        }

        logger.info("Found {} radars in use. {} Gather continuing ...",
                radarsInUse.size(), productType);
        return true;
    }

    private boolean gather(final Date currentRunDate) throws Exception {
        /*
         * Get the location of the Product Directory from Apps_Defaults.
         */
        Path productDir = AppsDefaultsConversionWrapper
                .getPathForTokenWithoutCreating(gatherDirToken);
        if (!Files.exists(productDir)) {
            /*
             * If the directory does not exist, there are no products to
             * process.
             */
            logger.info(
                    "The {} Product directory: {} does not exist. Halting {} Gather.",
                    productType, productDir, productType);
            return false;
        }

        /*
         * The legacy DHRgather script used to place an empty file on the
         * filesystem to determine when it last ran. The new version will use
         * cluster locks.
         */
        final ClusterTask clusterTask = ClusterLockUtils
                .lookupLock(clusterLockName, clusterLockDetails);
        Date lastRunDate = null;
        if (clusterTask != null && clusterTask.getExtraInfo() != null) {
            try {
                lastRunDate = MpeRadarGatherConstants.lockDF.get()
                        .parse(clusterTask.getExtraInfo());
            } catch (ParseException e) {
                logger.error(
                        "Failed to parse the date: " + lastRunDate
                                + "associated with the Cluster Lock. Resetting last run date.",
                        e);
            }
        }

        if (lastRunDate == null) {
            /*
             * No previous record of when this Gather was last ran or this is
             * the first time that Gather is running.
             */
            storeCurrentRunDate(currentRunDate, false);
            logger.info(
                    "First run of {} Gather. Successfully prepared for subsequent runs. Halting {} Gather.",
                    productType, productType);
            return false;
        }

        /*
         * Identify all product files that are newer than the last run date.
         */
        final long lastRunDateMillis = lastRunDate.getTime();
        final Set<Path> gatherFilesSet = new HashSet<>();
        Files.walkFileTree(productDir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path path,
                    BasicFileAttributes attrs) throws IOException {
                final long modifiedTime = Files.getLastModifiedTime(path)
                        .toMillis();
                if (modifiedTime >= lastRunDateMillis
                        && modifiedTime < currentRunDate.getTime()) {
                    gatherFilesSet.add(path);
                }
                return FileVisitResult.CONTINUE;
            }
        });

        if (gatherFilesSet.isEmpty()) {
            logger.info(
                    "No {} products created on or after {} were found in the {} Product directory: {}. Halting {} Gather.",
                    productType, lastRunDate.toString(), productType,
                    productDir.toString(), productType);
            storeCurrentRunDate(currentRunDate, true);
            return false;
        }

        /*
         * Determine if the start_hpe script should be started. TODO: this
         * version of DHR/DSP Gather will not actually interact with the
         * successor of start_hpe until it has been implemented in Java.
         */
        boolean precipFound = false;
        for (Path gatherFilePath : gatherFilesSet) {
            try {
                logger.info("Evaluating {} product: {} ...", productType,
                        gatherFilePath.toString());
                MpePrecipCheckRadarSymbologyData radarSymData = new MpePrecipCheckRadarSymbologyData();
                File f = gatherFilePath.toFile();

                boolean hasPrecipParams;
                try (FileInputStream fis = new FileInputStream(f);
                        FileChannel fc = fis.getChannel()) {
                    ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0,
                            f.length());
                    hasPrecipParams = radarSymData
                            .readPrecipIndicationParams(byteBuffer);
                }

                if (!hasPrecipParams) {
                    /*
                     * The precipitation information was not present in the file
                     * that was checked. Remove this file.
                     */
                    logger.info(
                            "Precipitation information not included in {} product: {}.",
                            productType, gatherFilePath);
                    deletePath(gatherFilePath);
                    continue;
                }

                final float param = radarSymData
                        .getPsmParams()[MpeRadarGatherConstants.DHR_PARAM_PRECIP];
                /*
                 * This if/else statement used to be reversed based on the if
                 * statement in: get_dhrpsm/TEXT/main_DHRPsm.c starting on line
                 * 115, which made it appear that a param value > 0 indicated no
                 * precipitation and a value <= 0 indicated precipitation.
                 *
                 * However, DHRgather actually uses the PSM value printed in
                 * that C function (line 113) and not its return value, so a
                 * param value > 0 actually indicates precipitation.
                 */
                if (param > 0.0) {
                    logger.info("Precipitation was detected in {} product: {}.",
                            productType, gatherFilePath);
                    precipFound = true;
                } else {
                    logger.info(
                            "Precipitation was NOT detected in {} product: {}.",
                            productType, gatherFilePath);
                    deletePath(gatherFilePath);
                }
                logger.info("{} product: {} evaluation complete.", productType,
                        gatherFilePath.toString());
            } catch (Exception e) {
                logger.error("Failed to load " + productType
                        + " Radar product: " + gatherFilePath.toString()
                        + ". Skipping file.", e);
            }
        }

        // Clear the gather directory if no precipitation was found
        if (!precipFound) {
            logger.info(
                    "Removing all {} products in {} due to no precipitation.",
                    productType, productDir);
            for (File file : productDir.toFile().listFiles()) {
                if (!file.isFile()) {
                    // Skip non-files
                    continue;
                }
                deletePath(file.toPath());
            }
        }

        storeCurrentRunDate(currentRunDate, true);
        return precipFound;
    }

    private void deletePath(Path path) {
        try {
            Files.delete(path);
        } catch (IOException e) {
            logger.warn("Failed to delete " + productType + " product: " + path,
                    e);
        }
    }

    private void storeCurrentRunDate(final Date currentRunDate,
            final boolean update) {
        final String extraInfo = MpeRadarGatherConstants.lockDF.get()
                .format(currentRunDate);
        if (update) {
            ClusterLockUtils.updateExtraInfo(clusterLockName,
                    clusterLockDetails, extraInfo);
        } else {
            ClusterLockUtils.lock(clusterLockName, clusterLockDetails,
                    extraInfo, 30, true);
        }
    }
}
