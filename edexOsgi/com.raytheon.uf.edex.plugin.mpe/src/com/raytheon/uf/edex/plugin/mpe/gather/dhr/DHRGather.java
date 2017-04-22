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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;

/**
 * Replacement for the DHRgather script. The DHRgather script does not perform
 * any file copying or moving (all commands that originally handled it have
 * previously been commented out in the script). {@link HPEDhrSrv} already
 * handles writing and/or moving the radar products it supports.
 * 
 * This script has been setup to scan the DHR product directory for new files
 * that have been added since its last run. The parameters will be checked in
 * each file that is scanned to determine if the file includes precipitation
 * data. For now, this implementation will just log the final result until more
 * of the downstream processes are converted (DR #5588) at which point a direct
 * "chain" between mpe phases can be established.
 * 
 * Any time a radar product that included precipitation was found by the legacy
 * script, it would start and manage and monitor an external process. This
 * external process would be allowed to run for a maximum of 20 minutes unless
 * additional precipitation data was discovered. That implementation will remain
 * as part of the legacy implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRGather {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public void execute() {
        final Date currentRunDate = TimeUtil.newDate();
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        logger.info("DHRGather has started ...");
        if (shouldExecute()) {
            try {
                gather(currentRunDate);
            } catch (Exception e) {

            }
        }
        timer.stop();
        logger.info("DHRGather has successfully finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    /**
     * {@link DHRGather} should only be executed when there is one or more radar
     * in use. This method will determine if there is a sufficient number of
     * radars in use.
     * 
     * @return {@code true}, if there are a sufficient number of radars in use.
     *         {@code false}, otherwise.
     */
    private boolean shouldExecute() {
        final List<Radarloc> radarsInUse = new RadarlocDao().getInUseRadarlocs();
        if (radarsInUse.isEmpty()) {
            logger.info("No radars are in use. Halting DHR Gather.");
            return false;
        }

        logger.info("Found {} radars in use. DHR Gather continuing ...",
                radarsInUse.size());
        return true;
    }

    private void gather(final Date currentRunDate) throws Exception {
        /*
         * Get the location of the DHR Product Directory from Apps_Defaults.
         */
        final String dhrProdDir = AppsDefaults.getInstance().getToken(
                DHRGatherConstants.AppsDefaults.DHR_PROD_DIR, null);
        if (dhrProdDir == null) {
            throw new DHRGatherFailedException(
                    "Failed to retrieve the DHR Product directory from "
                            + AppsDefaults.NAME + ". Please verify that the '"
                            + DHRGatherConstants.AppsDefaults.DHR_PROD_DIR
                            + "' property has been defined.");
        }

        Path dhrProductPath = Paths.get(dhrProdDir);
        if (!Files.exists(dhrProductPath)) {
            /*
             * If the directory does not exist, there are not any dhr products
             * to process.
             */
            logger.info(
                    "The DHR Product directory: {} does not exist. Halting DHR Gather.",
                    dhrProductPath.toString());
            return;
        }

        /*
         * The legacy DHRgather script used to place an empty file on the
         * filesystem to determine when it last ran. The new version will use
         * cluster locks.
         */
        final ClusterTask clusterTask = ClusterLockUtils.lookupLock(
                DHRGatherConstants.CLUSTER_LOCK_NAME,
                DHRGatherConstants.CLUSTER_LOCK_DETAILS);
        Date lastRunDate = null;
        if (clusterTask != null && clusterTask.getExtraInfo() != null) {
            try {
                lastRunDate = DHRGatherConstants.lockDF.get().parse(
                        clusterTask.getExtraInfo());
            } catch (ParseException e) {
                logger.error(
                        "Failed to parse the date: "
                                + lastRunDate
                                + "associated with the Cluster Lock. Resetting last run date.",
                        e);
            }

            if (lastRunDate == null) {
                /*
                 * No previous record of when DHRGather was last ran or this is
                 * the first time that DHRGather is running.
                 */
                storeCurrentRunDate(currentRunDate, false);
                logger.info("First run of DHR Gather. Successfully prepared for subsequent runs. Halting DHR Gather.");
                return;
            }
        }

        /*
         * Identify all DHR product files that are newer than the last run date.
         */
        final long lastRunDateMillis = lastRunDate.getTime();
        final Set<Path> gatherFilesSet = new HashSet<>();
        Files.walkFileTree(dhrProductPath, new SimpleFileVisitor<Path>() {
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
                    "No DHR products created on or after {} were found in the DHR Product directory: {}. Halting DHR Gather.",
                    lastRunDate.toString(), dhrProductPath.toString());
            storeCurrentRunDate(currentRunDate, true);
            return;
        }

        /*
         * Determine if the start_hpe script should be started. TODO: this
         * version of DHR Gather will not actually interact with the successor
         * of start_hpe until it has been implemented in Java.
         */
        for (Path gatherFilePath : gatherFilesSet) {
            try {
                logger.info("Evaluating DHR product: {} ...",
                        gatherFilePath.toString());
                DHRRadarFile dhrRadarFile = DHRRadarFile.loadDHRRadarFile(
                        gatherFilePath, DHRGatherConstants.DHR_DEFAULT_VERSION);
                final float param = dhrRadarFile.getDhParams()
                        .getAdaptableParams()[DHRGatherConstants.DHR_PARAM_PRECIP];
                /*
                 * This if statement is based on the if statement in:
                 * get_dhrpsm/TEXT/main_DHRPsm.c starting on line 115. A param
                 * value > 0 indicated no precipitation and a value <= 0
                 * indicated precipitation.
                 */
                if (param > 0.0) {
                    logger.info("Precipitation was NOT detected in DHR prodcut: {}.");
                } else {
                    logger.info("Precipitation was detected in DHR prodcut: {}.");
                }
                logger.info("DHR product: {} evaluation complete.",
                        gatherFilePath.toString());
            } catch (Exception e) {
                logger.error("Failed to load DHR Radar product: "
                        + gatherFilePath.toString() + ". Skipping file.", e);
            }
        }
        storeCurrentRunDate(currentRunDate, true);
    }

    private void storeCurrentRunDate(final Date currentRunDate,
            final boolean update) {
        final String extraInfo = DHRGatherConstants.lockDF.get().format(
                currentRunDate);
        if (update) {
            ClusterLockUtils.updateExtraInfo(
                    DHRGatherConstants.CLUSTER_LOCK_NAME,
                    DHRGatherConstants.CLUSTER_LOCK_DETAILS, extraInfo);
        } else {
            ClusterLockUtils.lock(DHRGatherConstants.CLUSTER_LOCK_NAME,
                    DHRGatherConstants.CLUSTER_LOCK_DETAILS, extraInfo, 30,
                    true);
        }
    }
}