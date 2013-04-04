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
package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Implementation of a Camel Message Processor to check if the ingested grib
 * file matches one of the designated "large" grib files. If this processor
 * detects a file containing one of WMO regexes contained in the
 * largeGribPatterns.xml, all other grib ingest threads will block to wait for
 * this file to finish processing. Cluster locking is used. The lock is obtained
 * in this Processor. A second processor, GribLockRelease, is used to release
 * the lock once the file is done being processed.
 * <p>
 * This processor was put in place to ensure large grib files will not throw out
 * of memory exceptions while processing.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/15/10     6644        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 * @see com.raytheon.edex.plugin.grib.GribLockRelease
 */
public class GribLargeFileChecker implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribLargeFileChecker.class);

    /**
     * The header attribute attached to the message to denote whether this file
     * will receive single threaded handling
     */
    public static final String LARGE_FILE_HEADER = "largeFile";

    /** The cluster task name used for cluster locking */
    public static final String CLUSTER_TASK_NAME = "GribIngestLargeFile";

    public static final String CLUSTER_TASK_DETAILS;

    /**
     * The base localization patterns used to specify which WMO header regexes
     * receive special handling
     */
    private static LargeGribPatterns basePatterns;

    /**
     * The site localization patterns used to specify which WMO header regexes
     * receive special handling
     */
    private static LargeGribPatterns sitePatterns;

    static {
        String host = null;
        try {
            // Initialize the cluster task name with the host name
            host = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            e.printStackTrace();
            host = "";
        }
        CLUSTER_TASK_DETAILS = host;
    }

    @Override
    public void process(Exchange exchange) throws Exception {
        if (basePatterns == null) {
            loadPatterns();
        }
        File gribFile = (File) exchange.getIn().getBody();
        String header = (String) exchange.getIn().getHeader("header");
        if (header == null) {
            // No header entry so will try and use the filename instead
            header = gribFile.getName();
        }

        ClusterTask task = ClusterLockUtils.lookupLock(CLUSTER_TASK_NAME,
                CLUSTER_TASK_DETAILS);
        boolean waitForLargeGrib = task.isRunning();

        while (waitForLargeGrib) {
            Thread.sleep(500);
            task = ClusterLockUtils.lookupLock(CLUSTER_TASK_NAME,
                    CLUSTER_TASK_DETAILS);
            waitForLargeGrib = task.isRunning();

            // need to handle timing out of large grib file process manually
            if (waitForLargeGrib
                    && (System.currentTimeMillis() - task.getLastExecution()) > 120000) {
                statusHandler
                        .handle(Priority.EVENTA,
                                "Large Grib file process timed out.  Clearing lock and resuming processing");
                ClusterLockUtils
                        .unlock(CLUSTER_TASK_NAME, CLUSTER_TASK_DETAILS);
                waitForLargeGrib = false;
            }
        }

        if (isLarge(header)) {
            statusHandler.handle(Priority.EVENTA,
                    "Large Grib file detected.  Establishing lock.");
            while (!ClusterLockUtils
                    .lock(CLUSTER_TASK_NAME, CLUSTER_TASK_DETAILS, 120000, true)
                    .getLockState().equals(LockState.SUCCESSFUL)) {
                Thread.sleep(100);
            }
            statusHandler.handle(Priority.EVENTA,
                    "Large Grib file lock established.");
            // Wait for other threads to complete processing before we proceed
            Thread.sleep(1000);
            exchange.getIn().setHeader(LARGE_FILE_HEADER, true);

        } else {
            exchange.getIn().setHeader(LARGE_FILE_HEADER, false);
        }
    }

    /**
     * Loads the WMO header patterns for grib files which receive special
     * handling
     * 
     * @throws GribException
     *             If the patterns cannot be loaded
     */
    private void loadPatterns() throws GribException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext siteStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.SITE);
        String path = "";
        String sitePath = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "grib" + File.separator + "largeGribPatterns.xml")
                    .getCanonicalPath();
            sitePath = pathMgr.getFile(siteStaticBase,
                    "grib" + File.separator + "largeGribPatterns.xml")
                    .getCanonicalPath();
        } catch (IOException e) {
            throw new GribException("Error reading large grib patterns", e);
        }

        File modelFile = new File(path);
        File siteModelFile = new File(sitePath);
        if (siteModelFile.exists()) {
            sitePatterns = loadPatterns(siteModelFile);
        } else {
            basePatterns = loadPatterns(modelFile);
        }

    }

    /**
     * Loads the regex patterns from the specified file
     * 
     * @param modelFile
     *            The file to load the regex patterns from
     * @return An object containing the compiled regex patterns
     * @throws GribException
     *             If the patterns cannot be loaded
     */
    private LargeGribPatterns loadPatterns(File modelFile) throws GribException {
        LargeGribPatterns patternSet = null;
        try {
            patternSet = (LargeGribPatterns) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(modelFile.getPath());
        } catch (Exception e) {
            throw new GribException("File " + modelFile.getAbsolutePath()
                    + " could not be unmarshalled.", e);
        }
        patternSet.compilePatterns();
        return patternSet;
    }

    /**
     * Checks whether the provided header matches the regexes designated for
     * special handling
     * 
     * @param header
     *            The header to check
     * @return True if the file associated with this WMO header is to receive
     *         special handling
     */
    private boolean isLarge(String header) {
        boolean isLarge = false;
        if (sitePatterns != null) {
            isLarge = sitePatterns.isDesiredHeader(header);
        }
        if (!isLarge) {
            isLarge = basePatterns.isDesiredHeader(header);
        }
        return isLarge;
    }
}
