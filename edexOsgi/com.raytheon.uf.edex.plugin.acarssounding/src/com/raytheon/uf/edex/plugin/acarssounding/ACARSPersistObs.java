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
package com.raytheon.uf.edex.plugin.acarssounding;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.tools.AirportsBean;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;
import com.raytheon.uf.edex.plugin.acarssounding.tools.SoundingBuilder;

/**
 * Class to process ACARS' persistent objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            jkorman     Initial creation
 * Jan 07, 2014 2658       rferrel     Ignore cut off time when allowing archive data.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSPersistObs {

    public static final int BASE_DIR_NULL = 1;

    public static final int DATA_DIR_NULL = 2;

    public static final int NO_BASE_DIR = 3;

    public static final int NO_DATA_DIR = 4;

    private Log logger = LogFactory.getLog(getClass());

    private SoundingBuilder builder;

    private File baseDir = null;

    // If a required resource is not available,
    // Set failSafe to true. This will "short circuit" processing.
    private boolean failSafe = false;

    private int failReason = 0;

    /**
     * 
     */
    public ACARSPersistObs(AirportsBean airports) {
        logger.info("Creating SoundingBuilder");
        try {
            builder = new SoundingBuilder(airports);

            setupFiles();

        } catch (Exception e) {
            failSafe = true;
            logger.error("Error creating SoundingBuilder", e);
        }
        failSafe = (builder == null) || (baseDir == null);
    }

    /**
     * Get the next decoded data record.
     * 
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public void process(PluginDataObject[] input) {
        if (failSafe) {
            return;
        }

        try {
            if ((builder != null) && (input != null)) {

                File out = new File(baseDir, ACARSSoundingTools.getFileName());
                FileWriter writer = null;
                try {
                    writer = new FileWriter(out, out.exists());

                    // Value to ignore cut off time when archive allowed.
                    long cTime = Long.MIN_VALUE;

                    if (!TimeTools.allowArchive()) {
                        cTime = ACARSSoundingTools
                                .getCutoffTime(ACARSSoundingTools.CUTOFF_HOURS);
                    }

                    for (PluginDataObject pdo : input) {
                        if (pdo.getDataTime().getRefTime().getTime() > cTime) {
                            try {
                                ACARSRecord acars = (ACARSRecord) pdo;

                                writer.write(String.format(
                                        ACARSSoundingTools.OUT_FMT,
                                        acars.getTimeObs().getTimeInMillis(),
                                        acars.getDataURI()));

                            } catch (Exception ee) {
                                logger.error("Error in process " + ee);
                            }
                        }
                    }
                } catch (IOException ioe) {
                    logger.error("Error opening acars output " + ioe);
                } finally {
                    if (writer != null) {
                        writer.close();
                    }
                }
            }
        } catch (Exception e) {
            logger.error("Error in process " + e);
        } finally {
        }
    }

    /**
     * 
     * @return
     */
    public boolean isFailSafe() {
        return failSafe;
    }

    /**
     * Get the failure reason.
     * 
     * @return
     */
    public int getFailReason() {
        return failReason;
    }

    /**
     * Set up file resources
     */
    private void setupFiles() {
        failSafe = true;
        try {
            PathManager pathMgr = (PathManager) PathManagerFactory
                    .getPathManager();

            LocalizationContext ctx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

            baseDir = pathMgr.getFile(ctx, ACARSSoundingTools.BASE_PATH
                    + ACARSSoundingTools.RAW_PATH);

            if (baseDir != null) {
                if (!baseDir.exists()) {
                    baseDir.mkdirs();
                }
                if (baseDir.exists()) {
                    // Ok, resources exist
                    failSafe = false;
                } else {
                    failReason = NO_BASE_DIR;
                }
            } else {
                failReason = BASE_DIR_NULL;
            }
        } catch (Exception e) {
            logger.error("Attempting to setup files", e);
        }
    }
}
