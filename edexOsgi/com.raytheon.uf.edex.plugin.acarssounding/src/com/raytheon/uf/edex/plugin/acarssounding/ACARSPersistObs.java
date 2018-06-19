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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;

/**
 * Class to process ACARS' persistent objects.
 *
 * This class is not cluster-safe due the file IO and should only be used within
 * a clustered context.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            jkorman     Initial creation
 * Jan 07, 2014 2658       rferrel     Ignore cut off time when allowing archive data.
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * Dec 10, 2015 5166       kbisanz     Update logging to use SLF4J
 * Jul 26, 2016 5757       nabowle     Move processing out of localization structure.
 * Aug 08, 2016 5757       nabowle     Cleanup code.
 *
 * </pre>
 *
 * @author jkorman
 */

public class ACARSPersistObs {
    private Logger logger = LoggerFactory.getLogger(getClass());

    private File baseDir;

    /**
     * Constructor.
     */
    public ACARSPersistObs() {
        try {
            baseDir = ACARSSoundingTools
                    .getInitializedDirectory(ACARSSoundingTools.RAW_PATH);
            if (baseDir == null) {
                logger.error("Unable to setup processing directory.");
            }
        } catch (Exception e) {
            baseDir = null;
            logger.error("Unable to setup processing directory.", e);
        }
    }

    /**
     * Writes the URIs of the input objects to a timestamped file within
     * {@link ACARSSoundingTools#RAW_PATH}.
     *
     * If baseDir can not be initialized, input is skipped.
     */
    public void process(PluginDataObject[] input) {
        if (baseDir == null || input == null || input.length == 0) {
            return;
        }

        try {
            File out = new File(baseDir, ACARSSoundingTools.getFileName());
            try (FileWriter writer = new FileWriter(out, out.exists())) {

                // Value to ignore cut off time when archive allowed.
                long cTime = Long.MIN_VALUE;

                if (!WMOTimeParser.allowArchive()) {
                    cTime = ACARSSoundingTools
                            .getCutoffTime(ACARSSoundingTools.CUTOFF_HOURS);
                }

                for (PluginDataObject pdo : input) {
                    if (pdo.getDataTime().getRefTime().getTime() > cTime) {
                        try {
                            ACARSRecord acars = (ACARSRecord) pdo;

                            writer.write(String.format(
                                    ACARSSoundingTools.OUT_FMT, acars
                                            .getTimeObs().getTimeInMillis(),
                                    acars.getDataURI()));

                        } catch (Exception ee) {
                            logger.error("Error in process " + ee);
                        }
                    }
                }
            } catch (IOException ioe) {
                logger.error("Error opening acars output " + ioe);
            }
        } catch (Exception e) {
            logger.error("Error in process " + e);
        }
    }
}
