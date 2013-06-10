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
package com.raytheon.uf.edex.datadelivery.bandwidth.sbn;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * The SBN simulator. Reads files from a configured directory, and then places
 * them into the ingest queue.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2013 1648       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SbnSimulator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SbnSimulator.class);

    /**
     * Interface to process files found in the scanned directory.
     */
    interface IFileProcessor {
        /**
         * Process the file.
         * 
         * @throws IOException
         */
        void processFile(File file) throws IOException;
    }

    private static class CopyFileToManualIngest implements IFileProcessor {
        /**
         * {@inheritDoc}
         */
        @Override
        public void processFile(File file) throws IOException {

            FileUtil.copyFile(
                    file,
                    new File(FileUtil
                            .join(EDEXUtil.EDEX_HOME, "data", "manual"), file
                            .getName()));

            statusHandler.info("Processed [" + file + "]");
        }
    }

    private final File directoryToScan;

    private final IFileProcessor fileProcessor;

    /**
     * Private constructor.
     */
    public SbnSimulator() {
        this(new File(System.getProperty("sbn.retrieval.transfer.directory")),
                new CopyFileToManualIngest());
    }

    /**
     * Set up an {@link SbnSimulator} with the specified file processor.
     * 
     * @param fileProcessor
     */
    @VisibleForTesting
    SbnSimulator(File scanDirectory, IFileProcessor fileProcessor) {
        this.fileProcessor = fileProcessor;
        this.directoryToScan = scanDirectory;
    }

    /**
     * Check for sbn data.
     * 
     * @throws IOException
     */
    public void checkForSbnData() throws IOException {
        final List<File> files = FileUtil.listFiles(directoryToScan,
                FilenameFilters.ACCEPT_FILES, false);

        statusHandler
                .info("Found [" + files.size() + "] files from the SBN...");

        for (File file : files) {

            try {
                fileProcessor.processFile(file);
            } catch (IOException e) {
                statusHandler.error("Error processing file [" + file + "]", e);
            } finally {
                if (!file.delete()) {
                    statusHandler.warn("Unable to delete [" + file + "]");
                }
            }
        }
    }

}
