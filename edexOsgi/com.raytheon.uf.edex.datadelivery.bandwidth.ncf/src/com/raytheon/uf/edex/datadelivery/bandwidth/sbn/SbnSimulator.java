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
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

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
 * Oct 18, 2013 2267       bgonzale     Added distribution to and check in site specific directories.
 * Feb 11, 2014   2771     bgonzale     Use Data Delivery ID instead of Site.
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

    private final File sitesDirectory;

    private final File localSiteDirectory;

    private final IFileProcessor fileProcessor;

    private String site;

    /**
     * Private constructor.
     */
    public SbnSimulator() {
        this(new File(System.getProperty("sbn.retrieval.transfer.directory")),
                new CopyFileToManualIngest(), DataDeliveryIdUtil.getId());
    }

    /**
     * Set up an {@link SbnSimulator} with the specified file processor.
     * 
     * @param fileProcessor
     */
    @VisibleForTesting
    SbnSimulator(File scanDirectory, IFileProcessor fileProcessor, String site) {
        this.fileProcessor = fileProcessor;
        this.directoryToScan = scanDirectory;
        this.sitesDirectory = new File(directoryToScan, "sbnSimulator");
        this.localSiteDirectory = new File(sitesDirectory, site);
        this.localSiteDirectory.mkdirs();
        this.site = site;
    }

    /**
     * Check for sbn data.
     * 
     * @throws IOException
     */
    public void checkForSbnData() throws IOException {
        final List<File> files = FileUtil.listFiles(localSiteDirectory,
                FilenameFilters.ACCEPT_VISIBLE_FILES, false);

        statusHandler.info("Found [" + files.size() + "] files for " + site
                + " from the SBN...");

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

    /**
     * Distribute to the site directories. Enables all site client registries to
     * ingest shared data.
     * 
     * @throws IOException
     */
    public void distributeToSiteDirs() throws IOException {
        final List<File> undistributedFiles = FileUtil.listFiles(
                directoryToScan,
 FilenameFilters.ACCEPT_FILES, false);
        // get list of site dirs
        final List<File> sites = FileUtil.listFiles(sitesDirectory,
                FilenameFilters.ACCEPT_DIRECTORIES, false);
        
        statusHandler.info("Found [" + undistributedFiles.size() + "] files to distribute...");
        
        // distribute to site specific directories
        for (File file : undistributedFiles) {
            statusHandler.info("Distributing file [" + file + "]");
            for (File siteDir : sites) {
                File dest = new File(siteDir, file.getName().toString());
                File hiddenDest = new File(siteDir, "."
                        + file.getName().toString());

                // move to site sbn directory as hidden
                FileUtil.copyFile(file, hiddenDest);
                // rename dest to un-hidden
                hiddenDest.renameTo(dest);
                statusHandler.info("===> to file [" + dest + "]");
            }
            // delete source file
            file.delete();
        }
    }

    // TODO Java 1.7 version of the distributeToSiteDirs() method
    // /**
    // * Distribute to the site directories. Enables all site client registries
    // * to ingest shared data.
    // *
    // * @throws IOException
    // */
    // public void distributeToSiteDirs() throws IOException {
    // final List<Path> undistributedFiles = FileUtil.listPaths(
    // directoryToScan,
    // FilenameFilters.ACCEPT_PATH_FILES, false);
    // // get list of site dirs
    // final List<Path> sites = FileUtil.listPaths(sitesDirectory,
    // FilenameFilters.ACCEPT_PATH_DIRECTORIES, false);
    //
    // statusHandler.info("Found [" + undistributedFiles.size() +
    // "] files to distribute...");
    //
    // // distribute to site specific directories
    // for (Path file : undistributedFiles) {
    // statusHandler.info("Distributing file [" + file + "]");
    // for (Path siteDir : sites) {
    // Path dest = FileSystems.getDefault().getPath(
    // siteDir.toString(), file.getFileName().toString());
    // Path hiddenDest = FileSystems.getDefault()
    // .getPath(siteDir.toString(),
    // "." + file.getFileName().toString());
    //
    // // move to site sbn directory as hidden
    // java.nio.file.Files.copy(file, hiddenDest,
    // StandardCopyOption.REPLACE_EXISTING);
    // // rename dest to un-hidden
    // java.nio.file.Files.move(hiddenDest, dest,
    // StandardCopyOption.ATOMIC_MOVE);
    // statusHandler.info("===> to file [" + dest + "]");
    // }
    // // delete source file
    // java.nio.file.Files.delete(file);
    // }
    // }

}
