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
package com.raytheon.uf.edex.plugin.manualIngest;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.commons.io.FileUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.file.Files;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.common.wmo.util.WMOHeaderFinder;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.distribution.DistributionPatterns;

/**
 * A bean based on FileToString that will take a message generated from a file
 * endpoint and attempt to search for a WMO header inside the first 100 bytes.
 * If found, that string is set to the message header and passed on. If it is
 * not found, it will use the filename.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Oct 28, 2009           brockwoo     Initial creation
 * Sep 03, 2013  2327     rjpeter      Added directory routing by plugin and
 *                                     date of product.
 * Apr 17, 2014  2942     skorolev     Updated throw exception in
 *                                     sendFileToIngest.
 * May 14, 2014  2536     bclement     removed TimeTools usage
 * Jul 10, 2014  2914     garmendariz  Remove EnvProperties
 * May 04, 2017  6255     bkowal       Updated to use IOPermissionsHelper and
 *                                     Files.
 * Mar 06, 2019  7582     randerso     Changed moveFileToArchive to actually
 *                                     move and not copy/delete. Code cleanup.
 * Mar 06, 2019  7582     randerso     Added code to create symlink for legacy
 *                                     endpoint. Code cleanup.
 * Mar  3, 2021  8326     tgurney      Camel 3 fixes
 *
 * </pre>
 *
 * @author brockwoo
 */

public class MessageGenerator implements Processor {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String DIR = System.getProperty("data.archive.root")
            + File.separator + "manual";

    private static final Set<PosixFilePermission> POSIX_FILE_PERMISSIONS = EnumSet
            .of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.GROUP_WRITE);

    private static final FileAttribute<Set<PosixFilePermission>> POSIX_DIRECTORY_ATTRIBUTES = PosixFilePermissions
            .asFileAttribute(EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.OWNER_EXECUTE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.GROUP_WRITE,
                    PosixFilePermission.GROUP_EXECUTE));

    private static MessageGenerator instance = new MessageGenerator();

    private String ingestRoute = null;

    private String dropBoxPath = null;

    private final ThreadLocal<SimpleDateFormat> sdfs = TimeUtil
            .buildThreadLocalSimpleDateFormat(
                    "yyyyMMdd" + File.separatorChar + "HH",
                    TimeUtil.GMT_TIME_ZONE);

    /**
     * Set of plugins that are not the primary decoder of the data. These are
     * secondary or additional information such as text, dhr, dpa, etc.
     */
    private final Set<String> secondaryPlugins = new HashSet<>();

    /**
     * @return the singleton instance
     */
    public static MessageGenerator getInstance() {
        return instance;
    }

    /**
     * Private constructor for singleton
     */
    private MessageGenerator() {
    }

    /**
     * @param ingestRoute
     *            the ingestRoute to set
     */
    public void setIngestRoute(String ingestRoute) {
        this.ingestRoute = ingestRoute;
    }

    /**
     * @param dropBoxPath
     *            the dropBoxPath to set
     */
    public void setDropBoxPath(String dropBoxPath) {
        this.dropBoxPath = dropBoxPath;

        /*
         * if legacy endpoint does not exist create a symbolic link to the
         * dropbox path
         */
        Path legacyEndpoint = Paths.get(System.getProperty("edex.home"), "data",
                "manual");
        if (!java.nio.file.Files.exists(legacyEndpoint,
                LinkOption.NOFOLLOW_LINKS)) {
            try {
                java.nio.file.Files.createSymbolicLink(legacyEndpoint,
                        Paths.get(dropBoxPath));
                statusHandler.info("Successfully created legacy endpoint link: "
                        + legacyEndpoint + "->" + dropBoxPath);
            } catch (IOException e) {
                statusHandler.error("Unable to create symbolic link: "
                        + legacyEndpoint + "->", dropBoxPath, e);
            }
        } else {
            /*
             * Verify the legacy endpoint is properly linked to the dropbox path
             */
            try {
                if (!Paths.get(dropBoxPath)
                        .equals(legacyEndpoint.toRealPath())) {
                    statusHandler.error("Legacy endpoint " + legacyEndpoint
                            + " is not a symbolic link to " + dropBoxPath);
                }
            } catch (IOException e) {
                statusHandler.error(
                        "Unable to resolve real path for " + legacyEndpoint, e);
            }

        }
    }

    /**
     * Register a secondary plugin, i.e. not the primary decoder of the data.
     * These are plugins that provide data in a different format or additional
     * information such as text, dhr, dpa, etc.
     *
     * @param plugin
     * @return this
     */
    public MessageGenerator registerSecondaryPlugin(String plugin) {
        secondaryPlugins.add(plugin);
        return this;
    }

    @Override
    public void process(Exchange arg0) throws Exception {
        File file = (File) arg0.getIn().getBody();
        if (file != null) {
            String messageHeader = WMOHeaderFinder.find(file);
            if (messageHeader == null) {
                messageHeader = file.getName();
            } else {
                messageHeader = messageHeader.trim();
            }
            arg0.getIn().setBody(file.toString());
            arg0.getIn().setHeader("header", messageHeader);
            arg0.getIn().setHeader("enqueueTime", System.currentTimeMillis());
        } else {
            // No file received
            arg0.setRouteStop(true);
        }
    }

    private String determinePath(File inFile)
            throws FileNotFoundException, IOException {
        StringBuilder path = new StringBuilder(inFile.getPath().length());
        path.append(DIR).append(File.separatorChar);

        // find header and determine file date
        Date fileTime = null;
        String header = WMOHeaderFinder.find(inFile);
        if (header == null) {
            header = inFile.getName();
        } else {
            header = header.trim();
            try {
                String dtg = WMOHeaderFinder.findDtg(header);
                Calendar headerTime = WMOTimeParser.findCurrentTime(dtg,
                        inFile.getName());
                if (headerTime != null) {
                    fileTime = headerTime.getTime();
                }
            } catch (Exception e) {
                statusHandler.error("Exception occurred parsing WMO Header", e);
            }
        }

        // determine the plugin
        List<String> plugins = DistributionPatterns.getInstance()
                .getMatchingPlugins(header);
        int numPlugins = plugins.size();
        if (numPlugins == 1) {
            path.append(plugins.get(0)).append(File.separatorChar);
        } else if (numPlugins > 1) {
            if (plugins.size() <= secondaryPlugins.size()) {
                // check for a non secondary plugin,
                String plugin = null;
                for (String pluginToCheck : plugins) {
                    if (!secondaryPlugins.contains(pluginToCheck)) {
                        plugin = pluginToCheck;
                        break;
                    }
                }

                if (plugin == null) {
                    // didn't find a non secondary plugin, just grab first
                    // plugin
                    plugin = plugins.get(0);
                }

                path.append(plugin).append(File.separatorChar);
            } else {
                // remove secondary and grab first one
                plugins.removeAll(secondaryPlugins);
                path.append(plugins.get(0)).append(File.separatorChar);
            }
        } else {
            path.append("unknown").append(File.separatorChar);
        }

        // append YYYYMMDD/HH
        if (fileTime == null) {
            // default to current time
            fileTime = SimulatedTime.getSystemTime().getTime();
        }
        path.append(sdfs.get().format(fileTime)).append(File.separatorChar);

        // Determine the sub-directory
        String inputPath = inFile.getParent();

        // Split on the dropbox directory to get the sub-directory
        String[] parts = inputPath.split(dropBoxPath);
        if (parts.length > 1) {
            path.append(parts[1]);
        }

        return path.toString();
    }

    /**
     * Copies the specified file to the archive directory.
     *
     * @param inFile
     * @return archive file name
     * @throws IOException
     */
    public File copyFileToArchive(File inFile) throws IOException {
        if (!inFile.exists()) {
            statusHandler
                    .warn("Attempting to process non-existent file: " + inFile);
            return null;
        }

        String path = determinePath(inFile);

        File dir = new File(path);

        if (!dir.exists()) {
            Files.createDirectories(dir.toPath(), POSIX_DIRECTORY_ATTRIBUTES);
        }

        File newFile = new File(dir, inFile.getName());

        try {
            FileUtils.copyFile(inFile, newFile);

            /*
             * Attempt to adjust the file permissions to fulfill the security
             * requirements. As of May 2017, all of the files that will be
             * processed are provided by an external source.
             */
            try {
                IOPermissionsHelper.applyFilePermissions(newFile.toPath(),
                        POSIX_FILE_PERMISSIONS);
            } catch (Exception e1) {
                /*
                 * Permission updates have failed. However, we still probably
                 * want to keep the file so that it can successfully be ingested
                 * and used?
                 */
                statusHandler.handle(Priority.WARN, e1.getMessage(), e1);
            }

            statusHandler.handle(Priority.INFO,
                    "DataManual: " + inFile.getAbsolutePath());
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to copy file [" + inFile.getAbsolutePath()
                            + "] to archive dir. File will be discarded.",
                    e);
            inFile.delete();
            return null;
        }

        return newFile;
    }

    /**
     * Moves the specified file to the archive directory.
     *
     * @param inFile
     * @return archive file name
     * @throws IOException
     */
    public File moveFileToArchive(File inFile) throws IOException {
        if (!inFile.exists()) {
            statusHandler
                    .warn("Attempting to process non-existent file: " + inFile);
            return null;
        }

        String path = determinePath(inFile);
        File dir = new File(path);
        File newFile = new File(dir, inFile.getName());

        try {
            if (!dir.exists()) {
                Files.createDirectories(dir.toPath(),
                        POSIX_DIRECTORY_ATTRIBUTES);
            }

            java.nio.file.Files.move(inFile.toPath(), newFile.toPath(),
                    StandardCopyOption.REPLACE_EXISTING);

            /*
             * Attempt to adjust the file permissions to fulfill the security
             * requirements. As of May 2017, all of the files that will be
             * processed are provided by an external source.
             */
            try {
                IOPermissionsHelper.applyFilePermissions(newFile.toPath(),
                        POSIX_FILE_PERMISSIONS);
            } catch (Exception e) {
                /*
                 * Permission updates have failed. However, we still probably
                 * want to keep the file so that it can successfully be ingested
                 * and used?
                 */
                statusHandler.handle(Priority.WARN, e.getMessage(), e);
            }

            statusHandler.handle(Priority.INFO,
                    "DataManual: " + inFile.getAbsolutePath());
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to move file [" + inFile.getAbsolutePath()
                            + "] to archive dir. File will be discarded.",
                    e);
            inFile.delete();
            return null;
        }

        return newFile;
    }

    /**
     * Copies a file to the archive directory and sends the path to the manual
     * ingest route.
     *
     * @param inFile
     * @return true if successful
     */
    public boolean sendFileToIngest(String inFile) {
        return sendFileToIngest(inFile, ingestRoute);
    }

    /**
     * Copies a file to the archive directory and sends the path to the
     * specified route.
     *
     * @param inFile
     * @param route
     * @return true if successful
     */
    public boolean sendFileToIngest(String inFile, String route) {
        boolean rval = true;

        try {
            File archiveFile = copyFileToArchive(new File(inFile));
            if (archiveFile == null) {
                throw new Exception("File " + inFile
                        + " has not been copied into archive.");
            }
            EDEXUtil.getMessageProducer().sendAsync(route,
                    archiveFile.getAbsolutePath());
        } catch (Exception e) {
            rval = false;
            statusHandler.handle(Priority.ERROR,
                    "Failed to insert file [" + inFile + "] into ingest stream",
                    e);
        }

        return rval;
    }
}
