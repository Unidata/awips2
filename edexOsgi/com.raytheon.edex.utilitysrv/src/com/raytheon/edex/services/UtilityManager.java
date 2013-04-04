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

package com.raytheon.edex.services;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Date;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.utility.ProtectedFiles;
import com.raytheon.uf.common.localization.Checksum;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.msgs.DeleteUtilityResponse;
import com.raytheon.uf.common.localization.msgs.ListResponseEntry;
import com.raytheon.uf.common.localization.msgs.ListUtilityResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * 
 * Utility manager
 * 
 * Provides the business logic for the utility service
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2007            chammack    Initial Creation.
 * Jul 24, 2007            njensen     Updated putFile()
 * Jul 30, 2007            njensen     Added deleteFile()
 * May 19, 2007    #1127   randerso    Implemented error reporting
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class UtilityManager {
    private static final Log logger = LogFactory.getLog(UtilityManager.class);

    private static final String CHECKSUM_FILE_EXTENSION = ".md5";

    public static final String NOTIFY_ID = "utilityNotify";

    // Do not allow instantiation
    private UtilityManager() {

    }

    /**
     * 
     * List available files in a context
     * 
     * @param baseDir
     *            the base directory
     * @param context
     *            the utility context
     * @param subPath
     *            file or directory path below context
     * @param recursive
     *            true if recursive file listing is desired
     * @param filesOnly
     *            true to return only files, false to return directories and
     *            files
     * @return the list response
     */
    public static ListUtilityResponse listFiles(String localizedSite,
            String baseDir, LocalizationContext context, String subPath,
            boolean recursive, boolean filesOnly) {
        ArrayList<ListResponseEntry> entries = new ArrayList<ListResponseEntry>();
        String msg = null;
        try {
            checkParameters(baseDir, context);
            String path = contextToDirectory(baseDir, context);
            File file = new File(path);

            recursiveFileBuild(localizedSite, context, file, subPath,
                    recursive, filesOnly, entries, 0);
        } catch (EdexException e) {
            msg = e.getMessage();
        }

        return new ListUtilityResponse(context, subPath, msg,
                entries.toArray(new ListResponseEntry[entries.size()]));
    }

    /**
     * @param file
     * @return
     * @throws EdexException
     */
    private static String getFileChecksum(File file) throws EdexException {
        // TODO: Fix FileLocker so it never times out in test driver
        File checksumFile = getChecksumFile(file);
        String chksum = null;
        try {
            if (checksumFile.exists()
                    && checksumFile.lastModified() >= file.lastModified()) {

                BufferedReader reader = new BufferedReader(new FileReader(
                        checksumFile));
                try {
                    chksum = reader.readLine();
                } finally {
                    reader.close();
                }
            }

            if (chksum == null) {
                chksum = writeChecksum(file);
            }
        } catch (Exception e) {
            // log, no checksum will be provided
            logger.error("Error determing file checksum for: " + file, e);
        }
        return chksum;
    }

    private static File getChecksumFile(File utilityFile) {
        return new File(utilityFile.getParentFile(), utilityFile.getName()
                + CHECKSUM_FILE_EXTENSION);
    }

    public static String writeChecksum(File file) throws Exception {
        String chksum = null;
        File checksumFile = getChecksumFile(file);
        BufferedWriter bw = new BufferedWriter(new FileWriter(checksumFile));
        try {
            chksum = Checksum.getMD5Checksum(file);
            bw.write(chksum);
        } finally {
            bw.close();
        }
        return chksum;
    }

    /**
     * Deletes a file in a context
     * 
     * @param baseDir
     *            the base dir
     * @param context
     *            the context
     * @param fileName
     *            the name of the file
     * @return the delete response
     */
    public static DeleteUtilityResponse deleteFile(String baseDir,
            LocalizationContext context, String fileName) {
        String msg = null;
        try {
            checkParameters(baseDir, context);

            String fullPath = contextToDirectory(baseDir, context)
                    + File.separator + fileName;
            File delFile = new File(fullPath);

            if (delFile.exists()) {
                if (!delFile.delete()) {
                    // Failed to delete file...
                    msg = "File could not be deleted: ";
                    if (delFile.isDirectory() && delFile.list().length > 0) {
                        msg += "Non-empty directory";
                    } else if (delFile.canWrite() == false) {
                        msg += "Do not have write permission to file";
                    } else if (delFile.getParentFile() != null
                            && delFile.getParentFile().canWrite() == false) {
                        msg += "Do not have write permission to file's parent directory";
                    } else {
                        msg += "Reason unknown";
                    }
                }
                String md5Path = fullPath + ".md5";
                File md5File = new File(md5Path);
                if (md5File.exists()) {
                    md5File.delete();
                }
            }
        } catch (Exception e) {
            return new DeleteUtilityResponse(context, e.getMessage(), fileName,
                    System.currentTimeMillis());
        }

        long timeStamp = System.currentTimeMillis();
        // send notification
        try {
            EDEXUtil.getMessageProducer().sendAsync(
                    NOTIFY_ID,
                    new FileUpdatedMessage(context, fileName,
                            FileChangeType.DELETED, timeStamp));
        } catch (Exception e) {
            logger.error("Error sending file updated message", e);
        }

        return new DeleteUtilityResponse(context, msg, fileName, timeStamp);
    }

    /**
     * Sanity check the user-provided parameters
     * 
     * @param baseDir
     *            the base directory
     * @param context
     *            the utility context
     * @throws EdexException
     */
    private static void checkParameters(String baseDir,
            LocalizationContext context) throws EdexException {
        File baseDirFile = new File(baseDir);

        if (!baseDirFile.exists()) {
            baseDirFile.mkdirs();
        }

        if (!baseDirFile.isDirectory()) {
            throw new EdexException(
                    "Utility service base directory does not contain a directory: "
                            + baseDir);
        }

        if (context == null) {
            throw new EdexException("Context is null");
        }
    }

    private static String contextToDirectory(String baseDir,
            LocalizationContext context) {
        String dir = baseDir + File.separator + context.toPath();

        return dir;
    }

    private static void addEntry(String localizedSite,
            LocalizationContext context, String path, File file,
            ArrayList<ListResponseEntry> entries) throws EdexException {

        if (!path.endsWith(CHECKSUM_FILE_EXTENSION)) {
            ListResponseEntry entry = new ListResponseEntry();
            entry.setContext(context);
            entry.setFileName(path);
            if (file.exists()) {
                entry.setExistsOnServer(true);
                entry.setDate(new Date(file.lastModified()));
                if (file.isDirectory()) {
                    entry.setDirectory(true);
                } else {
                    entry.setChecksum(getFileChecksum(file));
                }
            } else {
                entry.setExistsOnServer(false);
            }

            LocalizationLevel protectedLevel = ProtectedFiles
                    .getProtectedLevel(localizedSite,
                            context.getLocalizationType(), path);
            entry.setProtectedLevel(protectedLevel);

            // add to entry if not protected or we are requesting protected
            // version or levels below (BASE if SITE protected, etc)
            if (protectedLevel == null
                    || context.getLocalizationLevel().compareTo(protectedLevel) <= 0) {
                entries.add(entry);
            }
        }
    }

    private static void recursiveFileBuild(String localizedSite,
            LocalizationContext context, File dir, String subPath,
            boolean recursive, boolean filesOnly,
            ArrayList<ListResponseEntry> entries, int depth)
            throws EdexException {

        String path = dir.getPath();
        if ((subPath == null) || subPath.isEmpty()) {
            subPath = ".";
        } else {
            path += "/" + subPath;
        }
        String prependToPath = subPath + "/";

        File file = new File(path);
        if (depth == 0 && file.exists() == false) {
            // File doesn't exist, make sure we flush any NFS caches by listing
            // the parent files and recreating the file. We only need to perform
            // this hack if depth is 0 since otherwise we were called based on
            // results of a listFiles() call
            File parent = file.getParentFile();
            parent.listFiles();
            file = new File(path);
        }

        if (file.exists() && (!file.canRead() || file.isHidden())) {
            return;
        }

        if (!filesOnly || file.isFile()) {
            addEntry(localizedSite, context, subPath, file, entries);
        }

        if (file.isDirectory()) {
            for (File f : file.listFiles()) {
                if (!f.canRead() || f.isHidden()) {
                    continue;
                }
                if (f.isFile()) {
                    addEntry(localizedSite, context,
                            prependToPath + f.getName(), f, entries);
                } else if (f.isDirectory()) {
                    if (recursive) {
                        recursiveFileBuild(localizedSite, context, dir,
                                prependToPath + f.getName(), recursive,
                                filesOnly, entries, depth + 1);
                    } else if (!filesOnly) {
                        addEntry(localizedSite, context,
                                prependToPath + f.getName(), f, entries);
                    }
                }
            }
        }
    }

    public static ListUtilityResponse listContexts(String path,
            LocalizationLevel level) throws EdexException {
        ArrayList<ListResponseEntry> entries = new ArrayList<ListResponseEntry>();
        for (LocalizationType type : LocalizationType.values()) {
            if (type.name().equals("UNKNOWN")
                    || type.name().equals("EDEX_STATIC")) {
                continue;
            }
            String fullPath = path + File.separator + type.name().toLowerCase()
                    + File.separator + level.name().toLowerCase();
            File dir = new File(fullPath);

            // We only want directories
            FileFilter fileFilter = new FileFilter() {
                public boolean accept(File file) {
                    return file.isDirectory();
                }
            };

            File[] files = dir.listFiles(fileFilter);
            if (files != null) {
                for (File file : files) {
                    LocalizationContext context = new LocalizationContext(type,
                            level, file.getName());
                    addEntry(null, context, type.name().toLowerCase()
                            + File.separator + level.name().toLowerCase()
                            + File.separator + file.getName(), file, entries);
                }
            }
        }

        return new ListUtilityResponse(
                entries.toArray(new ListResponseEntry[entries.size()]));
    }

}
