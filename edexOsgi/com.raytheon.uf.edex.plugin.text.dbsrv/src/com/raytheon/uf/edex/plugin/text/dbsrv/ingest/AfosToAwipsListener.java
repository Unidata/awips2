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
package com.raytheon.uf.edex.plugin.text.dbsrv.ingest;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.text.AfosToAwipsUtil;

/**
 * Based on the supplied afos2awips.txt file, update localization files and the
 * legacy afos2awips location.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 11, 2011           bfarmer   Initial creation
 * Mar 06, 2014  2876     mpduff    New NDM plugin.
 * Jan 18, 2016  4562     tjensen   Moved from edex.plugin.text to
 *                                  edex.plugin.text.dbsrv
 * Aug 09, 2016  5801     tgurney   Use localization files instead of database
 * Aug 19, 2016  5801     tgurney   Better error messages
 * Jan 12, 2017  6070     tgurney   Update legacy file when site file changes
 * Jan 20, 2017  6090     tgurney   Add blacklists
 *
 * </pre>
 *
 * @author bfarmer
 */

public class AfosToAwipsListener implements INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosToAwipsListener.class);

    private String legacyFileLocation;

    private IPathManager pathMgr = PathManagerFactory.getPathManager();

    public AfosToAwipsListener(String legacyFileLocation) {
        this.legacyFileLocation = legacyFileLocation;
        ILocalizationPathObserver pathObserver = new ILocalizationPathObserver() {
            @Override
            public void fileChanged(ILocalizationFile file) {
                if (file.getContext().getLocalizationLevel()
                        .equals(LocalizationLevel.SITE)) {
                    refreshLegacyFile();
                }
            }
        };
        pathMgr.addLocalizationPathObserver(AfosToAwipsUtil.AFOS2AWIPS_FILE,
                pathObserver);
        pathMgr.addLocalizationPathObserver(
                AfosToAwipsUtil.AFOS2AWIPS_BLACKLIST_FILE, pathObserver);
        refreshLegacyFile();
    }

    private boolean refreshLegacyFile() {
        ClusterTask ct = null;
        do {
            ct = ClusterLockUtils.lock("AfosToAwipsListener",
                    "refreshLegacyFile", 120000, true);
        } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));
        try {
            writeLegacyFile(
                    new TreeSet<>(AfosToAwipsUtil.readAllLocalizationFiles()));
            return true;
        } finally {
            ClusterLockUtils.unlock(ct, false);
        }
    }

    private void writeLegacyFile(Collection<AfosToAwips> records) {
        File targetDir = new File(legacyFileLocation).getParentFile();
        File tmpFile = null;
        try {
            tmpFile = File.createTempFile("afos2awips.txt.",
                    UUID.randomUUID().toString().substring(0, 16), targetDir);
            try (PrintWriter out = new PrintWriter(tmpFile, "UTF-8")) {
                for (AfosToAwips a : records) {
                    String line = String.format("%s %s %s",
                            a.getAfosid().trim(), a.getWmottaaii().trim(),
                            a.getWmocccc().trim());
                    out.println(line);
                }
                out.flush();
            }
            Files.move(tmpFile.toPath(), Paths.get(legacyFileLocation),
                    StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            statusHandler.handle(Priority.WARN,
                    "Failed to write legacy file to " + legacyFileLocation, e);
        } finally {
            if (tmpFile != null) {
                try {
                    Files.deleteIfExists(tmpFile.toPath());
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Write records to specified path at CONFIGURED level
     *
     * @param path
     * @param records
     * @throws IOException
     * @throws LocalizationException
     */
    private static void writeConfiguredLocalizationFile(String path,
            Collection<AfosToAwips> records)
                    throws IOException, LocalizationException {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext configuredCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile file = pathManager.getLocalizationFile(configuredCtx,
                path);
        try (SaveableOutputStream lfStream = file.openOutputStream()) {
            for (AfosToAwips a : records) {
                String line = String.format("%s %s %s\n", a.getAfosid().trim(),
                        a.getWmottaaii().trim(), a.getWmocccc().trim());
                byte[] bytes = line.getBytes();
                lfStream.write(bytes);
            }
            lfStream.flush();
            lfStream.save();
        }
    }

    private static List<AfosToAwips> parseTxtFile(File file)
            throws IOException {
        String line = null;
        List<AfosToAwips> rval = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            while ((line = reader.readLine()) != null) {
                AfosToAwips record = AfosToAwipsUtil.parseLine(line);
                if (record != null) {
                    rval.add(record);
                }
            }
        }
        return rval;
    }

    /**
     * Update configured localization files and legacy file from received file.
     *
     * After this runs, configured afos2awips.txt will contain all received
     * products not in the base file. Configured afos2awips.blacklist.txt
     * contains all products from the base file that are not in the received
     * file. Legacy file is the result of both configured and site lists and
     * blacklists applied to the base file.
     *
     *
     * @param file
     * @return true on success. failure to write legacy file is ignored.
     * @throws IOException
     *             on error when reading or writing a file
     * @throws LocalizationException
     *             if failed to update the CONFIGURED localization file
     */
    private boolean updateFromIngestedFile(File file) {
        List<AfosToAwips> fromIngestedFile;
        try {
            fromIngestedFile = parseTxtFile(file);
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Could not read ingested afos2awips file:"
                            + file.getAbsolutePath(),
                    e);
            return false;
        }

        Set<AfosToAwips> toLegacyFile = new TreeSet<>();
        Set<AfosToAwips> fromBase = new HashSet<>();
        Set<AfosToAwips> toConfigured = new TreeSet<>();
        Set<AfosToAwips> toConfiguredBlacklist = new TreeSet<>();

        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext baseCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

        fromBase.addAll(AfosToAwipsUtil.readLocalizationFile(baseCtx,
                AfosToAwipsUtil.AFOS2AWIPS_FILE));
        toConfigured.addAll(fromIngestedFile);
        toConfigured.removeAll(fromBase);
        toConfiguredBlacklist.addAll(fromBase);
        toConfiguredBlacklist.removeAll(fromIngestedFile);
        Set<AfosToAwips> fromSite = AfosToAwipsUtil
                .readLocalizationFile(siteCtx, AfosToAwipsUtil.AFOS2AWIPS_FILE);
        Set<AfosToAwips> fromSiteBlacklist = AfosToAwipsUtil
                .readLocalizationFile(siteCtx,
                        AfosToAwipsUtil.AFOS2AWIPS_BLACKLIST_FILE);
        try {
            writeConfiguredLocalizationFile(AfosToAwipsUtil.AFOS2AWIPS_FILE,
                    toConfigured);
            writeConfiguredLocalizationFile(
                    AfosToAwipsUtil.AFOS2AWIPS_BLACKLIST_FILE,
                    toConfiguredBlacklist);
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to update CONFIGURED localization file: "
                            + e.getLocalizedMessage(),
                    e);
            return false;
        }

        toLegacyFile.addAll(fromBase);
        toLegacyFile.addAll(toConfigured);
        toLegacyFile.removeAll(toConfiguredBlacklist);
        toLegacyFile.addAll(fromSite);
        toLegacyFile.removeAll(fromSiteBlacklist);
        printDiffMessage(toLegacyFile);
        writeLegacyFile(toLegacyFile);
        return true;
    }

    /** Print log message showing the diff between old and new legacy file */
    private void printDiffMessage(Set<AfosToAwips> fromNewLegacyFile) {
        Set<AfosToAwips> fromOldLegacyFile = new HashSet<>();
        try (InputStream is = new FileInputStream(legacyFileLocation)) {
            fromOldLegacyFile = AfosToAwipsUtil.readA2aFile(is);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return;
        }
        Set<AfosToAwips> onlyInOld = new HashSet<>(fromOldLegacyFile);
        Set<AfosToAwips> onlyInNew = new HashSet<>(fromNewLegacyFile);
        onlyInOld.removeAll(fromNewLegacyFile);
        onlyInNew.removeAll(fromOldLegacyFile);
        StringBuilder sb = new StringBuilder();
        sb.append("Diff for legacy afos2awips.txt:\n");
        if (!onlyInNew.isEmpty()) {
            sb.append("Lines added: \n");
            for (AfosToAwips a2a : new TreeSet<>(onlyInNew)) {
                sb.append(String.format("  + %s %s %s\n", a2a.getAfosid(),
                        a2a.getWmottaaii(), a2a.getWmocccc()));
            }
        }
        if (!onlyInOld.isEmpty()) {
            sb.append("Lines removed: \n");
            for (AfosToAwips a2a : new TreeSet<>(onlyInOld)) {
                sb.append(String.format("  - %s %s %s\n", a2a.getAfosid(),
                        a2a.getWmottaaii(), a2a.getWmocccc()));
            }
        }
        if (onlyInOld.isEmpty() && onlyInNew.isEmpty()) {
            sb.append("There were no changes.\n");
        }
        statusHandler.info(sb.toString());
    }

    @Override
    public void notify(String fileName, File file) {
        boolean success = updateFromIngestedFile(file);
        if (success) {
            statusHandler.handle(Priority.INFO,
                    "Successfully processed " + file.getAbsolutePath());
        } else {
            statusHandler.handle(Priority.INFO, "Finished processing "
                    + file.getAbsolutePath() + " with errors.");
        }
    }
}
