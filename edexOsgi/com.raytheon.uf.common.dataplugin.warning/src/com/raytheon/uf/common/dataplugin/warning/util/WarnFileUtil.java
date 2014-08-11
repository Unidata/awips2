package com.raytheon.uf.common.dataplugin.warning.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.raytheon.uf.common.dataplugin.warning.WarningConstants;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Utility class to retrieve the appropriate file in localization and in backup
 * directories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2014 3033       jsanchez    Searches the backup site directory before the localized site directory.
 * Jul 02, 2014 DR 17450   D. Friedman Support using list of templates from backup site.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class WarnFileUtil {
    public static LocalizationFile findFileInLocalizationIncludingBackupSite(String filename,
            String issuingSiteID, String backupSiteID) throws FileNotFoundException {
        return findFileInLocalizationIncludingBackupSite(filename, issuingSiteID, backupSiteID, true);
    }

    /**
     * Returns the appropriate file in localization. If a backupSiteID is not
     * null and a corresponding file does exist in the backup site directory,
     * then that file in the backup site directory will be returned. However, if
     * that backup file does not exist, then regular localization handling for
     * the issuingSiteID is applied. For example, if a file exists in the
     * issuingSiteID directory then that the file with the returned. Otherwise,
     * the base level version of the file will be returned.
     * 
     * @param filename
     * @param issuingSiteID
     *            (optional)
     * @param backupSiteID
     *            (optional)
     * @return
     * @throws FileNotFoundException
     */
    public static LocalizationFile findFileInLocalizationIncludingBackupSite(String filename,
            String issuingSiteID, String backupSiteID, boolean allowUser)
            throws FileNotFoundException {

        IPathManager pm = PathManagerFactory.getPathManager();
        String fileToRetrieve = WarningConstants.WARNGEN_DIR
                + IPathManager.SEPARATOR + filename;

        if (backupSiteID != null) {
            LocalizationContext backupSiteCtx = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            backupSiteCtx.setContextName(backupSiteID);
            LocalizationFile backupFile = pm.getLocalizationFile(backupSiteCtx,
                    fileToRetrieve);
            if (backupFile != null && backupFile.exists()) {
                return backupFile;
            }
        }

        LocalizationFile fileToUse = null;
        LocalizationContext[] searchContext = pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        for (LocalizationContext ctx : searchContext) {
            if (!allowUser && ctx.getLocalizationLevel() == LocalizationLevel.USER)
                continue;

            if ((ctx.getLocalizationLevel() == LocalizationLevel.SITE || ctx
                    .getLocalizationLevel() == LocalizationLevel.CONFIGURED)
                    && issuingSiteID != null) {
                ctx.setContextName(issuingSiteID);
            }
            LocalizationFile file = pm.getLocalizationFile(ctx, fileToRetrieve);
            if (file != null && file.exists()) {
                fileToUse = file;
                break;
            }
        }

        if (fileToUse == null) {
            throw new FileNotFoundException("'" + filename
                    + "' can not be found");
        }
        return fileToUse;
    }

    public static boolean isLocalizationFileExtantAtSiteLevel(String filename, String siteID) {
        IPathManager pm = PathManagerFactory.getPathManager();
        String fileToRetrieve = WarningConstants.WARNGEN_DIR
                + IPathManager.SEPARATOR + filename;
        LocalizationContext backupSiteCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        backupSiteCtx.setContextName(siteID);
        LocalizationFile backupFile = pm.getLocalizationFile(backupSiteCtx,
                fileToRetrieve);
        return backupFile != null && backupFile.exists();
    }

    /**
     * Locates the appropriate file in the localization hierarchy including the
     * backupSite directory (if provided) and converts the content of the file
     * into a string.
     * 
     * @param filename
     * @param localizedSite
     * @param backupSite
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    public static String convertFileContentsToString(String filename,
            String localizedSite, String backupSite)
            throws FileNotFoundException, IOException {
        File file = findFileInLocalizationIncludingBackupSite(filename, localizedSite, backupSite)
                .getFile();
        return convertFileContentsToString(file);
    }

    public static String convertFileContentsToStringNoUser(String filename,
            String site) throws FileNotFoundException {
        File file = findFileInLocalizationIncludingBackupSite(filename, site, null, false).getFile();
        return convertFileContentsToString(file);
    }

    private static String convertFileContentsToString(File file) {
        StringBuffer sb = new StringBuffer();
        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(file));

            String line = null;
            while ((line = input.readLine()) != null) {
                sb.append(line + "\n");
            }
        } catch (IOException e) {

        } finally {
            if (input != null) {
                try {
                    input.close();
                    input = null;
                } catch (Exception e) {
                    input = null;
                }
            }
        }
        return sb.toString();
    }
}
