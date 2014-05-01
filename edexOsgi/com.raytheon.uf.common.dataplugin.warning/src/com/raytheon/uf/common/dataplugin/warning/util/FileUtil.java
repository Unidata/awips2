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

public class FileUtil {
    public static LocalizationFile getLocalizationFile(String filename,
            String siteID) throws FileNotFoundException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] searchContext = pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        LocalizationFile fileToUse = null;
        String fileToRetrieve = WarningConstants.WARNGEN_DIR
                + IPathManager.SEPARATOR + filename;
        for (LocalizationContext ctx : searchContext) {
            if ((ctx.getLocalizationLevel() == LocalizationLevel.SITE || ctx
                    .getLocalizationLevel() == LocalizationLevel.CONFIGURED)
                    && siteID != null) {
                ctx.setContextName(siteID);
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

    public static File getFile(String filename, String siteID)
            throws FileNotFoundException {
        return getLocalizationFile(filename, siteID).getFile();
    }

    public static String open(String filename, String localSite)
            throws FileNotFoundException, IOException {
        StringBuffer sb = new StringBuffer();
        BufferedReader input = null;
        File file = getFile(filename, localSite);
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
