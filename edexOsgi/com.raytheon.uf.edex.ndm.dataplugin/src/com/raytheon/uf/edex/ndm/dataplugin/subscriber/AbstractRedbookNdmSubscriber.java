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
package com.raytheon.uf.edex.ndm.dataplugin.subscriber;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Abstract class for listening for Redbook NDM files that are dropped into EDEX
 * ingest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2015 4512       mapeters    Initial creation.
 * Jan 08, 2016 5237       tgurney     Replace call to deprecated
 *                                     LocalizationFile.write()
 * Mar 02, 2016 5434       bkowal      Relocated to ndm dataplugin.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */

public abstract class AbstractRedbookNdmSubscriber implements
        INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRedbookNdmSubscriber.class);

    /**
     * Use this lock whenever the Redbook NDM files in localization
     * (redbook/redbook*.txt) are being accessed to prevent simultaneous access
     * by multiple threads.
     */
    protected static final Lock lock = new ReentrantLock();

    protected static final String DATA_KEYS_FILE_NAME = "redbookDataKeys.txt";

    protected static final String DEPICT_KEYS_FILE_NAME = "redbookDepictKeys.txt";

    protected static final String PRODUCT_BUTTONS_FILE_NAME = "redbookProductButtons.txt";

    protected static final String NDM_LOC_DIR = "ndm" + IPathManager.SEPARATOR;

    protected static final LocalizationContext LOC_CONTEXT = PathManagerFactory
            .getPathManager().getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);

    /**
     * Get a map of key to substitution values (e.g. 5013 --> PAWO82).
     * 
     * @param dataKeys
     *            List of strings from the redbookDataKeys.txt file
     * @return Map for key -> substitution string
     */
    protected static Map<String, String> getSubstitutionMap(
            List<String> dataKeys) {
        Map<String, String> substitutionMap = new HashMap<>();
        for (String line : dataKeys) {
            line = line.trim();
            // Skip comment/empty lines
            if (line.startsWith("#") || (line.length() == 0)) {
                continue;
            }
            String[] parts = line.split("\\|");
            substitutionMap.put(parts[0].trim(),
                    parts[10].trim().substring(0, 6));
        }

        return substitutionMap;
    }

    /**
     * 
     * Store the given file in localization in the ndm/ directory.
     * 
     * @param fileName
     *            the name of the file to store
     * @param file
     *            the file to store
     */
    protected static void storeFile(String fileName, File file) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile locFile = pm.getLocalizationFile(LOC_CONTEXT,
                NDM_LOC_DIR + fileName);
        // Make sure the directory exists
        if (!locFile.getFile().exists()) {
            locFile.getFile().getParentFile().mkdirs();
        }

        try (SaveableOutputStream locFileStream = locFile.openOutputStream()) {
            locFileStream.write(Files.readAllBytes(file.toPath()));
            locFileStream.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to write contents of " + file.getPath() + " to "
                            + locFile.getPath(), e);
        }
    }

    /**
     * Get a list of the lines of the given file.
     * 
     * @param fileName
     * @return a list of the lines of the file
     */
    protected static List<String> getNdmFileLines(String fileName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        File file = pm.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, NDM_LOC_DIR + fileName)
                .getFile();

        List<String> fileLines = null;
        try {
            fileLines = Files.readAllLines(file.toPath(),
                    Charset.defaultCharset());
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Failed to read lines from "
                    + file.getAbsolutePath(), e);
        }

        return fileLines;
    }
}
