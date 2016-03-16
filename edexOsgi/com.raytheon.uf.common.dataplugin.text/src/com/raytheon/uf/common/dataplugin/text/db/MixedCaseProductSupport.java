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
package com.raytheon.uf.common.dataplugin.text.db;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Utilities to support mixed case product generation on a per Product ID (nnn)
 * basis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 01, 2014  #3685     randerso    Initial creation
 * Mar 16, 2016  #5411     randerso    Added null checks, code cleanup
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MixedCaseProductSupport {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MixedCaseProductSupport.class);

    private static final String MIXED_CASE_DIR = "mixedCase";

    private static final String MIXED_CASE_PIDS_FILE = FileUtil.join(
            MIXED_CASE_DIR, "mixedCaseProductIds.txt");

    private static final char COMMENT_DELIMITER = '#';

    private static Set<String> mixedCasePids;

    private static IPathManager pm = PathManagerFactory.getPathManager();

    private static LocalizationFile baseDir;

    /**
     * @return list of Product IDs enabled for mixed case transmission
     */
    public static Set<String> getMixedCasePids() {
        // setup up the file updated observer
        synchronized (MixedCaseProductSupport.class) {
            if (baseDir == null) {
                baseDir = pm.getLocalizationFile(
                        pm.getContext(LocalizationType.COMMON_STATIC,
                                LocalizationLevel.BASE), MIXED_CASE_DIR);
                baseDir.addFileUpdatedObserver(new ILocalizationFileObserver() {

                    @Override
                    public void fileUpdated(FileUpdatedMessage message) {
                        mixedCasePids = null;
                    }
                });
            }
        }

        synchronized (MixedCaseProductSupport.class) {
            if (mixedCasePids == null) {

                // get all localization files in the hierarchy and merge them.
                Map<LocalizationLevel, LocalizationFile> fileHierarchy = pm
                        .getTieredLocalizationFile(
                                LocalizationType.COMMON_STATIC,
                                MIXED_CASE_PIDS_FILE);

                Set<String> newPids = new HashSet<String>();
                for (LocalizationFile lf : fileHierarchy.values()) {
                    String filePath = lf.getPath();
                    try (BufferedReader in = new BufferedReader(
                            new InputStreamReader(lf.openInputStream()))) {

                        String line;
                        while ((line = in.readLine()) != null) {
                            int pos = line.indexOf(COMMENT_DELIMITER);
                            if (pos >= 0) {
                                line = line.substring(0, pos);
                            }
                            line = line.trim().toUpperCase();
                            String[] pids = line.split("[\\s,]+");
                            for (String pid : pids) {
                                if (pid.length() == 3) {
                                    newPids.add(pid);
                                } else if (pid.isEmpty()) {
                                    continue;
                                } else {
                                    statusHandler.warn("Invalid Product ID \""
                                            + pid + "\" found in " + filePath
                                            + ", ignored.");
                                }
                            }
                        }
                        mixedCasePids = newPids;
                    } catch (IOException e) {
                        statusHandler.error("Error reading " + filePath, e);
                    } catch (LocalizationException e) {
                        statusHandler.error("Error retrieving " + filePath, e);
                    }
                }

                mixedCasePids = newPids;
            }
        }

        return mixedCasePids;
    }

    public static boolean isMixedCase(String pid) {
        if (pid == null) {
            return false;
        }
        return getMixedCasePids().contains(pid.toUpperCase());
    }

    public static String conditionalToUpper(String pid, String text) {
        if (!isMixedCase(pid)) {
            if (text != null) {
                text = text.toUpperCase();
            }
        }

        return text;
    }
}
