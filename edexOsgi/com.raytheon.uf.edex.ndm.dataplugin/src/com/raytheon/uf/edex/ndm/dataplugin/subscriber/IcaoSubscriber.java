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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.text.IcaoMap;

/**
 * Waits for new ICAO Lookup Tables. Copies any new/updated files that are
 * received to localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2016 5434       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class IcaoSubscriber implements INationalDatasetSubscriber {

    private final IUFStatusHandler logger = UFStatus.getHandler(getClass());

    /**
     * This copies a file to CONFIGURED directory and performs a readFile.
     * 
     * @param file
     * @return true when file successfully copied and parsed file
     */
    public synchronized boolean copyToConfiguration(File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile destFile = pathMgr.getLocalizationFile(lc,
                IcaoMap.ICAO_LOCALIZATION_PATH);

        boolean status = true;

        try (SaveableOutputStream out = destFile.openOutputStream()) {
            Path inPath = Paths.get(file.getAbsolutePath());
            Files.copy(inPath, out);
            out.save();
        } catch (IOException | LocalizationException e) {
            status = false;
            logger.error("Unable to create " + destFile.getPath(), e);
        }

        return status;
    }

    @Override
    public void notify(String fileName, File file) {
        if (IcaoMap.ICAO_LOOKUP_FILENAME.equals(fileName)) {
            if (!copyToConfiguration(file)) {
                logger.handle(Priority.PROBLEM,
                        "Could not copy file: " + file.getName());
            }
        }
    }
}