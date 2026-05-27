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
package com.raytheon.uf.edex.dissemination.ingest;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import com.raytheon.uf.common.localization.ILocalizationFile;
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
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Dissemination NDM subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 13, 2011           bfarmer   Initial creation
 * Mar 06, 2014  2876     mpduff    New NDM plugin.
 * Aug 01, 2016  5744     mapeters  Save priorities file to
 *                                  common_static.configured instead of
 *                                  edex_static.base, use ILocalizationFile
 * 
 * </pre>
 * 
 * @author bfarmer
 */

public class DisseminationNationalDatasetSubscriber implements
        INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DisseminationNationalDatasetSubscriber.class);

    private static final String AWIPS_PRIORITIES_FILENAME = "dissemination"
            + IPathManager.SEPARATOR + "awipsPriorities.txt";

    @Override
    public void notify(String fileName, File file) {
        if ("awipsPriorities.txt".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    AWIPS_PRIORITIES_FILENAME);
            long time = System.currentTimeMillis();
            String backupFilename = "dissemination" + IPathManager.SEPARATOR
                    + "awipsPriorities" + time + ".txt";
            ILocalizationFile backupFile = pathMgr.getLocalizationFile(lc,
                    backupFilename);

            saveFile(outFile, backupFile);
            saveFile(file, outFile);
        }
    }

    /**
     * Write the contents of inFile to outFile and save it to the localization
     * server
     * 
     * @param inFile
     * @param outFile
     */
    private void saveFile(ILocalizationFile inFile, ILocalizationFile outFile) {
        if (inFile != null && inFile.exists()) {
            try (InputStream is = inFile.openInputStream();
                    SaveableOutputStream os = outFile.openOutputStream()) {
                saveInputStreamToLocalization(is, os);
            } catch (IOException | LocalizationException e) {
                String msg = "Failed to save " + inFile.getPath() + " to "
                        + outFile.getPath();
                statusHandler.handle(Priority.PROBLEM, msg, e);
            }
        }
    }

    /**
     * Write the contents inFile to outFile and save it to the localization
     * server
     * 
     * @param inFile
     * @param outFile
     */
    private void saveFile(File inFile, ILocalizationFile outFile) {
        if (inFile != null && inFile.exists()) {
            try (FileInputStream is = new FileInputStream(inFile);
                    SaveableOutputStream os = outFile.openOutputStream()) {
                saveInputStreamToLocalization(is, os);
            } catch (IOException e) {
                String msg = "Failed to save " + inFile.getAbsolutePath()
                        + " to localization file " + outFile.getPath();
                statusHandler.handle(Priority.PROBLEM, msg, e);
            } catch (LocalizationException e) {
                String msg = "Failed to open output stream for file: "
                        + outFile.getPath();
                statusHandler.handle(Priority.PROBLEM, msg, e);
            }
        }
    }

    /**
     * Write the contents of the given InputStream to the SaveableOutputStream
     * and save it to the localization server
     * 
     * @param is
     * @param os
     * @throws IOException
     */
    private void saveInputStreamToLocalization(InputStream is,
            SaveableOutputStream os) throws IOException {
        byte[] buf = new byte[2048];
        int len = is.read(buf);
        while (len > 0) {
            os.write(buf, 0, len);
            len = is.read(buf);
        }

        os.save();
    }
}
