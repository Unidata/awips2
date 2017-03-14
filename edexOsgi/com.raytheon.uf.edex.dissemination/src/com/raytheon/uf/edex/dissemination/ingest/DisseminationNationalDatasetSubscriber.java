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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Date;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2011            bfarmer     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class DisseminationNationalDatasetSubscriber implements
        INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DisseminationNationalDatasetSubscriber.class);

    private static final String AWIPS_PRIORITIES_FILENAME = "dissemination/awipsPriorities.txt";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.site.ingest.INationalDatasetSubscriber#notify(
     * java.lang.String, java.io.File)
     */
    @Override
    public void notify(String fileName, File file) {
        if ("awipsPriorities.txt".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            File outFile = pathMgr.getFile(lc, AWIPS_PRIORITIES_FILENAME);
            Date datetime = new Date();
            String backupFilename = "dissemination/awipsPriorities"
                    + datetime.getTime() + ".txt";
            File backupFile = pathMgr.getFile(lc, backupFilename);
            try {
                backupFile.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to create file "
                        + backupFilename, e);
            }
            saveFile(outFile, backupFile);
            saveFile(file, outFile);
        }
    }

    private void saveFile(File file, File outFile) {
        if ((file != null) && file.exists()) {
            BufferedReader fis = null;
            BufferedWriter fos = null;
            try {
                fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(file)));
                fos = new BufferedWriter(new OutputStreamWriter(
                        new FileOutputStream(outFile)));
                String line = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        fos.write(line);
                        fos.newLine();
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read file: " + file.getName(), e);

                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to find file: "
                        + file.getName(), e);
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        }
    }
}
