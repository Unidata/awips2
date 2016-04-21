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
package com.raytheon.edex.plugin.bufrua.ingest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.vadriver.VA_Driver;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * RAOB NDM subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 08, 2011            bfarmer     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class RAOBSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RAOBSubscriber.class);

    private Thread combineThread = null;

    @Override
    public void notify(String fileName, File file) {
        if ("raob.spi".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "basemaps/raob.spi");
            saveFile(file, outFile);
        } else if ("raob.goodness".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "basemaps/raob.goodness");
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(new Runnable() {

                    @Override
                    public void run() {
                        try {
                            Thread.sleep(60 * 1000);
                        } catch (InterruptedException e) {
                            // In the unlikely event we are interrupted, we do
                            // not actually care, just go ahead and process
                            // things.
                        }
                        processGoodness();
                    }

                }, "raob.goodness Processing");
                combineThread.start();
            }
        } else if ("raob.primary".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "basemaps/raob.primary");
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(new Runnable() {

                    @Override
                    public void run() {
                        try {
                            Thread.sleep(60 * 1000);
                        } catch (InterruptedException e) {
                            // In the unlikely event we are interrupted, we do
                            // not actually care, just go ahead and process
                            // things.
                        }
                        processGoodness();
                    }

                }, "raob.goodness Processing");
                combineThread.start();
            }
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

    private void processGoodness() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        File goodness = pathMgr.getFile(lc, "basemaps/raob.goodness");
        File primary = pathMgr.getFile(lc, "basemaps/raob.primary");
        File spi = pathMgr.getFile(lc, "basemaps/raob.spi");
        if (!primary.exists()) {
            try {
                primary.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create primary file: " + primary.getName(),
                        e);
            }
        }
        if (!spi.exists()) {
            try {
                spi.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create spi file: " + spi.getName(), e);
            }
        }
        if (goodness.exists()) {
            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodness, primary, spi);
        }
        combineThread = null;
    }

}
