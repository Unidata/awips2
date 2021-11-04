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

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.vadriver.VA_Driver;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * MTR NDM subscriber.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2011            bfarmer     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * Mar 02, 2016   5434     bkowal      Relocated to ndm dataplugin.
 * Jan 10, 2018   6713     dgilling    Code cleanup.
 *
 * </pre>
 *
 * @author bfarmer
 */

public class MTRSubscriber implements INationalDatasetSubscriber {

    private static final String GOODNESS_FILE_NAME = "MTR.goodness";

    private static final String GOODNESS_FILE_PATH = LocalizationUtil
            .join("basemaps", GOODNESS_FILE_NAME);

    private static final String PRIMARY_FILE_NAME = "MTR.primary";

    private static final String PRIMARY_FILE_PATH = LocalizationUtil
            .join("basemaps", PRIMARY_FILE_NAME);

    private static final String SPI_FILE_NAME = "MTR.spi";

    private static final String SPI_FILE_PATH = LocalizationUtil
            .join("basemaps", SPI_FILE_NAME);

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MTRSubscriber.class);

    private final Runnable updateSPIFile = new Runnable() {

        @Override
        public void run() {
            try {
                Thread.sleep(60 * 1000);
            } catch (InterruptedException e) {
                /*
                 * In the unlikely event we are interrupted, we do not actually
                 * care, just go ahead and process things.
                 */
            }
            processGoodness();
        }
    };

    private Thread combineThread = null;

    @Override
    public void notify(String fileName, File file) {
        if (SPI_FILE_NAME.equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    SPI_FILE_PATH);
            saveFile(file, outFile);
        } else if (GOODNESS_FILE_NAME.equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    GOODNESS_FILE_PATH);
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(updateSPIFile,
                        "MTR.goodness Processing");
                combineThread.start();
            }
        } else if (PRIMARY_FILE_NAME.equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    PRIMARY_FILE_PATH);
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(updateSPIFile,
                        "MTR.goodness Processing");
                combineThread.start();
            }
        }
    }

    /**
     * Save the contents of the given File to the given ILocalizationFile
     *
     * @param file
     * @param outFile
     */
    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try (SaveableOutputStream fos = outFile.openOutputStream()) {
                Files.copy(file.toPath(), fos);
                fos.save();
            } catch (IOException e) {
                statusHandler.error("Error reading from " + file
                        + " or writing to file " + outFile, e);
            } catch (LocalizationException e) {
                statusHandler.error("Error writing to " + outFile, e);
            }
        }
    }

    private void processGoodness() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);

        File primary = pathMgr.getFile(lc, PRIMARY_FILE_PATH);
        File primaryArg = (primary.exists()) ? primary : null;

        File spi = pathMgr.getFile(lc, SPI_FILE_PATH);
        if (!spi.exists()) {
            try {
                spi.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create spi file: " + spi.getName(), e);
            }
        }

        File goodness = pathMgr.getFile(lc, GOODNESS_FILE_PATH);
        if (goodness.exists()) {
            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodness, primaryArg, spi);
        }
        combineThread = null;
    }
}
