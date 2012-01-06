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
package com.raytheon.uf.common.site.ingest;

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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2011            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class BuoySubscriber implements INationalDatasetSubscriber {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(BuoySubscriber.class);
    private Thread combineThread = null;

    @Override
    public void notify(String fileName, File file) {
        if ("BUOY.spi".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            File outFile = pathMgr.getFile(lc, "BUOY.spi");
            saveFile(file, outFile);
        } else if ("BUOY.goodness".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "basemaps/BUOY.goodness");
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(new Runnable() {

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

                }, "BUOY.goodness Processing");
                combineThread.start();
            }
        } else if ("BUOY.primary".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "basemaps/BUOY.primary");
            saveFile(file, outFile);
            if (null == combineThread) {
                combineThread = new Thread(new Runnable() {

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

                }, "BUOY.goodness Processing");
                combineThread.start();
            }
        } else if ("maritimeStationInfo.txt".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, "maritimeStationInfo.txt");
            File goodnessFile = pathMgr.getFile(lc, "BUOY.goodness");
            saveFile(file, outFile);
            generateSPI(file, goodnessFile);

            File spiFile = pathMgr.getFile(lc, "basemaps/BUOY.spi");
            if (!spiFile.exists()) {
                try {
                    spiFile.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not create primary file. ", e);
                }
            }

            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodnessFile, null, spiFile);
        }

    }

    private void generateSPI(File file, File goodnessFile) {
        String line;
        String[] splitLine;
        try {
            BufferedReader fis = new BufferedReader(new InputStreamReader(
                    new FileInputStream(file)));
            if (!goodnessFile.exists()) {
                goodnessFile.createNewFile();
            }
            BufferedWriter fos = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(goodnessFile)));
            for (line = fis.readLine(); line != null; line = fis.readLine()) {
                splitLine = line.split("\\|");
                Integer elevation;
                Double latitude;
                Double longitude;
                String stationName;
                try {
                    elevation = Integer.parseInt(splitLine[4].trim());
                } catch (Exception e) {
                    continue;
                }
                latitude = Double.parseDouble(splitLine[2].trim());
                longitude = Double.parseDouble(splitLine[3].trim());
                stationName = splitLine[1].trim();
                fos.write("0 ");
                fos.write(stationName);
                fos.write(String.format(" %8.4f %9.4f %5d %9d", latitude,
                        longitude, elevation, 0));
                fos.newLine();
            }
            fos.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM,
                    "Could not read File ", e);

        } catch (IOException e) {
        }

    }

    private void saveFile(File file, File outFile) {
        if ((file != null) && file.exists()) {
            try {
                BufferedReader fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(file)));
                BufferedWriter fos = new BufferedWriter(new OutputStreamWriter(
                        new FileOutputStream(outFile)));
                String line = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        fos.write(line);
                        fos.newLine();
                    }
                    fos.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read File ", e);

                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to find File ", e);

            }
        }
    }

    private void processGoodness() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        File goodness = pathMgr.getFile(lc, "basemaps/BUOY.goodness");
        File primary = pathMgr.getFile(lc, "basemaps/BUOY.primary");
        File spi = pathMgr.getFile(lc, "basemaps/BUOY.spi");
        if (!primary.exists()) {
            try {
                primary.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create primary file. ", e);
            }
        }
        if (!spi.exists()) {
            try {
                spi.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create primary file. ", e);
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
