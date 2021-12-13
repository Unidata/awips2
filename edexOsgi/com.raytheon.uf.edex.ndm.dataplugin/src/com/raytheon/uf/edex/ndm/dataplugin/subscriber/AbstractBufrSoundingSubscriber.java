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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.vadriver.VA_Driver;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * A National Dataset Subscriber for BUFR Sounding Data
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -----------------------------------------------------------------
 * Mar 23, 2016 5495       jschmid     Initial creation: process input and create localized (.spi) output.
 * 
 * </pre>
 * 
 * @author jschmid
 * @version 1.0
 */

public abstract class AbstractBufrSoundingSubscriber implements
        INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractBufrSoundingSubscriber.class);

    /**
     * Constructor
     */
    public AbstractBufrSoundingSubscriber() {
        super();
    }

    @Override
    public void notify(String fileName, File dropfile) {

        statusHandler.handle(Priority.INFO, "Processing input file ["
                + fileName + "]");

        if (fileName.endsWith(".spi")) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    IPathManager.SEPARATOR + "basemaps"
                            + IPathManager.SEPARATOR + fileName);
            saveFile(dropfile, outFile);

        } else if (fileName.endsWith("StationInfo.txt")) {
            String prefix = fileName.substring(0,
                    fileName.indexOf("StationInfo.txt"));
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile out_txtFile = pathMgr.getLocalizationFile(lc,
                    fileName);
            LocalizationFile goodnessFile = pathMgr.getLocalizationFile(lc,
                    prefix + ".goodness");
            saveFile(dropfile, out_txtFile);
            generateGoodnessFile(dropfile, goodnessFile);

            LocalizationFile spiFile = pathMgr.getLocalizationFile(lc,
                    File.separator + "basemaps" + File.separator + prefix
                            + ".spi");
            File spiFileHandle = spiFile.getFile();

            if (!spiFileHandle.exists()) {
                try {
                    spiFileHandle.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.ERROR,
                            "Could not create spi file: " + spiFile.getName(),
                            e);
                    return; // Need spi File created
                }
            }

            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f); // Goodness file is 'station file'
            driver.vaStationsFile(goodnessFile.getFile(), null, spiFileHandle);

            try {
                spiFile.save();
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.ERROR, "Could not save: "
                        + prefix + ".spi", e);
            }
        }

    }

    /**
     * 
     * @param txtFile
     * @param goodnessFile
     */
    private void generateGoodnessFile(File txtFile,
            ILocalizationFile goodnessFile) {
        String[] splitLine;
        try (BufferedReader fis = new BufferedReader(new InputStreamReader(
                new FileInputStream(txtFile)));
                SaveableOutputStream sos = goodnessFile.openOutputStream();
                BufferedWriter fos = new BufferedWriter(new OutputStreamWriter(
                        sos))) {
            String line;
            while ((line = fis.readLine()) != null) {
                if (line.length() > 0) {
                    // check for commented lines
                    if ('#' != line.charAt(0)) {
                        try {
                            splitLine = line.split("\\|");
                            Integer elevation;
                            Double latitude;
                            Double longitude;
                            String cause = "elevation";
                            try {
                                elevation = Integer.parseInt(splitLine[4]
                                        .trim());
                                cause = "latitude";
                                latitude = Double.parseDouble(splitLine[2]
                                        .trim());
                                cause = "longitude";
                                longitude = Double.parseDouble(splitLine[3]
                                        .trim());
                            } catch (NumberFormatException nfe) {
                                String err = String.format(
                                        "Invalid %s in data line [%s]", cause,
                                        line);
                                statusHandler.handle(Priority.PROBLEM, err);
                                continue;
                            }
                            String stationName = splitLine[1].trim();
                            fos.write("0 ");
                            fos.write(stationName);
                            fos.write(String.format(" %8.4f %9.4f %5d %9d",
                                    latitude, longitude, elevation, 0));
                            fos.newLine();
                        } catch (Exception e) {
                            String err = String.format(
                                    "Error in data line [%s]", line);
                            statusHandler.handle(Priority.PROBLEM, err, e);
                            continue;
                        }
                    }
                }
            }
        } catch (IOException ioe) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error closing input file [" + txtFile.getName() + "]");
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error closing input file [" + txtFile.getName() + "]");
        }
    }

    /**
     * 
     * @param file
     * @param outFile
     */
    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(new InputStreamReader(
                    new FileInputStream(file)));
                    SaveableOutputStream sos = outFile.openOutputStream();
                    BufferedWriter fos = new BufferedWriter(
                            new OutputStreamWriter(sos))) {
                String line = null;

                while ((line = fis.readLine()) != null) {
                    fos.write(line);
                    fos.newLine();
                }

                sos.save();
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to save: "
                        + file.getName(), e);
            }
        }
    }
}
