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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.vadriver.VA_Driver;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.modelsounding.decoder.ModelSoundingDataAdapter;

/**
 * Subscriber to update the local model sounding sites whenever the national spi
 * file changes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 29, 2011           bfarmer     Initial creation
 * Dec 02, 2013  2537     bsteffen    Ensure streams are closed.
 * Mar 06, 2014  2876     mpduff      New NDM plugin.
 * Mar 02, 2016  5434     bkowal      Relocated to ndm dataplugin.
 * Jul 11, 2016  5744     mapeters    Save to common_static (not edex_static)
 * 
 * </pre>
 * 
 * @author bfarmer
 */

public class ModelBufrSubscriber implements INationalDatasetSubscriber {

    private static final String MODEL_STATION_LIST = ModelSoundingDataAdapter.MODEL_STATION_LIST;

    private static final String MODEL_STATION_INFO = "modelBufrStationInfo.txt";

    private static final String MODEL_GOODNESS = "modelBufr.goodness";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ModelBufrSubscriber.class);

    @Override
    public void notify(String fileName, File file) {

        statusHandler.handle(Priority.EVENTA,
                "modelBufr:Processing input file [" + fileName + "]");

        if ("modelBufr.spi".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            ILocalizationFile outLocFile = pathMgr.getLocalizationFile(lc,
                    ModelSoundingDataAdapter.SPI_FILE);
            saveFile(file, outLocFile);
            ModelSoundingDataAdapter.updateSPIData();
        } else if (MODEL_STATION_LIST.equals(fileName)
                || MODEL_STATION_INFO.equals(fileName)) {
            // Both are saved as MODEL_STATION_LIST in localization
            processModelStationTxtFile(file);
        }
    }

    private void processModelStationTxtFile(File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile outLocFile = pathMgr.getLocalizationFile(lc,
                MODEL_STATION_LIST);
        File goodnessFile = pathMgr.getFile(lc, MODEL_GOODNESS);
        saveFile(file, outLocFile);
        generateSPI(file, goodnessFile);

        File spiFile = pathMgr.getFile(lc, ModelSoundingDataAdapter.SPI_FILE);
        if (!spiFile.exists()) {
            try {
                spiFile.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(
                        Priority.SIGNIFICANT,
                        "modelBufr:Could not create spiFile file: "
                                + spiFile.getName(), e);
            }
        }

        VA_Driver driver = new VA_Driver();
        driver.setWeight(0.5f);
        driver.vaStationsFile(goodnessFile, null, spiFile);
        // updateStationList will reload spi files also
        ModelSoundingDataAdapter.update();
    }

    /**
     * 
     * @param file
     * @param goodnessFile
     */
    private void generateSPI(File file, File goodnessFile) {
        String line;
        String[] splitLine;
        try {
            try (BufferedReader fis = new BufferedReader(new FileReader(file));
                    BufferedWriter fos = new BufferedWriter(new FileWriter(
                            goodnessFile))) {
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
                                    String err = String
                                            .format("modelBufr:Invalid %s in data line [%s]",
                                                    cause, line);
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
                                        "modelBufr:Error in data line [%s]",
                                        line);
                                statusHandler.handle(Priority.PROBLEM, err, e);
                                continue;
                            }
                        }
                    }
                }
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "modelBufr:Could not read File ", e);
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
            try (BufferedReader fis = new BufferedReader(new FileReader(file));
                    BufferedWriter fos = new BufferedWriter(
                            new OutputStreamWriter(outFile.openOutputStream()))) {
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
            } catch (LocalizationException e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Failed to open output stream for file: "
                                + outFile.getPath(), e);
            } catch (IOException e) {
                // Error occurred closing fis/fos, ignore
            }
        }
    }
}
