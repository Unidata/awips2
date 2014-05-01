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
package com.raytheon.uf.edex.plugin.modelsounding.ingest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.vadriver.VA_Driver;
import com.raytheon.uf.common.site.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class ModelBufrSubscriber implements INationalDatasetSubscriber {

    private static final String MODEL_STATION_LIST = ModelSoundingDataAdapter.MODEL_STATION_LIST;
    
    private static final String MODEL_STATION_INFO = "modelBufrStationInfo.txt";
    
    private static final String MODEL_GOODNESS = "modelBufr.goodness";
    
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ModelBufrSubscriber.class);

    @Override
    public void notify(String fileName, File file) {
        
        statusHandler.handle(Priority.EVENTA,
                "modelBufr:Processing input file [" + fileName + "]");
        
        if ("modelBufr.spi".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, ModelSoundingDataAdapter.SPI_FILE);
            saveFile(file, outFile);
            ModelSoundingDataAdapter.updateSPIData();
        } else if (MODEL_STATION_LIST.equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, MODEL_STATION_LIST);
            File goodnessFile = pathMgr.getFile(lc, MODEL_GOODNESS);
            saveFile(file, outFile);
            generateSPI(file, goodnessFile);

            File spiFile = pathMgr.getFile(lc, ModelSoundingDataAdapter.SPI_FILE);
            if (!spiFile.exists()) {
                try {
                    spiFile.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "modelBufr:Could not create primary file. ", e);
                }
            }

            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodnessFile, null, spiFile);
            // updateStationList will reload spi files also
            ModelSoundingDataAdapter.update();
        } else if (MODEL_STATION_INFO.equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File outFile = pathMgr.getFile(lc, MODEL_STATION_LIST);
            File goodnessFile = pathMgr.getFile(lc, MODEL_GOODNESS);
            saveFile(file, outFile);
            generateSPI(file, goodnessFile);

            File spiFile = pathMgr.getFile(lc, ModelSoundingDataAdapter.SPI_FILE);
            if (!spiFile.exists()) {
                try {
                    spiFile.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "modelBufr:Could not create primary file. ", e);
                }
            }

            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodnessFile, null, spiFile);
            // updateStationList will reload spi files also
            ModelSoundingDataAdapter.update();
            
        }
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
            BufferedReader fis = null;
//            if (!goodnessFile.exists()) {
//                goodnessFile.createNewFile();
//            }
            BufferedWriter fos = null;
            try {
                fis = new BufferedReader(new FileReader(file));
                fos = new BufferedWriter(new FileWriter(goodnessFile));
                while ((line = fis.readLine()) != null) {
                    if(line.length() > 0) {
                        // check for commented lines
                        if('#' != line.charAt(0)) {
                            try {
                                splitLine = line.split("\\|");
                                Integer elevation;
                                Double latitude;
                                Double longitude;
                                String cause = "elevation";
                                try {
                                    elevation = Integer.parseInt(splitLine[4].trim());
                                    cause = "latitude";
                                    latitude = Double.parseDouble(splitLine[2].trim());
                                    cause = "longitude";
                                    longitude = Double.parseDouble(splitLine[3].trim());
                                } catch (NumberFormatException nfe) {
                                    String err = String.format("modelBufr:Invalid %s in data line [%s]", cause, line);
                                    statusHandler.handle(Priority.PROBLEM,err);
                                    continue;
                                }
                                String stationName = splitLine[1].trim();
                                fos.write("0 ");
                                fos.write(stationName);
                                fos.write(String.format(" %8.4f %9.4f %5d %9d", latitude,
                                        longitude, elevation, 0));
                                fos.newLine();
                            } catch (Exception e) {
                                String err = String.format("modelBufr:Error in data line [%s]", line);
                                statusHandler.handle(Priority.PROBLEM,err,e);
                                continue;
                            } 
                        }
                    }
                }
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException ioe) {
                        statusHandler.handle(
                                Priority.SIGNIFICANT,
                                "modelBufr:Error closing input file ["
                                        + file.getName() + "]");
                    }
                }
                if(fos != null) {
                    try {
                        fos.close();
                    } catch (IOException ioe) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                "modelBufr:Error closing output file [" + goodnessFile.getName() + "]");
                    }
                }
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.SIGNIFICANT, "modelBufr:Could not read File ", e);
        }
    }
    
    
    /**
     * 
     * @param file
     * @param outFile
     */
    private void saveFile(File file, File outFile) {
        if ((file != null) && file.exists()) {
            BufferedReader fis = null;
            BufferedWriter fos = null;
            try {
                fis = new BufferedReader(new FileReader(file));
                fos = new BufferedWriter(new FileWriter(outFile));
                String line = null;
                while ((line = fis.readLine()) != null) {
                    fos.write(line);
                    fos.newLine();
                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.SIGNIFICANT, "modelBufr:Failed to find File ",
                        e);
            } catch (IOException e) {
                statusHandler.handle(Priority.SIGNIFICANT, "modelBufr:Error reading File ",
                        e);
            } finally {
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException e) {
                        statusHandler.handle(
                                Priority.SIGNIFICANT,
                                "Error closing output file ["
                                        + outFile.getName() + "]");
                    }
                }
            }
        }
    }
}
