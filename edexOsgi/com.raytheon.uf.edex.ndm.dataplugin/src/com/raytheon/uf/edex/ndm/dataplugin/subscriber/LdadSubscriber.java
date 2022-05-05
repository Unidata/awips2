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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

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
 * Ldad NDM subscriber.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2019 DCS 20569  MPorricelli Initial creation
 *
 * </pre>
 *
 */

public class LdadSubscriber implements INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LdadSubscriber.class);

    private static final String LDAD_DATA_INFO_NAME = "LDADinfo.txt";

    private static final String LDAD_DATA_PATH = "/data/fxa/LDAD/data/";

    private static final String SPI_MESO_FILE_NAME = "ldad15.spi";

    private static final String SPI_MESO_FILE_PATH = LocalizationUtil
            .join("basemaps", SPI_MESO_FILE_NAME);

    private static final String SPI_HYDRO_FILE_NAME = "ldad15prcp.spi";

    private static final String SPI_HYDRO_FILE_PATH = LocalizationUtil
            .join("basemaps", SPI_HYDRO_FILE_NAME);

    private static final String GOODNESS_MESO_FILE_NAME = "ldad15.goodness";

    private static final String GOODNESS_MESO_FILE_PATH = LocalizationUtil
            .join("basemaps", GOODNESS_MESO_FILE_NAME);

    private static final String GOODNESS_HYDRO_FILE_NAME = "ldad15prcp.goodness";

    private static final String GOODNESS_HYDRO_FILE_PATH = LocalizationUtil
            .join("basemaps", GOODNESS_HYDRO_FILE_NAME);

    private static final String PRIMARY_MESO_FILE_NAME = "ldad15.primary";

    private static final String PRIMARY_MESO_FILE_PATH = LocalizationUtil
            .join("basemaps", PRIMARY_MESO_FILE_NAME);

    private static final String PRIMARY_HYDRO_FILE_NAME = "ldad15prcp.primary";

    private static final String PRIMARY_HYDRO_FILE_PATH = LocalizationUtil
            .join("basemaps", PRIMARY_HYDRO_FILE_NAME);

    private class UpdateSPIFile implements Runnable {
        private String mesoOrHydro;

        public UpdateSPIFile(String type) {
            this.mesoOrHydro = type;
        }

        @Override
        public void run() {
            generateSpiFromGoodness(mesoOrHydro);
        }

    };

    private volatile UpdateSPIFile updateMesoSpi = null;

    private volatile UpdateSPIFile updateHydroSpi = null;

    @Override
    public void notify(String fileName, File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        switch (fileName) {
        case SPI_MESO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, SPI_MESO_FILE_PATH);
            saveFile(file, outFile);
            break;
        }
        case SPI_HYDRO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, SPI_HYDRO_FILE_PATH);
            saveFile(file, outFile);
            break;
        }
        case GOODNESS_MESO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, GOODNESS_MESO_FILE_PATH);
            saveFile(file, outFile);
            if (updateMesoSpi == null) {
                updateMesoSpi = new UpdateSPIFile("ldad15");
                new Thread(updateMesoSpi).start();
            }
            break;
        }
        case GOODNESS_HYDRO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, GOODNESS_HYDRO_FILE_PATH);
            saveFile(file, outFile);
            if (updateHydroSpi == null) {
                updateHydroSpi = new UpdateSPIFile("ldad15prcp");
                new Thread(updateHydroSpi).start();
            }
            break;
        }
        case PRIMARY_MESO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, PRIMARY_MESO_FILE_PATH);
            saveFile(file, outFile);
            if (updateMesoSpi == null) {
                updateMesoSpi = new UpdateSPIFile("ldad15");
                new Thread(updateMesoSpi).start();
            }
            break;
        }
        case PRIMARY_HYDRO_FILE_NAME: {
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc, PRIMARY_HYDRO_FILE_PATH);
            saveFile(file, outFile);
            if (updateHydroSpi == null) {
                updateHydroSpi = new UpdateSPIFile("ldad15prcp");
                new Thread(updateHydroSpi).start();
            }
            break;
        }
        case LDAD_DATA_INFO_NAME: {
            String line;
            String[] splitLine;
            List<String> staListMeso = new ArrayList<>();
            List<String> staListHydro = new ArrayList<>();

            File outFileMeso = pathMgr.getFile(lc, "LDADMesoStationInfo.tmp");
            // Will be appending, so want to start from nothing
            if (outFileMeso.exists()) {
                try {
                    outFileMeso.delete();
                } catch (SecurityException e) {
                    statusHandler.error("Unable to delete " + outFileMeso, e);
                }
            }
            if (!outFileMeso.exists()) {
                try {
                    outFileMeso.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not create LDADMesoStationInfo.tmp file. ",
                            e);
                }
            }
            File outFileHydro = pathMgr.getFile(lc, "LDADHydroStationInfo.tmp");
            // Will be appending, so want to start from nothing
            if (outFileHydro.exists()) {
                try {
                    outFileHydro.delete();
                } catch (SecurityException e) {
                    statusHandler.error("Unable to delete " + outFileHydro, e);
                }
            }
            if (!outFileHydro.exists()) {
                try {
                    outFileHydro.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not create LDADHydroStationInfo.tmp file. ",
                            e);
                }
            }
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(new FileInputStream(file)))) {
                /*- Read each line of LDADinfo.txt which is formatted like:
                      datatype |Station.txt | netCDF key|plot key | LDAD type |storage dir | preprocess script|
                 e.g. alert_wx |ALERT       |87         |86       |CVS_TYPE   |mesonet     |preProcessLDAD.pl |
                */
                for (line = fis.readLine(); line != null; line = fis
                        .readLine()) {
                    if (line.startsWith("#") || line.startsWith("msas")) {
                        continue;
                    }
                    splitLine = line.split("\\|");

                    String dataType = splitLine[0].trim();
                    String dataProvider = getDataProvider(dataType);

                    String staType = splitLine[1].trim();

                    String stationFileName = staType + "Station.txt";

                    String fullPathStationFile = LDAD_DATA_PATH
                            + stationFileName;
                    File stationFile = new File(fullPathStationFile);
                    if (!stationFile.exists())
                        continue;

                    if (splitLine[5].trim().contentEquals("mesonet")) {
                        if (staListMeso.contains(staType)) {
                            // This station file has already been processed
                            continue;
                        }
                        staListMeso.add(staType);
                        // append this station file's contents to
                        // LDADMesoStationInfo.txt */
                        createUnifiedStationFile(stationFile, outFileMeso,
                                dataProvider);
                    } else if (splitLine[5].trim().contentEquals("hydro")) {
                        if (staListHydro.contains(staType)) {
                            // This station file has already been processed
                            continue;
                        }
                        staListHydro.add(staType);
                        // append this station file's contents to
                        // LDADHydroStationInfo.txt
                        createUnifiedStationFile(stationFile, outFileHydro,
                                dataProvider);
                    }
                }

                File goodnessFileMeso = pathMgr.getFile(lc,
                        GOODNESS_MESO_FILE_PATH);
                File goodnessFileHydro = pathMgr.getFile(lc,
                        GOODNESS_HYDRO_FILE_PATH);

                generateGoodnessFile(outFileMeso, goodnessFileMeso);
                generateGoodnessFile(outFileHydro, goodnessFileHydro);

                // done with LDADMesoStationInfo.tmp and
                // LDADHydroStationInfo.tmp
                try {
                    outFileMeso.delete();
                } catch (SecurityException e) {
                    statusHandler.error("Unable to delete " + outFileMeso, e);
                }
                try {
                    outFileHydro.delete();
                } catch (SecurityException e) {
                    statusHandler.error("Unable to delete " + outFileHydro, e);
                }
                if (updateMesoSpi == null) {
                    updateMesoSpi = new UpdateSPIFile("ldad15");
                    new Thread(updateMesoSpi).start();
                }
                if (updateHydroSpi == null) {
                    updateHydroSpi = new UpdateSPIFile("ldad15prcp");
                    new Thread(updateHydroSpi).start();
                }

            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read file: " + file.getName(), e);

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error with file: " + file.getName(), e);
            }

            break;
        }

        } // end switch stmt
    }

    /**
     * Gets the data provider from the relevant description (.desc) file. Most
     * desc files (e.g. RAWS.desc) contain a line containing the data provider:
     *
     * <pre>
     * Data Source | Missing     | Data Provider
     * RAWS        | -99. -99.00 | NWSRAWS
     * </pre>
     *
     * This information is later combined with the provider ID and put into
     * column 7 of the ldad spi file
     *
     * @param dataType
     * @return dataProvider, or dataType if dataProvider unavailable
     */
    private String getDataProvider(String dataType) {
        String descFileName = dataType + ".desc";
        String fullPathDescFile = LDAD_DATA_PATH + descFileName;
        File descFile = new File(fullPathDescFile);
        if (!descFile.exists()) {
            return dataType;
        }
        String line;
        String[] splitLine;
        try (BufferedReader fis = new BufferedReader(
                new InputStreamReader(new FileInputStream(descFile)))) {
            for (line = fis.readLine(); line != null; line = fis.readLine()) {
                if (line.startsWith("#")) {
                    continue;
                }
                splitLine = line.split("\\|");
                if (splitLine[0].trim().toUpperCase()
                        .equals(dataType.toUpperCase())) {
                    return splitLine[2].trim();
                }
            }
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return dataType;
    }

    /**
     * Create goodness file that will be used by VA_Driver process to make final
     * spi output
     *
     * @param file
     * @param goodnessFile
     */
    private void generateGoodnessFile(File file, File goodnessFile) {
        String line;
        String[] splitLine;
        String dataProvider = null, accessId = null, providerId = null;
        try (BufferedReader fis = new BufferedReader(
                new InputStreamReader(new FileInputStream(file)))) {
            if (!goodnessFile.exists()) {
                goodnessFile.createNewFile();
            }
            try (BufferedWriter fos = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(goodnessFile)))) {
                for (line = fis.readLine(); line != null; line = fis
                        .readLine()) {
                    if (line.startsWith("#")) {
                        continue;
                    }
                    splitLine = line.split("\\|");
                    /*- Check if current line is the dataProvider info
                    // e.g.
                    // dataProvider | NWSRAWS
                     */
                    if (splitLine[0].trim().equals("dataProvider")) {
                        dataProvider = splitLine[1].trim();
                        continue;
                    }
                    if (splitLine.length < 3) {
                        continue;
                    }
                    providerId = splitLine[0].trim();
                    accessId = providerId + dataProvider;
                    Integer elevation;
                    try {
                        if (!splitLine[3].trim().isEmpty()) {
                            elevation = (int) Math.round(
                                    Double.parseDouble(splitLine[3].trim()));
                        } else {
                            elevation = -9999;
                        }
                    } catch (Exception e) {
                        statusHandler.warn(
                                "Error determining elevation from line " + line
                                        + "in file " + file.getName(),
                                e);
                        continue;
                    }
                    Double latitude = null;
                    Double longitude = null;
                    try {
                        if (!splitLine[4].trim().isEmpty()
                                && !splitLine[5].trim().isEmpty()) {
                            latitude = Double.parseDouble(splitLine[4].trim());
                            longitude = Double.parseDouble(splitLine[5].trim());
                        } else {
                            statusHandler
                                    .warn("latitide and/or longitude are blank in line "
                                            + line + ", skipping line");
                            continue;
                        }
                    } catch (NumberFormatException e) {
                        statusHandler.error(
                                "Error processing line " + line + " in file "
                                        + file.getPath() + ", skipping line",
                                e);
                        continue;
                    }
                    if (latitude == -9999. || longitude == -9999.) {
                        continue;
                    }
                    String stationName = splitLine[1].trim();
                    fos.write("0 ");
                    fos.write(stationName);
                    fos.write(String.format(" %8.4f %9.4f %5d %9d %s", latitude,
                            longitude, elevation, 0, accessId));
                    fos.newLine();
                }
            }
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not read file: " + file.getName(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error with file: " + file.getName(), e);
        }
    }

    /**
     * Appends the contents of a given ldad station file (e.g. RAWSStation.txt)
     * to a combined mesonet or hydro station file to create a single temporary
     * file containing all relevant stations
     *
     * @param file
     * @param outFile
     * @param dataProvider
     */
    private void createUnifiedStationFile(File file, File outFile,
            String dataProvider) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(new FileInputStream(file)));
                    BufferedWriter fos = new BufferedWriter(
                            new FileWriter(outFile, true))) {
                String line = null;
                if (!dataProvider.equals(null)) {
                    fos.write("dataProvider | " + dataProvider);
                    fos.newLine();
                }
                while ((line = fis.readLine()) != null) {
                    fos.write(line);
                    fos.newLine();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "IOError reading file  " + file.getName()
                                + " or writing file " + outFile.getName(),
                        e);
            }
        }
    }

    /**
     * Copy file into final location
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

    /**
     * Generate ldad15.spi from ldad15.goodness, ldad15prcp.spi from
     * ldad15prcp.goodness
     *
     * @param type
     */
    private synchronized void generateSpiFromGoodness(String type) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        File goodness = pathMgr.getFile(lc, "basemaps/" + type + ".goodness");
        File primary = pathMgr.getFile(lc, "basemaps/" + type + ".primary");
        File primaryArg = (primary.exists()) ? primary : null;

        File spi = pathMgr.getFile(lc, "basemaps/" + type + ".spi");
        if (!spi.exists()) {
            try {
                spi.createNewFile();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not create spi file. ", e);
            }
        }
        if (goodness.exists()) {
            VA_Driver driver = new VA_Driver();
            driver.setWeight(0.5f);
            driver.vaStationsFile(goodness, primaryArg, spi);
        }
        if (type.equals("ldad15")) {
            updateMesoSpi = null;
        } else {
        updateHydroSpi = null;
        }
    }

}
