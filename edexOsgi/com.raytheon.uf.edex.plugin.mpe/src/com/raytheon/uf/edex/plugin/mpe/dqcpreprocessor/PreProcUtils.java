package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.plugin.mpe.MpeException;

/**
 * Dqc PreProc Utils.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class PreProcUtils {

    private final static Logger logger = LoggerFactory
            .getLogger(PreProcUtils.class);

    private final static String PPD = "PPD";

    private final static String TAI = "TAI";

    private final static String TAB = "\t";

    private static final String STR_DATE_FORMAT = "yyyyMMdd";

    public static final ThreadLocal<SimpleDateFormat> strDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(STR_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private static final String STATION_LIST = "_station_list";

    private static final String TEMP_1 = "temperature_1_";

    private static final String PRECIP_1 = "precip_1_";

    private static final String POINT = "_point_";

    /**
     * This function reads the station info file. This file contains the lid,
     * latitude, longitude, etc of each station for PPH, PPD, HAI and HZI
     * blocks. Previously, the first record for each block is the number of
     * stations. However, due to requirement changes for station list file
     * format, this was changed so that the number of records for the PPD and
     * TAI stations were counted and then used later in determining the size of
     * the station structure arrays and filling those structures.
     * 
     * @param numDays
     * @param areaName
     * @return
     * @throws MpeException
     */
    static boolean readStationFile(int numDays, String areaName)
            throws MpeException {

        int numPrecipRecords = 0;
        int numTempRecords = 0;

        // Build the file path.
        Path stationListPath;
        try {
            stationListPath = AppsDefaultsConversionWrapper
                    .getPathForToken(PreProcConstants.MPE_STATION_LIST_DIR);
        } catch (AppsDefaultsPathException e) {
            logger.error("Error getting path for token "
                    + PreProcConstants.MPE_STATION_LIST_DIR, e);
            return false;
        }
        String stationListFile = AppsDefaults.getInstance().getToken(
                PreProcConstants.MPE_SITE_ID)
                + STATION_LIST;
        String fileName = stationListPath.toString() + File.separatorChar
                + stationListFile;
        // Test if file exists
        File dest = new File(fileName);
        if (!dest.exists()) {
            File source = new File(FileUtil.join(AppsDefaults.getInstance()
                    .getToken(PreProcConstants.MPE_STATION_LIST_DIR),
                    stationListFile));
            try {
                Files.copy(source.toPath(), dest.toPath());
            } catch (IOException ex) {
                logger.error("Could not copy file {} to destination {}",
                        source, dest, ex);
                return false;
            }
        }
        try (RandomAccessFile raf = new RandomAccessFile(fileName, "r")) {
            String line;
            // Count records in the file.
            while ((line = raf.readLine()) != null) {
                String[] lineItems = line.split(TAB);
                if (lineItems.length > 4) {
                    String tmpPE = lineItems[1].trim().substring(0, 3);
                    if (tmpPE.equals(PPD)) {
                        numPrecipRecords++;
                    } else if (tmpPE.equals(TAI)) {
                        numTempRecords++;
                    }
                }
            }
            if (numPrecipRecords > 0) {
                DqcPreProcInit.initPrecipInfoArray(numPrecipRecords, numDays);
                PrecipProc.setPrecipCount(numPrecipRecords);
            }
            if (numTempRecords > 0) {
                DqcPreProcInit.initTempInfoArray(numTempRecords, numDays);
                TemperatureProc.setTempCount(numTempRecords);
            }
            // rewind file
            raf.seek(0);

            List<PrecipInfo> precipInfoList = DqcPreProcInit
                    .getPrecipInfoList();
            List<TemperatureInfo> tempInfoList = DqcPreProcInit
                    .getTempInfoList();
            int p = 0;
            int t = 0;

            // fill data from file
            while ((line = raf.readLine()) != null) {
                String[] lineItems = line.split(TAB);
                if (lineItems.length > 4) {
                    String tmpPE = lineItems[1].trim().substring(0, 3);
                    if (tmpPE.equals(PPD) && numPrecipRecords > 0
                            && p < numPrecipRecords) {
                        PrecipInfo precipInfo = precipInfoList.get(p);

                        // fill precip info
                        precipInfo.setLid(lineItems[0].trim());
                        precipInfo.setLat(Double.parseDouble(lineItems[2]
                                .trim()));
                        precipInfo.setLon(Double.parseDouble(lineItems[3]
                                .trim()));
                        precipInfo.setSource(lineItems[1].trim().charAt(4));
                        precipInfoList.set(p, precipInfo);
                        p++;
                    } else if (tmpPE.equals(TAI) && numTempRecords > 0
                            && t < numTempRecords) {
                        TemperatureInfo tempInfo = tempInfoList.get(t);

                        // fill temp info
                        tempInfo.setLid(lineItems[0].trim());
                        tempInfo.setLat(Double.parseDouble(lineItems[2].trim()));
                        tempInfo.setLon(Double.parseDouble(lineItems[3].trim()));
                        tempInfo.setSource(lineItems[1].trim().charAt(4));
                        tempInfo.setExtremum(lineItems[1].trim().charAt(5));
                        tempInfoList.set(t, tempInfo);
                        t++;
                    } else {
                        // skip others
                        continue;
                    }
                }
            }
            // keep info
            DqcPreProcInit.setPrecipInfoList(precipInfoList);
            DqcPreProcInit.setTempInfoList(tempInfoList);
        } catch (IOException e) {
            logger.error(
                    "File Not Found. Error while opening file " + fileName, e);
            return false;
        }
        return true;
    }

    /**
     * This function writes out the temperature data into temperature level 1
     * point data file(s).
     * 
     * @param startTime
     * @param numDays
     * @param areaName
     * @return
     * @throws MpeException
     */
    public static void writeTemperatureData(Date startTime, int numDays,
            String areaName) throws MpeException {
        List<TemperatureInfo> tempInfoList = DqcPreProcInit.getTempInfoList();
        if (tempInfoList == null || tempInfoList.isEmpty()) {
            // exit if no data
            return;
        }
        boolean dqcEnding6hourObstimeFlag = DqcInitStruct.getInstance().dqcEnding6hourObstimeFlag;
        // Get path from AppsDef
        Path mpePointTemperaturePath;
        try {
            mpePointTemperaturePath = com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper
                    .getPathForToken(PreProcConstants.MPE_POINT_TEMPERATURE_DIR);
        } catch (AppsDefaultsPathException e) {
            logger.error("Error getting path for token "
                    + PreProcConstants.MPE_POINT_TEMPERATURE_DIR, e);
            return;
        }
        Calendar fileCal = TimeUtil.newGmtCalendar(startTime);
        fileCal.add(Calendar.DAY_OF_YEAR, 1);

        // Write files
        for (int index = 0; index < numDays; index++) {

            // Calculate date yyyyMMdd for file name
            String fileDate = strDF.get().format(fileCal.getTime());
            String fileName = TEMP_1 + areaName + POINT + fileDate;
            File outFile = new File(FileUtil.join(
                    mpePointTemperaturePath.toString(), fileName));

            try (BufferedWriter bw = new BufferedWriter(new FileWriter(outFile))) {

                // Calculate date yyyyMMdd for text lines
                String aDateTimeShef = strDF.get().format(fileCal.getTime());
                Calendar textCal = TimeUtil.newCalendar(fileCal);
                textCal.add(Calendar.DAY_OF_YEAR, -1);
                String eDateTimeShef = strDF.get().format(textCal.getTime());

                // Write text lines
                for (int i = 0; i < TemperatureProc.getTempCount(); i++) {
                    TemperatureInfo tempInfo = tempInfoList.get(i);
                    String lidf = String.format("%1$-6s", tempInfo.getLid());

                    // build the .A max temperature record
                    StringBuilder sba = new StringBuilder(".A ");
                    // %-5s %s DH12/TAI1%cXZ"
                    sba.append(lidf);
                    sba.append(aDateTimeShef).append(" DH12/TAI1");
                    sba.append(tempInfo.getSource()).append("XZ");
                    String tmpValue = "";
                    if (tempInfo.getValue().get(index).get(4) != PreProcConstants.MISSING_MAX_TEMPERATURE) {
                        tmpValue = ""
                                + String.format(
                                        " %5dS ",
                                        (int) (Math.round(tempInfo.getValue()
                                                .get(index).get(4))));
                    } else {
                        tmpValue = " m ";
                    }
                    sba.append(tmpValue);
                    sba.append("\n");
                    bw.write(sba.toString());

                    // build the .A min temperature record
                    sba = new StringBuilder(".A ");
                    // %-5s %s DH12/TAI1%cNZ"
                    sba.append(lidf);
                    sba.append(aDateTimeShef).append(" DH12/TAI1");
                    sba.append(tempInfo.getSource()).append("NZ");
                    if (tempInfo.getValue().get(index).get(5) != PreProcConstants.MISSING_MIN_TEMPERATURE) {
                        tmpValue = String
                                .format(" %5dS ", (int) Math.round((tempInfo
                                        .getValue().get(index).get(5))));
                    } else {
                        tmpValue = " m ";
                    }
                    sba.append(tmpValue);
                    sba.append("\n");
                    bw.write(sba.toString());

                    // build the .E temperature record
                    StringBuilder sbe = new StringBuilder(".E ");
                    // %-5s %s DH18/TAI1%cZZ/DIH+6/"
                    sbe.append(lidf);
                    sbe.append(eDateTimeShef);
                    if (dqcEnding6hourObstimeFlag == PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_12Z) {
                        sbe.append(" DH18/TAI1");
                    } else if (dqcEnding6hourObstimeFlag == PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_06Z) {
                        sbe.append(" DH12/TAI1");
                    }
                    sbe.append(tempInfo.getSource()).append("ZZ/DIH+6/");
                    for (int j = 0; j < 4; j++) {
                        if (tempInfo.getValue().get(index).get(j) != PreProcConstants.TEMPERATURE_MISSING) {
                            tmpValue = ""
                                    + String.format(" %5dS ", Math
                                            .round((tempInfo.getValue().get(
                                                    index).get(j))));
                        } else {
                            tmpValue = " m ";
                        }
                        sbe.append(tmpValue);
                        if (j != 3) {
                            sbe.append("/");
                        }
                    }
                    sbe.append("\n");
                    bw.write(sbe.toString());
                }
                // next day
                fileCal.add(Calendar.DAY_OF_YEAR, 1);
            } catch (IOException e) {
                throw new MpeException(
                        "In routine 'writeTemperatureData': Could not open file "
                                + outFile, e);
            }
        }
        logger.info("    STATUS: Output temperature data to: {}",
                mpePointTemperaturePath.toString());
        return;
    }

    /**
     * Writes Precipitation Data into file.
     * 
     * @param startTime
     * @param numDays
     * @param areaName
     * @return
     * @throws MpeException
     */
    public static void writePrecipData(Date startTime, int numDays,
            String areaName) throws MpeException {

        List<PrecipInfo> precipInfoList = DqcPreProcInit.getPrecipInfoList();
        if (precipInfoList == null || precipInfoList.isEmpty()) {
            // exit if no data
            return;
        }
        Path mpePointPrecipPath;
        try {
            mpePointPrecipPath = AppsDefaultsConversionWrapper
                    .getPathForToken(PreProcConstants.MPE_POINT_PRECIP_DIR);
        } catch (AppsDefaultsPathException e) {
            logger.error("Error getting path for token "
                    + PreProcConstants.MPE_POINT_PRECIP_DIR, e);
            return;
        }
        Calendar fileCal = TimeUtil.newGmtCalendar(startTime);
        fileCal.add(Calendar.DAY_OF_YEAR, 1);

        for (int index = 0; index < numDays; index++) {
            // Calculate date yyyyMMdd for file name
            String fileDate = strDF.get().format(fileCal.getTime());
            String fileName = PRECIP_1 + areaName + POINT + fileDate;
            File outFile = new File(FileUtil.join(
                    mpePointPrecipPath.toString(), fileName));
            // write file
            try (BufferedWriter bw = new BufferedWriter(new FileWriter(outFile))) {

                // Calculate date yyyyMMdd for text lines
                String aDateTimeShef = strDF.get().format(fileCal.getTime());
                Calendar textCal = TimeUtil.newCalendar(fileCal);
                textCal.add(Calendar.DAY_OF_YEAR, -1);
                String eDateTimeShef = strDF.get().format(textCal.getTime());

                // Write text lines
                for (int i = 0; i < PrecipProc.getPrecipCount(); i++) {
                    PrecipInfo precipInfo = precipInfoList.get(i);
                    String lidf = String.format("%1$-5s", precipInfo.getLid());

                    // build the .A record .A SFMS2 20160614 DH12/PPD1G/ m
                    StringBuilder sba = new StringBuilder(".A ");
                    sba.append(lidf).append(" ");
                    sba.append(aDateTimeShef).append(" DH12/PPD1");
                    sba.append(precipInfo.getSource()).append("/");
                    String tmpVal = "";
                    if (precipInfo.getpPPD().get(index) != PreProcConstants.PRECIP_MISSING) {
                        tmpVal = ""
                                + String.format(" %5.2fS", precipInfo.getpPPD()
                                        .get(index));
                    } else {
                        tmpVal = " m ";
                    }
                    sba.append(tmpVal).append("\n");
                    bw.write(sba.toString());

                    // build the .E record
                    StringBuilder sbe = new StringBuilder(".E ");
                    // ".E %-5s %s DH18/PPQ1%c/DIH+6/  0.00S/  0.00S/  0.00S/  0.00S\n"
                    sbe.append(lidf).append(" ");
                    sbe.append(eDateTimeShef).append(" DH18/");
                    sbe.append(precipInfo.getpPPQPE().get(index)).append("Q1");
                    sbe.append(precipInfo.getSource()).append("/DIH+6/");
                    for (int j = 0; j < 4; j++) {
                        String tmpValue = "";
                        if (precipInfo.getpPPQ().get(index).get(j) != PreProcConstants.PRECIP_MISSING) {
                            tmpValue = String.format(" %5.2fS ", precipInfo
                                    .getpPPQ().get(index).get(j));
                        } else {
                            tmpValue = " m ";
                        }
                        sbe.append(tmpValue);
                        if (j != 3) {
                            sbe.append("/");
                        }
                    }
                    sbe.append("\n");
                    bw.write(sbe.toString());
                }
                // next day
                fileCal.add(Calendar.DAY_OF_YEAR, 1);
            } catch (IOException e) {
                throw new MpeException(
                        "In routine 'writePrecipData': Could not open file "
                                + outFile, e);
            }
        }
        logger.info("    STATUS: Output precip data to: {}",
                mpePointPrecipPath.toString());
        return;
    }

    /**
     * Compute the date array.
     * 
     * @param startTime
     * @param numDays
     * @return
     */
    public static List<Date> getDateArray(Date startTime, int numDays) {
        List<Date> retVal = new ArrayList<>();
        Calendar clndr = TimeUtil.newGmtCalendar(startTime);
        clndr.set(Calendar.HOUR, 0);
        clndr.set(Calendar.MINUTE, 0);
        clndr.set(Calendar.SECOND, 0);
        clndr.set(Calendar.MILLISECOND, 0);
        for (int i = 0; i < numDays; i++) {
            clndr.add(Calendar.DATE, 1);
            retVal.add(clndr.getTime());
        }
        return retVal;
    }

}