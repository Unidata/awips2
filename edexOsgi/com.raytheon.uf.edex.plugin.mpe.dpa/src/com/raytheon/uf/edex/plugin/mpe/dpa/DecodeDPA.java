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
package com.raytheon.uf.edex.plugin.mpe.dpa;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.mpe.SequenceFinder;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;
import com.raytheon.uf.edex.plugin.mpe.dpa.dao.DpaAdaptDao;
import com.raytheon.uf.edex.plugin.mpe.dpa.dao.DpaRadarDao;

/**
 * Decode uncompressed radar product file after verification, or throw
 * DecodeFailedException.
 * 
 * Ported and modified from: { decodeDPA.c, get_radid_from_product.c,
 * get_radid_from_filename.c, check_radid.c, top_hour_check.c,
 * write_stage1_decoded.c, get_build_num.c, copy_to_archive.c,
 * convert_to_LittleEndian.c, convertJulianDate.c }
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2016 4622       jschmid     Initial creation
 * Aug 25, 2016 4622       njensen     Only initialize jaxbManager once
 * Sep 16, 2016 5631       bkowal      Use {@link DPAConstants}.
 * Sep 28, 2016 4622       skorolev    Added DPAConstants, Corrected Logs record data.
 * Dec 06, 2016 4622       bkowal      Re-implement generation of the LE file. Ensure it
 *                                     is written to a separate location.
 * Jan 12, 2016 6023       bkowal      Only open the LE file in Append mode if it already exists.
 *                                     Otherwise, open the file in Create mode.
 * 
 * </pre>
 * 
 * @author jschmid
 */
public class DecodeDPA {

    public static final String ID_NOT_FOUND = "ID_NOT_FOUND_IN_FILENAME";

    public static final short LEVEL_OUT_OF_RANGE = 255;

    public static final AppsDefaults APPS_DEFAULTS = AppsDefaults.getInstance();

    public static String DPA_GATHER_PATH, DPA_GRID_PATH, DPA_ARCHIVE_PATH;

    private static Path DPA_GATHER_PARALLEL_PATH;

    public static final String DPA_PREFIX_STR = "DPA";

    public static final Pattern DPA_PATTERN = Pattern.compile("DPA...");

    public static final Pattern ADAP_HEADER_PATTERN = Pattern
            .compile("ADAP\\((\\d\\d)\\)\\s+");

    public static final Pattern BUILD5_PATTERN = Pattern.compile("BLOCK");

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final RadarlocDao radarlocDao = new RadarlocDao();

    private final DpaRadarDao dparadarDao = new DpaRadarDao();

    private final DpaAdaptDao dpaAdaptDao = new DpaAdaptDao();

    private final SequenceFinder productDataSeq = new SequenceFinder();

    // window around top-of-hour for decoding. Default to 10 minutes.
    private int decodeWindow = 10;

    // window around top-of-hour for archiving. Default to 10 minutes.
    private int archWindow = 10;

    private Boolean decon;

    private Boolean archon;

    private int recordMinsFromTop;

    private boolean topHourDecFlag;

    private boolean tophArchFlag;

    private boolean writeToDB = false;

    private final String CONFIG_FILE = "dpa" + IPathManager.SEPARATOR
            + "writeToDB.xml";

    private SingleTypeJAXBManager<DpaXmlConfig> jaxbManager;

    public ConvertToArchiveFile saveToArchFile = new ConvertToArchiveFile();

    /**
     * Decode DPA.
     * 
     * @throws DecodeDPAException
     */
    public DecodeDPA() throws DecodeDPAException {
        // Commented for the future use.
        // writeToDB = getLocalizedXMLParameter();
        decon = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean("dpa_filter_decode");
        archon = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean("dpa_archive");
        try {
            decodeWindow = AppsDefaults.getInstance()
                    .getInt("dpa_decode_window", 10);
        } catch (Exception e) {
            logger.warn(
                    "Unable to parse APPS_DEFAULTS token 'dpa_decode_window', using 10 minutes default.");
        }
        try {
            archWindow = AppsDefaults.getInstance().getInt("dpa_archive_window",
                    10);
        } catch (Exception e) {
            logger.warn(
                    "Unable to parse APPS_DEFAULTS token 'dpa_archive_window', using 10 minutes default.");
        }
        DPA_GATHER_PATH = APPS_DEFAULTS.getToken("dpa_gather");
        if (null == DPA_GATHER_PATH) {
            throw new DecodeDPAException(
                    "Unable to find required localized path: 'dpa_gather' from APPS_DEFAULTS.");
        }
        /*
         * The parallel version of the DPA gather path is used to write .LE
         * files.
         */
        try {
            DPA_GATHER_PARALLEL_PATH = AppsDefaultsConversionWrapper
                    .getPathForToken("dpa_gather");
        } catch (AppsDefaultsPathException e) {
            throw new DecodeDPAException(
                    "Unable to find required localized output-path: 'dpa_gather' from APPS_DEFAULTS.");
        }
        try {
            DPA_GRID_PATH = AppsDefaultsConversionWrapper
                    .getPathForToken("dpa_grid_dir").toString();
        } catch (AppsDefaultsPathException e) {
            throw new DecodeDPAException(
                    "Unable to find required localized output-path: 'dpa_grid_dir' from APPS_DEFAULTS.");
        }
        try {
            DPA_ARCHIVE_PATH = AppsDefaultsConversionWrapper
                    .getPathForToken("dpa_arch_dir").toString();
        } catch (AppsDefaultsPathException e) {
            throw new DecodeDPAException(
                    "Unable to find required localized output-path: 'dpa_arch_dir' from APPS_DEFAULTS.");
        }
    }

    /**
     * Decode Record.
     * 
     * @param recordFilename
     * @throws DecodeDPAException
     */
    public void decodeRecord(String recordFilename) throws DecodeDPAException {

        File gatherDir = new File(DPA_GATHER_PATH);
        if (!gatherDir.exists()) {
            try {
                Files.createDirectories(gatherDir.toPath());
            } catch (IOException e) {
                throw new DecodeDPAException(
                        "DPA gather directory does not exist, unable to create: "
                                + gatherDir.getPath());
            }
        }
        byte[] fileDataBytes;
        String radarIdDataStr;
        File recordFile = new File(
                FileUtil.join(gatherDir.getPath(), recordFilename));

        try {
            fileDataBytes = Files.readAllBytes(recordFile.toPath());
            radarIdDataStr = new String(fileDataBytes);
        } catch (IOException e) {
            throw new DecodeDPAException(
                    "Product file data could not be read from 'gatherDir': "
                            + recordFile.getPath());
        }

        String radarId = getRadarIdFromFilename(recordFile.getPath());
        if (ID_NOT_FOUND == radarId) {
            radarId = getRadarIdFromProduct(radarIdDataStr);
        }
        if (null != radarId && radarIdLocActive(radarId)) {
            decodeProductData(fileDataBytes, radarId, recordFile);
        }
    }

    /**
     * Read 3-letter code preceding 'DPA' token in filename.
     * ("SDUS53_1558_KOAX_DPA_116526669.rad") --> "OAX"
     * 
     * @param filename
     * @return
     */
    private String getRadarIdFromFilename(String filename) {

        int dpaIdx = filename.lastIndexOf(DPA_PREFIX_STR);
        if (dpaIdx < 0) {
            return ID_NOT_FOUND;
        }
        int codeStart = (dpaIdx - 4), codeEnd = (dpaIdx - 1);
        if ((codeStart < 0) || (codeEnd < 0)) {
            return ID_NOT_FOUND;
        }
        /*
         * TODO: use regex to extract the radar id from the filename.
         */
        return (filename.substring(codeStart, codeEnd));
    }

    /**
     * Find 3-letter code in wmo header or throw DecodeFailedException.
     * 
     * @param productData
     * @return
     * @throws DecodeDPAException
     */
    private String getRadarIdFromProduct(String productData)
            throws DecodeDPAException {

        String radarId = null;
        Matcher dpaMatch = DPA_PATTERN.matcher(productData);
        if (dpaMatch.find()) {
            /*
             * TODO: fix this regex pattern so that it is not necessary to
             * invoke substring on a matched group.
             */
            radarId = dpaMatch.group(0).substring(3).toUpperCase();
        }
        return (radarId);
    }

    /**
     * Throws exception with message if the corresponding radarId of product in
     * radarloc table has 'use_radar' column: false, indicating do not use.
     * 
     * @param radarId
     *            'radarloc'-table radarId of product being decoded.
     */
    private boolean radarIdLocActive(String radarId) {

        if (!radarlocDao.isRadarActive(radarId)) {

            String msg = "Radar identifier " + radarId
                    + " column: use_radar is false. Product not decoded.";
            logger.info(msg);
            return false;
        }
        return true;
    }

    /**
     * Returns false if current product is not 'closer' than previous products
     * deemed to be within top-of-hour. If current product and previous product
     * are equal number of minutes from the top-of-hour, then current product is
     * defined to be 'closer' than previous.
     * 
     * A window is created extending before and after the Top-of-Hour mark with
     * a size determined by maxTopOfHrWindowDist to filter the matching-Id query
     * in 'getWindowObstimesForId'.
     * 
     * @param radarId
     *            column radarId in 'dparadar' table of product being decoded.
     * @param recordTime
     * @param recordMinute
     *            current record's minute field from timestamp.
     * @return : Whether the current record is closest to Top of Hour compared
     *         with records in 'dparadar' table.
     * @throws DecodeDPAException
     */
    private boolean isClosestToTopOfHr(String radarId, String recordTime,
            int recordMinute) throws DecodeDPAException {

        Date closestTimeMax = null, closestTimeMin = null;
        Timestamp recordTimestamp = null;
        try {
            recordTimestamp = Timestamp.valueOf(recordTime);
        } catch (IllegalArgumentException e) {
            throw new DecodeDPAException(
                    "Unable to create timestamp from product time: "
                            + recordTime
                            + ". Cannot determine if closest to top, forced return of false.");
        }

        Calendar topOfHourCal = TimeUtil.newGmtCalendar(recordTimestamp);
        if (topOfHourCal.get(Calendar.MINUTE) >= 30) {
            topOfHourCal.add(Calendar.HOUR_OF_DAY, 1);
        }
        TimeUtil.minCalendarFields(topOfHourCal, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);

        Date topOfHourMark = topOfHourCal.getTime();
        Date winLowerBound = new Date(topOfHourMark.getTime()
                - (decodeWindow * TimeUtil.MILLIS_PER_MINUTE));
        Date winUpperBound = new Date(topOfHourMark.getTime()
                + (decodeWindow * TimeUtil.MILLIS_PER_MINUTE));

        /*
         * closest record time within the lower half of the filter-window
         * surrounding the top of the hour
         */
        closestTimeMax = dparadarDao.getMaxWindowObstimeForId(radarId,
                winLowerBound, topOfHourMark);
        Calendar maxTime = TimeUtil.newGmtCalendar(closestTimeMax);

        /*
         * closest record time within the upper half of the filter-window
         * surrounding the top of the hour.
         */
        closestTimeMin = dparadarDao.getMinWindowObstimeForId(radarId,
                topOfHourMark, winUpperBound);
        Calendar minTime = TimeUtil.newGmtCalendar(closestTimeMin);

        // Smallest distance is 'closest' to Top of Hour, the record wins a tie
        recordMinsFromTop = minutesFromTop(recordMinute);
        int maxMinutesFromTop = 31, minMinutesFromTop = 31;
        if (null != maxTime) {
            maxMinutesFromTop = minutesFromTop(maxTime.get(Calendar.MINUTE));
        }
        if (null != minTime) {
            minMinutesFromTop = minutesFromTop(minTime.get(Calendar.MINUTE));
        }
        if ((recordMinsFromTop <= maxMinutesFromTop)
                && (recordMinsFromTop <= minMinutesFromTop)) {
            return true;
        }
        return false;
    }

    /**
     * Decode Product Data.
     * 
     * @param productFileBytes
     * @param radarId
     * @param filepath
     *            ingested filepath
     * @param fileName
     * @throws DecodeDPAException
     */
    private void decodeProductData(byte[] productFileBytes, String radarId,
            File file) throws DecodeDPAException {

        WMOHeader wmoHeader = new WMOHeader(productFileBytes);

        ByteBuffer productBytes = ByteBuffer.wrap(productFileBytes);

        short[] dmaHeaderData = new short[DPAConstants.DPA_HEADER_SHORT_COUNT];

        if (wmoHeader.isValid()) {
            productBytes.position(wmoHeader.getMessageDataStart() - 1);
        } else {
            logger.warn(
                    "Record's WMO header missing or invalid.  Searching from start of product file for DPA Marker."
                            + "Found header of: \n" + wmoHeader.toString());
            productBytes.position(0);
        }

        // Advance buffer to DPA Marker
        try {
            boolean foundDpaMarker = false;
            byte lastByte = productBytes.get();
            byte curByte;
            while (!foundDpaMarker) {

                // Match consecutive pattern: '0xFFFF'
                curByte = productBytes.get();
                foundDpaMarker = ((DPAConstants.BEGIN_DPA == curByte)
                        && (DPAConstants.BEGIN_DPA == lastByte));
                lastByte = curByte;
            }
        } catch (BufferUnderflowException e) {
            throw new DecodeDPAException(
                    "EOF encountered before reading DPA header.  Record does not contain '0xFFFF' (-1): "
                            + "bytes marking start of DPA header.");
        }

        // Read DPA Header as next 50 shorts
        try {
            for (int hdrShortIdx = 0; hdrShortIdx < DPAConstants.DPA_HEADER_SHORT_COUNT; hdrShortIdx++) {
                dmaHeaderData[hdrShortIdx] = productBytes.getShort();
            }
        } catch (BufferUnderflowException e) {
            throw new DecodeDPAException(
                    "EOF encountered while reading DPA header data.");
        }

        if (DPAConstants.DPA_MARKER != dmaHeaderData[5]) {
            throw new DecodeDPAException(
                    "Record's header does not indicate a DPA product.");
        }

        int julianDayNumber = dmaHeaderData[39];
        int headerMinutes = dmaHeaderData[40];
        Calendar gregCal = convertJulianDate(julianDayNumber, headerMinutes);
        // Date format for log.
        String dateTimeString = String.format("%1$tm%1$td%1$tY %1$tH:%1$tM",
                gregCal);
        // Date format for db.
        String apTimeString = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:00",
                gregCal);

        float bias = (float) (dmaHeaderData[37] / (float) 100);
        float maxvalh = (float) (dmaHeaderData[36] / (float) 10);
        String maxvalMsg = String.format(" maxvalh=%1$4.1f ", maxvalh);
        String recordMsg = "Processed DPA record: " + radarId + " "
                + dateTimeString + maxvalMsg + " filename: " + file.getPath();
        logger.info(recordMsg);

        if (headerMinutes > TimeUtil.MINUTES_PER_DAY) {
            throw new DecodeDPAException(
                    "End precip time invalid. Number of minutes was over maximum possible in a day."
                            + headerMinutes);
        }

        topHourDecFlag = false;
        tophArchFlag = false;
        /*
         * Decode data portion of product Read product symbology & header of
         * data array packet
         */
        short symHeaderData[] = new short[8];
        short headerData[] = new short[5];

        // Initialize precip array
        short precipData[][] = new short[DPAConstants.NUM_DPA_ROWS][DPAConstants.NUM_DPA_COLS];
        for (int i = 0; i < DPAConstants.NUM_DPA_ROWS; i++) {
            for (int j = 0; j < DPAConstants.NUM_DPA_COLS; j++) {
                precipData[j][i] = LEVEL_OUT_OF_RANGE;
            }
        }
        /*------------------------------------------------------------------*/
        /* datahdr[4] contains the number of rows of data in the product */
        /* normally, this is 131 */
        /* if the value is less than 131, then print message and continue */
        /*------------------------------------------------------------------*/
        if (dmaHeaderData[4] < DPAConstants.NUM_DPA_ROWS) {
            logger.warn("Product has only " + dmaHeaderData[4]
                    + " rows of radar data (less than the "
                    + DPAConstants.NUM_DPA_ROWS + " rows expected).");
        }
        try {
            for (int shortIdx = 0; shortIdx < 8; shortIdx++) {
                symHeaderData[shortIdx] = productBytes.getShort();
            }
            for (int shortIdx = 0; shortIdx < 5; shortIdx++) {
                headerData[shortIdx] = productBytes.getShort();
            }
            // Read run-length encoded 'precip' data
            short numBytes = 0;
            for (int i = 0; i < headerData[4]; i++) {
                numBytes = productBytes.getShort();
                if (-1 == numBytes) {
                    int j = (i - 1);
                    logger.warn(
                            "Incomplete DPA product, precip data complete through row "
                                    + j);
                    break;
                } else if ((numBytes < 2) || (numBytes > 32000)) {
                    throw new DecodeDPAException(
                            "Data corruption encountered while reading run-length encoded precip data.");
                }
                // Get block of precip data
                byte[] dataBlock = new byte[numBytes];
                productBytes.get(dataBlock, DPAConstants.START_OF_DATA_BLOCK,
                        numBytes);

                short numRuns = (short) ((float) numBytes / ((float) 2));
                if (numRuns > DPAConstants.NUM_DPA_COLS || numRuns < 1) { // [1-131]
                                                                          // expected
                    throw new DecodeDPAException(
                            "Value of numRuns < 1, or numRuns > 131 in row: "
                                    + i + ".");
                }
                // Fill in precipData[][]
                int k = 0;
                for (int j = 0; j < numRuns; j++) {

                    short xRun = (short) (dataBlock[(j * 2)] & 0xFF);
                    if (xRun > DPAConstants.NUM_DPA_COLS) {
                        throw new DecodeDPAException(
                                "Value of xRun cannot be > 131, was: " + xRun
                                        + " in row: " + i + ", set: " + j
                                        + ", can't decode.");
                    } else if (xRun < 1) {
                        throw new DecodeDPAException("j: " + j
                                + ", Value of xRun cannot be < 1, was: " + xRun
                                + " in row: " + i + ", set: " + j
                                + ", can't decode.");
                    }

                    short level = (short) (dataBlock[(1 + (j * 2))] & 0xFF);

                    for (int n = 0; n < xRun; n++) {
                        precipData[((DPAConstants.NUM_DPA_COLS - 1)
                                - i)][k] = level;
                        k++;
                    }
                }
            }
        } catch (BufferUnderflowException | IndexOutOfBoundsException e) {
            throw new DecodeDPAException(
                    "Error while decoding data portion of product. ", e);
        }

        String productFileAsStr = new String(productFileBytes);
        validateExpectedBuildNum(productFileAsStr);

        // Logs record data if writeToDB not enabled, or mode is parallel
        AdaptableParameters recordAP = new AdaptableParameters(productBytes,
                productDataSeq);

        recordAP.writeToDbAdaptableParameters(apTimeString, radarId,
                dpaAdaptDao, writeToDB);

        // Operational weather mode from header
        int operationalWeatherMode = (int) dmaHeaderData[6];
        SupplementalParameters sParms = null;
        try {
            sParms = new SupplementalParameters(productBytes, productDataSeq);
        } catch (DecodeDPAException e) {
            throw new DecodeDPAException(
                    "Could not find supplemental data header. ", e);
        }

        // Prepare decoded precipitation data
        float maxValD = (float) -99;
        if (0 == sParms.getPcipflg()) {
            maxValD = 0;
        }
        Calendar calPrGen = convertJulianDate(dmaHeaderData[13], headerMinutes);
        String prGenDateStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:00",
                calPrGen);
        String stage1Filename = String
                .format(radarId + "%1$tm%1$td%1$tY%1$tH%1$tMZ", calPrGen);
        String stage1Filepath = DPA_GRID_PATH + File.separatorChar
                + stage1Filename;
        // Test if record fits into archive window
        boolean isInArchWin = calPrGen.get(Calendar.MINUTE) <= archWindow;
        if (archon && isInArchWin) {
            tophArchFlag = true;
        }

        /*
         * Copy raw DPA file to archive directory && recordMinsFromTop <=
         * archWindow
         */
        if (tophArchFlag) {
            String stage1ArchFilepath = DPA_ARCHIVE_PATH + File.separatorChar
                    + stage1Filename;

            boolean success = saveToArchFile.writeToArchFile(stage1ArchFilepath,
                    productBytes);
            if (!success) {
                logger.warn("Archive file {} has not been saved",
                        stage1ArchFilepath);
            }
        }

        int recordMinutesFromTop = minutesFromTop(
                calPrGen.get(Calendar.MINUTE));

        // Save decoded files if flag is 4 - file indicates precipitation.
        if ((sParms.getPcipflg() == 4)) {
            if (decon) {
                topHourDecFlag = isClosestToTopOfHr(radarId, apTimeString,
                        calPrGen.get(Calendar.MINUTE));
                if (recordMinutesFromTop > decodeWindow) {
                    logger.info("Product had distance from TopOfHour value: "
                            + recordMinutesFromTop + " above max value: "
                            + decodeWindow
                            + " allowed by AppsDefaults token: 'dpa_filter_decode'");
                    return;
                }

                if (!topHourDecFlag) {
                    logger.info("Product is not closest to top-of-hour.");
                    return;
                }

                // Copy Little Endian version of product data
                String filenameLE = file.getName() + ".LE";
                final Path fileLEPath = DPA_GATHER_PARALLEL_PATH
                        .resolve(filenameLE);
                /*
                 * TODO: does this actually produce LE file contents equivalent
                 * to the LE file contents written by the legacy applications?
                 */
                ByteBuffer littleEndianBytes = ByteBuffer
                        .wrap(productFileBytes);
                littleEndianBytes.order(ByteOrder.LITTLE_ENDIAN);
                final OpenOption optionOption = (Files.exists(fileLEPath))
                        ? StandardOpenOption.APPEND : StandardOpenOption.CREATE;
                try {
                    Files.write(fileLEPath, littleEndianBytes.array(),
                            optionOption);
                    logger.info("Successfully wrote LE data file: {}.",
                            fileLEPath.toString());
                } catch (Exception e) {
                    logger.error("Failed to write LE data file: "
                            + fileLEPath.toString() + ".", e);
                }
                writeStage1Decoded(precipData, stage1Filepath,
                        dmaHeaderData[20], dmaHeaderData[21],
                        LEVEL_OUT_OF_RANGE, maxValD);
            }
        } else {
            if (0 == sParms.getPcipflg()) {
                logger.warn("No decoded file written (no precip detected).");
            }
            if (1 == sParms.getPcipflg()) {
                logger.warn("No decoded file written (bad rate scan).");
            }
            if (2 == sParms.getPcipflg()) {
                logger.warn("No decoded file written (not enough data).");
            }
            if (3 == sParms.getPcipflg()) {
                logger.warn("No decoded file written (disk error at radar).");
            }
        }
        // Logs record data if writeToDB not enabled, or mode is parallel

        sParms.writeToDbSupplementalParameters(radarId,
                (short) recordMinutesFromTop, maxvalh, maxValD, bias,
                prGenDateStr, operationalWeatherMode, radarlocDao, dparadarDao,
                writeToDB, stage1Filename);

    }

    /**
     * Throws exception with message if product is not Build 8 or compatible. To
     * distinguish between Build 8 and the other formats, the string "ADAP(32"
     * is searched for. If the string is found, then the product is Build 8 and
     * the routine returns.
     * 
     * The Build 8 product has 32 adaptable parameters.
     * 
     * @param productFileAsStr
     *            String created from bytes of DPA product file.
     */
    private void validateExpectedBuildNum(String productFileAsStr)
            throws DecodeDPAException {

        Matcher build8Matcher = ADAP_HEADER_PATTERN.matcher(productFileAsStr);
        if (build8Matcher.find()) {
            String buildNumDigits = build8Matcher.group(1);
            if ("32".equals(buildNumDigits)) {
                return;
            } else {
                throw new DecodeDPAException(
                        "Error, Build 8 or compatible expected with 32 Adaptable Parameters.  Found "
                                + buildNumDigits + " parameters.");
            }
        }
    }

    /**
     * Return Gregorian Calendar of date. Add Julian Day Number (JDN) to Epoch
     * of Gregorian calendar. As epoch begins on day 1, add one less than number
     * of Julian days from header.
     * 
     * @param julianDayNum
     * @param headerMinutes
     * @return Gregorian Calendar instance.
     */
    private Calendar convertJulianDate(int julianDayNum, int headerMinutes) {
        Calendar gregCal = TimeUtil.newEpochCalendar();
        gregCal.add(Calendar.DAY_OF_YEAR, (julianDayNum - 1));
        gregCal.add(Calendar.MINUTE, headerMinutes);
        return gregCal;
    }

    /**
     * This subroutine writes the array containing the decoded precip values to
     * a file. This is done if the maximum precip value is > 0.0 else no file is
     * written.
     * 
     * @param idat
     * @param decodedFilePath
     * @param min
     * @param incr
     * @param levelOut
     * @param xmax
     */
    private void writeStage1Decoded(short[][] idat, String decodedFilePath,
            short min, short incr, short levelOut, double xmax) {
        /**
         * Writes the array containing the decoded precip values to a file. This
         * is done if the maximum precip value is > 0.0 else no file is written.
         * 
         * Units of the precip values in the file = dBA.
         * 
         * IDAT() = data level from 0 - (numlevel - 1) IDAT() = 0 -- precip =
         * 0.0 precip value outside of radar = -99. precip value equal to 0.0 =
         * -98.
         */
        int i, j;
        float[][] precip = new float[DPAConstants.NUM_DPA_ROWS][DPAConstants.NUM_DPA_COLS];
        float xmin, xinc;

        xmin = min / (float) 10;
        xinc = incr / (float) 1000;
        xmax = DPAConstants.MISSING_VALUE;

        for (i = 0; i < DPAConstants.NUM_DPA_ROWS; i++) {
            for (j = 0; j < DPAConstants.NUM_DPA_COLS; j++) {
                if (levelOut == idat[i][j]) {
                    precip[i][j] = DPAConstants.PRECIP_OUTSIDE_RADAR;
                } else if (0 == idat[i][j]) {
                    precip[i][j] = DPAConstants.PRECIP_ZERO;
                } else {
                    precip[i][j] = xmin + (idat[i][j] - 1) * xinc;
                }

                if (precip[i][j] > xmax) {
                    xmax = (double) precip[i][j];
                }
            }
        }

        // Change xmax to 'mm'
        if ((xmax == DPAConstants.MISSING_VALUE)
                || (xmax == DPAConstants.PRECIP_ZERO)) {
            xmax = 0;
        } else {
            xmax = Math.pow(10, (xmax / (double) 10));
        }

        if (xmax > 0.0) {
            // Output in LITTLE ENDIAN
            ByteBuffer buf = ByteBuffer.allocate(
                    DPAConstants.NUM_DPA_ROWS * DPAConstants.NUM_DPA_COLS * 4);
            buf.order(ByteOrder.LITTLE_ENDIAN);
            for (i = 0; i < DPAConstants.NUM_DPA_ROWS; ++i) {
                for (j = 0; j < DPAConstants.NUM_DPA_COLS; ++j) {
                    buf.putFloat(precip[i][j]);
                }
            }
            buf.flip();
            try (FileChannel out = new FileOutputStream(decodedFilePath, false)
                    .getChannel();) {
                out.write(buf);
            } catch (Exception e) {
                logger.warn("Error: writing stage-1 decoded file: "
                        + decodedFilePath, e);
            }
        }
    }

    /**
     * Compute absolute value of the number of minutes between the reference and
     * the Top of the Hour (:00) position.
     */
    private int minutesFromTop(int referenceMinute) {

        referenceMinute = Math.abs(referenceMinute);
        if (referenceMinute > 30) {
            return (TimeUtil.MINUTES_PER_HOUR - referenceMinute);
        } else {
            return (referenceMinute);
        }
    }

    /**
     * Reads configuration parameter shouldUseDB from localization.
     * 
     * @throws DecodeDPAException
     *             if unable to find expected localization file.
     */
    private boolean getLocalizedXMLParameter() throws DecodeDPAException {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile localizationFile = pathManager
                .getStaticLocalizationFile(CONFIG_FILE);

        if (null == localizationFile) {
            throw new DecodeDPAException(
                    "Unable to find required localization file from PathManager: "
                            + CONFIG_FILE + ".");
        }

        DpaXmlConfig defaultConfigValues = null;
        if ((localizationFile != null) && localizationFile.exists()) {

            try (InputStream is = localizationFile.openInputStream()) {
                if (jaxbManager == null) {
                    jaxbManager = new SingleTypeJAXBManager<>(
                            DpaXmlConfig.class);
                }
                logger.debug(
                        "Configuring localized Decode_DPA parameters. Opening localization file: {} ...",
                        CONFIG_FILE);
                defaultConfigValues = jaxbManager.unmarshalFromInputStream(is);
            } catch (Exception e) {
                writeToDB = false;
                logger.warn(
                        "Failed to to load configuration from localization file: "
                                + CONFIG_FILE
                                + ". Defaulting value of writeToDB to false.");
            }
            if (null == defaultConfigValues) {
                writeToDB = false;
                logger.warn(
                        "Failed to load configuration from localization file: "
                                + CONFIG_FILE
                                + ". Defaulting value of writeToDB to false.");
            }
        }
        logger.debug("Successfully read localization file: {}  =>  {}",
                CONFIG_FILE, defaultConfigValues.getWriteToDB());
        return (defaultConfigValues.getWriteToDB());
    }
}