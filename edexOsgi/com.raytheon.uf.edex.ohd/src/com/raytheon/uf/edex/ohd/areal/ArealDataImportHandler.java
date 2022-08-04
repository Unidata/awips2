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
package com.raytheon.uf.edex.ohd.areal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.areal.ArealDataImportNotification;
import com.raytheon.uf.common.hydro.areal.ArealDataImportNotification.IMPORTSTATUS;
import com.raytheon.uf.common.hydro.areal.ArealDataImportRequest;
import com.raytheon.uf.common.hydro.areal.ArealDataImportResponse;
import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Request handler for the Areal Data Import functionality in Hydro.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2018  6979      mpduff      Initial creation
 * Mar 11, 2020 19533   mgamazaychikov Added code to handle request to import shapefile
 *
 * </pre>
 *
 * @author mpduff
 */

public class ArealDataImportHandler
        implements IRequestHandler<ArealDataImportRequest> {

    public static String IMPORT_NOTIFICATION_URI = "jms-generic:topic:edex.areal.data.import?timeToLive=60000";

    private static final String NEWLINE = System.getProperty("line.separator");

    private static final String AREAL_IMPORT_SHP = "SHP";

    private static final String PLUGIN = "com.raytheon.uf.edex.ohd";

    public static final String SOURCE = "EDEX";

    private static final String CATEGORY = "DEFAULT";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArealDataImportHandler.class);

    @Override
    public ArealDataImportResponse handleRequest(ArealDataImportRequest request)
            throws Exception {
        String fileName = request.getArealDataFileName();
        ArealTypeSelection type = request.getArealDataType();
        String arealImportType = request.getArealImportType();
        String responseMessage = "Received GeoArea Import Request for " + fileName;
        EDEXUtil.sendMessageAlertViz(Priority.INFO, PLUGIN, SOURCE, CATEGORY, responseMessage, arealImportType, null);
        Runnable runner = new ArealDataImportRunner(fileName, arealImportType, type);
        Thread thread = new Thread(runner);
        thread.start();
        ArealDataImportResponse response = new ArealDataImportResponse();
        response.setMessage(responseMessage);
        return response;
    }

    private class ArealDataImportRunner implements Runnable {

        private StringBuilder logger = new StringBuilder();
        ArealTypeSelection type;
        String fileName;
        String arealImportType;

        private ArealDataImportRunner(String fileName, String arealImportType, ArealTypeSelection type) {
            this.type = type;
            this.fileName = fileName;
            this.arealImportType = arealImportType;
        }

        @Override
        public void run() {
            SimpleDateFormat sdf = new SimpleDateFormat(CommonHydroConstants.IHFS_DATE_FORMAT);
            Date now = TimeUtil.newGmtCalendar().getTime();
            log("Starting import of " + type.getDataName() + " on " + sdf.format(now));
            boolean importSuccess = false;
            IMPORTSTATUS status;
            if (!arealImportType.equals(AREAL_IMPORT_SHP)) {
                importSuccess = importASCIIGeoArea();
            } else {
                importSuccess = importSHPGeoArea();
            }
            if (importSuccess) {
                status = IMPORTSTATUS.SUCCESS;
            } else {
                status = IMPORTSTATUS.FAILURE;
            }
            ArealDataImportNotification notification = new ArealDataImportNotification();
            notification.setType(type);
            notification.setStatus(status);
            String message = "Completed GeoArea Import for " + fileName + " status = " + status.toString();
            EDEXUtil.sendMessageAlertViz(Priority.INFO, PLUGIN, SOURCE, CATEGORY, message, arealImportType, null);
            try {
                EDEXUtil.getMessageProducer().sendAsyncUri(IMPORT_NOTIFICATION_URI,
                        SerializationUtil.transformToThrift(notification));
            } catch (EdexException e) {
                statusHandler.error("Error sending Areal Data Import Notification", e);
            } catch (SerializationException e) {
                statusHandler.error("Error transforming notification to thrift", e);
            }
            log(message);
            writeLogFile(type);
            logger.setLength(0);
        }

        private boolean importSHPGeoArea() {
            Date now = TimeUtil.newGmtCalendar().getTime();
            long start = now.getTime();
            log("Loading " + type.getDataName() + " from file " + fileName);
            GeoDataReader reader = new GeoDataReader();
            String logMsg = reader.importGeoSHPArea(fileName, type);
            if (logMsg.contains("ERROR")) {
                log(logMsg);
                return false;
            }
            List<GeoAreaData> geoDataList = new ArrayList<>();
            log(logMsg);
            geoDataList = reader.getGeoDataList();
            now = TimeUtil.newGmtCalendar().getTime();
            log("Reading file in took " + ((now.getTime() - start)) + "ms");

            try {
                now = TimeUtil.newGmtCalendar().getTime();
                start = now.getTime();
                ArealDataAccessor dman = new ArealDataAccessor();
                String msg = dman.updateArealData(geoDataList, type);
                log(msg);
                System.out.println();
                now = TimeUtil.newGmtCalendar().getTime();
                log("Processing file took " + ((now.getTime() - start)) + "ms");
            } catch (Exception e) {
                statusHandler.error("Error storing areal data.", e);
                log("Error storing areal data. " + e.getMessage());
                return false;
            }
            return true;
        }

        /**
         * Import the data from the file.
         *
         * <pre>
         * &lt;id&gt; &lt;name&gt; &lt;feature rank&gt; &lt;numpoints&gt; [center lat] [center lon]
         * &lt;lat&gt; &lt;lon&gt;
         * &lt;lat&gt; &lt;lon&gt;
         * &lt;lat&gt; &lt;lon&gt;
         * &lt;lat&gt; &lt;lon&gt;
         * &lt;lat&gt; &lt;lon&gt;
         * ... ...
         * &lt;lat&gt; &lt;lon&gt;
         *
         * where id is the 1-8 character id of the geoarea or geoline
         * name is the name of the geoarea or geoline. It may be up to
         *      20 characters long for a geoline and up to 40 characters
         *      long for a geoarea.
         * feature rank is the order of the geoarea or geoline. This allows
         *      geographic features to be displayed according to relative
         *      importance. Lower numbers take precedence over higher numbers.
         * numpoints is the number of latitude/longitude pairs defining the
         *      geoarea or geoline.
         * center lat is the centroid latitude.  This applies only to geoarea polygons.
         * center lon is the centroid longitude. This applies only to geoarea polygons.
         * </pre>
         * 
         * @return
         */
        private boolean importASCIIGeoArea() {
            Date now = TimeUtil.newGmtCalendar().getTime();
            long start = now.getTime();
            log("Loading " + type.getDataName() + " from file " + fileName);
            List<String> fileContents = null;

            // Open the file for reading
            try (BufferedReader br = Files.newBufferedReader(Paths.get(fileName))) {
                // br returns as stream and converts it into a List
                fileContents = br.lines().collect(Collectors.toList());
            } catch (IOException e) {
                statusHandler.error("Error reading file:" + fileName, e);
                log("Error reading file: " + fileName);
                return false;
            }

            List<GeoAreaData> geoDataList = new ArrayList<>();
            GeoDataReader reader = new GeoDataReader();
            String logMsg = reader.importGeoArea(fileContents, type);
            if (logMsg.contains("ERROR")) {
                return false;
            }
            log(logMsg);
            geoDataList = reader.getGeoDataList();
            now = TimeUtil.newGmtCalendar().getTime();
            log("Reading file in took " + ((now.getTime() - start)) + "ms");

            try {
                now = TimeUtil.newGmtCalendar().getTime();
                start = now.getTime();
                ArealDataAccessor dman = new ArealDataAccessor();
                String msg = dman.updateArealData(geoDataList, type);
                log(msg);
                now = TimeUtil.newGmtCalendar().getTime();
                log("Processing file took " + ((now.getTime() - start)) + "ms");
            } catch (Exception e) {
                statusHandler.error("Error storing areal data.", e);
                log("Error storing areal data. " + e.getMessage());
                return false;
            }
            return true;
        }

        /**
         * Send a message to the log file
         *
         * @param stmt
         *            A message to be logged
         */
        private void log(String stmt) {
            logger.append(stmt).append(NEWLINE);
        }

        /**
         * Get the log filename
         *
         * @return The File object associated with the log file
         */
        private File getLogFilename(ArealTypeSelection type) {
            /* build the file name */
            SimpleDateFormat sdLog = new SimpleDateFormat("yyyyMMdd");
            Date now = TimeUtil.newGmtCalendar().getTime();
            String ext = "_" + sdLog.format(now);
            String logfilename = null;
            String dir = AppsDefaults.getInstance().getToken("whfs_util_log_dir");
            if (dir.length() <= 0) {
                logfilename = "whfs_util_log_dir undefined.";
            } else {
                logfilename = dir + File.separator + CommonHydroConstants.IMPORT_LOGS[type.ordinal()] + ext;
            }

            File f = new File(logfilename);

            return f;
        }

        /**
         * Open the log file
         * 
         * @param string
         * @param type
         */
        private void writeLogFile(ArealTypeSelection type) {
            File logFile = getLogFilename(type);

            FileWriter fw = null;
            if (logFile.exists()) {
                try {
                    fw = new FileWriter(logFile, true);
                } catch (IOException e) {
                    statusHandler.error("Error opening log file: " + logFile, e);
                    return;
                }
            } else {
                try {
                    fw = new FileWriter(logFile);
                } catch (IOException e) {
                    statusHandler.error("Error opening log file: " + logFile, e);
                    return;
                }
            }
            try (BufferedWriter bw = new BufferedWriter(fw)) {
                bw.write(logger.toString());
            } catch (IOException e) {
                statusHandler.error("Error opening log file: " + logFile, e);
                return;
            }
        }

    }
}
