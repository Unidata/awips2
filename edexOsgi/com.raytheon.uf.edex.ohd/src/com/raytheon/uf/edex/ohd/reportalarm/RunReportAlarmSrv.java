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
package com.raytheon.uf.edex.ohd.reportalarm;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Provides SHEF with the ability to generate alert/alarms report products and
 * write them to the text database.
 * <p>
 * Based on AlertalarmStdTextProductUtil.java originally written by jnjanga.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2014  #2783     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class RunReportAlarmSrv {

    private static final DateFormat START_END_TIME_FORMAT = new SimpleDateFormat(
            "EEE MMM dd HH:mm:ss z yyyy");

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RunReportAlarmSrv.class);

    private RunReportAlarmSrv() {
        // no-op
    }

    public static void executeRunReportAlarm() {
        statusHandler.info("------------------------------ ");
        String currentTime = START_END_TIME_FORMAT.format(TimeUtil.newDate());
        statusHandler.info("Invoking report_alarm at " + currentTime);

        try {
            ReportOptions options = null;
            try {
                options = loadConfiguration();
            } catch (IllegalArgumentException e) {
                statusHandler
                        .error("Invalid configuration value specified.", e);
                return;
            }

            ReportWriter reportWriter;
            try {
                AlertalarmRecord aaRecord = RecordMgr.getAlarmData(options);
                reportWriter = new ReportWriter(options, TimeUtil.newDate());
                reportWriter.generateReport(aaRecord);
            } catch (Exception e) {
                statusHandler.error("Could not generate Alertalarm report.", e);
                return;
            }

            File outputFile;
            try {
                outputFile = createOutputFile(options.getFileName());
                reportWriter.writeToDisk(outputFile);
            } catch (IOException e) {
                statusHandler.error("Could not write output file.", e);
                return;
            }
            String fileName = outputFile.getPath();

            int alarmCount = reportWriter.getAlarmCount();
            if (alarmCount == 0) {
                statusHandler.info("No alarms reported, info sent to "
                        + fileName);
                statusHandler.info("File NOT sent to text database.");
            } else {
                statusHandler.info(alarmCount
                        + " alarms reported, report written " + fileName);
                statusHandler.info("Writing " + fileName + " to textdb as id "
                        + options.getProductId());

                try {
                    saveProductToTextDb(reportWriter.getReportData(),
                            options.getProductId());
                } catch (Exception e) {
                    statusHandler.error("Could not write product to textdb", e);
                }
            }
        } finally {
            currentTime = START_END_TIME_FORMAT.format(TimeUtil.newDate());
            statusHandler.info("Completed report_alarm at " + currentTime);
        }
    }

    private static ReportOptions loadConfiguration() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        ReportOptions userOptions = new ReportOptions(appsDefaults);
        return userOptions;
    }

    private static File createOutputFile(final String fileName)
            throws IOException {
        AppsDefaults appDefaults = AppsDefaults.getInstance();
        String basePath = appDefaults.getToken(Constants.WHFS_PRODUCT_DIR);
        if (basePath == null) {
            throw new IllegalArgumentException(
                    "whfs_product_dir directory undefined");
        }

        String fullPath = FileUtil.join(basePath, fileName);
        File outFile = new File(fullPath);
        outFile.createNewFile();

        return outFile;
    }

    private static void saveProductToTextDb(final String productText,
            final String productId) throws Exception {
        TextDB textdb = new TextDB();
        long statusCode = textdb.writeProduct(productId, productText, true,
                null);

        if (statusCode != Long.MIN_VALUE) {
            statusHandler.info("Product successfully sent");
        } else {
            statusHandler.error("Product send error detected.");
        }
    }
}
