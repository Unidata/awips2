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
package com.raytheon.edex.plugin.shef.alarms;

import java.io.File;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.dataplugin.text.request.WriteProductRequest;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Provides SHEF with the ability to generate alert/alarms report products and
 * write them to the text database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

public class AlertalarmStdTextProductUtil {

    private static Log log = LogFactory
            .getLog(AlertalarmStdTextProductUtil.class);

    private static final Options cliOptions = createCmdLineOptions();

    private static ReportWriter reportWriter = null;

    private static ReportOptions reportOptions = null;

    private static File currReport = null;

    private static String reportDir = null;

    /**
     * this application main entry point
     * 
     * @param args
     */
    public static void main(String[] args) {

        loadEnvironment();

        setApplicationSpringContext(Constants.IHFS_CONFIG);

        setReportOptions(args);

        setCurrentReportfile();

        reportAlarm();
    }

    /*
     * create the org.apache.commons.cli.Options object
     */
    private static Options createCmdLineOptions() {
        CmdlineOptions options = new CmdlineOptions();
        options.addMandatoryOption(CmdlineOptionId.DB_NAME);
        options.addMandatoryOption(CmdlineOptionId.PRODUCT_ID);
        options.addOption(CmdlineOptionId.REPORT_MODE);
        options.addOption(CmdlineOptionId.MINUTES);
        options.addOption(CmdlineOptionId.FILE_SUFFIX);
        options.addOption(CmdlineOptionId.FLAGS);
        options.addOption(CmdlineOptionId.PE);
        return options;
    }

    /*
     * initialize the ReportOptions object based on command line arguments
     */
    private static void setReportOptions(String[] args) {
        CommandLine line = null;
        ReportOptions rptOptions = new ReportOptions();

        // Check CmdLine arguments
        if (args.length < 3) {
            System.out.println("Invalid or missing arguments.");
            printUsage();
            System.exit(0);
        }

        // Process CmdLine Arguments
        GnuParser parser = new GnuParser();
        try {
            line = parser.parse(cliOptions, args);
        } catch (ParseException exp) {
            System.err.println("Command line Parsing failed.  Reason: "
                    + exp.getMessage());
            printUsage();
            System.exit(0);
        }

        // duplicate options
        if (hasDuplicateOptions(line)) {
            System.err.println("Error : Duplicate command line options.");
            printUsage();
            System.exit(0);
        }

        // All user specified options, including mandatory ones
        // are recognized , and there are no duplicate options.
        // Query the CmdLine and build the report options object
        for (CmdlineOptionId optId : CmdlineOptionId.values()) {
            String optName = optId.toString();
            System.out.println("optName = " + optName);
            System.out.println("  optValue = " + line.getOptionValue(optName));

            if (line.hasOption(optName)) {

                CmdlineOption option = new CmdlineOption(optId,
                        line.getOptionValue(optName));

                try {
                    rptOptions.addOption(option);
                } catch (IllegalArgumentException e) {
                    System.err.println(e.getMessage());
                    System.exit(0);
                }
            }
        }

        // The report options are all valid
        Set<ReportMode> ignoreMin = new HashSet<ReportMode>();
        ignoreMin.add(ReportMode.ALL);
        ignoreMin.add(ReportMode.UNREPORTED);
        ignoreMin.add(ReportMode.NEAREST);
        ignoreMin.add(ReportMode.LATEST_MAXFCST);
        if (rptOptions.isMinutesGiven()
                && ignoreMin.contains(rptOptions.getMode()))
            System.out
                    .println("Usage : -m<minutes> value ignored for this -r<report_mode>");

        reportOptions = rptOptions;

    }

    /*
     * check whether user has entered duplicate options on the command line
     */
    private static boolean hasDuplicateOptions(CommandLine line) {
        Option[] userOptions = line.getOptions();
        Set<String> unique = new HashSet<String>();
        for (Option option : userOptions)
            if (!unique.add(option.getOpt()))
                return true;
        return false;
    }

    /*
     * Display a usage message
     */
    private static void printUsage() {
        System.out.print("Usage: report_alarm -d<dbase> -p<product_id> ");
        System.out.print("[-r<report_mode>] [-m<minutes>] [-s<file_suffix>] ");
        System.out.println("[-f<include_flags>] [-e<PE>]");
    }

    /*
     * process alertalarmval data. if there data meets alert/alarm conditions,
     * report them in a text file and send it to the text database.
     */
    private static void reportAlarm() {
        Date now = new Date(System.currentTimeMillis());

        // Get the whole data for the report
        AlertalarmRecord aaRecord = RecordMgr.getAlarmData(reportOptions);

        // write the report
        reportWriter = new ReportWriter(currReport, reportOptions, now);
        reportWriter.writeHeader();
        reportWriter.writeBody(aaRecord);
        reportWriter.writeTrailer();

        // save it to the text database if indeed
        // there are alarm events to report.
        int alarmCount = reportWriter.getAlarmCount();
        if (alarmCount > 0) {
            log.info(alarmCount + " alarms reported, report written to "
                    + reportWriter.getFilename());
            saveReportTotextDb();
        } else {
            log.info("No alarms reported, info sent to "
                    + reportWriter.getFilename());
            log.info("File NOT sent to text database.");
        }

    }

    /*
     * saves the report contents to the text database.
     */
    private static void saveReportTotextDb() {

        setApplicationSpringContext(Constants.FXA_CONFIG);

        WriteProductRequest request = new WriteProductRequest();
        request.setProductId(reportOptions.getProductId());
        request.setOperationalMode(true);
        request.setNotifyAlarmAlert(true);
        String rptData = reportWriter.getReportData();
        request.setReportData(rptData);

        log.info("Sending " + reportWriter.getFilename() + " to textdb as id "
                + request.getProductId());

        TextDB textdb = new TextDB();
        long result = textdb.writeProduct(request.getProductId(),
                request.getReportData(), request.getOperationalMode(), null);

        if (result != Long.MIN_VALUE) {
            log.info("Product " + request.getProductId()
                    + " successfully sent to textdb");
        } else {
            log.error("Error sending product " + request.getProductId()
                    + " to textdb.  status=" + result);
        }

    }

    /*
     * verify whether necessary application tokens have been defined.
     */
    private static void loadEnvironment() {

        System.setProperty("edex.home", System.getenv("EDEX_HOME"));
        System.setProperty("db.addr", System.getenv("PGHOST"));
        System.setProperty("ih.db.name", System.getenv("DB_NAME"));
        System.setProperty("fxa.db.name", System.getenv("FXA_DB_NAME"));
        System.setProperty("db.port", System.getenv("PGPORT"));

        AppsDefaults appDefaults = AppsDefaults.getInstance();

        String aalogDir = appDefaults.getToken(Constants.WHFS_UTIL_LOG_DIR);

        if (aalogDir == null) {
            System.out
                    .println("whfs_util_log_dir directory undefined.  Aborting.");
        }

        reportDir = appDefaults.getToken(Constants.WHFS_PRODUCT_DIR);

        if (reportDir == null) {
            System.out
                    .println("whfs_product_dir directory undefined .  Aborting.");
            System.exit(0);
        }
    }

    /*
     * initializes a File object that points to the current alert/alarm report
     */
    private static void setCurrentReportfile() {
        String pid = reportOptions.getProductId();
        String suffix = reportOptions.getFileSuffix();

        if (suffix.length() > 0)
            currReport = new File(reportDir + File.separator + pid + "." + suffix);
        else
            currReport = new File(reportDir + File.separator + pid);

        try {
            currReport.createNewFile();
        } catch (Exception e) {
          log.fatal("Could not create report file "
                    + String.valueOf(currReport));
          log.info("Exiting.");
          System.exit(0);
        }
    }

    private static void setApplicationSpringContext(String... configLocations) {
        if (configLocations.length > 0) {
            ClassPathXmlApplicationContext ctxt = new ClassPathXmlApplicationContext(
                    configLocations);
            EDEXUtil edexUtil = new EDEXUtil();
            edexUtil.setApplicationContext(ctxt);
        } else {
            log.fatal("Application spring config location not specified");
            log.info("Exiting.");
            System.exit(0);
        }
    }

}