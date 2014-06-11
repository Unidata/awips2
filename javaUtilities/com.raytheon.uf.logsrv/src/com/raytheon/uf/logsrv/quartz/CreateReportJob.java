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
package com.raytheon.uf.logsrv.quartz;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

import com.raytheon.uf.logsrv.LogService;
import com.raytheon.uf.logsrv.LogServiceException;
import com.raytheon.uf.logsrv.config.LogSrvConfig;
import com.raytheon.uf.logsrv.derby.DerbyDao;
import com.raytheon.uf.logsrv.report.data.LogReportContainer;
import com.raytheon.uf.logsrv.report.email.HtmlGenerator;
import com.raytheon.uf.logsrv.report.email.ReportEmailer;

/**
 * A quartz job that queries the database to build report data, transforms the
 * report data into HTML, emails and/or saves the HTML, and then purges the
 * database of all logging events that were included in the report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2013            njensen     Initial creation
 * Jun 11, 2014 2840       njensen     Added save to dir
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CreateReportJob implements Job {

    private static final SimpleDateFormat SDF = new SimpleDateFormat(
            "yyyy-MM-dd");

    /*
     * (non-Javadoc)
     * 
     * @see org.quartz.Job#execute(org.quartz.JobExecutionContext)
     */
    @Override
    public void execute(JobExecutionContext ctx) throws JobExecutionException {
        LogService.getLogger().info(
                "Create report job triggered, preparing report");
        DerbyDao dao = DerbyDao.getInstance();
        // synchronize on dao to prevent new log entries from being added
        // while we're querying and then purging
        synchronized (dao) {
            LogReportContainer container = null;
            try {
                container = dao.buildReport();
            } catch (LogServiceException e) {
                LogService.getLogger().error("Error building report", e);
                throw new JobExecutionException("Error building report", e);
            }

            String html = HtmlGenerator.generateHtml(container);
            LogSrvConfig config = (LogSrvConfig) ctx.getJobDetail()
                    .getJobDataMap().get("config");

            boolean emailed = sendEmail(html, config);
            boolean saved = saveReport(html, config);
            boolean processed = emailed || saved;

            /*
             * Only clear out the database if we at least sent or saved the
             * report.
             * 
             * TODO If the service keeps erroring off on the report generation,
             * then in theory the databaseDir could grow out of control forever
             * until out of disk space. Should somehow set a configurable upper
             * limit where the derby db cannot go past a certain number of log
             * messages.
             */
            if (processed) {
                LogService.getLogger().info(
                        "Purging database of messages that were just reported");
                try {
                    dao.clearEntries();
                } catch (LogServiceException e) {
                    LogService.getLogger().error("Error purging database", e);
                    throw new JobExecutionException("Error purging database", e);
                }
                LogService.getLogger().info("Database purging complete");
            } else {
                LogService
                        .getLogger()
                        .warn("Did not save or send report, therefore skipping purge of database");
            }
        }
    }

    /**
     * Checks if the service is configured to send email, and if so sends the
     * report as configured
     * 
     * @param report
     * @param config
     * @return true if it sent, otherwise false
     */
    protected boolean sendEmail(String report, LogSrvConfig config) {
        boolean sent = false;
        if (shouldEmail(config)) {
            try {
                ReportEmailer.email(report, config);
                LogService.getLogger().info(
                        "Report has been sent to: " + config.getToAddress());
                sent = true;
            } catch (Exception e) {
                LogService.getLogger().error("Error emailing report", e);
            }
        } else {
            LogService
                    .getLogger()
                    .info("Skipping email of report"
                            + ", to enable emailing ensure the log service's config has a fromAddress, toAddress, and smtpHost");
        }

        return sent;
    }

    /**
     * Checks if the configuration indicates that the report should be emailed
     * 
     * @param cfg
     * @return
     */
    protected boolean shouldEmail(LogSrvConfig cfg) {
        return (cfg.getFromAddress() != null) && (cfg.getToAddress() != null)
                && (cfg.getSmtpHost() != null) && (cfg.getSmtpPort() > 0);
    }

    /**
     * Checks if the service is configured to save the reports, and if so, saves
     * the report
     * 
     * @param report
     * @param config
     * @return true if it saved, otherwise false
     */
    protected boolean saveReport(String report, LogSrvConfig config) {
        boolean saved = false;
        if (shouldSaveReport(config)) {
            File outputDir = new File(config.getOutputDir());
            if (!outputDir.exists()) {
                if (!outputDir.mkdirs()) {
                    LogService.getLogger()
                            .error("Error creating outputDir "
                                    + config.getOutputDir());
                    return false;
                }
            }

            String filename = config.getClusterName() + "_"
                    + SDF.format(new Date()) + ".html";
            String filePath = outputDir.getPath() + File.separator + filename;

            FileOutputStream fos = null;
            try {
                fos = new FileOutputStream(filePath, false);
                fos.write(report.getBytes());
                fos.flush();
                saved = true;
            } catch (FileNotFoundException e) {
                LogService.getLogger().error(
                        "Error opening file output stream " + filePath, e);
            } catch (IOException e) {
                LogService.getLogger().error(
                        "Error writing to file " + filePath, e);
            } finally {
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        } else {
            LogService
                    .getLogger()
                    .info("Skipping save of report"
                            + ", to enable saving reports ensure the log service's config has an outputDir");
        }

        return saved;
    }

    /**
     * Checks if the configuration indicates that the report should be saved
     * 
     * @param cfg
     * @return
     */
    protected boolean shouldSaveReport(LogSrvConfig cfg) {
        return (cfg.getOutputDir() != null);
    }
}
