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
 * report data into HTML, emails the HTML to the configured address, and then
 * purges the database of all logging events that were included in the report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CreateSendReportJob implements Job {

    /*
     * (non-Javadoc)
     * 
     * @see org.quartz.Job#execute(org.quartz.JobExecutionContext)
     */
    @Override
    public void execute(JobExecutionContext ctx) throws JobExecutionException {
        LogService.getLogger().info(
                "Create report job triggered, preparing to send report");
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

            try {
                ReportEmailer.email(html, config);
            } catch (Exception e) {
                LogService.getLogger().error("Error emailing report", e);
                throw new JobExecutionException("Error emailing report", e);
            }
            LogService.getLogger().info(
                    "Report has been sent to: " + config.getToAddress());

            // only clear out the database if the analysis report was
            // successfully emailed
            LogService.getLogger().info(
                    "Purging database of messages that were just reported");
            try {
                dao.clearEntries();
            } catch (LogServiceException e) {
                LogService.getLogger().error("Error purging database", e);
                throw new JobExecutionException("Error purging database", e);
            }
            LogService.getLogger().info("Database purging complete");
        }
    }

}
