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
package com.raytheon.uf.logsrv.report;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.logsrv.config.LogSrvConfig;
import com.raytheon.uf.logsrv.derby.DerbyDao;
import com.raytheon.uf.logsrv.report.data.LogReportContainer;
import com.raytheon.uf.logsrv.report.email.HtmlGenerator;
import com.raytheon.uf.logsrv.report.email.ReportEmailer;

/**
 * A simple main that generates a report based on the current entries in the
 * database, emails the report, and then clears the database. Allows skipping
 * waiting for the once a day email.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TestReportOutputter {

    public static void main(String[] args) throws Exception {
        JAXBContext context = JAXBContext.newInstance(LogSrvConfig.class);
        Unmarshaller m = context.createUnmarshaller();
        LogSrvConfig config = (LogSrvConfig) m
                .unmarshal(new File("config.xml"));
        config.validate();
        DerbyDao.getInstance().setConfig(config);
        LogReportContainer container = DerbyDao.getInstance().buildReport();
        String report = HtmlGenerator.generateHtml(container);
        ReportEmailer.email(report, config);
        DerbyDao.getInstance().clearEntries();
    }

}
