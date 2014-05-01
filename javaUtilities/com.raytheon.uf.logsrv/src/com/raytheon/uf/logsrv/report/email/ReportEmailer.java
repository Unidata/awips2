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
package com.raytheon.uf.logsrv.report.email;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import com.raytheon.uf.logsrv.config.LogSrvConfig;

/**
 * Emails a report using the options specified in the config.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ReportEmailer {

    /**
     * Emails the provided string, using the options from the config.
     * 
     * @param report
     *            the text to email
     * @param config
     *            the config of email options
     * @throws Exception
     */
    public static void email(String report, LogSrvConfig config)
            throws Exception {
        Properties props = new Properties();
        props.put("host", config.getSmtpHost());
        props.put("mail.smtp.user", config.getFromAddress());
        props.put("port", config.getSmtpPort());

        Session session = Session.getDefaultInstance(props, null);
        MimeMessage message = new MimeMessage(session);
        message.setFrom(new InternetAddress(config.getFromAddress()));
        String[] split = config.getToAddress().split(",");
        for (String to : split) {
            message.addRecipient(Message.RecipientType.TO, new InternetAddress(
                    to.trim()));
        }
        message.setSubject(config.getClusterName() + " error report");
        message.setContent(report, "text/html; charset=utf-8");
        Transport transport = session.getTransport("smtp");
        transport.connect(config.getSmtpHost(), config.getFromAddress(), null);
        transport.sendMessage(message, message.getAllRecipients());
        transport.close();
    }

}
