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
package com.raytheon.uf.edex.registry.ebxml.services.notification;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.InetAddress;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.xml.bind.JAXBException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.ExtrinsicObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Class used to send email registry notifications. This class uses the java
 * Mail api and is configured via the localized email properties file located at
 * ebxml/notification/email.properties
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1905        bphillip    Initial implementation
 * Apr 17, 2013 1914        djohnson    Now has reference to extrinsic object dao.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class EmailSender {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EmailSender.class);

    private final boolean emailEnabled;

    /** The subject prefix attached to all email notifications */
    private static final String SUBJECT_PREFIX = "Registry Event Notification for Subscription: ";

    /** The mailto: prefix */
    private static final String MAIL_TO = "mailto:";

    /** The email properties objects */
    private Properties emailProperties = new Properties();

    /** The email sender string */
    private final String sender;

    /** The JAXBManager */
    private JAXBManager jaxbManager;

    /** Factory for generating XSLT transformations */
    private final TransformerFactory factory = TransformerFactory.newInstance();

    /** Data access object for extrinsic objects */
    private final ExtrinsicObjectDao extrinsicObjectDao;

    /**
     * Creates a new EmailSender object
     * 
     * @throws IOException
     *             If problems occur while loading the email properties file
     * @throws JAXBException
     *             If problems occur initializing the JAXBManager
     */
    public EmailSender(boolean emailEnabled,
            ExtrinsicObjectDao extrinsicObjectDao) throws IOException,
            JAXBException {
        this.emailEnabled = emailEnabled;
        this.extrinsicObjectDao = extrinsicObjectDao;
        if (emailEnabled) {
            statusHandler.debug("Loading email properties...");
            File emailPropertiesFile = PathManagerFactory.getPathManager()
                    .getStaticFile("ebxml/notification/email.properties");
            emailProperties = PropertiesUtil.read(emailPropertiesFile);
            statusHandler.debug("Email properties loaded!");
            jaxbManager = new JAXBManager(NotificationType.class);
        } else {
            statusHandler
                    .warn("Registry email notification capability is disabled.");
        }
        sender = "Registry@"
                + InetAddress.getLocalHost().getCanonicalHostName();

    }

    /**
     * Sends a notification via email. Optionally including a style sheet for
     * formatting the email message
     * 
     * @param toAddress
     *            The address to send the notification
     * @param notification
     *            The notification object
     * @param styleSheet
     *            The style sheet used to format the message. This is an
     *            optional argument.
     * @throws EbxmlRegistryException
     *             If errors occur sending the notification
     */
    public void sendNotification(String toAddress,
            NotificationType notification, String styleSheet)
            throws EbxmlRegistryException {
        String message = null;
        try {
            if (styleSheet == null) {
                message = jaxbManager.marshalToXml(notification);
            } else {
                StringWriter outputWriter = new StringWriter();
                StringReader styleSheetReader = new StringReader(styleSheet);
                Transformer transformer = factory
                        .newTransformer(new StreamSource(styleSheetReader));
                transformer.transform(new StreamSource(new StringReader(
                        jaxbManager.marshalToXml(notification))),
                        new StreamResult(outputWriter));
                message = outputWriter.toString();
            }
        } catch (JAXBException e) {
            throw new EbxmlRegistryException(
                    "Error sending email notification", e);
        } catch (TransformerConfigurationException e) {
            throw new EbxmlRegistryException(
                    "Error configuring XSLT Transformer", e);
        } catch (TransformerException e) {
            throw new EbxmlRegistryException("Error applying XSLT transform", e);
        }
        if (toAddress.startsWith(MAIL_TO)) {
            toAddress = toAddress.substring(MAIL_TO.length());
        }
        sendMessage(toAddress, SUBJECT_PREFIX + notification.getSubscription(),
                message);
    }

    /**
     * Sends an email
     * 
     * @param toAddress
     *            The destination address of the email
     * @param subject
     *            The subject of the email
     * @param messageText
     *            The text of the email
     * @throws EbxmlRegistryException
     */
    public void sendMessage(String toAddress, String subject, String messageText)
            throws EbxmlRegistryException {
        if (emailEnabled) {
            statusHandler.info("Sending email [" + subject + "] to ["
                    + toAddress + "]...");
            Session mailSession = Session.getInstance(emailProperties);
            Message message = new MimeMessage(mailSession);
            try {
                InternetAddress fromAddress = new InternetAddress(sender);
                message.setFrom(fromAddress);
                InternetAddress[] toAddressArray = InternetAddress
                        .parse(toAddress);
                message.setRecipients(Message.RecipientType.TO, toAddressArray);
                message.setSentDate(TimeUtil.newDate());
                message.setSubject(subject);
                message.setText(messageText);
                Transport.send(message);
            } catch (MessagingException e) {
                throw new EbxmlRegistryException(e);
            }
            statusHandler.info("Email [" + subject + "] to [" + toAddress
                    + "] sent.");
        } else {
            statusHandler
                    .warn("Cannot send email message.  Registry email transmission disabled.");
        }
    }

    public boolean isEmailEnabled() {
        return emailEnabled;
    }

    /**
     * Get a style sheet by its id.
     * 
     * @param styleSheetId
     *            the style sheet id
     * @return the extrinsic object
     */
    public ExtrinsicObjectType getStyleSheetById(String styleSheetId) {
        return extrinsicObjectDao.getById(styleSheetId);
    }

}
