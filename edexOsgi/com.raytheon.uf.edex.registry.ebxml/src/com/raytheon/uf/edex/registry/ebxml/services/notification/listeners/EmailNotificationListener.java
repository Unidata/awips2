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
package com.raytheon.uf.edex.registry.ebxml.services.notification.listeners;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.EmailSender;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;

/**
 * Email notification listener.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1672       djohnson     Extracted from RegistryNotificationManager.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EmailNotificationListener implements NotificationListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EmailNotificationListener.class);

    /** The email sender used to send email notifications */
    private final EmailSender emailer;

    /** The destination **/
    private final NotificationDestination destination;

    /**
     * @param destination
     * @param emailer
     */
    public EmailNotificationListener(NotificationDestination destination,
            EmailSender emailer) {
        this.destination = destination;
        this.emailer = emailer;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onNotification(NotificationType notification) {
        if (emailer.isEmailEnabled()) {

            String styleSheetId = destination.getEmailNotificationFormatter();
            String styleSheetContents = null;
            /*
             * Get the style sheet used to format the email if one exists
             */
            final String emailAddress = destination.getDestination();
            if (styleSheetId != null) {
                ExtrinsicObjectType styleSheet = emailer
                        .getStyleSheetById(styleSheetId);

                if (styleSheet == null) {
                    statusHandler
                            .warn("Style sheet ["
                                    + styleSheetId
                                    + "] required but not found for subscription with address ["
                                    + emailAddress
                                    + "]. Sending unformatted notifications!");
                } else {
                    styleSheetContents = new String(
                            styleSheet.getRepositoryItem());
                }
            }
            try {
                emailer.sendNotification(emailAddress, notification,
                        styleSheetContents);
            } catch (EbxmlRegistryException e) {
                throw new RuntimeException("Error sending email notification.",
                        e);
            }
        }
    }

}
