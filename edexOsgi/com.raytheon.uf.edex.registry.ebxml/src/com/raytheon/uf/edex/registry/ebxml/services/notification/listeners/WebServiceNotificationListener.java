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

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Notifies listeners via a web service.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1672       djohnson     Extracted from RegistryNotificationManager.
 * 8/28/2013    1538       bphillip     Changed to catch a Throwable instead of just EbxmlRegistryException
 * 10/20/2013   1682       bphillip     Added synchronous notification delivery
 * 10/30/2013   1538       bphillip     Changed to use non-static soap service client
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class WebServiceNotificationListener implements NotificationListener {

    /** The log handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WebServiceNotificationListener.class);

    /** The destination of notifications sent via this listener */
    private final String destination;

    /** Registry soap service client */
    private RegistrySOAPServices registrySoapClient;

    /**
     * @param destination
     */
    public WebServiceNotificationListener(NotificationDestination destination,
            RegistrySOAPServices registrySoapClient) {
        this.destination = destination.getDestination();
        this.registrySoapClient = registrySoapClient;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onNotification(NotificationType notification) {
        try {
            sendNotificationViaSoap(notification, destination);
        } catch (Throwable e) {
            throw new RuntimeException(
                    "Error sending subscription notification.", e);
        }
    }

    @Override
    @WebMethod(action = "SynchronousNotification")
    @WebResult(name = "RegistryResponse", targetNamespace = EbxmlNamespaces.RS_URI, partName = "partRegistryResponse")
    public RegistryResponseType synchronousNotification(
            @WebParam(name = "Notification", targetNamespace = EbxmlNamespaces.RIM_URI, partName = "Notification") NotificationType notification)
            throws MsgRegistryException {
        RegistryResponseType response = new RegistryResponseType();
        response.setRequestId(notification.getId());
        try {
            onNotification(notification);
            response.setStatus(RegistryResponseStatus.SUCCESS);
            return response;
        } catch (Throwable e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error processing notification.", e);
        }
    }

    /**
     * Sends the notification via SOAP
     * 
     * @param notification
     *            The notification to send
     * @param serviceAddress
     *            The address to send the notification to
     * @throws EbxmlRegistryException
     *             If errors occur while sending the notification
     * @throws MsgRegistryException
     * @throws RegistryServiceException
     */
    protected void sendNotificationViaSoap(NotificationType notification,
            String serviceAddress) throws EbxmlRegistryException,
            RegistryServiceException, MsgRegistryException {
        statusHandler.info("Sending notification [" + notification.getId()
                + "]");
        registrySoapClient.getNotificationListenerServiceForUrl(serviceAddress)
                .synchronousNotification(notification);
        statusHandler.info("Notification [" + notification.getId() + "] sent!");
    }
}
