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

import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * 
 * Wrapper for the notificationlistener service to be used with the SOAP
 * interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * 10/20/2013   1682        bphillip    Added synchronous notification delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class NotificationListenerImplWrapper implements NotificationListener {

    private NotificationListenerImpl notificationListener;

    public NotificationListenerImplWrapper() {

    }

    public NotificationListenerImplWrapper(
            NotificationListenerImpl notificationListener) {
        this.notificationListener = notificationListener;
    }

    @Override
    @WebMethod(action = ON_NOTIFICATION_ACTION)
    @Oneway
    public void onNotification(
            @WebParam(name = "Notification", targetNamespace = EbxmlNamespaces.RIM_URI, partName = "Notification") NotificationType notification) {
        notificationListener.onNotification(notification);
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
            notificationListener.synchronousNotification(notification);
            response.setStatus(RegistryResponseStatus.SUCCESS);
            return response;
        } catch (Throwable e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error processing notification.", e);
        }
    }

}
