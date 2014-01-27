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

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Implements the plugin based notification listener of the EBXML specification.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2013 1672       djohnson     Initial creation
 * 10/20/2013   1682       bphillip    Added synchronous notification delivery
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SpringBeanNotificationListener implements NotificationListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SpringBeanNotificationListener.class);

    private final String beanName;

    /**
     * Constructor.
     * 
     * @param destination
     *            the name of the Spring bean which implements the
     *            {@link NotificationListener} interface
     */
    public SpringBeanNotificationListener(NotificationDestination destination) {
        this.beanName = destination.getDestination();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onNotification(NotificationType notification) {
        try {
            final Object bean = EDEXUtil
                    .getESBComponent(Object.class, beanName);
            if (!(bean instanceof NotificationListener)) {
                throw new IllegalArgumentException("Bean [" + beanName
                        + "] does not implement the ["
                        + NotificationListener.class.getName() + "] interface!");
            }
            NotificationListener beanAsListener = (NotificationListener) bean;
            beanAsListener.onNotification(notification);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error notifying spring bean [" + beanName + "]", e);
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

}
