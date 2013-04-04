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
package com.raytheon.uf.edex.datadelivery.service.services;

import java.util.List;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinitionServiceRequest;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.IGroupDefinitionService;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Handles request from the {@link IGroupDefinitionService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2013 1441       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GroupDefinitionServiceHandler extends
        AbstractPrivilegedRequestHandler<GroupDefinitionServiceRequest> {

    private final ISubscriptionNotificationService notificationService;

    /**
     * Constructor.
     * 
     * @param notificationService
     *            the subscription notification service
     */
    public GroupDefinitionServiceHandler(
            ISubscriptionNotificationService notificationService) {
        this.notificationService = notificationService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(GroupDefinitionServiceRequest request)
            throws Exception {
        final IUser user = request.getUser();
        switch (request.getType()) {
        case DELETE:
            handleDelete(request.getGroup(), user);
            break;
        }
        return null;
    }

    /**
     * Handles the delete of a group. First it updates any subscriptions in the
     * group to not have a group, and then deletes the actual group.
     * 
     * @param user
     * 
     * @param groupDefinition
     * @return
     * @throws RegistryHandlerException
     */
    private void handleDelete(GroupDefinition group, IUser user)
            throws RegistryHandlerException {

        ISubscriptionHandler handler = DataDeliveryHandlers
                .getSubscriptionHandler();
        List<Subscription> subsForGroup = handler.getByGroupName(group
                .getGroupName());
        if (!CollectionUtil.isNullOrEmpty(subsForGroup)) {
            for (Subscription sub : subsForGroup) {
                sub.setGroupName(GroupDefinition.NO_GROUP);
                handler.update(sub);

                // TODO: Would be nice to use a subset of the
                // SubscriptionService functionality here so we didn't have to
                // manually send the subscription updated notification
                String username = user != null ? user.uniqueId().toString()
                        : "none";
                notificationService.sendUpdatedSubscriptionNotification(sub,
                        username);
            }
        }

        DataDeliveryHandlers.getGroupDefinitionHandler().delete(group);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            GroupDefinitionServiceRequest request)
            throws AuthorizationException {
        return new AuthorizationResponse(true);
    }

}
