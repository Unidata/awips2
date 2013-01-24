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

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionDeleteRequest;
import com.raytheon.uf.common.datadelivery.registry.handlers.IBaseSubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.registry.ebxml.services.util.RegistrySessionManager;

/**
 * Handles deleting subscriptions or pending subscriptions, also any association
 * ties within an atomic transaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Nov 05, 2012 1306       djohnson     Remove dynamic serialize field level adapters.
 * Nov 15, 2012 1286       djohnson     Prevent NPE if user is null.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionDeleteHandler extends
        AbstractPrivilegedRequestHandler<SubscriptionDeleteRequest> {

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Object handleRequest(SubscriptionDeleteRequest request)
            throws Exception {
        try {
            RegistrySessionManager.openSession();

            Class handlerClass = ReflectionUtil.forName(request
                    .getHandlerClass());

            IBaseSubscriptionHandler handler = RegistryObjectHandlers
                    .get(handlerClass);
            final IUser user = request.getUser();
            final String username = (user == null) ? null : user.uniqueId()
                    .toString();

            handler.deleteByIds(username,
                    request.getSubscriptionIds());
        } finally {
            RegistrySessionManager.closeSession();
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            SubscriptionDeleteRequest request) throws AuthorizationException {
        return new AuthorizationResponse(true);
    }

}
