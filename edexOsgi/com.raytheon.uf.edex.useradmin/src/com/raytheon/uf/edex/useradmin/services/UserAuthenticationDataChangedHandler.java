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
package com.raytheon.uf.edex.useradmin.services;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.useradmin.request.UserAuthenticationDataChanged;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Receives requests signifying that user authentication data has changed, and
 * publishes them on the observer topic.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 09, 2013 1412       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class UserAuthenticationDataChangedHandler extends
        AbstractPrivilegedRequestHandler<UserAuthenticationDataChanged> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(UserAuthenticationDataChangedHandler.class);

    private final String topicUri;

    public UserAuthenticationDataChangedHandler(String topicUri) {
        this.topicUri = topicUri;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(UserAuthenticationDataChanged request)
            throws Exception {

        send(request, topicUri);

        return request;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            UserAuthenticationDataChanged request)
            throws AuthorizationException {
        // Returns false because it should only ever be invoked from another
        // plugin implements the useradmin API running in the local EDEX, which
        // does not require authentication
        return new AuthorizationResponse(false);
    }

    /**
     * Sends the object to the topic observers are listening on.
     * 
     * @param obj
     * @param endpoint
     *            the endpoint to send to
     */
    public void send(Object obj, String endpoint) {
        try {
            byte[] bytes = SerializationUtil.transformToThrift(obj);
            EDEXUtil.getMessageProducer().sendAsyncUri(endpoint, bytes);
        } catch (EdexException e) {
            statusHandler.error("Error sending object to " + endpoint, e);
        } catch (SerializationException e) {
            statusHandler.error("Error serializing object to " + endpoint, e);
        }
    }
}
