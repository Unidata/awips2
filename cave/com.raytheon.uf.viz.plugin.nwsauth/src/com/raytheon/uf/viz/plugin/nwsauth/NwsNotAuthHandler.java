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
package com.raytheon.uf.viz.plugin.nwsauth;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.resp.UserNotAuthenticated;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.INotAuthHandler;

/**
 * Implementation of INotAuthHandler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */

public class NwsNotAuthHandler implements INotAuthHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.requests.INotAuthHandler#notAuthenticated(com
     * .raytheon.uf.common.auth.resp.UserNotAuthenticated)
     */
    @Override
    public Object notAuthenticated(UserNotAuthenticated response)
            throws VizException {
        AbstractPrivilegedRequest request = response.getRequest();
        IUser user = request.getUser();
        String message = "User: <" + user.uniqueId()
                + "> is not authenticated to perform request:"
                + request.getClass();
        UFStatus.getHandler(NwsNotAuthHandler.class).handle(Priority.PROBLEM,
                message);
        throw new VizException(message);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.requests.INotAuthHandler#notAuthorized(com.raytheon
     * .uf.common.auth.resp.UserNotAuthorized)
     */
    @Override
    public Object notAuthorized(UserNotAuthorized response) throws VizException {
        String message = response.getMessage();
        if (message == null) {
            message = "Error sending request for user: "
                    + response.getRequest().getUser().uniqueId().toString();
        }
        UFStatus.getHandler(NwsNotAuthHandler.class).handle(Priority.PROBLEM,
                message);
        throw new VizException(message);
    }

}
