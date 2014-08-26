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
package com.raytheon.uf.edex.remote.script;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.remote.script.RemoteScriptConstants;
import com.raytheon.uf.common.remote.script.RemoteScriptRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Abstract class for the remote script handlers. Performs authorization and
 * timing of requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2014 2742       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AbstractRemoteScriptHandler extends
        AbstractPrivilegedRequestHandler<RemoteScriptRequest> {

    /** Status handler of the handling class using this class. */
    protected final transient IUFStatusHandler statusHandler;

    /** Common static directory for scripts. */
    protected final String scriptsDirectory;

    /** The handler's roleId defined in the common remoteScriptAdminRoles.xml */
    protected final String roleId;

    /**
     * Application name. This must match the application tag in the user role
     * file.
     */
    private static final String APPLICATION = "Remote Script";

    /**
     * Construct.
     * 
     * @param statusHandler
     */
    public AbstractRemoteScriptHandler(String roleId) {
        this.statusHandler = UFStatus.getHandler(this.getClass());
        this.roleId = roleId;

        String scriptsDirectory = FileUtil.edexPath(System.getProperty(
                RemoteScriptConstants.scriptDirectoryKey,
                RemoteScriptConstants.scriptDirectoryDefault));

        // Strip tailing separators.
        if (scriptsDirectory.endsWith(IPathManager.SEPARATOR)) {
            StringBuilder sb = new StringBuilder(scriptsDirectory);
            do {
                sb.setLength(sb.length() - 1);
            } while ((sb.length() > 0)
                    && (sb.lastIndexOf(IPathManager.SEPARATOR) == (sb.length() - 1)));
            scriptsDirectory = sb.toString();
        }
        this.scriptsDirectory = scriptsDirectory;
    }

    /**
     * The method a subclass must implement to perform the work for the desired
     * request.
     * 
     * @param request
     * @return results
     */
    abstract protected Object performRequest(RemoteScriptRequest request);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(RemoteScriptRequest request) throws Exception {
        Object result = null;

        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler.handle(Priority.INFO, String.format(
                    "Start for %s,  do %s", request.getUserId(), getRoleId()));
        }

        ITimer timer = TimeUtil.getTimer();
        timer.start();
        result = performRequest(request);
        timer.stop();

        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler.handle(
                    Priority.INFO,
                    String.format("Finish for %s,  do %s, took %s",
                            request.getUserId(), getRoleId(),
                            TimeUtil.prettyDuration(timer.getElapsedTime())));
        }

        return result;
    }

    protected String getRoleId() {
        return roleId;
    }

    /**
     * Performs the authorization work for the handlers.
     * 
     * @param user
     * @param request
     * @return authorizationResponse
     * @throws AuthorizationException
     */
    public AuthorizationResponse authorized(IUser user,
            RemoteScriptRequest request) throws AuthorizationException {
        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IRoleStorage roleStorage = manager.getRoleStorage();

        String roleId = getRoleId();

        boolean authorized = roleStorage.isAuthorized(roleId, user.uniqueId()
                .toString(), APPLICATION);

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            String message = "Not Authorized to run " + roleId;
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO,
                        String.format("%s,  %s", user.uniqueId(), message));
            }
            return new AuthorizationResponse(message);
        }
    }
}
