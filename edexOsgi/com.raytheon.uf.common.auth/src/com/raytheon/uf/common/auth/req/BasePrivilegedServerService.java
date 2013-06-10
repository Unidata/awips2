package com.raytheon.uf.common.auth.req;

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

import java.rmi.RemoteException;

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Send requests to a privileged service on the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2013 1643       djohnson     Initial creation.
 * May 20, 2013 1040       mpduff       Add check for UserNotAuthorized.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BasePrivilegedServerService<T extends AbstractPrivilegedRequest>
        extends BaseServerService<T> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BasePrivilegedServerService.class);

    /**
     * Constructor.
     * 
     * @param serviceKey
     */
    protected BasePrivilegedServerService(String serviceKey) {
        super(serviceKey);
    }

    @Override
    protected <U> U unwrapResponse(Class<U> responseType, Object object)
            throws RemoteException {
        if (object instanceof SuccessfulExecution) {
            SuccessfulExecution response = (SuccessfulExecution) object;
            return super.unwrapResponse(responseType, response.getResponse());
        } else if (object instanceof ServerErrorResponse) {
            throw new RemoteException("Error communicating with the server!",
                    ExceptionWrapper
                            .unwrapThrowable(((ServerErrorResponse) object)
                                    .getException()));
        } else if (object instanceof UserNotAuthorized) {
            return null;
        } else {
            statusHandler
                    .warn(String
                            .format("Expected a %s or %s response type from a privileged request, but received a payload of %s.  "
                                    + "Attempting to cast to the expected return type, but this is a configuration error because "
                                    + "privileged requests do not seem to be correctly processed!",
                                    SuccessfulExecution.class.getSimpleName(),
                                    ServerErrorResponse.class.getSimpleName(),
                                    responseType.getName()));
            return super.unwrapResponse(responseType, object);
        }
    }
}
