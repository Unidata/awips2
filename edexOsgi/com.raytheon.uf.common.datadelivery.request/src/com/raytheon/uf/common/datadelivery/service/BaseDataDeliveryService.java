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
package com.raytheon.uf.common.datadelivery.service;

import java.rmi.RemoteException;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;

/**
 * Base class for services that send requests to the data delivery server.
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
public class BaseDataDeliveryService<T extends AbstractPrivilegedRequest> {

    /**
     * Send a request to the data delivery server.
     * 
     * @param request
     * @return
     * @throws Exception
     */
    protected Object sendRequest(T request) throws Exception {
        Object object = RequestRouter.route(request,
                DataDeliveryConstants.DATA_DELIVERY_SERVER);
        if (object instanceof SuccessfulExecution) {
            SuccessfulExecution response = (SuccessfulExecution) object;
            return response.getResponse();
        } else {
            throw new RemoteException("Error communicating with the server!",
                    ExceptionWrapper
                            .unwrapThrowable(((ServerErrorResponse) object)
                                    .getException()));
        }
    }
}
