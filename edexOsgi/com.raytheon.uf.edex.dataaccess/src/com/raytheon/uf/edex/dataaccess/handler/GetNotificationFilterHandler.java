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
package com.raytheon.uf.edex.dataaccess.handler;

import com.raytheon.uf.common.dataaccess.DataNotificationLayer;
import com.raytheon.uf.common.dataaccess.INotificationFilter;
import com.raytheon.uf.common.dataaccess.request.GetNotificationFilterRequest;
import com.raytheon.uf.common.dataaccess.response.GetNotificationFilterResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for <code>GetNotificationFilterRequest</code>. Data returned
 * is in form of <code>GetNotificationFilterResponse</code> objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2016  2416       tgurney     Initial creation
 * 
 * </pre>
 * 
 * @author tgurney
 */

public final class GetNotificationFilterHandler implements
        IRequestHandler<GetNotificationFilterRequest> {

    @Override
    public GetNotificationFilterResponse handleRequest(
            final GetNotificationFilterRequest request) throws Exception {
        INotificationFilter filter = DataNotificationLayer
                .getNotificationFilter(request.getRequestParameters());
        GetNotificationFilterResponse response = new GetNotificationFilterResponse();
        response.setNotificationFilter(filter);
        response.setJmsConnectionInfo(System.getenv("JMS_SERVER"));
        return response;
    }
}
