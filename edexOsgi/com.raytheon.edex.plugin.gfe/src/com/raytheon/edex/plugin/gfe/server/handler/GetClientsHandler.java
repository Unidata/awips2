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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.request.GetClientsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.esb.camel.jms.IBrokerConnectionsProvider;

/**
 * Handler for get clients request. Returns list of Client IDs for all clients
 * connected to the JMS broker
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2009            randerso    Initial creation
 * Sep 22, 2009   3058     rjpeter     Converted to IRequestHandler
 * Apr 04, 2014  #2694     randerso    Changed to use Java implementation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class GetClientsHandler implements IRequestHandler<GetClientsRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetClientsHandler.class);

    private IBrokerConnectionsProvider provider;

    /**
     * Constructor
     * 
     * @param provider
     *            broker connections provider implementation
     */
    public GetClientsHandler(IBrokerConnectionsProvider provider) {
        this.provider = provider;
    }

    @Override
    public ServerResponse<List<String>> handleRequest(GetClientsRequest request)
            throws Exception {

        ServerResponse<List<String>> sr = new ServerResponse<List<String>>();
        try {
            List<String> clients = provider.getConnections();
            sr.setPayload(clients);
        } catch (Exception e) {
            statusHandler.error("Unable to retrieve active client list: ", e);
            sr.addMessage("Server error trying to retrieve clientlist: "
                    + e.getLocalizedMessage());
        }
        return sr;
    }
}
