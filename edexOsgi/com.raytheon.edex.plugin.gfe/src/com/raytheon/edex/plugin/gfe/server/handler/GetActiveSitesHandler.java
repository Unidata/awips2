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

import java.util.Set;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.GetActiveSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for GetActiveSitesRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2017 6298       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */

public class GetActiveSitesHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GetActiveSitesRequest> {

    @Override
    public ServerResponse<Set<String>> handleRequest(
            GetActiveSitesRequest request) throws Exception {
        Set<String> sites = IFPServer.getActiveSites();
        ServerResponse<Set<String>> sr = new ServerResponse<>();
        sr.setPayload(sites);
        return sr;
    }
}
