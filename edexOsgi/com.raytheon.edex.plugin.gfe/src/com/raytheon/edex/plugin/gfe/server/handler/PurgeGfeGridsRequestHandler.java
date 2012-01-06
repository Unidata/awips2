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

import com.raytheon.edex.plugin.gfe.isc.GfeScriptExecutor;
import com.raytheon.uf.common.dataplugin.gfe.request.PurgeGfeGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for PurgeGfeGrids. Will execute the purgeAllGrids.py script
 * with the supplied argument string from the request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PurgeGfeGridsRequestHandler implements
        IRequestHandler<PurgeGfeGridsRequest> {

    @Override
    public ServerResponse<String> handleRequest(PurgeGfeGridsRequest request)
            throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();
        GfeScriptExecutor scriptRunner = new GfeScriptExecutor();

        String retVal = scriptRunner.execute("purgeAllGrids "
                + request.getArgString());

        if (!retVal.equals(GfeScriptExecutor.SUCCESS)) {
            sr.addMessage(retVal);
        }

        return sr;
    }

}
