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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.gfe.isc.IscSendQueue;
import com.raytheon.edex.plugin.gfe.isc.IscSendRecord;
import com.raytheon.uf.common.dataplugin.gfe.request.SendIscGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SendISCRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Task for send ISC grids from a client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/19/09     1995       bphillip    Initial creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SendIscGridHandler implements IRequestHandler<SendIscGridRequest> {

    @Override
    public ServerResponse<?> handleRequest(SendIscGridRequest request)
            throws Exception {
        ServerResponse<String> response = new ServerResponse<String>();
        // String siteID = request.getSiteID();
        List<IscSendRecord> newReqs = new ArrayList<IscSendRecord>(request
                .getRequests().size());
        for (SendISCRequest sendReq : request.getRequests()) {
            if (sendReq.getParmId() == null && sendReq.getTimeRange() == null) {
                IscSendQueue.getInstance().sendPending(request.getSiteID());
            } else {
                IscSendRecord rec = new IscSendRecord(sendReq.getParmId(),
                        sendReq.getTimeRange(), "", true);
                newReqs.add(rec);
            }
        }
        if (!newReqs.isEmpty()) {
            IscSendQueue.sendToQueue(newReqs);
        }

        return response;
    }

}
