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
 * Task for sending ISC grids from a client
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Aug 19, 2009  1995     bphillip  Initial creation
 * Sep 22, 2009  3058     rjpeter   Converted to IRequestHandler
 * Feb 02, 2017  3847     randerso  Converted ISCSendQueue to proper singleton
 *
 * </pre>
 *
 * @author bphillip
 */
public class SendIscGridHandler implements IRequestHandler<SendIscGridRequest> {

    @Override
    public ServerResponse<?> handleRequest(SendIscGridRequest request)
            throws Exception {
        ServerResponse<String> response = new ServerResponse<>();
        // String siteID = request.getSiteID();
        List<IscSendRecord> newReqs = new ArrayList<>(
                request.getRequests().size());
        for (SendISCRequest sendReq : request.getRequests()) {
            if ((sendReq.getParmId() == null)
                    && (sendReq.getTimeRange() == null)) {
                IscSendQueue.getInstance().sendPending(request.getSiteID());
            } else {
                IscSendRecord rec = new IscSendRecord(sendReq.getParmId(),
                        sendReq.getTimeRange(), "", true);
                newReqs.add(rec);
            }
        }
        if (!newReqs.isEmpty()) {
            IscSendQueue.getInstance().sendToQueue(newReqs);
        }

        return response;
    }

}
