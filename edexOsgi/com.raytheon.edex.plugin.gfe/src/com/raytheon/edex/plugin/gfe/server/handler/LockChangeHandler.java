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

import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.request.LockChangeRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task for requesting a lock change
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 04/24/13     1949       rjpeter     Added list sizing
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class LockChangeHandler implements IRequestHandler<LockChangeRequest> {
    @Override
    public ServerResponse<List<LockTable>> handleRequest(
            LockChangeRequest request) throws Exception {
        String siteID = request.getSiteID();
        ServerResponse<List<LockTable>> sr = LockManager.getInstance()
                .requestLockChange(request.getRequests(),
                        request.getWorkstationID(), siteID);

        if (sr.isOkay()) {
            try {
                List<LockTable> lockTables = sr.getPayload();
                List<GfeNotification> notes = new ArrayList<GfeNotification>(
                        lockTables.size());

                for (LockTable table : lockTables) {
                    notes.add(new LockNotification(table, siteID));
                }
                ServerResponse<?> notifyResponse = SendNotifications
                        .send(notes);
                if (!notifyResponse.isOkay()) {
                    for (ServerMsg msg : notifyResponse.getMessages()) {
                        sr.addMessage(msg.getMessage());
                    }
                }
            } catch (Exception e) {
                sr.addMessage("Error sending lock notification - "
                        + e.getMessage());
            }
        }
        return sr;
    }
}
