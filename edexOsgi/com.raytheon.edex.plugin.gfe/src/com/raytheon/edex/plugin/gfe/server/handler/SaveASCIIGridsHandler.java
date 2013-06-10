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

import com.raytheon.edex.plugin.gfe.ifpAG.ASCIIGrid;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.SaveASCIIGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            dgilling     Initial creation
 * Apr 23, 2013 1949       rjpeter      Removed extra lock table look up
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SaveASCIIGridsHandler implements
        IRequestHandler<SaveASCIIGridsRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveASCIIGridsHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<String> handleRequest(SaveASCIIGridsRequest request)
            throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();

        LocalizationFile tempFile = getTempFile(request.getWorkstationID(),
                request.getAsciiGridData());
        ASCIIGrid agrid = new ASCIIGrid();
        String msg = agrid.readASCIIGridData(tempFile.getFile());
        if (!msg.isEmpty()) {
            sr.addMessage(msg);
        }

        int ngrids = agrid.getGridSlices().size();
        for (int i = 0; i < ngrids; i++) {
            ParmID pid = agrid.getGridSlices().get(i).getGridInfo().getParmID();
            String siteId = pid.getDbId().getSiteId();

            // get a list of available databases, see if the grid is part of an
            // existing databse.
            ServerResponse<List<DatabaseID>> srDbInv = GridParmManager
                    .getDbInventory(siteId);
            if (!srDbInv.isOkay()) {
                msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                        + "]. Unable to get database inventory. net: "
                        + srDbInv.message();
                sr.addMessage(msg);
                continue; // skip this grid
            }
            List<DatabaseID> databases = srDbInv.getPayload();

            // if database doesn't exist, then we need to create it
            if (!databases.contains(pid.getDbId())) {
                ServerResponse<?> srCreate = GridParmManager.createNewDb(pid
                        .getDbId());
                if (!srCreate.isOkay()) {
                    msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                            + "]. Unable to create database. net: "
                            + srCreate.message();
                    sr.addMessage(msg);
                    continue; // skip this grid
                }
            }

            // get the grid parm info for this grid slice from the ifpServer.
            // check for any translation needed and instruct ASCIIGrid to
            // perform the translation
            ServerResponse<GridParmInfo> srGpi = GridParmManager
                    .getGridParmInfo(pid);
            if (!srGpi.isOkay()) {
                msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                        + "]. Unable to get grid parm info for "
                        + pid.toString() + " net: " + srGpi.message();
                sr.addMessage(msg);
                continue; // skip this grid
            }
            GridParmInfo gpi = srGpi.getPayload();
            if (!gpi.equals(agrid.getGridSlices().get(i).getGridInfo())) {
                if (!agrid.translateGrid(i, gpi)) {
                    msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                            + "]. Unable to translate grid information for "
                            + pid.toString();
                    sr.addMessage(msg);
                    continue; // skip this grid
                }
            }

            // make a LockTableRequest
            LockTableRequest ltr = new LockTableRequest(pid);

            // make the Lock Request object to lock
            LockRequest lrl = new LockRequest(pid, agrid.getGridSlices().get(i)
                    .getValidTime(), LockMode.LOCK);

            // make the request lock change
            ServerResponse<List<LockTable>> srLockChange = LockManager
                    .getInstance().requestLockChange(lrl,
                            request.getWorkstationID(), siteId);
            if (!srLockChange.isOkay()) {
                msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                        + "]. Unable to obtain lock for " + pid.toString()
                        + ": " + srLockChange.message();
                sr.addMessage(msg);
                continue;
            }

            // make SaveGridRequest object
            final List<GFERecord> records = new ArrayList<GFERecord>();
            records.clear();
            GFERecord grid = new GFERecord(pid, agrid.getGridSlices().get(i)
                    .getValidTime());
            grid.setMessageData(agrid.getGridSlices().get(i));
            grid.setGridHistory(agrid.getGridSlices().get(i).getHistory());
            records.add(grid);
            final List<SaveGridRequest> sgrs = new ArrayList<SaveGridRequest>(1);
            SaveGridRequest sgr = new SaveGridRequest(pid, agrid
                    .getGridSlices().get(i).getValidTime(), records);
            sgrs.add(sgr);

            // save the grid
            ServerResponse<?> srSave = GridParmManager.saveGridData(sgrs,
                    request.getWorkstationID(), siteId);
            if (!srSave.isOkay()) {
                msg = "Skipping grid storage [" + (i + 1) + " of " + ngrids
                        + "]. Unable to store grid for " + pid.toString()
                        + ": " + srSave.message();
                sr.addMessage(msg);
                continue;
            }

            // send notifications
            try {
                ServerResponse<?> notifyResponse = SendNotifications
                        .send(srSave.getNotifications());
                if (!notifyResponse.isOkay()) {
                    for (ServerMsg smsg : notifyResponse.getMessages()) {
                        sr.addMessage(smsg.getMessage());
                    }
                }
            } catch (Exception e) {
                statusHandler.error("Error sending save notification", e);
                sr.addMessage("Error sending save notification - "
                        + e.getMessage());
            }

            // make the Lock Request to unlock
            LockRequest lrul = new LockRequest(pid, agrid.getGridSlices()
                    .get(i).getValidTime(), LockMode.UNLOCK);

            // make the request unlock change
            srLockChange = LockManager.getInstance().requestLockChange(lrul,
                    request.getWorkstationID(), siteId);
        }

        tempFile.delete();

        return sr;
    }

    private LocalizationFile getTempFile(WsId requestor, String filename) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        ctx.setContextName(requestor.getUserName());
        return pathManager.getLocalizationFile(ctx, filename);
    }
}
