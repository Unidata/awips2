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
package com.raytheon.viz.gfe.actions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Handler class for Clear Practice forecast grids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2010     #4475  randerso     Initial creation
 * May 02, 2013      1949  rjpeter      Change ServerResponse return type.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class ClearPracticeGrids extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearPracticeGrids.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dataManager = DataManager.getCurrentInstance();
        if (dataManager == null) {
            return null;
        }
        IFPClient client = dataManager.getClient();
        IParmManager pm = dataManager.getParmManager();

        String s = dataManager.getSiteID() + "_GRID_Prac_Fcst_00000000_0000";
        DatabaseID dbid = new DatabaseID(s);

        try {
            // for each parmId in the database
            ParmID[] parmList = pm.getAvailableParms(dbid);
            for (ParmID parmID : parmList) {
                // if the parm exists
                Parm parm = pm.getParm(parmID);
                if (parm != null) {

                    TimeRange tr = parm.getInventorySpan();
                    if (tr.getDuration() > 0) {
                        // break any locks held by others
                        List<TimeRange> locks = parm.getLockTable()
                                .lockedByOther();
                        if (locks.size() > 0) {
                            ArrayList<LockRequest> lreq = new ArrayList<LockRequest>();
                            for (TimeRange lock : locks) {
                                lreq.add(new LockRequest(parmID, lock,
                                        LockMode.BREAK_LOCK));
                            }
                            ServerResponse<List<LockTable>> sr = client
                                    .requestLockChange(lreq);
                            if (!sr.isOkay()) {
                                List<ServerMsg> messages = sr.getMessages();
                                StringBuilder msg = new StringBuilder(
                                        "Error attempting to remove locks.");
                                for (ServerMsg serverMsg : messages) {
                                    msg.append("\n").append(
                                            serverMsg.getMessage());
                                }
                                statusHandler.handle(Priority.PROBLEM,
                                        msg.toString());
                            }
                        }

                        // delete all grids
                        parm.deleteTR(tr);
                        parm.saveParameter(tr);
                    }
                }
            }

            statusHandler.handle(Priority.SIGNIFICANT,
                    "Prac_Fcst database has been cleared.");

        } catch (GFEServerException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unexpected exception while attempting to clear practice grids",
                            e);
        }

        return null;
    }
}
