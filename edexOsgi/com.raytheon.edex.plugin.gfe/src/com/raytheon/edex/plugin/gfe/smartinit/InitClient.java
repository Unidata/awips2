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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Init Client used by smart init for retrieving specific info
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 29, 2008				njensen	    Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InitClient {

    private static final Log logger = LogFactory.getLog(InitClient.class);

    private DatabaseID destinationDB;

    public InitClient(String dbId) {
        destinationDB = new DatabaseID(dbId);
    }

    /**
     * Returns a list of the databases in the system
     * 
     * @return
     * @throws GfeException
     */
    public List<DatabaseID> getKeys() throws GfeException {
        List<DatabaseID> dbIds = GridParmManager.getDbInventory(
                destinationDB.getSiteId()).getPayload();
        return dbIds;
    }

    /**
     * Creates a new database with the specified name
     * 
     * @param key
     * @throws GfeException
     */
    public void createDB(String key) throws GfeException {
        DatabaseID id = new DatabaseID(key);
        GridParmManager.createNewDb(id);
    }

    /**
     * Returns a list of the singleton databases as specified in the server
     * config
     * 
     * @return
     * @throws GfeException
     */
    public List<DatabaseID> getSingletonIDs() throws GfeException {
        List<DatabaseID> list = null;
        try {
            list = IFPServerConfigManager.getServerConfig(
                    destinationDB.getSiteId()).getSingletonDatabases();
        } catch (GfeException e) {
            throw new GfeException("Error determining singleton databases", e);
        }

        return list;
    }

    public List<String> getEditAreaNames() {
        ArrayList<String> list = new ArrayList<String>();
        // TODO implement something here
        return list;
    }

    public IGridSlice getTopo() throws GfeException {
        IGridSlice topo = null;
        try {
            List<ParmID> parms = GridParmManager.getParmList(
                    TopoDatabaseManager.getTopoDbId(destinationDB.getSiteId()))
                    .getPayload();
            if (parms.size() == 1) {
                ParmID p = parms.get(0);
                GetGridRequest req = new GetGridRequest();
                req.setParmId(p);
                GFERecord gfeRec = new GFERecord(p, TimeRange.allTimes());
                ArrayList<GFERecord> gfeList = new ArrayList<GFERecord>();
                gfeList.add(gfeRec);
                req.setRecords(gfeList);
                ArrayList<GetGridRequest> reqList = new ArrayList<GetGridRequest>();
                reqList.add(req);

                List<IGridSlice> data = GridParmManager.getGridData(reqList)
                        .getPayload();
                if (data != null && data.size() == 1) {
                    topo = data.get(0);
                } else {
                    throw new GfeException("Error getting grid data for "
                            + p.toString()
                            + ". Smart init requires topo and will stop.");
                }
            } else {
                throw new GfeException("Multiple topos, update InitClient");
            }
        } catch (GfeException e) {
            throw new GfeException("Error with topography for grid location", e);
        }
        return topo;
    }

    public void sendUserMessage(String msg, String group) {
        UserMessageNotification message = new UserMessageNotification(msg,
                Priority.EVENTA, group, destinationDB.getSiteId());
        SendNotifications.send(message);
    }

}
