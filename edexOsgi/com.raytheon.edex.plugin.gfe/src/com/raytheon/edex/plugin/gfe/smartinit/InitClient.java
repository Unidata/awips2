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

import java.util.List;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceMgr;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 *
 * Init Client used by smart init for retrieving specific info
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 29, 2008           njensen   Initial creation
 * Jul 25, 2012  957      dgilling  Implement getEditAreaNames().
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer
 * Nov 20, 2013  2331     randerso  Changed return type of getTopoData
 * Oct 08, 2014  3684     randerso  Changed createDB to return status
 * Oct 27, 2014  3766     randerso  Fixed return type for createDB
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * Jun 12, 2017  6298     mapeters  Update references to refactored ReferenceMgr
 * Jul 31, 2017  6342     randerso  Get ReferenceMgr from IFPServer. Code
 *                                  cleanup
 *
 * </pre>
 *
 * @author njensen
 */

public class InitClient {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(InitClient.class);

    private final DatabaseID destinationDB;

    private final IFPServer ifpServer;

    /**
     * @param dbId
     * @throws GfeException
     */
    public InitClient(String dbId) throws GfeException {
        destinationDB = new DatabaseID(dbId);
        if (!destinationDB.isValid()) {
            throw new GfeException("Invalid databaseID: " + dbId);
        }

        ifpServer = IFPServer.getActiveServer(destinationDB.getSiteId());
        if (ifpServer == null) {
            throw new GfeException("No active IFPServer for site: "
                    + destinationDB.getSiteId());
        }
    }

    /**
     * Returns a list of the databases in the system
     *
     * @return list of databases
     */
    public List<DatabaseID> getKeys() {
        List<DatabaseID> dbIds = ifpServer.getGridParmMgr().getDbInventory()
                .getPayload();
        return dbIds;
    }

    /**
     * Creates a new database with the specified name
     *
     * @param key
     */
    public ServerResponse<?> createDB(String key) {
        DatabaseID id = new DatabaseID(key);
        ServerResponse<?> sr = ifpServer.getGridParmMgr().createNewDb(id);
        return sr;
    }

    /**
     * Returns a list of the singleton databases as specified in the server
     * config
     *
     * @return list of singleton databases
     */
    public List<DatabaseID> getSingletonIDs() {
        return ifpServer.getConfig().getSingletonDatabases();
    }

    /**
     * Get list of edit area names
     *
     * @return array of edit area names, possibly empty
     */
    public String[] getEditAreaNames() {
        String siteId = destinationDB.getSiteId();
        IFPServer ifpServer = IFPServer.getActiveServer(siteId);
        if (ifpServer != null) {
            ReferenceMgr refMgr = ifpServer.getReferenceMgr();

            ServerResponse<List<ReferenceID>> sr = refMgr.getInventory();
            if (sr.isOkay()) {
                List<ReferenceID> ids = sr.getPayload();
                String[] l = new String[ids.size()];
                for (int i = 0; i < ids.size(); i++) {
                    l[i] = ids.get(i).getName();
                }

                return l;
            } else {
                statusHandler.error("Unable to retrieve edit area inventory: "
                        + sr.message());
            }
        } else {
            statusHandler.error("No active IFPServer for site: " + siteId);
        }

        return new String[0];
    }

    /**
     * Get topo data
     *
     * @return the topo grid slice
     * @throws GfeException
     */
    public IGridSlice getTopo() throws GfeException {
        IGridSlice topo = null;
        GridLocation gloc = ifpServer.getConfig().dbDomain();
        ServerResponse<ScalarGridSlice> sr = ifpServer.getTopoMgr()
                .getTopoData(gloc);
        if (sr.isOkay()) {
            topo = sr.getPayload();
        } else {
            throw new GfeException(
                    "Error retrieving topo data: " + sr.message());
        }
        return topo;
    }

    /**
     * Sends a user message
     *
     * @param msg
     * @param group
     */
    public void sendUserMessage(String msg, String group) {
        UserMessageNotification message = new UserMessageNotification(msg,
                Priority.EVENTA, group, destinationDB.getSiteId());
        SendNotifications.send(message);
    }

}
