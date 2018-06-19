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

package com.raytheon.edex.plugin.gfe.isc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Class used to service ISC requests
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 21, 2009  1995     bphillip  Initial port
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer
 * Oct 13, 2015  4961     randerso  Add support for ISC from NewTerrain database
 * Feb 02, 2017  3847     randerso  Converted ISCSendQueue to proper singleton
 *
 * </pre>
 *
 * @author bphillip
 */
public class ServiceISCRequest {

    /**
     * @param parmNameAndLevels
     * @param requestorDestinationXML
     * @param siteID
     * @throws GfeException
     */
    public static void serviceRequest(List<String> parmNameAndLevels,
            String requestorDestinationXML, String siteID) throws GfeException {

        IFPServer ifpServer = IFPServer.getActiveServer(siteID);
        if (ifpServer == null) {
            throw new GfeException("No active IFPServer for site: " + siteID);
        }

        IFPServerConfig config = ifpServer.getConfig();

        // find the forecast database
        Map<DatabaseID, List<String>> parmsAvailable = new HashMap<>();
        List<DatabaseID> dbs = ifpServer.getGridParmMgr().getDbInventory()
                .getPayload();

        for (DatabaseID db : dbs) {
            if ((("Fcst".equals(db.getModelName()) && db.getDbType().isEmpty())
                    || ("NewTerrain".equals(db.getModelName())
                            && "EditTopo".equals(db.getDbType())))
                    && db.getSiteId().equals(siteID)) {
                GridDbConfig gdc = config.gridDbConfig(db);
                parmsAvailable.put(db, gdc.parmAndLevelList());
            }
        }

        // convert parmNameAndLevels into ParmIDs and queue ISC
        List<ParmID> requests = new ArrayList<>();
        for (int i = 0; i < parmNameAndLevels.size(); i++) {
            // make a uniform format for parmNameAndLevels (parmName_level)
            String parmAndLevel = parmNameAndLevels.get(i);
            if (!parmAndLevel.contains("_")) {
                // assign SFC
                parmAndLevel = parmAndLevel + "_SFC";
            }

            // verify that we have the data in our database configuration
            DatabaseID db = null;
            for (Entry<DatabaseID, List<String>> entry : parmsAvailable
                    .entrySet()) {
                if (entry.getValue().contains(parmAndLevel)) {
                    db = entry.getKey();
                    break;
                }
            }

            // if db found, add parm to request
            if (db != null) {
                // separate out the parm name and level
                String parmName = parmAndLevel.substring(0,
                        parmAndLevel.indexOf('_'));
                String level = parmAndLevel
                        .substring(parmAndLevel.indexOf('_') + 1);

                ParmID pid = new ParmID(parmName, db, level);
                requests.add(pid);
            }
        }

        List<IscSendRecord> sendReqs = new ArrayList<>(requests.size());
        for (ParmID id : requests) {
            sendReqs.add(new IscSendRecord(id, TimeRange.allTimes(),
                    requestorDestinationXML, true));
        }
        IscSendQueue.getInstance().sendToQueue(sendReqs);
    }
}
