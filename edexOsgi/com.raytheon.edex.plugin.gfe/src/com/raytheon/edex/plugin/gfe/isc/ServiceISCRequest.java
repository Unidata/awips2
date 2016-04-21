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
import java.util.List;

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
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/21/09      1995       bphillip    Initial port
 * 06/13/13      2044       randerso    Refactored to use IFPServer
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
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
        List<String> parmsAvailable = new ArrayList<String>();
        List<DatabaseID> dbs = ifpServer.getGridParmMgr().getDbInventory()
                .getPayload();
        DatabaseID db = null;

        for (int i = 0; i < dbs.size(); i++) {
            if (dbs.get(i).getModelName().equals("Fcst")
                    && dbs.get(i).getDbType().isEmpty()
                    && dbs.get(i).getSiteId().equals(siteID)) {
                GridDbConfig gdc = config.gridDbConfig(dbs.get(i));
                parmsAvailable = gdc.parmAndLevelList();
                db = dbs.get(i);
                break;
            }
        }

        // convert parmNameAndLevels into ParmIDs and queue ISC
        List<ParmID> requests = new ArrayList<ParmID>();
        for (int i = 0; i < parmNameAndLevels.size(); i++) {
            // make a uniform format for parmNameAndLevels (parmName_level)
            String parmAndLevel = parmNameAndLevels.get(i);
            if (!parmAndLevel.contains("_")) {
                parmAndLevel = parmAndLevel + "_SFC"; // assign SFC
            }

            // verify that we have the data in our database configuration
            boolean found = parmsAvailable.contains(parmAndLevel);
            if (!found) {
                continue;
            }

            String parmName = parmAndLevel;
            String level = "SFC";
            // separate out the parm name and level
            if (parmAndLevel.contains("_")) {
                parmName = parmAndLevel.substring(0, parmAndLevel.indexOf("_"));
                level = parmAndLevel.substring(parmAndLevel.indexOf("_") + 1);
            }

            ParmID pid = new ParmID(parmName, db, level);
            requests.add(pid);
        }

        List<IscSendRecord> sendReqs = new ArrayList<IscSendRecord>(
                requests.size());
        for (ParmID id : requests) {
            sendReqs.add(new IscSendRecord(id, TimeRange.allTimes(),
                    requestorDestinationXML, true));
        }
        IscSendQueue.sendToQueue(sendReqs);
    }
}
