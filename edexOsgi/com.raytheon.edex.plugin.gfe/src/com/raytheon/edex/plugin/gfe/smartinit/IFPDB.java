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
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;

/**
 * IFP Database, originally C++ <--> Python bridge, ported to Java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 7, 2008				njensen	    Initial creation
 * Jun 13, 2013     #2044   randerso    Refactored to use IFPServer
 * Jul 28, 2014   RM 15655  ryu         Negate raising exception for empty db in constructor
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IFPDB {

    private final DatabaseID dbid;

    private final GridParmManager gridParmMgr;

    private final LockManager lockMgr;

    private List<String> keys;

    /**
     * Constructor
     * 
     * @param db
     *            Database ID in string form
     * @throws GfeException
     */
    public IFPDB(String db) throws GfeException {
        dbid = new DatabaseID(db);
        if (!dbid.isValid()) {
            throw new GfeException("Invalid databaseID: " + db);
        }

        IFPServer ifpServer = IFPServer.getActiveServer(dbid.getSiteId());
        if (ifpServer == null) {
            throw new GfeException("No active IFPServer for site: "
                    + dbid.getSiteId());
        }
        this.gridParmMgr = ifpServer.getGridParmMgr();
        this.lockMgr = ifpServer.getLockMgr();
        ServerResponse<List<ParmID>> sr = gridParmMgr.getParmList(dbid);

        if (sr.isOkay()) {
            List<ParmID> list = sr.getPayload();
            this.keys = new ArrayList<String>(list.size());
            for (ParmID p : list) {
                this.keys.add(p.getCompositeName());
            }
        } else {
            this.keys = Collections.emptyList();
        }
    }

    /**
     * Returns a list of available parms corresponding to the DatabaseID
     * 
     * @return the list of available parms
     */
    public List<String> getKeys() {
        return keys;
    }

    /**
     * Returns an IFPWE from the database
     * 
     * @param parmName
     *            the name of the parm
     * @return IFPWE instance for parm
     * @throws GfeException
     */
    public IFPWE getItem(String parmName) throws GfeException {
        return getItem(parmName, IFPWE.SMART_INIT_USER);
    }

    /**
     * Returns an IFPWE from the database
     * 
     * @param parmName
     * @param userName
     * @return IFPWE instance for parmName
     */
    public IFPWE getItem(String parmName, String userName) {
        String[] split = parmName.split("_");
        String param = split[0];
        String level = split[1];

        ParmID pid = new ParmID(param, dbid, level);
        return new IFPWE(pid, userName, gridParmMgr, lockMgr);
    }

    /**
     * Returns the time of the database
     * 
     * @return the model time
     */
    public Date getModelTime() {
        return dbid.getModelDate();
    }

    /**
     * Returns the short model name of the database
     * 
     * @return the short model name
     */
    public String getShortModelIdentifier() {
        return dbid.getShortModelId();
    }

    /**
     * Returns the name of the database
     * 
     * @return the model name
     */
    public String getModelIdentifier() {
        return dbid.getModelId();
    }

}
