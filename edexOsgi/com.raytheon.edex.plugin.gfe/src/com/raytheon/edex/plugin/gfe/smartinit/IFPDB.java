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
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * IFP Database, originally C++ <--> Python bridge, ported to Java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 7, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IFPDB {

    private DatabaseID dbid;

    private List<String> keys;

    public IFPDB(String db) {
        dbid = new DatabaseID(db);
    }

    /**
     * Returns a list of available parms corresponding to the DatabaseID
     * 
     * @return
     */
    public List<String> getKeys() {
        if (keys == null) {
            List<ParmID> list = GridParmManager.getParmList(dbid).getPayload();

            keys = new ArrayList<String>();
            if (list != null) {
                for (ParmID p : list) {
                    keys.add(p.getCompositeName());
                }
            }
        }
        return keys;
    }

    /**
     * Returns an IFPWE from the database
     * 
     * @param parmName
     *            the name of the parm
     * @return
     * @throws GfeException
     */
    public IFPWE getItem(String parmName) throws GfeException {
        return getItem(parmName,IFPWE.SMART_INIT_USER);
    }
    
    public IFPWE getItem(String parmName, String userName){
        String[] split = parmName.split("_");
        String param = split[0];
        String level = split[1];

        ParmID pid = new ParmID(param, dbid, level);
        return new IFPWE(pid,userName);
    }

    /**
     * Returns the time of the database
     * 
     * @return
     */
    public Date getModelTime() {
        return dbid.getModelDate();
    }

    /**
     * Returns the short model name of the database
     * 
     * @return
     */
    public String getShortModelIdentifier() {
        return dbid.getShortModelId();
    }

    /**
     * Returns the name of the database
     * 
     * @return
     */
    public String getModelIdentifier() {
        return dbid.getModelId();
    }

}
