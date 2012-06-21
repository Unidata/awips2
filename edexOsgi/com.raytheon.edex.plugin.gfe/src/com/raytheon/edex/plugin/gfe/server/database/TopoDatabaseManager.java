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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * Manages the TopoDatabase instances of active GFE sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TopoDatabaseManager {

    private static Map<String, TopoDatabase> topoDbMap = new HashMap<String, TopoDatabase>();

    public static void initializeTopoDatabase(String siteID)
            throws GfeException {
        TopoDatabase topo = new TopoDatabase(siteID);
        topoDbMap.put(siteID, topo);
    }

    public static TopoDatabase getTopoDatabase(String siteID) {
        return topoDbMap.get(siteID);
    }

    public static void removeTopoDatabase(String siteID) {
        topoDbMap.remove(siteID);
    }

    public static DatabaseID getTopoDbId(String siteID) {
        return new DatabaseID(siteID, DataType.GRID, "EditTopo", "Topo");
    }

}
