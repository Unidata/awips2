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
package com.raytheon.edex.plugin.gfe.cache.ifpparms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

public class IFPParmIdCache {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPParmIdCache.class);

    private static IFPParmIdCache instance;

    private Map<DatabaseID, List<ParmID>> parmIds;

    public synchronized static IFPParmIdCache getInstance() {
        if (instance == null) {
            instance = new IFPParmIdCache();
        }
        return instance;
    }

    private IFPParmIdCache() {
        parmIds = new HashMap<DatabaseID, List<ParmID>>();
    }

    public void removeSiteDbs(String siteID) {
        statusHandler.handle(Priority.EVENTA, "Purging " + siteID
                + " parmIDs from IFP parmID cache.");

        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            StringBuffer msg = new StringBuffer();
            msg.append("\nRemoving site information from IFPParmIdCache\nInitial Database Inventory:\n");
            for (DatabaseID dbId : parmIds.keySet()) {
                msg.append(dbId.toString()).append("\n");
            }
            statusHandler.handle(Priority.DEBUG, msg.toString());
        }

        List<DatabaseID> dbsToRemove = new ArrayList<DatabaseID>();
        for (DatabaseID dbId : parmIds.keySet()) {
            if (dbId.getSiteId().equalsIgnoreCase(siteID)) {
                dbsToRemove.add(dbId);
            }
        }

        for (DatabaseID db : dbsToRemove) {
            parmIds.remove(db);
            if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.handle(Priority.DEBUG, "IFPParmIdCache Removed "
                        + db);
            }
        }

        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            StringBuffer msg = new StringBuffer();
            msg.append("\nIFPParmIdCache Post-Purge Database Inventory:\n");
            for (DatabaseID dbId : parmIds.keySet()) {
                msg.append(dbId.toString()).append("\n");
            }
            statusHandler.handle(Priority.DEBUG, msg.toString());
        }

        statusHandler.handle(Priority.EVENTA, "Successfully purged all "
                + siteID + " parmIDs from IFP parmID cache...");

    }

    public List<ParmID> getParmIds(GridDbConfig gridConfig, DatabaseID dbId) {
        synchronized (parmIds) {
            if (!parmIds.containsKey(dbId)) {
                addParmIds(gridConfig, dbId);
            }
        }
        return parmIds.get(dbId);

    }

    private void addParmIds(GridDbConfig gridConfig, DatabaseID dbId) {
        String key = null;
        List<ParmID> parmIdList = new ArrayList<ParmID>();
        for (Iterator<String> iterator = gridConfig.get_gridInfoDict().keySet()
                .iterator(); iterator.hasNext();) {
            key = iterator.next();
            String[] nameLevel = key.split("_");
            parmIdList.add(new ParmID(nameLevel[0], dbId, nameLevel[1]));
        }
        synchronized (parmIds) {
            parmIds.put(dbId, parmIdList);
        }
    }
}
