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
package com.raytheon.uf.edex.activetable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Service for the VTEC active table. Determines if the VTEC product corresponds
 * to a site we want in the active table, and if so, updates the active table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2009            njensen     Initial creation
 * Jul 14, 2009   #2950    njensen     Multiple site support
 * Dec 21, 2009   #4055    njensen     No site filtering
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ActiveTableSrv {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTableSrv.class);

    private static Map<Long, ActiveTable> activeTableMap = new HashMap<Long, ActiveTable>();

    public void vtecArrived(List<AbstractWarningRecord> records) {
        try {
            long threadId = Thread.currentThread().getId();
            ActiveTable activeTable = activeTableMap.get(threadId);
            if (activeTable == null) {
                activeTable = new ActiveTable();
                activeTableMap.put(threadId, activeTable);
            }
            if (records != null && records.size() > 0) {
                activeTable.merge(ActiveTableRecord.transformFromWarnings(
                        records, ActiveTableMode.OPERATIONAL));
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error merging active table", t);
        }
    }

    public void practiceVtecArrived(List<AbstractWarningRecord> records,
            Headers headers) {
        Integer offsetSeconds = null;
        if (headers != null) {
            offsetSeconds = (Integer) headers.get("offsetseconds");
        }
        if (offsetSeconds == null) {
            offsetSeconds = Integer.valueOf(0);
        }
        if (records != null && records.size() > 0) {
            long threadId = Thread.currentThread().getId();
            ActiveTable activeTable = activeTableMap.get(threadId);
            if (activeTable == null) {
                activeTable = new ActiveTable();
                activeTableMap.put(threadId, activeTable);
            }
            try {
                activeTable.merge(ActiveTableRecord.transformFromWarnings(
                        records, ActiveTableMode.PRACTICE), offsetSeconds
                        .intValue());
            } catch (Throwable t) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Error processing new VTEC products for PRACTICE active table",
                                t);
            }
        }
    }
}
