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

import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

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
 * Jun 17, 2014    3296    randerso    Added performance logging
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ActiveTableSrv {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTableSrv.class);

    private static ThreadLocal<ActiveTable> threadLocalActiveTable = new ThreadLocal<ActiveTable>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        protected ActiveTable initialValue() {
            return new ActiveTable();
        }

    };

    /**
     * Merge VTEC info from new warning records into the active table
     * 
     * @param records
     */
    public void vtecArrived(List<AbstractWarningRecord> records) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        try {
            ActiveTable activeTable = threadLocalActiveTable.get();
            if (records != null && records.size() > 0) {
                activeTable.merge(ActiveTableRecord.transformFromWarnings(
                        records, ActiveTableMode.OPERATIONAL));
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error merging active table", t);
        }
        timer.stop();
        PerformanceStatus.getHandler("ActiveTable").logDuration(
                "Total time to process " + records.size() + " records",
                timer.getElapsedTime());
    }

    /**
     * Merge new warning records into the practice active table
     * 
     * @param records
     * @param headers
     */
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
            ActiveTable activeTable = threadLocalActiveTable.get();
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
