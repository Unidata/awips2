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
package com.raytheon.uf.edex.datadelivery.harvester.purge;

import java.util.concurrent.atomic.AtomicBoolean;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * Container class to hold the {@link IDataSetMetaDataPurgeTask} instance. It
 * will be called via Spring to run the purge task.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2012  1102      djohnson     Initial creation
 * Apr 12,2014   3012     dhladky      Purge never worked, fixed to make work.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DataSetMetaDataPurgeLauncher {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSetMetaDataPurgeLauncher.class);

    private IDataSetMetaDataPurgeTask PURGE_TASK = null;

    private static final AtomicBoolean running = new AtomicBoolean();

    /**
     * Public constructor.
     */
    public DataSetMetaDataPurgeLauncher(RegistryObjectDao rdo) {
        PURGE_TASK = new DataSetMetaDataPurgeTaskImpl(rdo);
    }

    /**
     * Try purging datasets.
     */
    public void runPurge() {
        try {
            if (running.compareAndSet(false, true)) {
                PURGE_TASK.run();
            } else if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler
                        .info("DataSetMetaData purge already running, skipping...");
            }
        } finally {
            running.set(false);
        }
    }
}
