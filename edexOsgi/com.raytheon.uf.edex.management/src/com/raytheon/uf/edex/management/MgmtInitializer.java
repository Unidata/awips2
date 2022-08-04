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
package com.raytheon.uf.edex.management;

import java.net.UnknownHostException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MgmtInitializer {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(MgmtInitializer.class);

    public MgmtInitializer() {
        try {
            String port = System.getProperty("management.port");
            ClusterTask lock = ClusterLockUtils.lock(MgmtUtil.TASK,
                    MgmtUtil.buildLocalDetails(), port, 0, false);
            if (!lock.getLockState().equals(LockState.SUCCESSFUL)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to store JVM's information management information to db");
            }
        } catch (UnknownHostException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

}
