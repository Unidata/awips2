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
package com.raytheon.uf.edex.esb.camel.context;

import java.util.concurrent.ExecutorService;

import org.apache.camel.CamelContext;

import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Implementation of {@link IContextStateManager} that handles clustered
 * contexts. Extends {@code DependencyContextStateManager} to allow for
 * clustered contexts to work with dependencies also.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ClusteredContextStateManager extends DependencyContextStateManager {

    /**
     * Name field for cluster task.
     */
    private static final String taskName = "ClusteredContext";

    /**
     * Field for extra-info to designate this host.
     */
    private final String myName;

    /**
     * Constructor that takes an {@code ExecutorService}. The
     * {@code ExecutorService} is used for starting/stopping dependent contexts.
     * 
     * @param service
     */
    public ClusteredContextStateManager(ExecutorService service) {
        super(service);
        myName = SystemUtil.getHostName() + ":"
                + System.getProperty("edex.run.mode");
    }

    /**
     * Get the {@code ClusterLock} details field.
     * 
     * @param context
     * @return
     */
    protected static String getLockDetails(CamelContext context) {
        return context.getName() + ClusterLockUtils.CLUSTER_SUFFIX;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.esb.camel.context.DependencyContextStateManager#
     * isContextStartable(org.apache.camel.CamelContext)
     */
    @Override
    public boolean isContextStartable(CamelContext context) throws Exception {
        boolean canStartContext = super.isContextStartable(context);

        /*
         * Check cluster lock if we can start the context or if context is
         * already started in case we need to update the cluster lock.
         */
        if (canStartContext || context.getStatus().isStarted()) {
            ClusterTask lock = ClusterLockUtils.lock(taskName,
                    getLockDetails(context), myName, ContextManager
                            .getInstance().getTimeOutMillis(), false);

            switch (lock.getLockState()) {
            case ALREADY_RUNNING:
                // check if we already have lock
                canStartContext = lock.getExtraInfo().equals(myName);
                if (canStartContext) {
                    // update the lock time
                    ClusterLockUtils
                            .updateLockTime(taskName, getLockDetails(context),
                                    System.currentTimeMillis());
                }
                break;
            case SUCCESSFUL:
                canStartContext = true;
                break;

            default:
                canStartContext = false;
            }
        }

        return canStartContext;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.esb.camel.context.DependencyContextStateManager#
     * stopContext(org.apache.camel.CamelContext)
     */
    @Override
    public boolean stopContext(CamelContext context) throws Exception {
        // on stop, unlock the cluster lock if we own it
        String contextName = context.getName()
                + ClusterLockUtils.CLUSTER_SUFFIX;
        ClusterTask lock = ClusterLockUtils.lookupLock(taskName, contextName);
        if (lock.getExtraInfo().equals(myName) && lock.isRunning()) {
            ClusterLockUtils.unlock(taskName, contextName);
        }

        return super.stopContext(context);
    }
}
