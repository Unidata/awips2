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
package com.raytheon.uf.edex.esb.camel.cluster.quartz;

import org.apache.camel.Exchange;
import org.apache.camel.component.quartz.QuartzEndpoint;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ClusteredQuartzEndpoint extends QuartzEndpoint {

    private static final String TASK = "ClusteredQuartz";

    protected transient Log logger = LogFactory
            .getLog(ClusteredQuartzEndpoint.class);

    public ClusteredQuartzEndpoint(String uri,
            ClusteredQuartzComponent clusteredQuartzComponent) {
        super(uri, clusteredQuartzComponent);
    }

    @Override
    public void onJobExecute(final JobExecutionContext jobExecutionContext)
            throws JobExecutionException {
        String jName = jobExecutionContext.getJobDetail().getName();
        long period = Math.abs(jobExecutionContext.getFireTime().getTime()
                - jobExecutionContext.getNextFireTime().getTime()) / 2;
        ClusterTask ct = ClusterLockUtils.lock(TASK, jName, period, false);

        if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
            // ClusterLockUtils.unlock(ct, false);
            super.onJobExecute(jobExecutionContext);
        }
    }

    @Override
    public Exchange createExchange(final JobExecutionContext jobExecutionContext) {
        Exchange exchange = createExchange();
        exchange.setIn(new ClusteredQuartzMessage(exchange, jobExecutionContext));
        return exchange;
    }
}
