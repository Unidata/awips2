package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

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

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.springframework.stereotype.Service;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;

/**
 * Provider Retrieval Handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky     Initial creation
 * Aug 09, 2012 1022       djohnson    Use {@link ExecutorService} for retrieval.
 * Mar 04, 2013 1647       djohnson    RetrievalTasks are now scheduled via constructor parameter.
 * Mar 27, 2013 1802       bphillip    Scheduling of retrieval tasks now occurs after camel/spring have been initialized
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@Service
public class RetrievalHandler implements RegistryInitializedListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalHandler.class);

    private final ScheduledExecutorService executorService;

    private final List<RetrievalTask> retrievalTasks;

    private IRetrievalDao retrievalDao;

    private IDuration retrievalTaskFrequency;

    private IDuration subnotifyTaskFrequency;

    private SubscriptionNotifyTask subNotifyTask;

    public RetrievalHandler(ScheduledExecutorService executorService,
            IRetrievalDao retrievalDao, List<RetrievalTask> retrievalTasks,
            SubscriptionNotifyTask subNotifyTask,
            IDuration retrievalTaskFrequency, IDuration subnotifyTaskFrequency) {
        this.executorService = executorService;
        this.retrievalTasks = retrievalTasks;
        this.retrievalDao = retrievalDao;
        this.retrievalTaskFrequency = retrievalTaskFrequency;
        this.subnotifyTaskFrequency = subnotifyTaskFrequency;
        this.subNotifyTask = subNotifyTask;
    }

    public void notify(List<String> subscriptions) {
        statusHandler.debug("Notifying that subscriptions are available.");

        for (RetrievalTask retrievalTask : retrievalTasks) {
            executorService.execute(retrievalTask);
        }
    }

    @Override
    public void executeAfterRegistryInit() {
        // set all Running state retrievals to pending
        retrievalDao.resetRunningRetrievalsToPending();

        for (RetrievalTask retrievalTask : retrievalTasks) {
            executorService.scheduleWithFixedDelay(retrievalTask, 30000,
                    retrievalTaskFrequency.getMillis(), TimeUnit.MILLISECONDS);
        }
        executorService.scheduleWithFixedDelay(subNotifyTask, 30000,
                subnotifyTaskFrequency.getMillis(), TimeUnit.MILLISECONDS);

    }
}
