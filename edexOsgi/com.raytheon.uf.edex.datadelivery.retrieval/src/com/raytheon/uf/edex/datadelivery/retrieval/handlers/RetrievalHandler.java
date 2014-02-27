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

import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.springframework.stereotype.Service;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
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

    private final ScheduledExecutorService scheduledExecutorService;

    private IRetrievalDao retrievalDao;

    private IDuration subnotifyTaskFrequency;

    private SubscriptionNotifyTask subNotifyTask;

    private Map<String, RetrievalTaskFactory> taskFactories;

    public RetrievalHandler(ScheduledExecutorService scheduledExecutorService,
            IRetrievalDao retrievalDao, SubscriptionNotifyTask subNotifyTask,
            IDuration subnotifyTaskFrequency,
            Map<String, RetrievalTaskFactory> taskFactories) {
        this.scheduledExecutorService = scheduledExecutorService;
        this.retrievalDao = retrievalDao;
        this.subnotifyTaskFrequency = subnotifyTaskFrequency;
        this.subNotifyTask = subNotifyTask;
        this.taskFactories = taskFactories;
    }

    /**
     * Pull a SubscriptionRetrievalRequestWrapper off of the retrieval queue
     * Will be two types 1.) SBN retrievals will come in the form of XML
     * containing the data itself 2.) OPSNET retrievals will be the
     * RetrievalRequestRecordPK object
     * 
     * @param SubscriptionRetrievalRequestWrapper
     *            wrapper as byte array
     */
    public void notify(byte[] bytes) {
        
        SubscriptionRetrievalRequestWrapper srrw = null;

        try {
            srrw = SerializationUtil.transformFromThrift(
                    SubscriptionRetrievalRequestWrapper.class, bytes);

            if (srrw != null) {
                RetrievalTask task = getTasker(srrw);
                task.run();
            }

        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Can't deserialize RetrievalRequestWrapper!", e);
        }
    }

    @Override
    public void executeAfterRegistryInit() {
        // set all Running state retrievals to pending
        retrievalDao.resetRunningRetrievalsToPending();
        // run the sub notifier every 30 sec for notifications
        scheduledExecutorService.scheduleWithFixedDelay(subNotifyTask, 30000,
                subnotifyTaskFrequency.getMillis(), TimeUnit.MILLISECONDS);
    }
    
    /**
     * Get a RetrievalTask for the given network
     * @param network
     * @return RetrievalTask
     */
    private RetrievalTask getTasker(SubscriptionRetrievalRequestWrapper wrapper) {

        return taskFactories.get(wrapper.getNetwork().name()).create(wrapper);
    }

}
