package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;

/**
 * 
 * Manages the retrieval agents.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2012 0726       djohnson     Add SW history, prevent duplicate start calls, add generics.
 * Oct 29, 2012 1286       djohnson     Only start threads if bandwidth management enabled.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class RetrievalAgentManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalAgentManager.class);

    /**
     * The contents of this Map will be injected
     */
    private final Map<String, RetrievalAgent<?>> agents;

    private final Object notifier;

    private volatile boolean started;

    /**
     * Constructor.
     * 
     * @param agents
     *            the retrieval agents
     * @param path
     */
    public RetrievalAgentManager(final Object notifier,
            final Map<String, RetrievalAgent<?>> agents) {
        this.notifier = notifier;
        this.agents = agents;

        // set all Running state retrievals to pending
        RetrievalDao dao = new RetrievalDao();
        dao.resetRunningRetrievalsToPending();
    }

    public void start() {
        if (!started) {
            for (RetrievalAgent<?> agent : agents.values()) {
                agent.start();
            }
        } else {
            statusHandler
                    .warn("start() has already been called, ignoring further requests!");
        }
    }

    public void wake() {
        synchronized (notifier) {
            statusHandler.info("Waking up retrieval threads");
            notifier.notifyAll();
        }
    }
}
