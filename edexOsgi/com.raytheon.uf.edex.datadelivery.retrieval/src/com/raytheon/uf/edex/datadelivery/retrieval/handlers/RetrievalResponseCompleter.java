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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;

/**
 * Default implementation of {@link IRetrievalResponseCompleter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalResponseCompleter implements IRetrievalResponseCompleter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalResponseCompleter.class);

    private final SubscriptionNotifyTask notifyTask;

    private final IRetrievalDao dao;

    public RetrievalResponseCompleter(SubscriptionNotifyTask notifyTask,
            IRetrievalDao dao) {
        this.notifyTask = notifyTask;
        this.dao = dao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void completeRetrieval(RetrievalRequestRecord retrieval,
            IRetrievalResponseStatus status) {
        RetrievalRequestRecord.State state = status.isSucceeded() ? RetrievalRequestRecord.State.COMPLETED
                : RetrievalRequestRecord.State.FAILED;
        retrieval.setState(state);

        // update database
        try {
            dao.completeRetrievalRequest(retrieval);
            notifyTask.checkNotify(retrieval);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Unable to communicate with the database", e);
        }
    }

}
