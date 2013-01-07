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
package com.raytheon.uf.edex.registry.ebxml.services.util;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Transaction;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.session.SessionContext;
import com.raytheon.uf.edex.event.EventBus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;

/**
 * A {@link SessionContext} that stores the transaction and events for the
 * registry interactions within a {@link Thread}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2012 1187       djohnson     Moved in from {@link RegistrySessionManager}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RegistrySessionContext implements SessionContext {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySessionContext.class);

    private final List<Event> events = new ArrayList<Event>();

    private final Transaction transaction = new RegistryObjectTypeDao()
            .getSessionFactory().getCurrentSession().beginTransaction();

    /**
     * {@inheritDoc}
     */
    @Override
    public void open() {
        // The transaction is actually opened upon construction, but this is
        // good enough to log it
        statusHandler.info(">>>>>>>> Hibernate Transactional Session Opened");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        if (transaction.isActive()) {
            try {
                transaction.commit();

                // Now that the Objects are persisted in the database, send
                // the notifications to the other components that might be
                // looking for them.
                EventBus eventBus = EventBus.getInstance();
                for (Event event : events) {
                    eventBus.publish(event);
                }

                statusHandler
                        .info(">>>>>>>> Hibernate Transactional Session Closed, ["
                                + events.size() + "] Events published.");
            } catch (HibernateException e) {
                statusHandler.error("Error committing transaction.", e);
            }
        } else {
            statusHandler
                    .warn("Transaction is no longer active due to previous errors");
        }
    }

    /**
     * Add the posting of an event to the current, active Session. Any attempt
     * to add an event to an inactive Session will result in an
     * IllegalStateException.
     * 
     * @param event
     *            The event Object to post when the transaction successfully
     *            completes.
     * 
     * @throws IllegalStateException
     *             If an attempt is made to add an event to an inactive Session.
     */
    public void postEvent(Event event)
            throws IllegalStateException {
        if (!transaction.isActive()) {
            throw new IllegalStateException(
                    "Attempt to add event to an inactive transaction.");
        }
        events.add(event);
    }
}
