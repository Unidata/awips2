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
package com.raytheon.uf.edex.event;

import org.junit.Ignore;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.event.IEventBusHandler;
import com.raytheon.uf.edex.event.NonTransactionalSynchronousEventBusHandler.SynchronousEventBusFactory;

/**
 * {@link IEventBusHandler} which uses a synchronous {@link EventBus} that
 * participates in transactions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2013 1802       djohnson    Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Ignore
public class TransactionalSynchronousEventBusHandler extends EdexEventBusHandler {

    public TransactionalSynchronousEventBusHandler() {
        super(new SynchronousEventBusFactory());
    }

}
