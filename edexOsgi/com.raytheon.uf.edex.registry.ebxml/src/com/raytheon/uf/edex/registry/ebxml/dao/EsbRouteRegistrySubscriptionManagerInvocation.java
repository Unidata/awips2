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
package com.raytheon.uf.edex.registry.ebxml.dao;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.registry.ebxml.services.IRegistrySubscriptionManager;

/**
 * Sends a request to the esb route to request that registry subscriptions are
 * processed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1914       djohnson     Extracted from AuditableEventTypeDao.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EsbRouteRegistrySubscriptionManagerInvocation implements
        IRegistrySubscriptionManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EsbRouteRegistrySubscriptionManagerInvocation.class);

    private static final String ESB_ROUTE = "vm:processSubscriptions";

    /**
     * {@inheritDoc}
     */
    @Override
    public void processSubscriptions() {
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(
                    ESB_ROUTE, null);
        } catch (EdexException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to post message to esb route to process registry subscriptions!",
                            e);
        }
    }

}
