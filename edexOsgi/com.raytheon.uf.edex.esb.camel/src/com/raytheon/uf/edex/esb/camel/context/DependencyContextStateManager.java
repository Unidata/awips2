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

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import org.apache.camel.CamelContext;
import org.apache.camel.Route;

/**
 * Implementation of IContextStateManager that handles dependencies between
 * contexts so that contexts start/stop in the correct order. Can be given an
 * ExecutorService to use to start/stop dependent contexts.
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
public class DependencyContextStateManager extends DefaultContextStateManager {

    /**
     * Service to use to start/stop dependent contexts. If null, context
     * processing will happen on current thread.
     */
    protected final ExecutorService service;

    public DependencyContextStateManager() {
        this(null);
    }

    public DependencyContextStateManager(ExecutorService service) {
        this.service = service;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.esb.camel.context.DefaultContextStateManager#
     * isContextStartable(org.apache.camel.CamelContext)
     */
    @Override
    public boolean isContextStartable(CamelContext context) throws Exception {
        if (!super.isContextStartable(context)) {
            return false;
        }

        Set<CamelContext> requiredContexts = ContextManager.getInstance()
                .getDependencyMapping(false).getRequiredContexts(context);

        if (requiredContexts != null) {
            for (CamelContext rContext : requiredContexts) {
                if (!rContext.getStatus().isStarted()) {
                    return false;
                } else {
                    for (Route rRoute : rContext.getRoutes()) {
                        if (!rContext.getRouteStatus(rRoute.getId())
                                .isStarted()) {
                            return false;
                        }
                    }
                }
            }
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.esb.camel.context.DefaultContextStateManager#
     * startContext(org.apache.camel.CamelContext)
     */
    @Override
    public boolean startContext(CamelContext context) throws Exception {
        boolean rval = super.startContext(context);
        ContextManager ctxMgr = ContextManager.getInstance();

        if (rval) {
            Set<CamelContext> dContexts = ctxMgr.getDependencyMapping(false)
                    .getDependentContexts(context);
            if (dContexts != null) {
                List<Future<Boolean>> callbacks = null;

                for (final CamelContext dCtx : dContexts) {
                    final IContextStateManager stateMgr = ctxMgr
                            .getStateManager(dCtx);
                    if (stateMgr.isContextStartable(dCtx)) {
                        if (service != null) {
                            if (callbacks == null) {
                                callbacks = new LinkedList<Future<Boolean>>();
                            }

                            callbacks.add(service
                                    .submit(new Callable<Boolean>() {
                                        @Override
                                        public Boolean call() throws Exception {
                                            return stateMgr.startContext(dCtx);
                                        }
                                    }));
                        } else {
                            stateMgr.startContext(dCtx);
                        }
                    }
                }

                if (callbacks != null) {
                    for (Future<Boolean> callback : callbacks) {
                        rval &= callback.get().booleanValue();
                    }
                }
            }
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.esb.camel.context.DefaultContextStateManager#
     * isContextStoppable(org.apache.camel.CamelContext)
     */
    @Override
    public boolean isContextStoppable(CamelContext context) throws Exception {
        if (!super.isContextStoppable(context)) {
            return false;
        }

        Set<CamelContext> dContexts = ContextManager.getInstance()
                .getDependencyMapping(true).getDependentContexts(context);

        if (dContexts != null) {
            for (CamelContext dContext : dContexts) {
                /*
                 * only need to check if the context has stopped, can't have a
                 * stopped context with started routes.
                 */
                if (!dContext.getStatus().isStopped()) {
                    return false;
                }
            }
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.esb.camel.context.DefaultContextStateManager#stopContext
     * (org.apache.camel.CamelContext)
     */
    @Override
    public boolean stopContext(CamelContext context) throws Exception {
        boolean rval = super.stopContext(context);
        ContextManager ctxMgr = ContextManager.getInstance();

        if (rval) {
            Set<CamelContext> rContexts = ctxMgr.getDependencyMapping(true)
                    .getRequiredContexts(context);
            if (rContexts != null) {
                List<Future<Boolean>> callbacks = null;

                for (final CamelContext rCtx : rContexts) {
                    final IContextStateManager stateMgr = ctxMgr
                            .getStateManager(rCtx);
                    if (stateMgr.isContextStoppable(rCtx)) {
                        if (service != null) {
                            if (callbacks == null) {
                                callbacks = new LinkedList<Future<Boolean>>();
                            }

                            callbacks.add(service
                                    .submit(new Callable<Boolean>() {
                                        @Override
                                        public Boolean call() throws Exception {
                                            return stateMgr.stopContext(rCtx);
                                        }
                                    }));
                        } else {
                            stateMgr.stopContext(rCtx);
                        }
                    }
                }

                if (callbacks != null) {
                    for (Future<Boolean> callback : callbacks) {
                        rval &= callback.get().booleanValue();
                    }
                }
            }
        }

        return rval;
    }
}
