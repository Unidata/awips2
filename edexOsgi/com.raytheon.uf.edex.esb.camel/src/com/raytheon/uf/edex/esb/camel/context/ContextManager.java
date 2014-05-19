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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.naming.ConfigurationException;

import org.apache.camel.CamelContext;
import org.apache.camel.Route;
import org.apache.camel.impl.DefaultCamelContext;
import org.apache.camel.model.RouteDefinition;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.edex.core.IContextStateProcessor;

/**
 * Tracks all contexts and is used to auto determine context dependencies and
 * start/stop them in the right order. Dynamically starts/stops a clustered
 * context and its associated routes so that only one context in the cluster is
 * running. This should mainly be used for reading from topics so that only box
 * is processing the topic data in the cluster for singleton type events.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2010 5050       rjpeter     Initial creation.
 * May 13, 2013 1989       njensen     Camel 2.11 compatibility.
 * Mar 11, 2014 2726       rjpeter     Implemented graceful shutdown.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ContextManager implements ApplicationContextAware,
        BeanFactoryPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContextManager.class);

    private static ContextManager instance = new ContextManager();

    /**
     * Endpoint types that are internal only. Mainly used at shutdown time to
     * designate routes that shouldn't be shutdown immediately.
     */
    private static final Set<String> INTERNAL_ENDPOINT_TYPES;

    static {
        HashSet<String> set = new HashSet<String>(
                ContextDependencyMapping.DEPENDENCY_ENDPOINT_TYPES);
        set.add("timer");
        set.add("quartz");
        set.add("direct");
        INTERNAL_ENDPOINT_TYPES = Collections.unmodifiableSet(set);
    }

    /**
     * Service used for start up and shut down threading.
     */
    private final ExecutorService service = Executors.newCachedThreadPool();

    private final Set<CamelContext> clusteredContexts = new HashSet<CamelContext>();

    /**
     * State Manager for all contexts that are not clustered.
     */
    private final IContextStateManager defaultStateManager = new DependencyContextStateManager(
            service);

    /**
     * State Manager used for all clustered contexts.
     */
    private final IContextStateManager clusteredStateManager = new ClusteredContextStateManager(
            service);

    /**
     * Spring context. Set by the spring container after bean construction.
     */
    private ApplicationContext springCtx = null;

    /**
     * Map of context processors that have been registered for a given context.
     * Used to allow contexts to do custom worn on startup/shutdown.
     */
    private final Map<CamelContext, IContextStateProcessor> contextProcessors = new HashMap<CamelContext, IContextStateProcessor>();

    /**
     * Cluster lock timeout for clustered contexts.
     */
    private int timeOutMillis;

    /**
     * Parsed context data for all contexts known in the spring container.
     */
    private volatile ContextData contextData;

    /**
     * Flag to control shutting down the jvm. This handles shutdown being called
     * during startup to short circuit startup.
     */
    private final AtomicBoolean shuttingDown = new AtomicBoolean(false);

    /**
     * Dependency mappings for all camel contexts in the spring container. This
     * should only be changed in a sync block. Otherwise mark as volatile.
     */
    private ContextDependencyMapping dependencyMapping = null;

    /**
     * Last time dependency mapping was generated. Used to periodically
     * regenerate the dependency mappings.
     */
    private long lastDependencyTime = 0;

    public static ContextManager getInstance() {
        return instance;
    }

    /**
     * Private constructor.
     */
    private ContextManager() {
    }

    /**
     * Returns a set of endpoint types that are considered internal for routing
     * purposes.
     * 
     * @return
     */
    public Set<String> getInternalEndpointTypes() {
        return INTERNAL_ENDPOINT_TYPES;
    }

    /**
     * Gets the context data.
     * 
     * @return
     * @throws ConfigurationException
     */
    public ContextData getContextData() throws ConfigurationException {
        if (contextData == null) {
            synchronized (this) {
                if (contextData == null) {
                    contextData = new ContextData(new ArrayList<CamelContext>(
                            springCtx.getBeansOfType(CamelContext.class)
                                    .values()));
                }
            }
        }

        return contextData;
    }

    /**
     * Get the {@link IContextStateManager} for the passed {@code CamelContext}.
     * 
     * @param context
     * @return
     */
    protected IContextStateManager getStateManager(CamelContext context) {
        if (clusteredContexts.contains(context)) {
            return clusteredStateManager;
        }

        return defaultStateManager;
    }

    /**
     * Get the {@link IContextStateProcessor} for the passed
     * {@code CamelContext}.
     * 
     * @param context
     * @return
     */
    public IContextStateProcessor getStateProcessor(CamelContext context) {
        return contextProcessors.get(context);
    }

    /**
     * Get the {@link ContextDependencyMapping} for all contexts.
     * 
     * @param suppressExceptions
     * @return
     * @throws ConfigurationException
     */
    public ContextDependencyMapping getDependencyMapping(
            boolean suppressExceptions) throws ConfigurationException {
        /*
         * This is not permanently cashed and regenerated periodically since
         * routing via code can change at runtime.
         */
        synchronized (this) {
            long millis = System.currentTimeMillis();
            if ((dependencyMapping == null)
                    || (millis > (lastDependencyTime + (3 * TimeUtil.MILLIS_PER_MINUTE)))) {
                lastDependencyTime = millis;
                dependencyMapping = new ContextDependencyMapping(
                        getContextData(), suppressExceptions);
            }
        }

        return dependencyMapping;

    }

    /**
     * Starts all routes for all contexts. If a route fails to start the entire
     * jvm will be shutdown.
     */
    public void startContexts() {
        statusHandler.info("Context Manager starting contexts");

        try {
            ContextData cxtData = getContextData();

            for (final CamelContext context : cxtData.getContexts()) {
                /*
                 * Enforce startup order so that internal endpoints start first
                 * and shutdown last. Each route must have a unique number under
                 * 1000. Camel documentation doesn't state if numbers can be
                 * negative or not. Order is reverse of how they are found in
                 * the file with internal types going first followed by external
                 * types.
                 */
                int externalCount = 999;
                int internalCount = externalCount - context.getRoutes().size();

                for (Route route : context.getRoutes()) {
                    String uri = route.getEndpoint().getEndpointUri();
                    Pair<String, String> typeAndName = ContextData
                            .getEndpointTypeAndName(uri);
                    String type = typeAndName.getFirst();
                    RouteDefinition def = route.getRouteContext().getRoute();

                    if (INTERNAL_ENDPOINT_TYPES.contains(type)) {
                        def.setStartupOrder(internalCount--);
                    } else {
                        def.setStartupOrder(externalCount--);
                    }
                }
            }

            List<Future<Pair<CamelContext, Boolean>>> callbacks = new LinkedList<Future<Pair<CamelContext, Boolean>>>();
            for (final CamelContext context : cxtData.getContexts()) {
                final IContextStateManager stateManager = getStateManager(context);
                if (stateManager.isContextStartable(context)) {
                    /*
                     * Have the ExecutorService start the context to allow for
                     * quicker startup.
                     */
                    callbacks
                            .add(service
                                    .submit(new Callable<Pair<CamelContext, Boolean>>() {
                                        @Override
                                        public Pair<CamelContext, Boolean> call()
                                                throws Exception {
                                            boolean rval = false;
                                            try {
                                                rval = stateManager
                                                        .startContext(context);

                                                if (!rval) {
                                                    statusHandler.error("Context ["
                                                            + context.getName()
                                                            + "] failed to start, shutting down");
                                                    System.exit(1);
                                                }
                                            } catch (Throwable e) {
                                                statusHandler.fatal(
                                                        "Error occurred starting context: "
                                                                + context
                                                                        .getName(),
                                                        e);
                                                System.exit(1);
                                            }

                                            return new Pair<CamelContext, Boolean>(
                                                    context, rval);
                                        }
                                    }));
                }
            }

            /*
             * Double check call backs that everything started successfully. If
             * some did not start successfully, force shutdown.
             */
            for (Future<Pair<CamelContext, Boolean>> callback : callbacks) {
                Pair<CamelContext, Boolean> val = callback.get();
                if (!val.getSecond().booleanValue()) {
                    statusHandler.error("Context [" + val.getFirst().getName()
                            + "] failed to start, shutting down");
                    System.exit(1);
                }
            }

        } catch (Throwable e) {
            statusHandler.fatal(
                    "Error occurred starting contexts, shutting down", e);
            System.exit(1);
        }
    }

    /**
     * Register a clustered context that is meant to run as a singleton in the
     * cluster.
     * 
     * @param context
     * @return
     */
    public ContextManager registerClusteredContext(final CamelContext context) {
        clusteredContexts.add(context);
        return this;
    }

    /**
     * Register a context state processor to be called on start/stop of the
     * context.
     * 
     * @param context
     * @param processor
     * @return
     */
    public ContextManager registerContextStateProcessor(
            final CamelContext context, final IContextStateProcessor processor) {
        contextProcessors.put(context, processor);
        return this;
    }

    /**
     * Stops all contexts. Note this method can only be called once for the life
     * of the jvm and will gracefully shut down all of camel.
     */
    public void stopContexts() {
        /*
         * flag to ensure no one else runs shutdown also stops
         * checkClusteredContext from starting contexts once shutdown has been
         * initiated
         */
        if (shuttingDown.compareAndSet(false, true)) {
            if (springCtx == null) {
                statusHandler
                        .info("Spring Context not set.  Start up never completed, cannot orderly shutdown");
            }

            statusHandler.info("Context Manager stopping contexts");

            try {
                ContextData ctxData = getContextData();
                List<CamelContext> contexts = ctxData.getContexts();
                List<Future<Pair<CamelContext, Boolean>>> callbacks = new LinkedList<Future<Pair<CamelContext, Boolean>>>();

                for (final CamelContext context : contexts) {
                    callbacks.add(service.submit(new StopContext(context)));
                }

                List<CamelContext> failures = waitForCallbacks(callbacks,
                        "Waiting for contexts to shutdown: ", 1000);

                for (CamelContext failure : failures) {
                    statusHandler.error("Context [" + failure.getName()
                            + "] had a failure trying to stop");
                }
            } catch (Throwable e) {
                statusHandler.error("Error occurred during shutdown", e);
            }
        }
    }

    /**
     * Private Callable for stopping a context. If context not immediately
     * stoppable will instead shutdown external routes first.
     */
    private class StopContext implements Callable<Pair<CamelContext, Boolean>> {
        final CamelContext context;

        private StopContext(CamelContext context) {
            this.context = context;
        }

        @Override
        public Pair<CamelContext, Boolean> call() throws Exception {
            boolean rval = false;
            IContextStateManager stateManager = getStateManager(context);

            if (stateManager.isContextStoppable(context)) {
                try {
                    statusHandler.info("Stopping context [" + context.getName()
                            + "]");
                    rval = stateManager.stopContext(context);

                    if (!rval) {
                        statusHandler.error("Context [" + context.getName()
                                + "] failed to stop");
                    }
                } catch (Throwable e) {
                    statusHandler.fatal("Error occurred stopping context: "
                            + context.getName(), e);
                }
            } else {
                /*
                 * context not immediately stoppable, begin shutting down
                 * external routes instead
                 */
                List<Route> routes = context.getRoutes();
                rval = true;

                for (Route route : routes) {
                    String uri = route.getEndpoint().getEndpointUri();
                    Pair<String, String> typeAndName = ContextData
                            .getEndpointTypeAndName(uri);
                    String type = typeAndName.getFirst();
                    if (!INTERNAL_ENDPOINT_TYPES.contains(type)) {
                        try {
                            statusHandler.info("Stopping route ["
                                    + route.getId() + "]");
                            rval &= stateManager.stopRoute(route);
                        } catch (Exception e) {
                            statusHandler.error(
                                    "Error occurred Stopping route: "
                                            + route.getId(), e);
                        }
                    }
                }
            }

            return new Pair<CamelContext, Boolean>(context, rval);
        }
    }

    /**
     * Waits for all callbacks to finish printing a periodic message with number
     * of remaining callbacks. Returns a list of contexts that had a failure
     * status.
     * 
     * @param callbacks
     * @param message
     * @param sleepInterval
     * @return
     */
    private static List<CamelContext> waitForCallbacks(
            List<Future<Pair<CamelContext, Boolean>>> callbacks,
            String message, long sleepInterval) {
        statusHandler.info(message + callbacks.size() + " remaining");
        List<CamelContext> failures = new LinkedList<CamelContext>();

        while (!callbacks.isEmpty()) {
            boolean foundOne = false;

            Iterator<Future<Pair<CamelContext, Boolean>>> callbackIter = callbacks
                    .iterator();
            while (callbackIter.hasNext()) {
                Future<Pair<CamelContext, Boolean>> callback = callbackIter
                        .next();
                if (callback.isDone()) {
                    foundOne = true;
                    callbackIter.remove();
                    try {
                        Pair<CamelContext, Boolean> val = callback.get();
                        if (!val.getSecond().booleanValue()) {
                            failures.add(val.getFirst());
                        }
                    } catch (Exception e) {
                        statusHandler.error("Failure in callback task", e);
                    }
                }
            }

            if (!foundOne) {
                statusHandler.info(message + callbacks.size() + " remaining");
                try {
                    Thread.sleep(sleepInterval);
                } catch (Exception e) {
                    // ignore
                }
            }
        }

        return failures;
    }

    /**
     * Checks the clustered contexts. If context is not running in the cluster
     * the context will be started.
     */
    public void checkClusteredContexts() {
        if (!shuttingDown.get()) {
            for (CamelContext camelContext : clusteredContexts) {
                boolean activateRoute = true;
                try {
                    IContextStateManager stateManager = getStateManager(camelContext);

                    if (stateManager.isContextStartable(camelContext)) {
                        stateManager.startContext(camelContext);
                    } else if (stateManager.isContextStoppable(camelContext)) {
                        activateRoute = false;
                        stateManager.stopContext(camelContext);
                    }
                } catch (Exception e) {
                    StringBuilder msg = new StringBuilder();
                    msg.append("Failed to ");
                    if (activateRoute) {
                        msg.append("start ");
                    } else {
                        msg.append("stop ");
                    }
                    msg.append("context ");
                    msg.append(camelContext.getName());
                    statusHandler.handle(Priority.ERROR, msg.toString(), e);
                }
            }
        }
    }

    @Override
    public void setApplicationContext(ApplicationContext context)
            throws BeansException {
        springCtx = context;
    }

    public int getTimeOutMillis() {
        return timeOutMillis;
    }

    public void setTimeOutMillis(int timeOutMillis) {
        this.timeOutMillis = timeOutMillis;
    }

    public boolean isShuttingDown() {
        return shuttingDown.get();
    }

    /**
     * Update all camel beans to have autoStartup to false and handles quartz
     * workaround when JMX is disabled.
     */
    @Override
    public void postProcessBeanFactory(
            ConfigurableListableBeanFactory beanFactory) throws BeansException {
        for (CamelContext ctx : beanFactory.getBeansOfType(CamelContext.class)
                .values()) {
            /*
             * set contexts to not auto start to enforce dependency order
             * correctly.
             */
            ctx.setAutoStartup(false);

            /*
             * Quartz work around to set management name.
             * 
             * TODO: Remove with camel 2.12.3 upgrade:
             * https://issues.apache.org/jira/browse/CAMEL-7132
             */
            if ((ctx instanceof DefaultCamelContext)
                    && (ctx.getManagementName() == null)) {
                ((DefaultCamelContext) ctx).setManagementName(ctx.getName());
            }
        }
    }
}
