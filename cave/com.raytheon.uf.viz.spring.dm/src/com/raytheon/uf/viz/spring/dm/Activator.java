package com.raytheon.uf.viz.spring.dm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Timer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.SynchronousBundleListener;
import org.osgi.framework.Version;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.core.CollectionFactory;
import org.springframework.core.task.SyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.osgi.context.ConfigurableOsgiBundleApplicationContext;
import org.springframework.osgi.context.DelegatedExecutionOsgiBundleApplicationContext;
import org.springframework.osgi.context.event.OsgiBundleApplicationContextEventMulticaster;
import org.springframework.osgi.context.event.OsgiBundleApplicationContextListener;
import org.springframework.osgi.extender.OsgiApplicationContextCreator;
import org.springframework.osgi.extender.internal.activator.ContextLoaderListener;
import org.springframework.osgi.extender.internal.dependencies.shutdown.BundleDependencyComparator;
import org.springframework.osgi.extender.internal.dependencies.shutdown.ComparatorServiceDependencySorter;
import org.springframework.osgi.extender.internal.dependencies.shutdown.ServiceDependencySorter;
import org.springframework.osgi.extender.internal.dependencies.startup.DependencyWaiterApplicationContextExecutor;
import org.springframework.osgi.extender.internal.support.NamespaceManager;
import org.springframework.osgi.extender.internal.support.OsgiBeanFactoryPostProcessorAdapter;
import org.springframework.osgi.extender.internal.util.concurrent.Counter;
import org.springframework.osgi.extender.internal.util.concurrent.RunnableTimedExecution;
import org.springframework.osgi.extender.support.ApplicationContextConfiguration;
import org.springframework.osgi.extender.support.internal.ConfigUtils;
import org.springframework.osgi.service.importer.support.Cardinality;
import org.springframework.osgi.service.importer.support.CollectionType;
import org.springframework.osgi.service.importer.support.OsgiServiceCollectionProxyFactoryBean;
import org.springframework.osgi.util.OsgiBundleUtils;
import org.springframework.osgi.util.OsgiStringUtils;

/**
 * 
 * Custom version of spring osgi ContextLoaderListener to turn off xml
 * validation
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class Activator implements BundleActivator {

    /**
     * Common base class for {@link ContextLoaderListener} listeners.
     * 
     * @author Costin Leau
     */
    private abstract class BaseListener implements SynchronousBundleListener {

        /**
         * A bundle has been started, stopped, resolved, or unresolved. This
         * method is a synchronous callback, do not do any long-running work in
         * this thread.
         * 
         * @see org.osgi.framework.SynchronousBundleListener#bundleChanged
         */
        public void bundleChanged(BundleEvent event) {

            boolean trace = log.isTraceEnabled();

            // check if the listener is still alive
            synchronized (monitor) {
                if (isClosed) {
                    if (trace)
                        log.trace("Listener is closed; events are being ignored");
                    return;
                }
            }
            if (trace) {
                log.debug("Processing bundle event ["
                        + OsgiStringUtils.nullSafeToString(event)
                        + "] for bundle ["
                        + OsgiStringUtils.nullSafeSymbolicName(event
                                .getBundle()) + "]");
            }
            try {
                handleEvent(event);
            } catch (Exception ex) {
                /* log exceptions before swallowing */
                log.warn("Got exception while handling event " + event, ex);
            }
        }

        protected abstract void handleEvent(BundleEvent event);
    }

    /**
     * Bundle listener used for detecting namespace handler/resolvers. Exists as
     * a separate listener so that it can be registered early to avoid race
     * conditions with bundles in INSTALLING state but still to avoid premature
     * context creation before the Spring {@link ContextLoaderListener} is not
     * fully initialized.
     * 
     * @author Costin Leau
     */
    private class NamespaceBundleLister extends BaseListener {

        protected void handleEvent(BundleEvent event) {

            Bundle bundle = event.getBundle();

            switch (event.getType()) {
            case BundleEvent.STARTED: {
                maybeAddNamespaceHandlerFor(bundle);
                break;
            }
            case BundleEvent.STOPPED: {
                maybeRemoveNameSpaceHandlerFor(bundle);
                break;
            }
            default:
                break;
            }
        }
    }

    /**
     * Bundle listener used for context creation/destruction.
     */
    private class ContextBundleListener extends BaseListener {

        protected void handleEvent(BundleEvent event) {

            Bundle bundle = event.getBundle();

            // ignore current bundle for context creation
            if (bundle.getBundleId() == bundleId) {
                return;
            }

            switch (event.getType()) {
            case BundleEvent.STARTED: {
                maybeCreateApplicationContextFor(bundle);
                break;
            }
            case BundleEvent.STOPPING: {
                if (OsgiBundleUtils.isSystemBundle(bundle)) {
                    if (log.isDebugEnabled()) {
                        log.debug("System bundle stopping");
                    }
                    // System bundle is shutting down; Special handling for
                    // framework shutdown
                    shutdown();
                } else {
                    maybeCloseApplicationContextFor(bundle);
                }
                break;
            }
            default:
                break;
            }
        }
    }

    private static final Log log = LogFactory
            .getLog(ContextLoaderListener.class);

    // "Spring Application Context Creation Timer"
    private Timer timer = new Timer(true);

    /** extender bundle id */
    private long bundleId;

    /** extender configuration */
    private ExtenderConfiguration extenderConfiguration;

    /**
     * The contexts we are currently managing. Keys are bundle ids, values are
     * ServiceDependentOsgiApplicationContexts for the application context
     */
    private final Map managedContexts;

    /** Task executor used for bootstraping the Spring contexts in async mode */
    private TaskExecutor taskExecutor;

    /** ApplicationContext Creator */
    private OsgiApplicationContextCreator contextCreator;

    /** BFPP list */
    private List postProcessors;

    /**
     * Task executor which uses the same thread for running tasks. Used when
     * doing a synchronous wait-for-dependencies.
     */
    private TaskExecutor sameThreadTaskExecutor = new SyncTaskExecutor();

    /** listener counter - used to properly synchronize shutdown */
    private Counter contextsStarted = new Counter("contextsStarted");

    /** Spring namespace/resolver manager */
    private NamespaceManager nsManager;

    /** The bundle's context */
    private BundleContext bundleContext;

    /** Bundle listener interested in context creation */
    private SynchronousBundleListener contextListener;

    /** Bundle listener interested in namespace resolvers/parsers discovery */
    private SynchronousBundleListener nsListener;

    /** Service-based dependency sorter for shutdown */
    private ServiceDependencySorter shutdownDependencySorter = new ComparatorServiceDependencySorter();

    /**
     * Monitor used for dealing with the bundle activator and synchronous bundle
     * threads
     */
    private transient final Object monitor = new Object();

    /**
     * flag indicating whether the context is down or not - useful during
     * shutdown
     */
    private boolean isClosed = false;

    /** This extender version */
    private Version extenderVersion;

    private OsgiBundleApplicationContextEventMulticaster multicaster;

    /** listeners interested in monitoring managed OSGi appCtxs */
    private List applicationListeners;

    /** dynamicList clean up hook */
    private DisposableBean applicationListenersCleaner;

    /** shutdown task executor */
    private TaskExecutor shutdownTaskExecutor;

    /** Required by the BundleActivator contract */
    public Activator() {
        this.managedContexts = CollectionFactory.createConcurrentMap(16);
    }

    /**
     * <p/>
     * Called by OSGi when this bundle is started. Finds all previously resolved
     * bundles and adds namespace handlers for them if necessary.
     * </p>
     * <p/>
     * Creates application contexts for bundles started before the extender was
     * started.
     * </p>
     * <p/>
     * Registers a namespace/entity resolving service for use by web app
     * contexts.
     * </p>
     * 
     * @see org.osgi.framework.BundleActivator#start
     */
    public void start(BundleContext context) throws Exception {

        this.bundleContext = context;
        this.bundleId = context.getBundle().getBundleId();

        this.extenderVersion = OsgiBundleUtils.getBundleVersion(context
                .getBundle());
        log.info("Starting [" + bundleContext.getBundle().getSymbolicName()
                + "] bundle v.[" + extenderVersion + "]");

        // Step 1 : discover existing namespaces (in case there are fragments
        // with custom XML definitions)
        nsManager = new NamespaceManager(context);

        // register listener first to make sure any bundles in INSTALLED state
        // are not lost
        nsListener = new NamespaceBundleLister();
        context.addBundleListener(nsListener);

        Bundle[] previousBundles = context.getBundles();

        for (int i = 0; i < previousBundles.length; i++) {
            Bundle bundle = previousBundles[i];
            if (OsgiBundleUtils.isBundleResolved(bundle)) {
                maybeAddNamespaceHandlerFor(bundle);
            }
        }

        // discovery finished, publish the resolvers/parsers in the OSGi space
        nsManager.afterPropertiesSet();

        // Step 2: initialize the extender configuration
        try {
            extenderConfiguration = new ExtenderConfiguration(context);
        } catch (Exception ex) {
            log.error("Unable to process extender configuration", ex);
            throw ex;
        }

        // initialize the configuration once namespace handlers have been
        // detected
        this.taskExecutor = extenderConfiguration.getTaskExecutor();
        this.shutdownTaskExecutor = extenderConfiguration
                .getShutdownTaskExecutor();

        this.contextCreator = extenderConfiguration.getContextCreator();
        this.postProcessors = extenderConfiguration.getPostProcessors();

        // init the OSGi event dispatch/listening system
        initListenerService();

        // Step 3: discover the bundles that are started
        // and require context creation

        // register the context creation listener
        contextListener = new ContextBundleListener();
        // listen to any changes in bundles
        context.addBundleListener(contextListener);
        // get the bundles again to get an updated view
        previousBundles = context.getBundles();

        // Instantiate all previously resolved bundles which are Spring
        // powered
        for (int i = 0; i < previousBundles.length; i++) {
            if (OsgiBundleUtils.isBundleActive(previousBundles[i])) {
                try {
                    maybeCreateApplicationContextFor(previousBundles[i]);
                } catch (Throwable e) {
                    log.warn(
                            "Cannot start bundle "
                                    + OsgiStringUtils
                                            .nullSafeSymbolicName(previousBundles[i])
                                    + " due to", e);
                }
            }
        }

    }

    /**
     * Called by OSGi when this bundled is stopped. Unregister the
     * namespace/entity resolving service and clear all state. No further
     * management of application contexts created by this extender prior to
     * stopping the bundle occurs after this point (even if the extender bundle
     * is subsequently restarted).
     * 
     * @see org.osgi.framework.BundleActivator#stop
     */
    public void stop(BundleContext context) throws Exception {
        shutdown();
    }

    /**
     * Shutdown the extender and all bundled managed by it. Shutdown of contexts
     * is in the topological order of the dependency graph formed by the service
     * references.
     */
    protected void shutdown() {
        synchronized (monitor) {
            // if already closed, bail out
            if (isClosed)
                return;
            else
                isClosed = true;
        }
        log.info("Stopping [" + bundleContext.getBundle().getSymbolicName()
                + "] bundle v.[" + extenderVersion + "]");

        // first stop the watchdog
        stopTimer();

        // remove the bundle listeners (we are closing down)
        if (contextListener != null) {
            bundleContext.removeBundleListener(contextListener);
            contextListener = null;
        }

        if (nsListener != null) {
            bundleContext.removeBundleListener(nsListener);
            nsListener = null;
        }

        // destroy bundles
        Bundle[] bundles = new Bundle[managedContexts.size()];

        int i = 0;
        for (Iterator it = managedContexts.values().iterator(); it.hasNext();) {
            ConfigurableOsgiBundleApplicationContext context = (ConfigurableOsgiBundleApplicationContext) it
                    .next();
            bundles[i++] = context.getBundle();
        }

        bundles = shutdownDependencySorter
                .computeServiceDependencyGraph(bundles);

        boolean debug = log.isDebugEnabled();

        StringBuffer buffer = new StringBuffer();

        if (debug) {
            buffer.append("Shutdown order is: {");
            for (i = 0; i < bundles.length; i++) {
                buffer.append("\nBundle [" + bundles[i].getSymbolicName() + "]");
                ServiceReference[] services = bundles[i].getServicesInUse();
                HashSet usedBundles = new HashSet();
                if (services != null) {
                    for (int j = 0; j < services.length; j++) {
                        if (BundleDependencyComparator
                                .isSpringManagedService(services[j])) {
                            Bundle used = services[j].getBundle();
                            if (!used.equals(bundleContext.getBundle())
                                    && !usedBundles.contains(used)) {
                                usedBundles.add(used);
                                buffer.append("\n  Using ["
                                        + used.getSymbolicName() + "]");
                            }
                        }

                    }
                }
            }
            buffer.append("\n}");
            log.debug(buffer);
        }

        final List taskList = new ArrayList(managedContexts.size());
        final List closedContexts = Collections
                .synchronizedList(new ArrayList());
        final Object[] contextClosingDown = new Object[1];

        for (i = 0; i < bundles.length; i++) {
            Long id = new Long(bundles[i].getBundleId());
            final ConfigurableOsgiBundleApplicationContext context = (ConfigurableOsgiBundleApplicationContext) managedContexts
                    .get(id);
            if (context != null) {
                closedContexts.add(context);
                // add a new runnable
                taskList.add(new Runnable() {

                    private final String toString = "Closing runnable for context "
                            + context.getDisplayName();

                    public void run() {
                        contextClosingDown[0] = context;
                        // eliminate context
                        closedContexts.remove(context);
                        if (log.isDebugEnabled())
                            log.debug("Closing appCtx "
                                    + context.getDisplayName());
                        context.close();
                    }

                    public String toString() {
                        return toString;
                    }
                });
            }
        }

        // tasks
        final Runnable[] tasks = (Runnable[]) taskList
                .toArray(new Runnable[taskList.size()]);

        // start the ripper >:)
        for (int j = 0; j < tasks.length; j++) {
            if (RunnableTimedExecution.execute(tasks[j],
                    extenderConfiguration.getShutdownWaitTime(),
                    shutdownTaskExecutor)) {
                if (debug) {
                    log.debug(contextClosingDown[0]
                            + " context did not close successfully; forcing shutdown...");
                }
            }
        }

        this.managedContexts.clear();
        // clear the namespace registry
        nsManager.destroy();

        // release listeners
        if (applicationListeners != null) {
            applicationListeners = null;
            try {
                applicationListenersCleaner.destroy();
            } catch (Exception ex) {
                log.warn(
                        "exception thrown while releasing OSGi event listeners",
                        ex);
            }
        }

        // release multicaster
        if (multicaster != null) {
            multicaster.removeAllListeners();
            multicaster = null;
        }

        // before bailing out; wait for the threads that might be left by
        // the task executor
        stopTaskExecutor();

        extenderConfiguration.destroy();
    }

    /**
     * Cancel any tasks scheduled for the timer.
     */
    private void stopTimer() {
        if (timer != null) {
            if (log.isDebugEnabled())
                log.debug("Canceling timer tasks");
            timer.cancel();
        }
        timer = null;
    }

    /**
     * Do some additional waiting so the service dependency listeners detect the
     * shutdown.
     */
    private void stopTaskExecutor() {
        boolean debug = log.isDebugEnabled();

        if (debug)
            log.debug("Waiting for " + contextsStarted
                    + " service dependency listener(s) to stop...");

        contextsStarted
                .waitForZero(extenderConfiguration.getShutdownWaitTime());

        if (!contextsStarted.isZero()) {
            if (debug)
                log.debug(contextsStarted.getValue()
                        + " service dependency listener(s) did not responded in time; forcing them to shutdown...");
            extenderConfiguration.setForceThreadShutdown(true);
        }

        else
            log.debug("All listeners closed");
    }

    /**
     * Utility method that does extender range versioning and approapriate
     * logging.
     * 
     * @param bundle
     */
    private boolean handlerBundleMatchesExtenderVersion(Bundle bundle) {
        if (!ConfigUtils.matchExtenderVersionRange(bundle, extenderVersion)) {
            if (log.isDebugEnabled())
                log.debug("Bundle ["
                        + OsgiStringUtils.nullSafeNameAndSymName(bundle)
                        + "] expects an extender w/ version["
                        + OsgiBundleUtils.getHeaderAsVersion(bundle,
                                ConfigUtils.EXTENDER_VERSION)
                        + "] which does not match current extender w/ version["
                        + extenderVersion
                        + "]; skipping bundle from handler detection");
            return false;
        }
        return true;
    }

    private void maybeAddNamespaceHandlerFor(Bundle bundle) {
        if (handlerBundleMatchesExtenderVersion(bundle))
            nsManager.maybeAddNamespaceHandlerFor(bundle);
    }

    private void maybeRemoveNameSpaceHandlerFor(Bundle bundle) {
        if (handlerBundleMatchesExtenderVersion(bundle))
            nsManager.maybeRemoveNameSpaceHandlerFor(bundle);
    }

    /**
     * Context creation is a potentially long-running activity (certainly more
     * than we want to do on the synchronous event callback).
     * 
     * <p/>
     * Based on our configuration, the context can be started on the same thread
     * or on a different one.
     * 
     * <p/>
     * Kick off a background activity to create an application context for the
     * given bundle if needed.
     * 
     * <b>Note:</b> Make sure to do the fastest filtering first to avoid
     * slowdowns on platforms with a big number of plugins and wiring (i.e.
     * Eclipse platform).
     * 
     * @param bundle
     */
    protected void maybeCreateApplicationContextFor(Bundle bundle) {

        boolean debug = log.isDebugEnabled();
        String bundleString = "["
                + OsgiStringUtils.nullSafeNameAndSymName(bundle) + "]";

        final Long bundleId = new Long(bundle.getBundleId());

        if (managedContexts.containsKey(bundleId)) {
            if (debug) {
                log.debug("Bundle " + bundleString
                        + " is already managed; ignoring...");
            }
            return;
        }

        if (!ConfigUtils.matchExtenderVersionRange(bundle, extenderVersion)) {
            if (debug)
                log.debug("Bundle "
                        + bundleString
                        + " expects an extender w/ version["
                        + OsgiBundleUtils.getHeaderAsVersion(bundle,
                                ConfigUtils.EXTENDER_VERSION)
                        + "] which does not match current extender w/ version["
                        + extenderVersion
                        + "]; skipping bundle from context creation");
            return;
        }

        BundleContext localBundleContext = OsgiBundleUtils
                .getBundleContext(bundle);

        if (debug)
            log.debug("Scanning bundle " + bundleString
                    + " for configurations...");

        // initialize context
        final DelegatedExecutionOsgiBundleApplicationContext localApplicationContext;

        if (debug)
            log.debug("Creating an application context for bundle "
                    + bundleString);

        try {
            localApplicationContext = contextCreator
                    .createApplicationContext(localBundleContext);
        } catch (Exception ex) {
            log.error("Cannot create application context for bundle "
                    + bundleString, ex);
            return;
        }

        if (localApplicationContext == null) {
            log.debug("No application context created for bundle "
                    + bundleString);
            return;
        }

        // create a dedicated hook for this application context
        BeanFactoryPostProcessor processingHook = new OsgiBeanFactoryPostProcessorAdapter(
                localBundleContext, postProcessors);

        // add in the post processors
        localApplicationContext.addBeanFactoryPostProcessor(processingHook);

        // add the context to the tracker
        managedContexts.put(bundleId, localApplicationContext);

        localApplicationContext.setDelegatedEventMulticaster(multicaster);

        // create refresh runnable
        Runnable contextRefresh = new Runnable() {

            public void run() {
                localApplicationContext.refresh();
            }
        };

        // executor used for creating the appCtx
        // chosen based on the sync/async configuration
        TaskExecutor executor = null;

        ApplicationContextConfiguration config = new ApplicationContextConfiguration(
                bundle);

        String creationType;

        // synch/asynch context creation
        if (config.isCreateAsynchronously()) {
            // for the async stuff use the executor
            executor = taskExecutor;
            creationType = "Asynchronous";
        } else {
            // for the sync stuff, use this thread
            executor = sameThreadTaskExecutor;
            creationType = "Synchronous";
        }

        if (debug) {
            log.debug(creationType + " context creation for bundle "
                    + bundleString);
        }

        // wait/no wait for dependencies behaviour
        if (config.isWaitForDependencies()) {
            DependencyWaiterApplicationContextExecutor appCtxExecutor = new DependencyWaiterApplicationContextExecutor(
                    localApplicationContext, !config.isCreateAsynchronously(),
                    extenderConfiguration.getDependencyFactories());

            long timeout;
            // check whether a timeout has been defined

            if (ConfigUtils.isDirectiveDefined(bundle.getHeaders(),
                    ConfigUtils.DIRECTIVE_TIMEOUT)) {
                timeout = config.getTimeout();
                if (debug)
                    log.debug("Setting bundle-defined, wait-for-dependencies timeout value="
                            + timeout + " ms, for bundle " + bundleString);

            } else {
                timeout = extenderConfiguration.getDependencyWaitTime();
                if (debug)
                    log.debug("Setting globally defined wait-for-dependencies timeout value="
                            + timeout + " ms, for bundle " + bundleString);
            }

            appCtxExecutor.setTimeout(config.getTimeout());

            appCtxExecutor.setWatchdog(timer);
            appCtxExecutor.setTaskExecutor(executor);
            appCtxExecutor.setMonitoringCounter(contextsStarted);
            // set events publisher
            appCtxExecutor.setDelegatedMulticaster(this.multicaster);

            contextsStarted.increment();
        } else {
            // do nothing; by default contexts do not wait for services.
        }

        executor.execute(contextRefresh);
    }

    /**
     * Closing an application context is a potentially long-running activity,
     * however, we *have* to do it synchronously during the event process as the
     * BundleContext object is not valid once we return from this method.
     * 
     * @param bundle
     */
    protected void maybeCloseApplicationContextFor(Bundle bundle) {
        final ConfigurableOsgiBundleApplicationContext context = (ConfigurableOsgiBundleApplicationContext) managedContexts
                .remove(new Long(bundle.getBundleId()));
        if (context == null) {
            return;
        }

        RunnableTimedExecution.execute(new Runnable() {

            private final String toString = "Closing runnable for context "
                    + context.getDisplayName();

            public void run() {
                if (context.isActive()) {
                    context.close();
                }
            }

            public String toString() {
                return toString;
            }

        }, extenderConfiguration.getShutdownWaitTime(), shutdownTaskExecutor);
    }

    private void initListenerService() {
        multicaster = extenderConfiguration.getEventMulticaster();

        createListenersList();
        // register the listener that does the dispatching
        multicaster.addApplicationListener(new ListListenerAdapter(
                applicationListeners));
        multicaster.addApplicationListener(extenderConfiguration
                .getContextEventListener());

        if (log.isDebugEnabled())
            log.debug("Initialization of OSGi listeners service completed...");
    }

    /**
     * Creates a dynamic OSGi list of OSGi services interested in receiving
     * events for OSGi application contexts.
     */
    private void createListenersList() {
        OsgiServiceCollectionProxyFactoryBean fb = new OsgiServiceCollectionProxyFactoryBean();
        fb.setBundleContext(bundleContext);
        fb.setCardinality(Cardinality.C_0__N);
        fb.setCollectionType(CollectionType.LIST);
        fb.setInterfaces(new Class[] { OsgiBundleApplicationContextListener.class });
        fb.setBeanClassLoader(extenderConfiguration.getClassLoader());
        fb.afterPropertiesSet();

        applicationListenersCleaner = fb;
        applicationListeners = (List) fb.getObject();
    }
}
