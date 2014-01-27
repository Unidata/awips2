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
package com.raytheon.uf.edex.ingest.notification;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataquery.DecisionTree;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ingest.notification.PluginNotifierConfig.EndpointType;
import com.raytheon.uf.edex.ingest.notification.PluginNotifierConfig.NotifyFormat;
import com.raytheon.uf.edex.ingest.notification.router.DataUriRouter;
import com.raytheon.uf.edex.ingest.notification.router.INotificationRouter;
import com.raytheon.uf.edex.ingest.notification.router.PdoRouter;

/**
 * Plugins can register routes with this and then be generically fired to. Helps
 * to reduce dependencies as we no longer need to call a route directly. All
 * registration must occur before messages are being picked up. Otherwise
 * concurrency problems may occur.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2013            mnash       Initial creation.
 * Nov 19, 2013 2170       rjpeter     Add plugin contributed config files, filtering,
 *                                     and support for pdo vs datauri.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PluginNotifier {
    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(PluginNotifier.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("Notification:");

    private static final int DEFAULT_TIME_TO_LIVE = 300000;

    /**
     * Decision tree for plugin notification.
     */
    private final DecisionTree<INotificationRouter> tree = new DecisionTree<INotificationRouter>();

    private final List<INotificationRouter> receiveAllRoutes = new LinkedList<INotificationRouter>();

    private final List<INotificationRouter> filteredRoutes = new LinkedList<INotificationRouter>();

    /**
     * Set of loaded names. Used for duplicate detection.
     */
    private final Set<String> loadedNames = new HashSet<String>();

    public PluginNotifier() throws JAXBException {
        loadConfigurations();
    }

    private synchronized void loadConfigurations() throws JAXBException {
        JAXBManager mgr = new JAXBManager(PluginNotifierConfigList.class,
                PluginNotifierConfig.class);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile[] files = pathMgr.listStaticFiles("notification",
                new String[] { ".xml" }, false, true);
        for (LocalizationFile lf : files) {
            try {
                File f = lf.getFile(true);
                if (f.length() > 0) {
                    // empty files may be used to override base files to remove
                    // functionality
                    InputStream is = null;

                    try {
                        is = lf.openInputStream();

                        PluginNotifierConfigList confList = (PluginNotifierConfigList) mgr
                                .unmarshalFromInputStream(is);
                        List<PluginNotifierConfig> configs = confList
                                .getNotificationConfigs();
                        if ((configs != null) && !configs.isEmpty()) {
                            for (PluginNotifierConfig conf : configs) {
                                register(conf, false);
                            }
                        }
                    } catch (SerializationException e) {
                        theHandler.handle(Priority.PROBLEM,
                                "Unable to deserialize " + f.getPath(), e);
                    } catch (InvalidNotificationConfigException e) {
                        theHandler.handle(
                                Priority.PROBLEM,
                                "Unable to load plugin configuration "
                                        + f.getPath(), e);
                    } finally {
                        if (is != null) {
                            try {
                                is.close();
                            } catch (IOException e) {
                                // ignore
                            }
                        }
                    }
                }
            } catch (LocalizationException e) {
                theHandler.handle(Priority.PROBLEM,
                        "Error occurred accessing file: " + lf, e);
            }
        }

        rebuildTree();

    }

    /**
     * Register the given PluginNotifierConfig.
     * 
     * @param config
     * @return
     */
    public synchronized void register(PluginNotifierConfig config)
            throws InvalidNotificationConfigException {
        register(config, true);
    }

    /**
     * Register the given PluginNotifierConfig.
     * 
     * @param config
     * @param rebuildTree
     *            Whether or not to rebuild the internal tree. If many things
     *            are being registered can improve performance to only build
     *            tree once at the end.
     * @return
     */
    public synchronized void register(PluginNotifierConfig config,
            boolean rebuildTree) throws InvalidNotificationConfigException {
        register(config, null, rebuildTree);
    }

    /**
     * Register the given PluginNotifierConfig.
     * 
     * @param config
     * @param router
     *            The INotificationRouter to use for this config. If null, will
     *            use the default based on the config format.
     * @return
     */
    public synchronized void register(PluginNotifierConfig config,
            INotificationRouter router)
            throws InvalidNotificationConfigException {
        register(config, router, true);
    }

    /**
     * Register the given PluginNotifierConfig.
     * 
     * @param config
     * @param router
     *            The INotificationRouter to use for this config. If null, will
     *            use the default based on the config format.
     * @param rebuildTree
     *            Whether or not to rebuild the internal tree. If many things
     *            are being registered can improve performance to only build
     *            tree once at the end.
     * @return
     */
    public synchronized void register(PluginNotifierConfig config,
            INotificationRouter router, boolean rebuildTree)
            throws InvalidNotificationConfigException {
        validate(config);

        if (router == null) {
            switch (config.getFormat()) {
            case DATAURI:
                router = new DataUriRouter(config);
                break;
            case PDO:
                router = new PdoRouter(config);
                break;
            default:
                throw new InvalidNotificationConfigException(
                        "No INotificationRouter registered for format: "
                                + config.getFormat());
            }
        }

        Map<String, RequestConstraint>[] metadataMaps = config.getMetadataMap();
        boolean receiveAll = (metadataMaps == null)
                || (metadataMaps.length == 0)
                || ((metadataMaps.length == 1) && ((metadataMaps[0] == null) || metadataMaps[0]
                        .isEmpty()));

        if (receiveAll) {
            // null or empty constraint map implies receive all data
            receiveAllRoutes.add(router);
        } else {
            filteredRoutes.add(router);
            for (Map<String, RequestConstraint> metadataMap : metadataMaps) {
                tree.insertCriteria(metadataMap, router);
            }

            if (rebuildTree) {
                tree.rebuildTree();
            }
        }

        loadedNames.add(config.getEndpointName());
    }

    /**
     * Validate the passed config
     * 
     * @param config
     * @return
     */
    private void validate(PluginNotifierConfig config)
            throws InvalidNotificationConfigException {
        String endpoint = config.getEndpointName();
        if ((endpoint == null) || (endpoint.trim().length() == 0)) {
            throw new InvalidNotificationConfigException(
                    "endpointName is required");
        }

        if (loadedNames.contains(endpoint)) {
            throw new InvalidNotificationConfigException("PluginConfiguration "
                    + endpoint + ": endpointName is already in use");
        }

        EndpointType type = config.getEndpointType();
        if (type == null) {
            StringBuilder msg = new StringBuilder(180);
            msg.append("PluginConfiguration ")
                    .append(endpoint)
                    .append(": missing required field endpointType.  Valid values for ")
                    .append(NotifyFormat.PDO).append(" format are ")
                    .append(EndpointType.DIRECTVM).append(" and ")
                    .append(EndpointType.VM).append(".  Valid values for ")
                    .append(NotifyFormat.DATAURI).append(" format are ")
                    .append(EndpointType.DIRECTVM).append(", ")
                    .append(EndpointType.VM).append(", ")
                    .append(EndpointType.QUEUE).append(", and ")
                    .append(EndpointType.TOPIC);
            throw new InvalidNotificationConfigException(msg.toString());
        }

        NotifyFormat format = config.getFormat();
        if (NotifyFormat.PDO.equals(format)
                && (EndpointType.QUEUE.equals(type) || EndpointType.TOPIC
                        .equals(type))) {
            StringBuilder msg = new StringBuilder(120);
            msg.append("PluginConfiguration ").append(endpoint)
                    .append(": endpointType ").append(type)
                    .append(" is invalid for format ").append(format)
                    .append(".  Valid values for ").append(NotifyFormat.PDO)
                    .append(" format are ").append(EndpointType.DIRECTVM)
                    .append(" and ").append(EndpointType.VM);
            throw new InvalidNotificationConfigException(msg.toString());
        }

        if (!EndpointType.QUEUE.equals(type) && config.isDurable()) {
            theHandler.warn("PluginConfiguration: " + endpoint
                    + " durable setting only valid on QUEUE type endpoints");
        }

        if ((EndpointType.QUEUE.equals(type) || EndpointType.TOPIC.equals(type))
                && (config.getTimeToLive() < 0)) {
            theHandler
                    .warn("PluginConfiguration: "
                            + endpoint
                            + " has invalid time to live.  Time to live for JMS endpoints must be 0 or greater.  Setting to default of: "
                            + DEFAULT_TIME_TO_LIVE + " ms");
            config.setTimeToLive(DEFAULT_TIME_TO_LIVE);
        }
    }

    /**
     * Rebuild the tree based on all register'd configurations.
     */
    public void rebuildTree() {
        tree.rebuildTree();
    }

    /**
     * Checks the pdo's against the registered routes. Data will then be
     * transformed and queued or sent immediately depending on configuration.
     * 
     * @param pdos
     * @return
     */
    public void notify(PluginDataObject... pdos) {
        if ((pdos != null) && (pdos.length > 0)) {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            if (!receiveAllRoutes.isEmpty()) {
                for (PluginDataObject pdo : pdos) {
                    for (INotificationRouter router : receiveAllRoutes) {
                        router.process(pdo);
                    }
                }

                for (INotificationRouter router : receiveAllRoutes) {
                    try {
                        router.sendImmediateData();
                    } catch (EdexException e) {
                        theHandler.handle(
                                Priority.PROBLEM,
                                "Unable to send notification data to "
                                        + router.getRoute(), e);
                    }
                }
            }

            if (!filteredRoutes.isEmpty()) {
                Set<INotificationRouter> routesWithData = new HashSet<INotificationRouter>();
                for (PluginDataObject pdo : pdos) {
                    try {
                        List<INotificationRouter> routers = tree
                                .searchTree(DataURIUtil.createDataURIMap(pdo));
                        for (INotificationRouter router : routers) {
                            router.process(pdo);
                            routesWithData.add(router);
                        }
                    } catch (PluginException e) {
                        theHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }

                for (INotificationRouter router : routesWithData) {
                    try {
                        router.sendImmediateData();
                    } catch (EdexException e) {
                        theHandler.handle(
                                Priority.PROBLEM,
                                "Unable to send notification data to "
                                        + router.getRoute(), e);
                    }
                }
            }
            timer.stop();
            perfLog.logDuration("Processed " + pdos.length + " pdos",
                    timer.getElapsedTime());
        }
    }

    /**
     * Send the queued notifications.
     * 
     * @return
     */
    public void sendQueuedNotifications() {
        for (INotificationRouter router : receiveAllRoutes) {
            try {
                router.sendQueuedData();
            } catch (EdexException e) {
                theHandler.handle(
                        Priority.PROBLEM,
                        "Unable to send notification data to "
                                + router.getRoute(), e);
            }
        }

        for (INotificationRouter router : filteredRoutes) {
            try {
                router.sendQueuedData();
            } catch (EdexException e) {
                theHandler.handle(
                        Priority.PROBLEM,
                        "Unable to send notification data to "
                                + router.getRoute(), e);
            }
        }
    }
}
