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
package com.raytheon.uf.viz.thinclient.cave;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.jobs.StatsJob;
import com.raytheon.uf.viz.core.localization.BundleScanner;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.IThinClientComponent;
import com.raytheon.uf.viz.thinclient.ThinClientNotificationManagerJob;
import com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager;
import com.raytheon.uf.viz.thinclient.cave.cache.CachingDataStoreFactory;
import com.raytheon.uf.viz.thinclient.cave.cache.GeometryCachePersistence;
import com.raytheon.uf.viz.thinclient.cave.cache.MapQueryCachePersistence;
import com.raytheon.uf.viz.thinclient.cave.refresh.DataRefreshTask;
import com.raytheon.uf.viz.thinclient.cave.refresh.MenuTimeRefreshTask;
import com.raytheon.uf.viz.thinclient.cave.refresh.ThinClientDataUpdateTree;
import com.raytheon.uf.viz.thinclient.cave.refresh.ThinClientURICatalog;
import com.raytheon.uf.viz.thinclient.localization.LocalizationCachePersistence;
import com.raytheon.uf.viz.thinclient.localization.ThinClientLocalizationInitializer;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;
import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher;
import com.raytheon.viz.ui.personalities.awips.AWIPSWorkbenchAdvisor;
import com.raytheon.viz.ui.personalities.awips.AbstractAWIPSComponent;
import com.raytheon.viz.ui.personalities.awips.CAVE;

/**
 * Component for running CAVE in thin client mode
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 04, 2011  10477    njensen   Initial creation
 * Apr 23, 2013  1939     randerso  Return null from initializeSerialization
 * Nov 14, 2013  2361     njensen   Remove initializeSerialization()
 * Nov 06, 2014  3356     njensen   Always initialize ILocalizationAdapter in
 *                                  case cache preference is not enabled
 * Feb 23, 2015  4164     dlovely   Call AlertViz initialize.
 * Jun 26, 2015  4474     bsteffen  Register the PathManager as an OSGi service.
 * Dec 04, 2015  5169     bsteffen  Allow ProductAlertObserver to send messages
 *                                  to the AutoUpdater
 * Feb 08, 2016, 5281     tjensen   Combined Data and Menu Refresh Intervals
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ThinClientComponent extends CAVE implements IThinClientComponent {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractAWIPSComponent.class, "ThinClient");

    private ThinClientCacheManager cacheManager;

    private StatsJob statsJob;

    @Override
    protected void startInternal(String componentName) throws Exception {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        // Initialize the timed data refresher
        store.addPropertyChangeListener(new TimedRefresher(
                new DataRefreshTask(),
                ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL));
        // Initialize the timed menu time refresher
        store.addPropertyChangeListener(new TimedRefresher(
                new MenuTimeRefreshTask(),
                ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL));

        // Start network statistics
        statsJob = new StatsJob("HTTP Network Statistics", HttpClient
                .getInstance().getStats());
        statsJob.schedule();

        // Set ourselves as the activators component
        Activator.getDefault().setComponent(this);

        ThinClientURICatalog.getInstance();
        ThinClientDataUpdateTree.getInstance();
        List<String> pluginBlacklist = new ArrayList<String>();
        File blacklistFile = BundleScanner.searchInBundle(
                com.raytheon.uf.viz.thinclient.cave.Activator.PLUGIN_ID, "",
                "ThinClientPluginBlacklist.txt");
        if (blacklistFile != null && blacklistFile.exists()) {
            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new FileReader(blacklistFile));
                String line = null;
                while (null != (line = reader.readLine())) {
                    pluginBlacklist.add(line.trim());
                }
            } finally {
                if (reader != null) {
                    reader.close();
                }
            }
        }
        try {
            for (Bundle b : Activator.getDefault().getContext().getBundles()) {
                if (pluginBlacklist.contains(b.getSymbolicName())) {
                    b.stop();
                    b.uninstall();
                }
            }
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    @Override
    protected void initializeDataStoreFactory() {
        // Allow default factory to be populated
        super.initializeDataStoreFactory();
        // Grab default factory and use as delegate for caching factory
        DataStoreFactory.getInstance().setUnderlyingFactory(
                new CachingDataStoreFactory(DataStoreFactory.getInstance()
                        .getUnderlyingFactory()));
    }

    @Override
    protected void initializeLocalization() throws Exception {
        /*
         * Set the normal adapter first, based on cache preference settings the
         * next few lines may replace it with the thin client localization
         * adapter
         */
        PathManagerFactory.setAdapter(new CAVELocalizationAdapter());
        cacheManager = new ThinClientCacheManager(
                new GeometryCachePersistence(),
                new LocalizationCachePersistence(),
                new MapQueryCachePersistence());
        cacheManager.restoreCaches();
        try {
            new ThinClientLocalizationInitializer(!isNonUIComponent(),
                    !LocalizationManager.internalAlertServer).run();
        } catch (Exception e1) {
            e1.printStackTrace();
            statusHandler.handle(Priority.CRITICAL,
                    "Error setting up localization", e1);
        }
        FrameworkUtil
                .getBundle(getClass())
                .getBundleContext()
                .registerService(IPathManager.class,
                        PathManagerFactory.getPathManager(), null);
    }

    @Override
    protected AWIPSWorkbenchAdvisor createAWIPSWorkbenchAdvisor() {
        return new ThinClientWorkbenchAdvisor();
    }

    @Override
    protected void initializeObservers() {
        ThinClientNotificationManagerJob.getInstance();
        registerProductAlerts();
        initializeAlertViz();
    }

    @Override
    public void stopComponent() {
        // Persist caches
        cacheManager.storeCaches();

        // Shutdown stats job
        statsJob.shutdown();
    }

}
