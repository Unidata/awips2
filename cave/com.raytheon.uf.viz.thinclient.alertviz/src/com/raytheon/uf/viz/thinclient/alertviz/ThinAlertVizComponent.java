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
package com.raytheon.uf.viz.thinclient.alertviz;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVisualization;
import com.raytheon.uf.viz.core.jobs.StatsJob;
import com.raytheon.uf.viz.product.alertviz.AlertVizApplication;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.IThinClientComponent;
import com.raytheon.uf.viz.thinclient.ThinClientNotificationManagerJob;
import com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager;
import com.raytheon.uf.viz.thinclient.localization.LocalizationCachePersistence;
import com.raytheon.uf.viz.thinclient.localization.ThinClientLocalizationInitializer;

/**
 * "Thin" Alertviz component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2011            mschenke     Initial creation
 * Jan 12, 2012  27        rferrel      Added exit option
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ThinAlertVizComponent extends AlertVizApplication implements
        IThinClientComponent {

    private ThinClientCacheManager cacheManager;

    private StatsJob statsJob;

    @Override
    public Object startComponent(String componentName) throws Exception {
        Activator.getDefault().setComponent(this);
        // Start network statistics
        statsJob = new StatsJob("HTTP Network Statistics", HttpClient
                .getInstance().getStats());
        statsJob.schedule();
        return super.startComponent(componentName);
    }

    @Override
    public void stopComponent() {
        // Persist caches
        cacheManager.storeCaches();

        // Shutdown stats job
        statsJob.shutdown();
    }

    @Override
    protected void initializeLocalization() throws Exception {
        cacheManager = new ThinClientCacheManager(
                new LocalizationCachePersistence());
        cacheManager.restoreCaches();
        new ThinClientLocalizationInitializer(true, false).run();
    }

    @Override
    protected void initializeObservers() {
        ; // don't do this.
        ThinClientNotificationManagerJob.getInstance();
    }

    protected AlertVisualization createAlertVisualization(
            boolean runningStandalone, final Display display) {
        return new AlertVisualization(runningStandalone, display) {

            @Override
            protected void createTrayMenuItems() {
                super.createTrayMenuItems();
                if (!runningStandalone) {
                    new MenuItem(trayItemMenu, SWT.SEPARATOR);
                }
                MenuItem exitMI = new MenuItem(trayItemMenu, SWT.NONE);
                exitMI.setText("Exit...");
                exitMI.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                                | SWT.YES | SWT.NO);
                        mb.setText("Confirm Exit");
                        mb.setMessage("Any unsaved changes will be lost.\n Are you sure you want to exit?");
                        if (mb.open() == SWT.YES) {
                            display.dispose();
                        }
                    }
                });
            }
        };
    }
}
