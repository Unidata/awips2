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
package com.raytheon.viz.gfe;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.dialogs.GFEConfigDialog;

/**
 * The activator class controls the plug-in life cycle
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Oct 30, 2012 1298       rferrel     Must be a blocking dialog.
 * Dec 09, 2013 #2367      dgilling    Remove shutdown of ProcedureJob and
 *                                     SmartToolJob.
 * Oct 28, 2015 #5054      randerso    Make GfeConfigDlg parented by workbench window if
 *                                     workbench is running.
 * Jan 15, 2016  5193      bsteffen    Add checkPreferenceStore.
 * Jan 26, 2015 #5054      randerso    If no workbench is running use null shell as parent,
 *                                     Results in display as parent for jface dialogs
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.viz.gfe";

    // The shared instance
    private static Activator plugin;

    private PythonPreferenceStore pythonPrefs;

    private GFEConfigDialog cfgDlg;

    private Set<EventHandler> preferenceListeners = new HashSet<>();

    /**
     * The constructor
     */
    public Activator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.
     * BundleContext )
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.
     * BundleContext )
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /**
     * Check if the preference store has been created yet and if it has not been
     * created the provided {@link EventHandler} will be notified when it is
     * created. The first time {@link #getPreferenceStore()} is called it may
     * create a dialog for user input, this can be a problem for some components
     * which may be sensitive to changes caused by opening a dialog. This method
     * can be used to check if the preference store is ready for use and
     * provides a mechanism to be notified when it is ready. If the preference
     * store is already initialized this method will return true and the event
     * handler is ignored.
     * 
     * @param handler
     *            the handler that will be notified when the preferences are
     *            created, null is acceptable.
     * @return true if the preference store is initialized, false otherwise.
     */
    public boolean checkPreferenceStore(EventHandler handler) {
        synchronized (this) {
            if (pythonPrefs == null) {
                if (handler != null) {
                    preferenceListeners.add(handler);
                }
                return false;
            } else {
                return true;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#getPreferenceStore()
     */
    @Override
    public PythonPreferenceStore getPreferenceStore() {
        synchronized (this) {
            if (pythonPrefs == null) {
                if ((cfgDlg == null) || (cfgDlg.getShell() == null)
                        || cfgDlg.isDisposed()) {
                    Shell shell = null;
                    if (PlatformUI.isWorkbenchRunning()) {
                        shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                    }

                    cfgDlg = new GFEConfigDialog(shell);
                    // Must keep as a blocking dialog for eclipse plugins to
                    // work properly.
                    cfgDlg.setBlockOnOpen(true);
                    cfgDlg.open();
                } else {
                    cfgDlg.bringToTop();
                }

                // this is necessary because we sometimes get in here
                // recursively and only want to do this once
                if (pythonPrefs == null) {
                    String config = cfgDlg.getConfig();
                    pythonPrefs = new PythonPreferenceStore(config);
                    if (!preferenceListeners.isEmpty()) {
                        Event event = new Event(config,
                                Collections.<String, Object> emptyMap());
                        for (EventHandler handler : preferenceListeners) {
                            handler.handleEvent(event);
                        }
                        preferenceListeners.clear();
                    }
                    statusHandler.handle(Priority.EVENTA,
                            "GFE started with configuration: " + config);
                }
            }
        }

        return pythonPrefs;
    }

    public void setPreferenceStore(PythonPreferenceStore prefs) {
        this.pythonPrefs = prefs;
    }
}
