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

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

import com.raytheon.viz.gfe.dialogs.GFEConfigDialog;

/**
 * The activator class controls the plug-in life cycle
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 *                                  Initial creation
 * Oct 30, 2012  1298     rferrel   Must be a blocking dialog.
 * Dec 09, 2013  2367     dgilling  Remove shutdown of ProcedureJob and
 *                                  SmartToolJob.
 * Oct 28, 2015  5054     randerso  Make GfeConfigDlg parented by workbench
 *                                  window if workbench is running.
 * Jan 15, 2016  5193     bsteffen  Add checkPreferenceStore.
 * Jan 26, 2015  5054     randerso  If no workbench is running use null shell as
 *                                  parent, Results in display as parent for
 *                                  jface dialogs
 * Apr 21, 2017  6239     randerso  Prevent UI deadlock if an async task calls
 *                                  getPreferenceStore() while the dialog is
 *                                  open.
 * Jan 29, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author unknown
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {
    /** The plug-in ID */
    public static final String PLUGIN_ID = "com.raytheon.viz.gfe";

    // The shared instance
    private static Activator plugin;

    private final PythonPreferenceStore pythonPrefs = new PythonPreferenceStore();

    private GFEConfigDialog cfgDlg;

    private Set<EventHandler> preferenceListeners = new HashSet<>();

    /**
     * The constructor
     */
    public Activator() {
    }

    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

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
            if (!pythonPrefs.isConfigurationLoaded()) {
                if (handler != null) {
                    preferenceListeners.add(handler);
                }
                return false;
            } else {
                return true;
            }
        }
    }

    private PythonPreferenceStore awaitPrefs() {

        /*
         * Keep UI thread processing events while waiting for preferences to be
         * set from the dialog
         */
        Display display = Display.getDefault();
        while (!pythonPrefs.isConfigurationLoaded()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        return pythonPrefs;
    }

    @Override
    public PythonPreferenceStore getPreferenceStore() {
        if (pythonPrefs.isConfigurationLoaded()) {
            return pythonPrefs;
        }

        /* Open the dialog if it's not already open */
        synchronized (this) {
            if (!pythonPrefs.isConfigurationLoaded()) {
                if ((cfgDlg == null) || cfgDlg.isDisposed()) {
                    Shell shell = null;
                    if (PlatformUI.isWorkbenchRunning()) {
                        shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                    }

                    cfgDlg = new GFEConfigDialog(shell);
                    cfgDlg.setBlockOnOpen(false);
                    cfgDlg.open();
                } else {
                    cfgDlg.bringToTop();
                }
            }
        }

        /* wait for dialog to set the preferences */
        return awaitPrefs();
    }

    /**
     * Load a configuration
     *
     * @param configName
     */
    public void loadConfiguration(String configName) {
        synchronized (this) {
            this.pythonPrefs.loadConfiguration(configName);
            this.cfgDlg = null;
                if (!preferenceListeners.isEmpty()) {
                    Event event = new Event("GfeConfig",
                            Collections.<String, Object> emptyMap());
                    for (EventHandler handler : preferenceListeners) {
                        handler.handleEvent(event);
                    }
                    preferenceListeners.clear();
                }
        }
    }
}
