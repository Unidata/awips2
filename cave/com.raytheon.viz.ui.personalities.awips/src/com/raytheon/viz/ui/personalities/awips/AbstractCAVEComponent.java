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
package com.raytheon.viz.ui.personalities.awips;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.JAXBException;

import org.eclipse.core.internal.runtime.InternalPlatform;
import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.statushandlers.StatusAdapter;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.pypies.PyPiesDataStoreFactory;
import com.raytheon.uf.common.pypies.PypiesProperties;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVisualization;
import com.raytheon.uf.viz.application.ProgramArguments;
import com.raytheon.uf.viz.application.component.IStandaloneComponent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.CAVELocalizationNotificationObserver;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationInitializer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.alerts.jobs.AutoUpdater;
import com.raytheon.viz.alerts.jobs.MenuUpdater;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.core.preferences.PreferenceConstants;
import com.raytheon.viz.core.units.UnitRegistrar;

/**
 * Abstract CAVE component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2011            mschenke     Initial creation
 * May 16, 2012   #636     dgilling     Ensure exception is thrown
 *                                      and CAVE immediately exits
 *                                      if connection cannot be made to
 *                                      localization server.
 * May 31, 2012   #674     dgilling     Allow SimulatedTime to be set from
 *                                      the command line.
 * Oct 02, 2012   #1236    dgilling     Allow SimulatedTime to be set from
 *                                      the command line even if practice
 *                                      mode is off.
 * Jan 09, 2013   #1442    rferrel      Changes to notify SimultedTime listeners.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractCAVEComponent implements IStandaloneComponent {

    public static final int NON_UI = 1 << 1;

    public static final int ALERT_VIZ = 1 << 2;

    public static final int WORKBENCH = 1 << 3;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractCAVEComponent.class, "CAVE");

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.viz.component.IStandaloneComponent#
     * startComponent(java.lang.String)
     */
    @SuppressWarnings("restriction")
    @Override
    public final Object startComponent(String componentName) throws Exception {
        // This is a workaround to receive status messages because without the
        // PlatformUI initialized Eclipse throws out the status
        // messages. Once PlatformUI has started, the status handler
        // will take over.
        InternalPlatform.getDefault()
                .getLog(WorkbenchPlugin.getDefault().getBundle())
                .addLogListener(new ILogListener() {

                    @Override
                    public void logging(IStatus status, String plugin) {
                        if (PlatformUI.isWorkbenchRunning() == false) {
                            new SystemStatusHandler().handle(new StatusAdapter(
                                    status), 0);
                        }
                    }

                });

        UnitRegistrar.registerUnits();
        CAVEMode.performStartupDuties();

        long t0 = System.currentTimeMillis();

        Display display = null;
        int modes = getRuntimeModes();
        boolean alertviz = (modes & ALERT_VIZ) != 0;
        boolean cave = (modes & WORKBENCH) != 0;
        boolean nonui = (modes & NON_UI) != 0;
        if (!nonui) {
            display = PlatformUI.createDisplay();
        } else {
            display = new Display();
        }

        try {
            initializeLocalization(nonui);
        } catch (Exception e) {
            e.printStackTrace();
            statusHandler.handle(
                    Priority.CRITICAL,
                    "Could not connect to localization server: "
                            + e.getLocalizedMessage(), e);
            // we return EXIT_OK here so eclipse doesn't try to pop up an error
            // dialog which would break gfeClient-based cron jobs.
            return IApplication.EXIT_OK;
        }
        initializeSerialization();
        initializeDataStoreFactory();
        initializeObservers();

        int returnCode = IApplication.EXIT_OK;

        AlertVisualization av = null;
        if (alertviz) {
            // Set up alertviz
            if (LocalizationManager.internalAlertServer && !nonui) {
                com.raytheon.uf.viz.alertviz.AlertvizJob as = new com.raytheon.uf.viz.alertviz.AlertvizJob(
                        61998);

                if (as.isAlreadyStarted() == false) {

                    // only run alert visualization inside viz if the server
                    // is running, otherwise it is running as a separate pid
                    try {
                        av = new AlertVisualization(false, display);
                    } catch (RuntimeException e) {
                        e.printStackTrace();
                        String err = "Error starting alert visualization.";
                        statusHandler.handle(Priority.PROBLEM, err, e);
                    }
                }
            } else {
                new com.raytheon.uf.viz.alertviz.AlertvizJob(
                        LocalizationManager
                                .getInstance()
                                .getLocalizationStore()
                                .getString(LocalizationConstants.P_ALERT_SERVER),
                        !nonui);
            }
        }

        WorkbenchAdvisor workbenchAdvisor = null;
        // A component was passed as command line arg
        // launch cave normally, should cave be registered as component?
        long t1 = System.currentTimeMillis();
        System.out.println("Localization time: " + (t1 - t0) + "ms");

        try {
            initializeSimulatedTime();

            if (cave) {
                workbenchAdvisor = getWorkbenchAdvisor();
            } else if (!nonui) {
                workbenchAdvisor = new HiddenWorkbenchAdvisor(componentName,
                        this);
            }

            if (workbenchAdvisor instanceof HiddenWorkbenchAdvisor == false) {
                startInternal(componentName);
            }
            if (workbenchAdvisor != null) {
                returnCode = PlatformUI.createAndRunWorkbench(display,
                        workbenchAdvisor);
            }
        } catch (Throwable t) {
            t.printStackTrace();
            MessageDialog
                    .openError(
                            null,
                            "Error!", "Error instantiating workbench: " + t.getMessage()); //$NON-NLS-1$" +
        } finally {
            try {
                if (CAVEMode.getMode() == CAVEMode.PRACTICE) {
                    saveUserTime();
                }
            } catch (RuntimeException e) {
                // catch any exceptions to ensure rest of finally block
                // executes
            }
            if (av != null) {
                av.dispose();
            }
            if (display != null) {
                display.dispose();
            }
        }

        if (returnCode == PlatformUI.RETURN_RESTART) {
            return IApplication.EXIT_RESTART;
        }
        return IApplication.EXIT_OK;
    }

    /**
     * Get the workbench advisor for the application
     * 
     * @return
     */
    protected WorkbenchAdvisor getWorkbenchAdvisor() {
        return new VizWorkbenchAdvisor();
    }

    /**
     * Save the current state of SimulatedTime
     * 
     * @throws IOException
     */
    private void saveUserTime() throws IOException {
        /*
         * Save the current workstation time
         */
        long timeValue = 0;
        if (!SimulatedTime.getSystemTime().isRealTime()) {
            timeValue = SimulatedTime.getSystemTime().getTime().getTime();
        }
        CorePlugin.getDefault().getPreferenceStore()
                .setValue(PreferenceConstants.P_LAST_USER_TIME, timeValue);
        CorePlugin
                .getDefault()
                .getPreferenceStore()
                .setValue(PreferenceConstants.P_LAST_USER_TIME_FROZEN,
                        SimulatedTime.getSystemTime().isFrozen());
        CorePlugin.getDefault().getPreferenceStore().save();
    }

    /**
     * Restore the prior state of SimulatedTime
     */
    private void initializeSimulatedTime() {
        long timeValue = 0;
        boolean isFrozen = false;

        // If CorePlugin.getDefault() == null, assume running from a unit test
        if (CorePlugin.getDefault() != null) {
            String dateString = ProgramArguments.getInstance().getString(
                    "-time");
            if (dateString != null && !dateString.isEmpty()) {
                try {
                    DateFormat dateParser = new SimpleDateFormat(
                            "yyyyMMdd_HHmm");
                    dateParser.setTimeZone(TimeZone.getTimeZone("GMT"));
                    Date newSimTime = dateParser.parse(dateString);
                    timeValue = newSimTime.getTime();
                } catch (ParseException e) {
                    statusHandler
                            .handle(Priority.WARN,
                                    "Invalid argument specified for command-line parameter '-time'.",
                                    e);
                }
            }
        }

        // if we're in practice mode and the user did not specify a DRT value on
        // the CLI, restore their previous time setting
        if ((CAVEMode.getMode() == CAVEMode.PRACTICE) && (timeValue == 0)) {
            // Get the last saved time from the localization settings
            timeValue = CorePlugin.getDefault().getPreferenceStore()
                    .getLong(PreferenceConstants.P_LAST_USER_TIME);

            // Get if the last saved time was a frozen modified time
            isFrozen = CorePlugin.getDefault().getPreferenceStore()
                    .getBoolean(PreferenceConstants.P_LAST_USER_TIME_FROZEN);
        }

        SimulatedTime systemTime = SimulatedTime.getSystemTime();
        systemTime.notifyListeners(false);
        systemTime.setRealTime();
        systemTime.setFrozen(isFrozen);
        if (timeValue != 0) {
            systemTime.setTime(new Date(timeValue));
        }
        systemTime.notifyListeners(true);
    }

    /**
     * Internal start method for subclasses. For components that do not use the
     * Workbench, when this method returns, the application will exit. For
     * components that do use the workbench, this method can be used to
     * initialize component specific things before the workbench is opened
     * 
     * @param componentName
     * @throws Exception
     */
    protected abstract void startInternal(String componentName)
            throws Exception;

    /**
     * Get the modes to run with
     * 
     * @return
     */
    protected abstract int getRuntimeModes();

    /**
     * Initializes the DataStoreFactory for the component.
     */
    protected void initializeDataStoreFactory() {
        PypiesProperties pypiesProps = new PypiesProperties();
        pypiesProps.setAddress(VizApp.getPypiesServer());
        DataStoreFactory.getInstance().setUnderlyingFactory(
                new PyPiesDataStoreFactory(pypiesProps));
    }

    protected void initializeLocalization(boolean nonui) throws Exception {
        new LocalizationInitializer(!nonui,
                !LocalizationManager.internalAlertServer).run();
    }

    protected void initializeSerialization() {
        new Job("Loading Serialization") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    SerializationUtil.getJaxbContext();
                } catch (JAXBException e) {
                    statusHandler.handle(Priority.CRITICAL,
                            "An error occured initializing Serialization", e);
                }
                return Status.OK_STATUS;
            }

        }.schedule();
    }

    /**
     * Initialize any observers needed by the application
     */
    protected void initializeObservers() {
        // Setup cave notification observer
        CAVELocalizationNotificationObserver.register();
        // Register product observers
        ProductAlertObserver.addObserver(null, new MenuUpdater());
        ProductAlertObserver.addObserver(null, new AutoUpdater());
    }

}
