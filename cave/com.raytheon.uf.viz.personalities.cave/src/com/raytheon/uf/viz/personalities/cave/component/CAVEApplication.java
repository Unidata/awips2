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
package com.raytheon.uf.viz.personalities.cave.component;

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

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.pypies.PyPiesDataStoreFactory;
import com.raytheon.uf.common.pypies.PypiesProperties;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.application.component.IStandaloneComponent;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.CAVELocalizationNotificationObserver;
import com.raytheon.uf.viz.core.localization.LocalizationInitializer;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.personalities.cave.workbench.VizWorkbenchAdvisor;
import com.raytheon.viz.alerts.jobs.AutoUpdater;
import com.raytheon.viz.alerts.jobs.MenuUpdater;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.core.preferences.PreferenceConstants;
import com.raytheon.viz.core.units.UnitRegistrar;

/**
 * {@link IStandaloneComponent} that starts and initializes the CAVE Workbench
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CAVEApplication implements IStandaloneComponent {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CAVEApplication.class, "CAVE");

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.application.component.IStandaloneComponent#startComponent
     * (java.lang.String)
     */
    @Override
    @SuppressWarnings("restriction")
    public Object startComponent(String componentName) throws Exception {
        InternalPlatform.getDefault()
                .getLog(WorkbenchPlugin.getDefault().getBundle())
                .addLogListener(new ILogListener() {

                    @Override
                    public void logging(IStatus status, String plugin) {
                        if (status.getMessage() != null) {
                            System.out.println(status.getMessage());
                        }
                        if (status.getException() != null) {
                            status.getException().printStackTrace();
                        }
                    }

                });

        UnitRegistrar.registerUnits();
        CAVEMode.performStartupDuties();

        Display display = PlatformUI.createDisplay();

        try {
            initializeLocalization();
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

        Job serializationJob = initializeSerialization();
        initializeDataStoreFactory();
        initializeObservers();
        initializeSimulatedTime();

        // wait for serialization initialization to complete before
        // opening JMS connection to avoid deadlock in class loaders
        if (serializationJob != null) {
            serializationJob.join();
        }

        // open JMS connection to allow alerts to be received
        NotificationManagerJob.connect();

        int returnCode = IApplication.EXIT_OK;

        try {
            returnCode = PlatformUI.createAndRunWorkbench(display,
                    getWorkbenchAdvisor());
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

            try {
                // disconnect from JMS
                NotificationManagerJob.disconnect();
            } catch (RuntimeException e) {
                // catch any exceptions to ensure rest of finally block
                // executes
            }
        }

        if (returnCode == PlatformUI.RETURN_RESTART) {
            return IApplication.EXIT_RESTART;
        }
        return returnCode;
    }

    protected void initializeLocalization() throws Exception {
        new LocalizationInitializer(true, false).run();
    }

    /**
     * Initializes the DataStoreFactory for the component.
     */
    protected void initializeDataStoreFactory() {
        PypiesProperties pypiesProps = new PypiesProperties();
        pypiesProps.setAddress(VizApp.getPypiesServer());
        DataStoreFactory.getInstance().setUnderlyingFactory(
                new PyPiesDataStoreFactory(pypiesProps));
    }

    protected Job initializeSerialization() {
        Job job = new Job("Loading Serialization") {

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

        };
        job.schedule();
        return job;
    }

    /**
     * Initialize any observers needed by the application
     */
    protected void initializeObservers() {
        // Setup cave notification observer
        CAVELocalizationNotificationObserver.register();
        registerProductAlerts();
    }

    protected void registerProductAlerts() {
        // Register product observers
        ProductAlertObserver.addObserver(null, new MenuUpdater());
        for (String plugin : RecordFactory.getInstance().getSupportedPlugins()) {
            // Create separate AutoUpdater per plugin
            ProductAlertObserver.addObserver(plugin, new AutoUpdater());
        }
    }

    /**
     * Save the current state of SimulatedTime
     * 
     * @throws IOException
     */
    protected void saveUserTime() throws IOException {
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
    protected void initializeSimulatedTime() {
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
     * Get the workbench advisor for the application
     * 
     * @return
     */
    protected WorkbenchAdvisor getWorkbenchAdvisor() {
        return new VizWorkbenchAdvisor();
    }
}
