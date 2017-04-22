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

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.ui.workbench.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.statushandlers.StatusAdapter;

import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVisualization;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.personalities.cave.component.CAVEApplication;

/**
 * Abstract CAVE component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Apr 28, 2011           mschenke    Initial creation
 * May 16, 2012  636      dgilling    Ensure exception is thrown and CAVE
 *                                    immediately exits if connection cannot be
 *                                    made to localization server.
 * May 31, 2012  674      dgilling    Allow SimulatedTime to be set from the
 *                                    command line.
 * Oct 02, 2012  1236     dgilling    Allow SimulatedTime to be set from the 
 *                                    command line even if practice mode is
 *                                    off.
 * Jan 09, 2013  1442     rferrel     Changes to notify SimultedTime listeners.
 * Apr 17, 2013  1786     mpduff      startComponent now sets
 *                                    StatusHandlerFactory
 * Apr 23, 2013  1939     randerso    Allow serialization to complete
 *                                    initialization before connecting to JMS
 *                                    to avoid deadlock
 * May 23, 2013  2005     njensen     Shutdown on spring initialization errors
 * Oct 15, 2013  2361     njensen     Added startupTimer
 * Nov 14, 2013  2361     njensen     Removed initializing serialization at
 *                                    startup
 * Dec 10, 2013  2602     bsteffen    Start loading ProcedureXmlManager in
 *                                    startComponent.
 * Aug 26, 2014  3356     njensen     Explicitly set localization adapter
 * Sep 10, 2014  3612     mschenke    Refactored to extend CAVEApplication
 * Jan 15, 2015  3947     mapeters    cleanup() doesn't throw Exception.
 * Feb 23, 2015  4164     dlovely     Extracted AlertViz initialize.
 * Jun 03, 2015  4473     njensen     If running with AlertViz, start a job to
 *                                    continuously check AlertViz status.
 * Jun 22, 2015  4474     njensen     Don't check for alertviz if alertview is enabled
 * Jan 21, 2016  5193     bsteffen    Disable IWorkbench.PERSIST_STATE
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractAWIPSComponent extends CAVEApplication {

    public static final int NON_UI = 1 << 1;

    public static final int ALERT_VIZ = 1 << 2;

    public static final int WORKBENCH = 1 << 3;

    private AlertVisualization alertViz = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#cleanup
     * ()
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        if (this.alertViz != null) {
            this.alertViz.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#
     * getEclipseLogListener()
     */
    @Override
    protected ILogListener getEclipseLogListener() {
        return new ILogListener() {
            @Override
            public void logging(IStatus status, String plugin) {
                if (PlatformUI.isWorkbenchRunning() == false) {
                    new SystemStatusHandler().handle(new StatusAdapter(status),
                            0);
                }
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#
     * getWorkbenchAdvisor()
     */
    @Override
    protected final WorkbenchAdvisor getWorkbenchAdvisor() {
        WorkbenchAdvisor workbenchAdvisor = null;
        if ((getRuntimeModes() & WORKBENCH) != 0) {
            workbenchAdvisor = createAWIPSWorkbenchAdvisor();
        } else if (!isNonUIComponent()) {
            /*
             * Disable restore and save state of the workbench since it's going
             * to be invisible.
             */
            System.setProperty(IWorkbench.PERSIST_STATE,
                    Boolean.FALSE.toString());
            workbenchAdvisor = new HiddenWorkbenchAdvisor(getComponentName(),
                    this);
        }

        if (workbenchAdvisor instanceof HiddenWorkbenchAdvisor == false) {
            try {
                startInternal(getComponentName());
            } catch (Exception e) {
                throw new RuntimeException("Error initializing component", e);
            }
        }
        return workbenchAdvisor;
    }

    /**
     * @return A new instance of {@link AWIPSWorkbenchAdvisor} to use for the
     *         component's {@link WorkbenchAdvisor}
     */
    protected AWIPSWorkbenchAdvisor createAWIPSWorkbenchAdvisor() {
        return new AWIPSWorkbenchAdvisor();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#
     * isNonUIComponent()
     */
    @Override
    protected boolean isNonUIComponent() {
        return (getRuntimeModes() & NON_UI) != 0;
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#
     * initializeLocalization()
     */
    @Override
    protected void initializeLocalization() throws Exception {
        initializeLocalization(!isNonUIComponent(),
                !LocalizationManager.internalAlertServer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.personalities.cave.component.CAVEApplication#
     * initializeObservers()
     */
    @Override
    protected void initializeObservers() {
        super.initializeObservers();
        initializeAlertViz();
    }

    /**
     * Initialize AlertViz.
     */
    protected void initializeAlertViz() {
        // Setup AlertViz observer
        if ((getRuntimeModes() & ALERT_VIZ) != 0) {
            // Set up alertviz
            if (LocalizationManager.internalAlertServer && !isNonUIComponent()
                    && !Boolean.getBoolean("alertview.enabled")) {
                /*
                 * Potentially run alertviz inside viz. Will repeatedly schedule
                 * a check to verify it's running. If not found, it will try and
                 * grab the connection in case another viz instance went down.
                 */
                Activator.getDefault().getAlertVizCheckJob().schedule();
            }

        }
    }

}
