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

import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.contexts.IContextService;

import com.raytheon.uf.common.menus.MenuCreationRequest;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.preferences.PreferenceConstants;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.personalities.cave.workbench.VizWorkbenchAdvisor;

/**
 * AWIPS {@link VizWorkbenchAdvisor} that reqeusts menu creation service to run
 * before discovering dynamic menus
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

public class AWIPSWorkbenchAdvisor extends VizWorkbenchAdvisor {

    protected boolean logPeformance = false;

    protected IExecutionListener performanceListener;

    protected IJobChangeListener jobChangeListener;

    protected boolean singlePerspective;

    public AWIPSWorkbenchAdvisor() {
        performanceListener = CaveCommandExecutionListener.getInstance();
        jobChangeListener = CaveJobChangeListener.getInstance();

        Activator.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent event) {
                        if (PreferenceConstants.P_LOG_PERF.equals(event
                                .getProperty())) {
                            Boolean log = (Boolean) event.getNewValue();
                            if (log != logPeformance) {
                                toggleLogging();
                            }
                        }
                    }
                });

        singlePerspective = ProgramArguments.getInstance().getString(
                "-perspective") != null;
    }

    @Override
    protected WorkbenchWindowAdvisor createNewWindowAdvisor(
            IWorkbenchWindowConfigurer configurer) {
        return new AWIPSWorkbenchWindowAdvisor(configurer, singlePerspective);
    }

    @Override
    public String getInitialWindowPerspectiveId() {
        if (singlePerspective) {
            IPerspectiveDescriptor desc = getSpecifiedPerspective(ProgramArguments
                    .getInstance().getString("-perspective"));
            if (desc != null) {
                return desc.getId();
            }
        }
        // Fall back to a reasonable default if not available
        return "com.raytheon.uf.viz.d2d.ui.perspectives.D2D5Pane";
    }

    /**
     * Toggle whether we are logging or not
     */
    private void toggleLogging() {
        logPeformance = !logPeformance;

        // add command execution listener
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        if (logPeformance) {
            service.addExecutionListener(performanceListener);
        } else {
            service.removeExecutionListener(performanceListener);
        }

        // add job change listener
        IJobManager jobManager = Job.getJobManager();
        if (logPeformance) {
            jobManager.addJobChangeListener(jobChangeListener);
        } else {
            jobManager.removeJobChangeListener(jobChangeListener);
        }
    }

    /**
     * Added by Wufeng Zhou to hook up command execution listener and job
     * listener
     * 
     */
    @Override
    public void postStartup() {
        super.postStartup();

        Boolean log = Activator.getDefault().getPreferenceStore()
                .getBoolean(PreferenceConstants.P_LOG_PERF);

        if (log != logPeformance) {
            toggleLogging();
        }
        IContextService service = (IContextService) PlatformUI.getWorkbench()
                .getService(IContextService.class);
        service.activateContext("com.raytheon.uf.viz.application.awips");
    }

    @Override
    protected void createDynamicMenus() {
        // create the request to send to EDEX to generate the menus
        MenuCreationRequest request = new MenuCreationRequest();
        request.setSite(LocalizationManager.getInstance().getSite());
        try {
            ThriftClient.sendRequest(request);
        } catch (VizException e) {
            UFStatus.getHandler(AWIPSWorkbenchAdvisor.class).handle(
                    Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        super.createDynamicMenus();
    }

    @Override
    public void preStartup() {
        // only restore state if no perspective passed in
        getWorkbenchConfigurer().setSaveAndRestore(!singlePerspective);
    }
}
