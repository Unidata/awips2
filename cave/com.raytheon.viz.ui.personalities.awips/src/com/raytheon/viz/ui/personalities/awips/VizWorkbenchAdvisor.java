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

//import gov.noaa.nws.ost.awips.viz.CaveCommandExecutionListener;
//import gov.noaa.nws.ost.awips.viz.CaveJobChangeListener;

import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.contexts.IContextService;

import com.raytheon.uf.viz.application.ProgramArguments;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.preferences.PreferenceConstants;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Workbench Advisor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Mar 5, 2013     1753     njensen     Added shutdown printout
 * Mar 20, 2013    1638     mschenke    Added overrideable method for dynamic menu creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class VizWorkbenchAdvisor extends WorkbenchAdvisor {

    protected boolean logPeformance = false;

    protected IExecutionListener performanceListener;

    protected IJobChangeListener jobChangeListener;

    protected CloseNonRestorableDetachedViewsListener detachedViewsListener;

    protected boolean singlePerspective;

    private boolean createdMenus = false;

    public VizWorkbenchAdvisor() {
        performanceListener = CaveCommandExecutionListener.getInstance();
        jobChangeListener = CaveJobChangeListener.getInstance();
        detachedViewsListener = new CloseNonRestorableDetachedViewsListener();

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchAdvisor#initialize(org.eclipse.ui
     * .application.IWorkbenchConfigurer)
     */
    @Override
    public void initialize(IWorkbenchConfigurer configurer) {
        super.initialize(configurer);

        // make sure we always save and restore workspace state
        configurer.setSaveAndRestore(true);

        customizeAppearance();
        PlatformUI.getWorkbench().addWindowListener(
                VizWorkbenchManager.getInstance());
    }

    /**
     * Removes extra menus, perspectives, and preferences from the system that
     * were included with bundled plugins
     */
    protected void customizeAppearance() {
        removeExtraMenus();
        removeExtraPerspectives();
        removeExtraPreferences();
    }

    /**
     * Removes menus from the rcp menu system
     */
    @SuppressWarnings("restriction")
    private void removeExtraMenus() {
        // For standalone app, remove the stuff we don't use
        org.eclipse.ui.internal.registry.ActionSetRegistry reg = org.eclipse.ui.internal.WorkbenchPlugin
                .getDefault().getActionSetRegistry();

        org.eclipse.ui.internal.registry.IActionSetDescriptor[] actionSets = reg
                .getActionSets();
        String[] removeActionSets = new String[] {
                "org.eclipse.search.searchActionSet",
                // "org.eclipse.ui.cheatsheets.actionSet",
                "org.eclipse.ui.actionSet.keyBindings",
                "org.eclipse.ui.edit.text.actionSet.navigation",
                "org.eclipse.ui.edit.text.actionSet.annotationNavigation",
                "org.eclipse.ui.edit.text.actionSet.convertLineDelimitersTo",
                // "org.eclipse.ui.actionSet.openFiles",

                "org.eclipse.ui.edit.text.actionSet.openExternalFile",
                // "org.eclipse.ui.externaltools.ExternalToolsSet",
                // "org.eclipse.ui.WorkingSetActionSet",
                "org.eclipse.update.ui.softwareUpdates" };

        for (int i = 0; i < actionSets.length; i++) {
            boolean found = false;
            for (int j = 0; j < removeActionSets.length; j++) {
                if (removeActionSets[j].equals(actionSets[i].getId())) {
                    found = true;
                }
            }

            if (!found) {
                continue;
            }
            IExtension ext = actionSets[i].getConfigurationElement()
                    .getDeclaringExtension();
            reg.removeExtension(ext, new Object[] { actionSets[i] });
        }
    }

    /**
     * Removes perspectives from the rcp perspectives menu
     */
    @SuppressWarnings("restriction")
    private void removeExtraPerspectives() {
        IPerspectiveRegistry reg = PlatformUI.getWorkbench()
                .getPerspectiveRegistry();
        IPerspectiveDescriptor ipydev = reg
                .findPerspectiveWithId("org.python.pydev.ui.PythonPerspective");
        if (ipydev != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor pydev = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) ipydev;

            IExtension ext = pydev.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { ipydev });
        }

        IPerspectiveDescriptor isync = reg
                .findPerspectiveWithId("org.eclipse.team.ui.TeamSynchronizingPerspective");
        if (isync != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor sync = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) isync;

            IExtension ext = sync.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { isync });
        }

        IPerspectiveDescriptor idebug = reg
                .findPerspectiveWithId("org.eclipse.debug.ui.DebugPerspective");
        if (idebug != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor debug = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) idebug;

            IExtension ext = debug.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { idebug });
        }

        IPerspectiveDescriptor ijava = reg
                .findPerspectiveWithId("org.eclipse.jdt.ui.JavaPerspective");
        if (ijava != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor java = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) ijava;

            IExtension ext = java.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { ijava });
        }

        IPerspectiveDescriptor ijavaBrowse = reg
                .findPerspectiveWithId("org.eclipse.jdt.ui.JavaBrowsingPerspective");
        if (ijavaBrowse != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor java = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) ijavaBrowse;

            IExtension ext = java.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { ijavaBrowse });
        }

        IPerspectiveDescriptor ijavaHier = reg
                .findPerspectiveWithId("org.eclipse.jdt.ui.JavaHierarchyPerspective");
        if (ijavaHier != null) {
            org.eclipse.ui.internal.registry.PerspectiveDescriptor java = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) ijavaHier;

            IExtension ext = java.getConfigElement().getDeclaringExtension();
            ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                    .removeExtension(ext, new Object[] { ijavaHier });
        }
    }

    /**
     * Removes options from the rcp preferences menu
     */
    private void removeExtraPreferences() {
        PreferenceManager preferenceManager = PlatformUI.getWorkbench()
                .getPreferenceManager();
        preferenceManager.remove("org.eclipse.team.ui.TeamPreferences");
        preferenceManager
                .remove("org.eclipse.update.internal.ui.preferences.MainPreferencePage");
        preferenceManager.remove("org.eclipse.debug.ui.DebugPreferencePage");
        preferenceManager
                .remove("org.eclipse.jdt.ui.preferences.JavaBasePreferencePage");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId
     * ()
     */
    @Override
    public String getInitialWindowPerspectiveId() {
        if (singlePerspective) {
            String perspective = ProgramArguments.getInstance().getString(
                    "-perspective");
            // Check Id first
            for (IPerspectiveDescriptor desc : PlatformUI.getWorkbench()
                    .getPerspectiveRegistry().getPerspectives()) {
                if (perspective.equals(desc.getId())) {
                    return perspective;
                }
            }

            // Check label second
            for (IPerspectiveDescriptor desc : PlatformUI.getWorkbench()
                    .getPerspectiveRegistry().getPerspectives()) {
                if (perspective.equalsIgnoreCase(desc.getLabel())) {
                    return desc.getId();
                }
            }
        }

        // Fall back to a reasonable default if not available
        return "com.raytheon.uf.viz.d2d.ui.perspectives.D2D5Pane";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchAdvisor#createWorkbenchWindowAdvisor
     * (org.eclipse.ui.application.IWorkbenchWindowConfigurer)
     */
    @Override
    public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
            IWorkbenchWindowConfigurer configurer) {
        if (createdMenus == false) {
            createdMenus = true;
            createDynamicMenus();
        }
        return new VizWorkbenchWindowAdvisor(configurer, singlePerspective);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.application.WorkbenchAdvisor#preShutdown()
     */
    @Override
    public boolean preShutdown() {
        boolean bResult = super.preShutdown();
        if (bResult) {
            bResult = MessageDialog.openQuestion(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), "Confirm Exit",
                    "Are you sure you want to exit?");
        }

        if (bResult) {
            System.out
                    .println("VizWorkbenchAdvisor: User exiting CAVE, shutdown initiated");

            // close all non-restorable detached views
            detachedViewsListener.handleEvent(null);
        }

        return bResult;
    }

    /**
     * Added by Wufeng Zhou to hook up command execution listener and job
     * listener
     * 
     */
    @Override
    public void postStartup() {
        super.postStartup();

        // createDynamicMenus();

        Boolean log = Activator.getDefault().getPreferenceStore()
                .getBoolean(PreferenceConstants.P_LOG_PERF);

        if (log != logPeformance) {
            toggleLogging();
        }
        IContextService service = (IContextService) PlatformUI.getWorkbench()
                .getService(IContextService.class);
        service.activateContext("com.raytheon.uf.viz.application.awips");
    }

    /**
     * Uses {@link DiscoverMenuContributions} to create dynamic menu
     * contributions
     */
    protected void createDynamicMenus() {
        DiscoverMenuContributions.discoverContributions();
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

    @Override
    public void preStartup() {
        // only restore state if no perspective passed in
        getWorkbenchConfigurer().setSaveAndRestore(!singlePerspective);
    }

}
