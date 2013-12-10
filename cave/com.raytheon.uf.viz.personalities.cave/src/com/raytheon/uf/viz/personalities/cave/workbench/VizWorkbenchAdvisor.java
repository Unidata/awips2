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

package com.raytheon.uf.viz.personalities.cave.workbench;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IExtension;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.contexts.IContextService;

import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Workbench Advisor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 01, 2006             chammack    Initial Creation.
 * Mar 05, 2013 1753        njensen     Added shutdown printout
 * May 28, 2013 1967        njensen     Remove unused subnode preferences
 * Jul 16, 2013 2158        bsteffen    Allow VizGlobalsManager to work without
 *                                      accessing UI thread.
 * Oct 15, 2013 2361        njensen     Added startupTimer
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class VizWorkbenchAdvisor extends WorkbenchAdvisor {

    protected CloseNonRestorableDetachedViewsListener detachedViewsListener;

    private boolean createdMenus = false;

    protected ITimer startupTimer;

    public VizWorkbenchAdvisor() {
        detachedViewsListener = new CloseNonRestorableDetachedViewsListener();
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
        VizGlobalsManager.startForWorkbench(PlatformUI.getWorkbench());
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
     * Removes perspectives from the rcp perspectives menu that are not managed
     * by an {@link AbstractVizPerspectiveManager}
     */
    @SuppressWarnings("restriction")
    private void removeExtraPerspectives() {
        IPerspectiveRegistry reg = PlatformUI.getWorkbench()
                .getPerspectiveRegistry();
        Set<String> managed = new HashSet<String>(
                VizPerspectiveListener.getManagedPerspectives());
        for (IPerspectiveDescriptor perspective : reg.getPerspectives()) {
            if (managed.contains(perspective.getId()) == false) {
                org.eclipse.ui.internal.registry.PerspectiveDescriptor sync = (org.eclipse.ui.internal.registry.PerspectiveDescriptor) perspective;
                if (sync.getConfigElement() != null) {
                    IExtension ext = sync.getConfigElement()
                            .getDeclaringExtension();
                    ((org.eclipse.ui.internal.registry.PerspectiveRegistry) reg)
                            .removeExtension(ext, new Object[] { perspective });
                } else {
                    sync.deleteCustomDefinition();
                }
            }
        }
    }

    /**
     * Removes options from the rcp preferences menu
     */
    private void removeExtraPreferences() {
        // remove top level preference pages
        PreferenceManager preferenceManager = PlatformUI.getWorkbench()
                .getPreferenceManager();
        preferenceManager.remove("org.eclipse.team.ui.TeamPreferences");
        preferenceManager
                .remove("org.eclipse.update.internal.ui.preferences.MainPreferencePage");
        preferenceManager.remove("org.eclipse.debug.ui.DebugPreferencePage");
        preferenceManager
                .remove("org.eclipse.jdt.ui.preferences.JavaBasePreferencePage");
        preferenceManager.remove("ValidationPreferencePage");

        // remove subnode preference pages
        IPreferenceNode[] topNodes = preferenceManager.getRootSubNodes();
        for (IPreferenceNode root : topNodes) {
            String rootId = root.getId();
            if (rootId.equals("org.eclipse.ui.preferencePages.Workbench")) {
                root.remove("org.eclipse.search.preferences.SearchPreferencePage");
                root.remove("org.eclipse.ui.preferencePages.Workspace");
            } else if (rootId.equals("org.python.pydev.prefs")) {
                root.remove("org.python.pydev.ui.pythonpathconf.interpreterPreferencesPageJython");
                root.remove("org.python.pydev.ui.pythonpathconf.interpreterPreferencesPageIronpython");
                root.remove("org.python.pydev.prefs.pylint");
                root.remove("org.python.pydev.prefs.pyunitPage");
                root.remove("org.python.pydev.jython.ui.JyScriptingPreferencesPage");
            } else if (rootId.equals("org.eclipse.wst.xml.ui.preferences.xml")) {
                root.remove("org.eclipse.wst.xml.core.ui.XMLCatalogPreferencePage");
            }
        }

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
        String perspective = ProgramArguments.getInstance().getString(
                "-perspective");
        IPerspectiveDescriptor desc = getSpecifiedPerspective(perspective);
        if (desc != null) {
            return desc.getId();
        }
        IPerspectiveRegistry registry = PlatformUI.getWorkbench()
                .getPerspectiveRegistry();
        perspective = registry.getDefaultPerspective();
        desc = getSpecifiedPerspective(perspective);
        if (desc != null) {
            return desc.getId();
        }

        // No default perspective specified in preference, look for managed
        for (String pid : VizPerspectiveListener.getManagedPerspectives()) {
            perspective = pid;
            break;
        }
        return null;
    }

    protected IPerspectiveDescriptor getSpecifiedPerspective(String perspective) {
        IPerspectiveRegistry registry = PlatformUI.getWorkbench()
                .getPerspectiveRegistry();

        if (perspective != null) {
            // Check Id first
            for (IPerspectiveDescriptor desc : registry.getPerspectives()) {
                if (perspective.equals(desc.getId())) {
                    return desc;
                }
            }

            // Check label second
            for (IPerspectiveDescriptor desc : registry.getPerspectives()) {
                if (perspective.equalsIgnoreCase(desc.getLabel())) {
                    return desc;
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchAdvisor#createWorkbenchWindowAdvisor
     * (org.eclipse.ui.application.IWorkbenchWindowConfigurer)
     */
    @Override
    public final WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
            IWorkbenchWindowConfigurer configurer) {
        if (createdMenus == false) {
            createdMenus = true;
            createDynamicMenus();
        }
        return createNewWindowAdvisor(configurer);
    }

    /**
     * Create a new {@link WorkbenchWindowAdvisor}
     * 
     * @param configurer
     * @return
     */
    protected WorkbenchWindowAdvisor createNewWindowAdvisor(
            IWorkbenchWindowConfigurer configurer) {
        return new VizWorkbenchWindowAdvisor(configurer);
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

    @Override
    public void postStartup() {
        super.postStartup();

        IContextService service = (IContextService) PlatformUI.getWorkbench()
                .getService(IContextService.class);
        service.activateContext("com.raytheon.uf.viz.application.cave");

        if (startupTimer != null) {
            startupTimer.stop();
            System.out.println("Workbench startup time: "
                    + startupTimer.getElapsedTime() + " ms");
        }

    }

    /**
     * Uses {@link DiscoverMenuContributions} to create dynamic menu
     * contributions
     */
    protected void createDynamicMenus() {
        DiscoverMenuContributions.discoverContributions();
    }

    public void setStartupTimer(ITimer startupTimer) {
        this.startupTimer = startupTimer;
    }

}
