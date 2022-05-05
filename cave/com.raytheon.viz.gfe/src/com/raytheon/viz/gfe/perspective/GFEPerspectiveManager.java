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
package com.raytheon.viz.gfe.perspective;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.Parameterization;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.bindings.Binding;
import org.eclipse.jface.bindings.BindingManager;
import org.eclipse.jface.bindings.Scheme;
import org.eclipse.jface.bindings.keys.KeyBinding;
import org.eclipse.jface.bindings.keys.KeySequence;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.internal.keys.BindingService;
import org.eclipse.ui.keys.IBindingService;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.actions.FormatterlauncherAction;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.GFEMapRenderableDisplay;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.internal.GFESpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.rsc.GFELegendResourceData;
import com.raytheon.viz.gfe.statusline.ISCSendEnable;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.cmenu.ZoomMenuAction;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Manages the life cycle of the GFE Perspectives
 *
 * Installs a perspective watcher that handles the transitions in and out of the
 * GFE perspectives.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 17, 2008  1223     randerso  Initial creation
 * Oct 06, 2008  1433     chammack  Removed gfe status bars
 * Apr 09, 2009  1288     rjpeter   Added saving of the renderable display.
 * Jun 11, 2009  1947     rjpeter   Moved parm save hook to GridManagerView.
 * Apr 27, 2010           mschenke  refactor for common perspective switching
 * Jul 07, 2011  9897     ryu       close formatters on perspective close/reset
 * Aug 20,2012   1077     randerso  Added support for bgColor setting
 * Oct 23, 2012  1287     rferrel   Changes for non-blocking
 *                                  FormattrLauncherDialog.
 * Dec 09, 2013  2367     dgilling  Remove shutdown of ProcedureJob and
 *                                  SmartToolJob.
 * Jan 14, 2014  2594     bclement  added low memory notification
 * Aug 24, 2015  4749     dgilling  Shutdown TaskManager on perspective close.
 * Sep 21, 2015  4858     dgilling  Display warning message when DRT mode is
 *                                  enabled.
 * Jan 25, 2015  5256     njensen   Add bindings to existing BindingManager and
 *                                  BindingService
 * Mar 16, 2017  6092     randerso  Moved dispose of spatialDisplayManager into
 *                                  DataManager
 * Apr 21, 2017  6239     randerso  Prevent UI deadlock if an async task calls
 *                                  getPreferenceStore() while the dialog is
 *                                  open.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Mar 07, 2018  6608     randerso  Added shutdown listener to stop all
 *                                  background jobs
 * Jul 09, 2018  7315     bsteffen  Make editors unclosable in the E4 model.
 * Jan 31, 2019  7545     randerso  Updated to install/remove key bindings every
 *                                  time the perspective is opened/closed.
 *
 * </pre>
 *
 * @author randerso
 */

@SuppressWarnings("restriction")
public class GFEPerspectiveManager extends AbstractCAVEPerspectiveManager
        implements ISimulatedTimeChangeListener, IWorkbenchListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEPerspectiveManager.class);

    private static final String PERSPECTIVE_NAME = "GFE";

    private static final List<String> FEATURES_DISABLED_IN_SIM_TIME = Arrays
            .asList("ISC send", "Publishing grids",
                    "Running text formatters and/or transmitting products",
                    "Running scripts from the Product Scripts Dialog");

    /** The GFE Perspective Class */
    public static final String GFE_PERSPECTIVE = "com.raytheon.viz.ui.GFEPerspective";

    private List<KeyBinding> keyBindings;

    /**
     * Constructor
     */
    public GFEPerspectiveManager() {

    }

    @Override
    public void open() {
        PlatformUI.getWorkbench().addWorkbenchListener(this);
        contextActivator = new GFEContextActivator(page);
        loadDefaultBundle("gfe/default-procedure.xml");

        AbstractEditor gfeEditor = (AbstractEditor) EditorUtil
                .getActiveEditor();
        // Disable closing on the active editor
        MPart modelPart = gfeEditor.getSite().getService(MPart.class);
        modelPart.setCloseable(false);

        IDisplayPane pane = gfeEditor.getActiveDisplayPane();

        DataManager dm = DataManagerUIFactory.getInstance(perspectiveWindow);

        String[] maps = GFEPreference.getStringArray("MapBackgrounds_default");

        MapManager mapMgr = MapManager
                .getInstance((IMapDescriptor) pane.getDescriptor());

        for (String map : maps) {
            mapMgr.loadMapByBundleName(map);
        }

        registerKeyBindings();

        IRenderableDisplay display = pane.getRenderableDisplay();
        if (display instanceof GFEMapRenderableDisplay) {
            ((GFEMapRenderableDisplay) display).setDataManager(dm);
        }
        // Add GFE Legend
        try {
            ResourcePair legend = ResourcePair
                    .constructSystemResourcePair(new GFELegendResourceData(dm));
            legend.instantiateResource(pane.getDescriptor(), true);
            pane.getDescriptor().getResourceList().add(legend);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding GFE Legend",
                    e);
        }

        if (dm != null) {
            ISpatialDisplayManager mgr = dm.getSpatialDisplayManager();

            if (mgr instanceof GFESpatialDisplayManager) {
                try {
                    ((GFESpatialDisplayManager) mgr).populate(gfeEditor);
                } catch (Throwable e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Populating the map with GFE data has failed.", e);
                }
            } else {
                throw new IllegalStateException(
                        this.getClass().getName() + " must be used with "
                                + GFESpatialDisplayManager.class.getName());
            }
        }

        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
    }

    @Override
    public void activate() {
        super.activate();

        displaySimulatedTimeWarning();

        // Hack to disable editor closing
        IWorkbenchPage activePage = perspectiveWindow.getActivePage();
        if (activePage != null) {
            for (IEditorReference ref : activePage.getEditorReferences()) {
                IEditorPart part = ref.getEditor(true);
                if (part instanceof AbstractEditor) {
                    AbstractEditor editor = (AbstractEditor) part;
                    MPart modelPart = editor.getSite().getService(MPart.class);
                    modelPart.setCloseable(false);
                }
            }
        }
    }

    @Override
    protected String getTitle(String title) {
        String config = GFEPreference.getConfigName();
        String gfeTitle = super.getTitle(title);
        gfeTitle += "(" + config + ")";
        return gfeTitle;
    }

    @Override
    public void close() {
        super.close();

        SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(this);

        DataManagerUIFactory.dispose(perspectiveWindow);

        FormatterlauncherAction.closeDialog();

        TaskManager.getInstance().shutdown();

        Message.clearHistory();

        unregisterKeyBindings();

        // Force prompt for GFE config file on next open
        Activator.getDefault().getPreferenceStore().clearConfiguration();

        PlatformUI.getWorkbench().removeWorkbenchListener(this);
    }

    @Override
    protected List<ContributionItem> getStatusLineItems() {
        List<ContributionItem> items = super.getStatusLineItems();
        items.add(new ISCSendEnable());
        return items;
    }

    private void registerKeyBindings() {
        ICommandService commandService = PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        IBindingService bindingService = PlatformUI.getWorkbench()
                .getAdapter(IBindingService.class);
        Scheme activeScheme = bindingService.getActiveScheme();
        String schemeId = activeScheme.getId();
        BindingManager bindingManager = ((BindingService) bindingService)
                .getBindingManager();

        try {
            // get currentBindings and remove any GFE ShortCut bindings
            String contextId = "com.raytheon.viz.gfe.GFEShortCutContext";
            keyBindings = new ArrayList<>();
            for (int i = 1; i < 201; i++) {
                String shortCut = "ShortCut" + i;
                String[] keyDef = GFEPreference.getStringArray(shortCut);
                if ((keyDef != null) && (keyDef.length > 0)) {
                    if (keyDef.length != 4) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Invalid GFE ShortCut definition " + shortCut
                                        + ": definition should contain 4 strings");
                        continue;
                    }

                    String key;
                    if (keyDef[1].isEmpty()
                            || "None".equalsIgnoreCase(keyDef[1])) {
                        key = keyDef[0].toUpperCase();
                    } else if ("Ctl".equalsIgnoreCase(keyDef[1])) {
                        key = "CTRL" + KeyStroke.KEY_DELIMITER
                                + keyDef[0].toUpperCase();
                    } else {
                        key = keyDef[1].toUpperCase() + KeyStroke.KEY_DELIMITER
                                + keyDef[0].toUpperCase();
                    }
                    KeyStroke keyStroke;
                    try {
                        keyStroke = KeyStroke.getInstance(key);
                    } catch (Exception e) {
                        statusHandler.error("Invalid GFE ShortCut definition "
                                + shortCut + ": " + e.getLocalizedMessage(), e);
                        continue;
                    }
                    KeySequence keySequence = KeySequence
                            .getInstance(new KeyStroke[] { keyStroke });

                    Command command = null;
                    Parameterization[] parms = null;
                    if ("SmartTool".equals(keyDef[2])) {
                        command = commandService.getCommand(
                                "com.raytheon.viz.gfe.actions.RunSmartToolAction");
                        parms = new Parameterization[] { new Parameterization(
                                command.getParameter("name"), keyDef[3]) };
                    } else if ("Procedure".equals(keyDef[2])) {
                        command = commandService.getCommand(
                                "com.raytheon.viz.gfe.actions.RunProcedureAction");
                        parms = new Parameterization[] { new Parameterization(
                                command.getParameter("name"), keyDef[3]) };
                    } else if ("EditTool".equals(keyDef[2])) {
                        String commandId = null;
                        if ("Sample".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.editTools.sample";
                        } else if ("Pencil".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.editTools.pencil";
                        } else if ("Contour".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.editTools.contour";
                        } else if ("MoveCopy".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.editTools.moveCopy";
                        } else if ("DrawEditArea".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.editTools.selectPoints";
                        } else {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Invalid GFE ShortCut definition "
                                            + shortCut + ": " + keyDef[3]
                                            + " is not a valid EditTool");
                            continue;
                        }
                        command = commandService.getCommand(commandId);
                    } else if ("Toggle".equals(keyDef[2])) {
                        String commandId = null;
                        if ("ISC".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.actions.showISCGrids";
                        } else if ("TEGM".equals(keyDef[3])) {
                            commandId = "com.raytheon.viz.gfe.actions.toggleTemporalEditor";
                        } else {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Invalid GFE ShortCut definition "
                                            + shortCut + ": " + keyDef[3]
                                            + " is not a valid Toggle");
                            continue;
                        }
                        command = commandService.getCommand(commandId);
                    } else {
                        statusHandler.handle(Priority.PROBLEM,
                                "Invalid GFE ShortCut definition " + shortCut
                                        + ": " + keyDef[2]
                                        + " is not a valid Action");
                        continue;
                    }
                    ParameterizedCommand parmCmd = new ParameterizedCommand(
                            command, parms);

                    // add the binding
                    KeyBinding b = new KeyBinding(keySequence, parmCmd,
                            schemeId, contextId, null, null, null,
                            Binding.USER);
                    keyBindings.add(b);
                    bindingManager.addBinding(b);
                    ((BindingService) bindingService).addBinding(b);
                }
            }
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private void unregisterKeyBindings() {
        if (keyBindings != null) {
            IBindingService bindingService = PlatformUI.getWorkbench()
                    .getAdapter(IBindingService.class);
            BindingManager bindingManager = ((BindingService) bindingService)
                    .getBindingManager();

            for (KeyBinding b : keyBindings) {
                bindingManager.removeBinding(b);
                ((BindingService) bindingService).removeBinding(b);
            }
            keyBindings.clear();
            keyBindings = null;
        }
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        super.addContextMenuItems(menuManager, container, pane);
        menuManager.add(new ZoomMenuAction(container));
    }

    @Override
    protected String getLowMemoryMessage(long availMemory) {
        return super.getLowMemoryMessage(availMemory)
                + "\n\nConsider saving Fcst grids to free up memory.";
    }

    @Override
    public void timechanged() {
        /*
         * Have to jump to the UI thread or else getActiveWorkbenchWindow will
         * fail.
         */
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                String activePerspective = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .getPerspective().getId();
                if (GFE_PERSPECTIVE.equals(activePerspective)) {
                    displaySimulatedTimeWarning();
                }
            }
        });
    }

    private void displaySimulatedTimeWarning() {
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayPerspectiveLevelWarning(
                    perspectiveWindow.getShell(), PERSPECTIVE_NAME,
                    FEATURES_DISABLED_IN_SIM_TIME);
        }
    }

    @Override
    public boolean preShutdown(IWorkbench workbench, boolean forced) {
        return true;
    }

    @Override
    public void postShutdown(IWorkbench workbench) {
        DataManagerUIFactory.dispose(perspectiveWindow);
        TaskManager.getInstance().shutdown();
    }
}