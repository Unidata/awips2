package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.control.IToolModeChangedListener;
import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.PreferencesDialog;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.MatrixNavigatorComposite;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;

/***
 * 
 * This class is a Composite which contains only a ToolBar. It is tightly
 * coupled with the EnsembleToolViewer (ETV) and is intended to be stored only
 * inside the ETV's top-level CTabFolder.
 * 
 * It contains tool bar items ("buttons") which change behavior depending upon
 * which state the Ensemble Tool is in: Legend browser or Matrix navigator mode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer      Description
 * ------------ ---------- ----------- --------------------------
 * Oct 15, 2015   12565      polster     Initial creation
 * Jan 15, 2016   12301      jing        Added distribution feature
 * Oct 12, 2016   19443      polster     Moved model family dialog access
 * Dec 29, 2016   19325      jing        Added image items in the calculation menu
 * Mar 01, 2017   19443      polster     Fixed toggle editability problem
 * Jun 01, 2017   19443      polster     Switched to using Eclipse contribution/actions
 * Dec 01, 2017   41520      polster     Added test for isDisposed
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public class EnsembleToolBar extends Composite
        implements IToolModeChangedListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleToolBar.class);

    private EnsembleToolViewer ensembleToolViewer = null;

    private RemoveAllAction removeAllAction = null;

    private ToolRelevantDropDownAction toolRelevantAction = null;

    private OpenBrowserAction openBrowserAction = null;

    private EditableToggleAction toggleEditableAction = null;

    private Menu toolRelevantMenu = null;

    private ToolItem actionsDropdownToolItem = null;

    private PreferencesDialog prefsDialog = null;

    private IToolBarManager toolbarMgr = null;

    private final LegendsBrowserCalculationSelectionAdapter legendsCalculationListener = new LegendsBrowserCalculationSelectionAdapter();

    public EnsembleToolBar(Composite parent, IToolBarManager tbm, int style,
            EnsembleToolViewer etv) {
        super(parent, style);
        ensembleToolViewer = etv;
        toolbarMgr = tbm;
        EnsembleTool.getInstance().addToolModeChangedListener(this);
        createToolBarActions();
    }

    private void createToolBarActions() {

        toolRelevantMenu = new Menu(getShell());

        removeAllAction = new RemoveAllAction();

        toolRelevantAction = new ToolRelevantDropDownAction();

        openBrowserAction = new OpenBrowserAction();

        toggleEditableAction = new EditableToggleAction();

        toolbarMgr.add(openBrowserAction);
        toolbarMgr.add(removeAllAction);
        toolbarMgr.add(toolRelevantAction);
        toolbarMgr.add(toggleEditableAction);
    }

    @Override
    public void dispose() {
        EnsembleTool.getInstance().removeToolModeChangedListener(this);
    }

    public void setToolMode(EnsembleToolMode mode) {
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
            addLegendsPlanViewItems();
        } else if (mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            addLegendsTimeSeriesItems();
        } else if (mode == EnsembleToolMode.MATRIX) {
            addMatrixItems();
        }
        /*
         * TODO: Need the menu changes to refresh immediately.
         */
        if (!getParent().isDisposed()) {
            getParent().redraw();
        }
    }

    public void setEnabled(boolean isEnabled) {
        actionsDropdownToolItem.setEnabled(isEnabled);
    }

    public void setEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (enabled) {
                    toggleEditableAction.setToolTipText("Tool Off");
                } else {
                    toggleEditableAction.setToolTipText("Tool On");
                }

                openBrowserAction.setEnabled(enabled);
                toolRelevantAction.setEnabled(enabled);
                removeAllAction.setEnabled(enabled);

                /* Always on items */
                toggleEditableAction.setEnabled(true);
            }
        });

    }

    public void setToolbarMode(EnsembleToolMode mode) {

        // toolRelevantActions.setToolMode(mode);
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            openBrowserAction.setToolTipText("Open Volume Browser");
        } else if (mode == EnsembleToolMode.MATRIX) {
            openBrowserAction.setToolTipText("Open a Model Family");
        }

    }

    private boolean isToolEnabled() {
        boolean isToolEnabled = false;
        EnsembleTool et = EnsembleTool.getInstance();
        if (et != null) {
            IDisplayPaneContainer editor = et.getActiveEditor();
            if (editor != null) {
                EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(editor);
                if (toolLayer != null) {
                    if (toolLayer.isEmpty()) {
                        isToolEnabled = false;
                    } else {
                        isToolEnabled = true;
                    }
                }
            }
        }
        return isToolEnabled;
    }

    protected void addLegendsPlanViewItems() {

        toolRelevantAction.setEnabled(true);
        removeAllMenuItems();

        new MenuItem(toolRelevantMenu, SWT.SEPARATOR);

        boolean isEnabled = isToolEnabled();

        add(Calculation.MEAN.getTitle(), "Calculate mean of visible resources",
                isEnabled, legendsCalculationListener);
        add(Calculation.MEAN_IMAGE.getTitle(),
                "Calculate mean image of visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MIN.getTitle(),
                "Calculate minimum on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MIN_IMAGE.getTitle(),
                "Calculate minimum image on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MAX.getTitle(),
                "Calculate maxixum on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MAX_IMAGE.getTitle(),
                "Calculate maxixum image on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MEDIAN.getTitle(),
                "Calculate median on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MEDIAN_IMAGE.getTitle(),
                "Calculate median image on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.RANGE.getTitle(),
                "Calculate range on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.RANGE_IMAGE.getTitle(),
                "Calculate range image on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.SUMMATION.getTitle(),
                "Calculate summation on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.SUMMATION_IMAGE.getTitle(),
                "Calculate summation image on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.STANDARD_DEVIATION.getTitle(),
                "Calculate standard deviation on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.STANDARD_DEVIATION_IMAGE.getTitle(),
                "Calculate standard deviation image on visible resources",
                isEnabled, legendsCalculationListener);
        add(Calculation.VALUE_SAMPLING.getTitle(), "Turn on value sampling",
                isEnabled, legendsCalculationListener);
        add(Calculation.HISTOGRAM_SAMPLING.getTitle(),
                "Turn on text histogram sampling", isEnabled,
                legendsCalculationListener);
        add(Calculation.HISTOGRAM_GRAPHICS.getTitle(),
                "Turn on distribution viewer sampling", isEnabled,
                legendsCalculationListener);

        /*
         * TODO: Preferences have been disabled for the 17.3.1 release. Will put
         * back in the next release.
         */

        // new MenuItem(toolRelevantMenu, SWT.SEPARATOR);
        //
        // MenuItem mi = new MenuItem(toolRelevantMenu, SWT.NONE);
        // mi.setText(GlobalPreferencesComposite.PREFERENCES_NAME);
        // mi.setEnabled(true);
        // mi.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // prefsDialog = new PreferencesDialog(getParent().getShell());
        // if (prefsDialog.open() == Window.OK) {
        // prefsDialog.close();
        // prefsDialog = null;
        // }
        //
        // }
        // });

    }

    private void addLegendsTimeSeriesItems() {
        toolRelevantAction.setEnabled(true);

        removeAllMenuItems();

        new MenuItem(toolRelevantMenu, SWT.SEPARATOR);

        boolean isEnabled = isToolEnabled();

        add(Calculation.MEAN.getTitle(), "Calculate mean of visible resources",
                true, legendsCalculationListener);
        add(Calculation.MIN.getTitle(),
                "Calculate minimum on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MAX.getTitle(),
                "Calculate maxixum on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.MEDIAN.getTitle(),
                "Calculate median on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.RANGE.getTitle(),
                "Calculate range on visible resources", isEnabled,
                legendsCalculationListener);

        /*
         * TODO: Preferences have been disabled for the 17.3.1 release. Will put
         * back in the next release.
         */

        // new MenuItem(toolRelevantMenu, SWT.SEPARATOR);
        //
        // MenuItem mi = new MenuItem(toolRelevantMenu, SWT.NONE);
        // mi.setText(GlobalPreferencesComposite.PREFERENCES_NAME);
        // mi.setEnabled(true);
        // mi.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // prefsDialog = new PreferencesDialog(getParent().getShell());
        // if (prefsDialog.open() == Window.OK) {
        // prefsDialog.close();
        // prefsDialog = null;
        // }
        //
        // }
        // });

    }

    /**
     * TODO: No tool bar drop down menu items for the Matrix tool. Save for
     * future use.
     */
    protected void addMatrixItems() {
        removeAllMenuItems();
        toolRelevantAction.setEnabled(false);
    }

    protected void removeAllMenuItems() {
        for (MenuItem mi : toolRelevantMenu.getItems()) {
            mi.dispose();
        }
    }

    protected void add(String item, String tooltip, boolean isEnabled,
            SelectionAdapter selectionListener) {
        MenuItem mi = new MenuItem(toolRelevantMenu, SWT.NONE);
        mi.setText(item);
        mi.setEnabled(isEnabled);
        mi.addSelectionListener(selectionListener);
    }

    class LegendsBrowserCalculationSelectionAdapter extends SelectionAdapter {

        public void widgetSelected(SelectionEvent event) {
            MenuItem selected = (MenuItem) event.widget;

            for (Calculation c : Calculation.values()) {
                if (c.getTitle().equals(selected.getText())) {
                    EnsembleTool.getInstance().calculate(c);
                }
            }
        }
    }

    public class RemoveAllAction extends Action implements IWorkbenchAction {

        private static final String ID = "gov.noaa.gsd.viz.ensemble.remove.all.action";

        public RemoveAllAction() {
            super("Remove All", Action.AS_PUSH_BUTTON);
            setId(ID);
            ImageDescriptor imgDscr = ImageDescriptor
                    .createFromImage(EnsembleToolImageStore.REMOVE_ALL_IMG);
            setImageDescriptor(imgDscr);

        }

        public void run() {
            String clearResourcesPrompt = null;
            /* only act on clearing resources if the active tool layer exists */
            if (EnsembleTool.getInstance().getToolLayer() != null) {
                if (EnsembleTool.getInstance()
                        .getToolMode() == EnsembleToolMode.MATRIX) {

                    if (!EnsembleTool.getInstance().getToolLayer().isEmpty()
                            || (MatrixNavigatorComposite.isExtant()
                                    && (MatrixNavigatorComposite
                                            .isEmpty() == false))) {
                        clearResourcesPrompt = "Are you sure you want to clear all Matrix resources in the active editor?";
                        boolean isOkay = MessageDialog.open(
                                MessageDialog.QUESTION, getShell(),
                                "Confirm Clear All Matrix Entries",
                                clearResourcesPrompt, SWT.NONE);
                        if (isOkay) {
                            EnsembleTool.getInstance().clearToolLayer();
                        }
                    }
                } else {
                    boolean isFull = (EnsembleTool.getInstance().getToolLayer()
                            .isEmpty()) ? false : true;

                    clearResourcesPrompt = "Are you sure you want to clear all Ensemble Tool resources in the active editor?";
                    if (isFull) {
                        boolean isOkay = MessageDialog.open(
                                MessageDialog.QUESTION, getShell(),
                                "Confirm Clear All Entries",
                                clearResourcesPrompt, SWT.NONE);
                        if (isOkay) {
                            EnsembleTool.getInstance().clearToolLayer();
                        }
                    }
                }
            }
        }

        public void dispose() {
        }

    }

    public class ToolRelevantDropDownAction extends Action
            implements IWorkbenchAction, IMenuCreator {

        private static final String ID = "gov.noaa.gsd.viz.ensemble.tool.relevant.action";

        public ToolRelevantDropDownAction() {
            super("Tool Actions", Action.AS_DROP_DOWN_MENU);
            setId(ID);
            setToolTipText("Actions");
            ImageDescriptor imgDscr = ImageDescriptor
                    .createFromImage(EnsembleToolImageStore.OPEN_TOOLS_IMG);
            setImageDescriptor(imgDscr);
            setMenuCreator(this);
        }

        public void run() {
            if (toolRelevantMenu == null || toolRelevantMenu.isDisposed()
                    || toolRelevantMenu.getItemCount() <= 0) {
                setToolMode(EnsembleTool.getInstance().getToolMode());
            }
        }

        public void dispose() {
        }

        @Override
        public Menu getMenu(Control parent) {
            return toolRelevantMenu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            return toolRelevantMenu;
        }

    }

    public class OpenBrowserAction extends Action implements IWorkbenchAction {

        private static final String ID = "gov.noaa.gsd.viz.ensemble.open.browser.action";

        public OpenBrowserAction() {
            super("Open Browser", Action.AS_PUSH_BUTTON);
            setId(ID);
            setToolTipText("Open Volume Browser");
            ImageDescriptor imgDscr = ImageDescriptor
                    .createFromImage(EnsembleToolImageStore.OPEN_BROWSER_IMG);
            setImageDescriptor(imgDscr);
        }

        public void run() {
            EnsembleToolMode mode = EnsembleTool.getInstance().getToolMode();
            if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                    || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {

                IServiceLocator serviceLocator = PlatformUI.getWorkbench();
                ICommandService commandService = (ICommandService) serviceLocator
                        .getService(ICommandService.class);

                Command command = commandService.getCommand(
                        "com.raytheon.viz.volumebrowser.volumeBrowserRef");

                /**
                 * Optionally pass a ExecutionEvent instance, default (empty)
                 * signature creates blank event
                 */
                try {
                    command.executeWithChecks(new ExecutionEvent());
                } catch (ExecutionException | NotDefinedException
                        | NotEnabledException | NotHandledException e1) {
                    statusHandler.warn(e1.getLocalizedMessage()
                            + "; Unable to open Volume Browser");
                }

            } else if (mode == EnsembleTool.EnsembleToolMode.MATRIX) {

                ensembleToolViewer.getMatrixNavigator()
                        .openFamilyLoaderDialog();

            }
        }

        public void dispose() {
        }

    }

    /**
     * This action turns the entire Ensemble Tool on/off by making it
     * editable/not-editable.
     */
    public class EditableToggleAction extends Action
            implements IWorkbenchAction {

        private static final String ID = "gov.noaa.gsd.viz.ensemble.editable.toggle.action";

        public EditableToggleAction() {
            super("Toggle Editable", Action.AS_PUSH_BUTTON);
            setId(ID);
            setToolTipText("Toggle Editable");
            ImageDescriptor imgDscr = ImageDescriptor.createFromImage(
                    EnsembleToolImageStore.TOGGLE_EDITABLE_IMG);
            setImageDescriptor(imgDscr);
        }

        public void run() {
            /* In association with VLab AWIPS2_GSD Issue #29762 */
            EnsembleTool.getInstance()
                    .setEditable(!EnsembleTool.getInstance().isToolEditable());
        }

        public void dispose() {
        }

    }

    @Override
    public void toolModeChanged(EnsembleToolMode toolmode) {
        setToolMode(toolmode);
    }

}
