package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.GlobalPreferencesComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.PreferencesDialog;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.ModelFamilyDialog;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.EditorUtil;

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
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public class EnsembleToolBar extends Composite {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleToolBar.class);

    private EnsembleToolViewer ensembleToolViewer = null;

    private CTabFolder rootTabFolder = null;

    private ToolBar toolBar = null;

    private ToolItem browserToolItem = null;

    private ToolItem powerToggleToolItem = null;

    private ToolItem clearAllToolItem = null;

    private Menu dropdownMenu = null;

    private ToolItem actionsDropdownToolItem = null;

    private EnsembleToolItemActionDropdown toolRelevantActions = null;

    private PreferencesDialog prefsDialog = null;

    private final LegendsBrowserCalculationSelectionAdapter legendsCalculationListener = new LegendsBrowserCalculationSelectionAdapter();

    public EnsembleToolBar(Composite parent, int style, EnsembleToolViewer etv) {
        super(parent, style);
        rootTabFolder = (CTabFolder) parent;
        ensembleToolViewer = etv;
        createToolBar();
    }

    private void createToolBar() {
        Composite toolbarComposite = new Composite(rootTabFolder, SWT.NONE);
        toolbarComposite.setBackground(GlobalColor.get(GlobalColor.WHITE));

        FillLayout toolbarContainer_fl = new FillLayout(SWT.HORIZONTAL);
        toolbarContainer_fl.marginWidth = 1;
        toolbarContainer_fl.marginHeight = 1;
        toolbarComposite.setLayout(toolbarContainer_fl);

        /* Fill the tool bar and add it to the main tab folder */
        toolBar = makeToolBar(toolbarComposite);
        Rectangle r = toolbarComposite.getBounds();
        r.height = r.height + 32;
        toolbarComposite.setBounds(r);
        rootTabFolder.setTopRight(toolbarComposite);

    }

    public void disableTool() {
        powerToggleToolItem.setEnabled(false);
    }

    synchronized public void setViewEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (enabled) {
                    powerToggleToolItem
                            .setImage(EnsembleToolImageStore.POWER_ON_IMG);
                    powerToggleToolItem.setToolTipText("Tool Off");
                } else {
                    powerToggleToolItem
                            .setImage(EnsembleToolImageStore.POWER_OFF_IMG);
                    powerToggleToolItem.setToolTipText("Tool On");
                }

                browserToolItem.setEnabled(enabled);
                toolRelevantActions.setEnabled(enabled);

                /* Always on items */
                powerToggleToolItem.setEnabled(true);
                toolBar.setEnabled(true);

            }
        });

    }

    public void setToolbarMode(EnsembleToolMode mode) {

        toolRelevantActions.setToolMode(mode);
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            browserToolItem.setToolTipText("Open Volume Browser");
            browserToolItem.setImage(EnsembleToolImageStore.VOLUME_BROWSER_IMG);
        } else if (mode == EnsembleToolMode.MATRIX) {
            browserToolItem.setToolTipText("Open a Model Family");
            browserToolItem.setImage(EnsembleToolImageStore.MATRIX_BROWSER_IMG);
        }

    }

    /*
     * Create the ViewPart's main tool bar.
     */
    private ToolBar makeToolBar(Composite parent) {

        toolBar = new ToolBar(parent, SWT.BORDER_SOLID);

        browserToolItem = new ToolItem(toolBar, SWT.PUSH);
        browserToolItem.setImage(EnsembleToolImageStore.VOLUME_BROWSER_IMG);
        browserToolItem.setToolTipText("Volume Browser");

        browserToolItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                EnsembleToolMode mode = EnsembleTool.getInstance()
                        .getToolMode();
                if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                        || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {

                    IServiceLocator serviceLocator = PlatformUI.getWorkbench();
                    ICommandService commandService = (ICommandService) serviceLocator
                            .getService(ICommandService.class);

                    Command command = commandService
                            .getCommand("com.raytheon.viz.volumebrowser.volumeBrowserRef");

                    /**
                     * Optionally pass a ExecutionEvent instance, default
                     * (empty) signature creates blank event
                     */
                    try {
                        command.executeWithChecks(new ExecutionEvent());
                    } catch (ExecutionException | NotDefinedException
                            | NotEnabledException | NotHandledException e1) {
                        statusHandler.warn(e1.getLocalizedMessage()
                                + "; Unable to open Volume Browser");
                    }

                } else if (mode == EnsembleTool.EnsembleToolMode.MATRIX) {

                    ModelFamilyDialog cd = new ModelFamilyDialog(
                            EnsembleToolViewer.getShell(), ensembleToolViewer
                                    .getMatrixNavigator());
                    cd.setBlockOnOpen(true);
                    if (cd.open() == Window.OK) {
                        cd.close();
                    }

                }
            }
        });

        clearAllToolItem = new ToolItem(toolBar, SWT.PUSH);
        clearAllToolItem.setImage(EnsembleToolImageStore.CLEAR_ALL_IMG);
        clearAllToolItem.setToolTipText("Clear All");
        clearAllToolItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                boolean isFull = (EnsembleTool.getInstance().getToolLayer() == null || EnsembleTool
                        .getInstance().getToolLayer().isEmpty()) ? false : true;

                if (isFull) {
                    boolean isOkay = MessageDialog
                            .open(MessageDialog.QUESTION_WITH_CANCEL,
                                    getShell(),
                                    "Confirm Clear All",
                                    "Are you sure you want to clear all resources? \n \n  \t\t\t ... Cannot be undone!",
                                    SWT.NONE);
                    if (isOkay) {
                        EnsembleTool.getInstance().clearToolLayer();
                    }
                }

            }

        });

        toolRelevantActions = new EnsembleToolItemActionDropdown(toolBar);
        toolRelevantActions.setToolMode(EnsembleTool.getToolMode(EditorUtil
                .getActiveVizContainer()));

        ToolItem separator_3 = new ToolItem(toolBar, SWT.SEPARATOR);
        separator_3.setWidth(0);

        powerToggleToolItem = new ToolItem(toolBar, SWT.PUSH);
        powerToggleToolItem.setImage(EnsembleToolImageStore.POWER_ON_IMG);
        powerToggleToolItem.setToolTipText("Tool Off");
        powerToggleToolItem.setSelection(EnsembleToolViewer.isViewEditable());
        powerToggleToolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ensembleToolViewer.toggleViewEditable();
            }
        });

        parent.pack();

        return toolBar;
    }

    protected void addLegendsPlanViewItems() {
        removeAllMenuItems();

        new MenuItem(dropdownMenu, SWT.SEPARATOR);

        boolean isEnabled = (EnsembleTool.getInstance().getToolLayer() == null || EnsembleTool
                .getInstance().getToolLayer().isEmpty()) ? false : true;

        add(Calculation.MEAN.getTitle(), "Calculate mean of visible resources",
                isEnabled, legendsCalculationListener);
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
        add(Calculation.SUMMATION.getTitle(),
                "Calculate summation on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.STANDARD_DEVIATION.getTitle(),
                "Calculate standard deviation on visible resources", isEnabled,
                legendsCalculationListener);
        add(Calculation.VALUE_SAMPLING.getTitle(), "Turn on value sampling",
                isEnabled, legendsCalculationListener);
        add(Calculation.HISTOGRAM_SAMPLING.getTitle(),
                "Turn on text histogram sampling", isEnabled,
                legendsCalculationListener);
        add(Calculation.HISTOGRAM_GRAPHICS.getTitle(),
                "Turn on distribution viewer sampling", isEnabled,
                legendsCalculationListener);

        new MenuItem(dropdownMenu, SWT.SEPARATOR);

        MenuItem mi = new MenuItem(dropdownMenu, SWT.NONE);
        mi.setText(GlobalPreferencesComposite.PREFERENCES_NAME);
        mi.setEnabled(true);
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                prefsDialog = new PreferencesDialog(getParent().getShell());
                if (prefsDialog.open() == Window.OK) {
                    prefsDialog.close();
                    prefsDialog = null;
                }

            }
        });
    }

    private void addLegendsTimeSeriesItems() {
        removeAllMenuItems();
        new MenuItem(dropdownMenu, SWT.SEPARATOR);
        boolean isEnabled = (EnsembleTool.getInstance().getToolLayer() == null || EnsembleTool
                .getInstance().getToolLayer().isEmpty()) ? false : true;
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

        new MenuItem(dropdownMenu, SWT.SEPARATOR);

        MenuItem mi = new MenuItem(dropdownMenu, SWT.NONE);
        mi.setText(GlobalPreferencesComposite.PREFERENCES_NAME);
        mi.setEnabled(true);
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                prefsDialog = new PreferencesDialog(getParent().getShell());
                if (prefsDialog.open() == Window.OK) {
                    prefsDialog.close();
                    prefsDialog = null;
                }

            }
        });
    }

    protected void addMatrixItems() {
        removeAllMenuItems();
        new MenuItem(dropdownMenu, SWT.SEPARATOR);
        boolean isEnabled = (EnsembleTool.getInstance().getToolLayer() == null || EnsembleTool
                .getInstance().getToolLayer().isEmpty()) ? false : true;

        SelectionAdapter matchRscAction = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (ensembleToolViewer != null) {
                    ensembleToolViewer.matchLikeResources();
                }
            }
        };
        add("Match Resources",
                "Match colors and density for similar resources", isEnabled,
                matchRscAction);

        SelectionAdapter refreshRscAction = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (ensembleToolViewer != null) {
                    ensembleToolViewer.refreshResources();
                }
            }
        };
        add("Refresh", "Refresh the resource", isEnabled, refreshRscAction);
    }

    protected void removeAllMenuItems() {
        for (MenuItem mi : dropdownMenu.getItems()) {
            mi.dispose();
        }
    }

    protected void add(String item, String tooltip, boolean isEnabled,
            SelectionAdapter selectionListener) {
        MenuItem mi = new MenuItem(dropdownMenu, SWT.NONE);
        mi.setText(item);
        mi.setEnabled(isEnabled);
        mi.addSelectionListener(selectionListener);
    }

    private class EnsembleToolItemActionDropdown extends SelectionAdapter {

        public EnsembleToolItemActionDropdown(ToolBar parentToolBar) {
            actionsDropdownToolItem = new ToolItem(parentToolBar, SWT.DROP_DOWN);
            actionsDropdownToolItem.setImage(EnsembleToolImageStore.GEAR_IMG);
            actionsDropdownToolItem.addSelectionListener(this);
            actionsDropdownToolItem.setToolTipText("Actions");
            dropdownMenu = new Menu(actionsDropdownToolItem.getParent()
                    .getShell());
        }

        public void setToolMode(EnsembleToolMode mode) {
            if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                addLegendsPlanViewItems();
            } else if (mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                addLegendsTimeSeriesItems();
            } else if (mode == EnsembleToolMode.MATRIX) {
                addMatrixItems();
            }
        }

        public void setEnabled(boolean isEnabled) {
            actionsDropdownToolItem.setEnabled(isEnabled);
        }

        /*
         * This is the selection listener that acts when the tool bar "actions"
         * drop down is selected.
         */
        public void widgetSelected(SelectionEvent event) {

            setToolMode(EnsembleTool.getInstance().getToolMode());

            if ((event.detail == SWT.ARROW) || (event.detail == SWT.MENU_MOUSE)) {
                ToolItem item = (ToolItem) event.widget;
                Rectangle rect = item.getBounds();
                Point pt = item.getParent()
                        .toDisplay(new Point(rect.x, rect.y));
                dropdownMenu.setLocation(pt.x, pt.y + rect.height);
                dropdownMenu.setVisible(true);
            }
        }

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
}
