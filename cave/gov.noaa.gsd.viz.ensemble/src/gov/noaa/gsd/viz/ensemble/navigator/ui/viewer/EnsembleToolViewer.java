package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.MatrixNavigationOperation;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.DistributionViewerComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.legend.LegendBrowserComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.MatrixNavigatorComposite;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

/**
 * This class represents the Ensemble Tool navigator widget, which is an RCP
 * View (ViewPart). It contains all user interface control logic for those
 * features which are otherwise accomplished by the user through the main CAVE
 * (primary) editor pane (e.g. Map editor, TimeSeries editor, etc.).
 * 
 * Features in this class include the ability to:
 * 
 * 1) Behave as proxy for the current active CAVE "editable" ensemble tool; this
 * class is an RCP CAVE view that is coupled to the EnsembleTool which
 * associates an active RCP CAVE editor ("Map", "TimeSeries", etc.) to an
 * EnsembleToolLayer. This view will display the resources for the active
 * EnsembleToolLayer.
 * 
 * 2) Display resources for the active EnsembleToolLayer.
 * 
 * 3) Allow D/2D-style resource interactivity over a given currently displayed
 * resource. This would include all the things you can do to a viz resource via
 * the context-sensitive popup menu via the D2D legend (i.e. right-click on the
 * legend).
 * 
 * 4) Allow new derived resources to be created via well-known calculation
 * methods (mean, median, sum, probability, etc.), by using only non-hidden
 * resources.
 * 
 * 5) Allow users to change interactivity behavior via a Preferences control.
 * 
 * Information pane. 6) Allow users to display meta-information for a selected
 * resource via an
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 08, 2014    5056      polster     Initial creation
 * Apr 21, 2015    7681      polster     Ignore toggle group visibility for 14.4.1
 * Apr 21, 2015    7684      polster     Fixes job which constantly checks for refresh
 * Apr 21, 2015    7682      polster     ERF 'below threshold' field entry fixed
 * Apr 21, 2015    7653      polster     Ctrl-MB1 selects viz resource again
 * Oct 15, 2015   12565      polster     Decompose this class into sub-components
 * Nov 13, 2015   13211      polster     Initiate the matrix vs legend task
 * Jan 15, 2016   12301      jing        Added distribution feature
 * Oct 12, 2016   19443      polster     Create matrix contents on first request to open
 * Mar 01, 2017   19443      polster     Fixed invalid thread access problem on cursor change
 * Mar 01, 2017   19443      polster     Fixed clear of Matrix navigator problem.
 * Dec 01, 2017   41520      polster     Added update matrix controls when view is refreshed
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public class EnsembleToolViewer extends ViewPart implements ISaveablePart2 {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleToolViewer.class);

    public static final String ID = "gov.noaa.gsd.viz.ensemble.tool.viewer";

    private LegendBrowserComposite legendBrowser = null;

    private MatrixNavigatorComposite matrixNavigator = null;

    private EnsembleToolBar ensembleToolBar = null;

    private CTabFolder mainToolsTabFolder = null;

    private CTabItem itemLegendsTabItem = null;

    private CTabItem itemMatrixTabItem = null;

    private EnsembleToolViewerPartListener viewPartListener = null;

    protected Composite matrixRootContainerComposite = null;

    protected ScrolledComposite matrixTreeContainerScrolledComposite = null;

    protected TreeItem calculationSelectedTreeItem = null;

    /**
     * Static members
     */

    public static final String NAME = "Ensembles";

    protected static boolean WAS_MAP_CHECKBOX_SELECTION = false;;

    protected static Composite rootComposite = null;

    private static Font smallViewFont = null;

    private static Font viewFont = null;

    public static int FEATURE_SET_COLUMN_INDEX = 1;

    public static int VISIBLE_COLUMN_INDEX = 2;

    public static int SAMPLING_COLUMN_INDEX = 3;

    private static Cursor selectionModeCursor = null;

    private static Cursor normalCursor = null;

    private static Cursor waitCursor = null;

    private static boolean isDisposing = false;

    private static boolean isEditable = true;

    private IToolBarManager toolbarMgr = null;

    public EnsembleToolViewer() {
    }

    /**
     * This is a call-back that will allow us to create the viewer and
     * initialize it.
     * 
     * @param parent
     */
    public void createPartControl(Composite parent) {

        toolbarMgr = getViewSite().getActionBars().getToolBarManager();

        /* fonts, images, and cursors */
        setupResources();

        /* the layout of the parent must be a Grid Layout */
        setupRoot(parent);

        /* this upper sash main tab folder is for all major ET tools */
        createMainToolsTabFolder();

        /*
         * this tool bar fits nicely inside the main tab folder and should
         * change depending upon which tool tab has been chosen
         */
        createMainToolsTabToolBar();

        /* create the contents of the Legend Browser tab */
        createLegendToolContents();

        /*
         * Change the toolbar actions and tooltip text depending upon whether
         * the Legend or the Matrix tab is selected.
         */
        handleChangeInTool();

        /* start in Legend browse mode */
        mainToolsTabFolder.setSelection(itemLegendsTabItem);

        // rootComposite.pack();

        /* need to listen for actions that the user takes on the ViewPart */
        if (viewPartListener == null) {
            viewPartListener = new EnsembleToolViewerPartListener(this);
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            IPartService service = window.getPartService();
            service.addPartListener(viewPartListener);
        }

    }

    private void setupRoot(Composite parent) {

        rootComposite = new Composite(parent, SWT.NONE);
        GridData rootComposite_gd = new GridData(SWT.FILL, SWT.FILL, true, true,
                1, 1);
        rootComposite.setLayoutData(rootComposite_gd);

        /* Set the grid layout for the root of all composites/controls */
        GridLayout rootComposite_gl = new GridLayout(1, false);
        rootComposite_gl.marginHeight = 0;
        rootComposite_gl.marginWidth = 0;
        rootComposite.setLayout(rootComposite_gl);

        rootComposite.setBackground(parent.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
    }

    private void setupResources() {

        /* Create the defaut icons when the view is initially opened */
        EnsembleToolImageStore.constructImages();

        /* Create fonts */
        viewFont = SWTResourceManager.getFont("Dialog", 9, SWT.NONE);
        smallViewFont = SWTResourceManager.getFont("Dialog", 8, SWT.NONE);

        /* Create cursors */

    }

    /* Fixed in association with AWIPS2_GSD repository Issue #29526 */
    public static Cursor getNormalCursor() {

        if (normalCursor == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    if (rootComposite != null && !rootComposite.isDisposed()) {
                        normalCursor = rootComposite.getDisplay()
                                .getSystemCursor(SWT.CURSOR_ARROW);
                    }
                }
            });
        }
        return normalCursor;
    }

    /* Fixed in association with AWIPS2_GSD repository Issue #29526 */
    public static Cursor getSelectionCursor() {

        if (selectionModeCursor == null) {

            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    if (rootComposite != null && !rootComposite.isDisposed()) {
                        selectionModeCursor = rootComposite.getDisplay()
                                .getSystemCursor(SWT.CURSOR_HAND);
                    }
                }
            });
        }
        return selectionModeCursor;
    }

    /* Fixed in association with AWIPS2_GSD repository Issue #29526 */
    public static Cursor getWaitCursor() {

        if (waitCursor == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    if (rootComposite != null && !rootComposite.isDisposed()) {
                        waitCursor = rootComposite.getDisplay()
                                .getSystemCursor(SWT.CURSOR_WAIT);

                    }
                }
            });

        }
        return waitCursor;
    }

    private void createLegendToolContents() {
        legendBrowser = new LegendBrowserComposite(mainToolsTabFolder, SWT.NONE,
                this, itemLegendsTabItem);
    }

    private void createMainToolsTabToolBar() {
        ensembleToolBar = new EnsembleToolBar(mainToolsTabFolder, toolbarMgr,
                SWT.None, this);
    }

    private void createMainToolsTabFolder() {
        /*
         * Main tab container to allow user to switch between legends, matrix,
         * and tools of the future...
         */
        mainToolsTabFolder = new CTabFolder(rootComposite, SWT.NONE);
        mainToolsTabFolder.setFont(viewFont);
        mainToolsTabFolder.setSelectionBackground(
                GlobalColor.get(GlobalColor.PALE_LIGHT_AZURE));
        mainToolsTabFolder.setTabHeight(46);
        mainToolsTabFolder.setBorderVisible(false);
        mainToolsTabFolder
                .setFont(SWTResourceManager.getFont("SansSerif", 11, SWT.NONE));

        GridData mainToolsTabFolder_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        mainToolsTabFolder.setLayoutData(mainToolsTabFolder_gd);

        /* Tab item for Legend browse tool */
        itemLegendsTabItem = new CTabItem(mainToolsTabFolder, SWT.NONE);
        itemLegendsTabItem.setText("  Legends  ");

        /* Tab item for Matrix navigator tool */
        itemMatrixTabItem = new CTabItem(mainToolsTabFolder, SWT.NONE);
        itemMatrixTabItem.setText("  Matrix  ");
    }

    /**
     * Did the user select the Legend or Matrix tab then change the editor and
     * refresh the tool (this viewer).
     */
    private void handleChangeInTool() {

        mainToolsTabFolder.addSelectionListener(new SelectionAdapter() {

            public void widgetSelected(SelectionEvent e) {

                IDisplayPaneContainer idpc = null;
                if (mainToolsTabFolder.getSelection() == itemLegendsTabItem) {
                    idpc = EnsembleTool.getInstance().getCurrentLegendsEditor();
                }
                if (mainToolsTabFolder.getSelection() == itemMatrixTabItem) {

                    /*
                     * create the contents of the Matrix Navigator tab the first
                     * time it is requested
                     */
                    if (matrixNavigator == null) {

                        try {
                            matrixNavigator = new MatrixNavigatorComposite(
                                    mainToolsTabFolder, SWT.BORDER,
                                    itemMatrixTabItem);
                        } catch (InstantiationException ex) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Unable to create the Matrix Editor: "
                                                    + ex.getLocalizedMessage(),
                                            ex);
                        }
                        mainToolsTabFolder.redraw();

                    }
                    idpc = (IDisplayPaneContainer) matrixNavigator
                            .getMatrixEditor();

                }

                if (idpc != null) {
                    EnsembleTool.getInstance().setEditor(idpc);
                    EnsembleTool.getInstance().refreshTool(true);
                    EnsembleTool.getInstance().showEditor(idpc);
                }
            }
        });
    }

    /**
     * Set the state of the viewer based on the given tool mode.
     */
    public void setToolMode(EnsembleTool.EnsembleToolMode mode) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (isWidgetReady()) {
                    if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW
                            | mode == EnsembleTool.EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        mainToolsTabFolder.setSelection(itemLegendsTabItem);
                        legendBrowser.setToolMode(mode);
                    } else if (mode == EnsembleTool.EnsembleToolMode.MATRIX) {
                        mainToolsTabFolder.setSelection(itemMatrixTabItem);
                    }
                    ensembleToolBar.setToolbarMode(mode);
                }
            }
        });

    }

    public static Font getViewFontSmall() {
        return smallViewFont;
    }

    public static Font getViewFontNormal() {
        return viewFont;
    }

    public static Shell getShell() {
        return rootComposite.getShell();
    }

    public MatrixNavigatorComposite getMatrixNavigator() {
        return matrixNavigator;
    }

    /**
     * Clean up all resources, fonts, providers, etc.
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {

        super.dispose();

        if (ensembleToolBar != null) {
            ensembleToolBar.dispose();
        }

        EnsembleToolViewer.setDisposing(true);

        if (legendBrowser != null) {
            legendBrowser.dispose();
            legendBrowser = null;
        }

        if (matrixNavigator != null) {
            matrixNavigator.dispose();
            matrixNavigator = null;
        }

        SWTResourceManager.dispose();

        EnsembleTool.getInstance().handleViewerDisposed();

        EnsembleToolViewer.setDisposing(false);

    }

    public static boolean isDisposing() {
        return isDisposing;
    }

    public static void setDisposing(boolean id) {
        isDisposing = id;
    }

    public static boolean isEditable() {
        return isEditable;
    }

    /**
     * The ViewPart should be enabled when an ensemble tool layer is editable in
     * the currently active editor, and disabled when the tool layer is set to
     * not-editable.
     */
    public void setEditable(boolean enabled) {

        isEditable = enabled;

        if (legendBrowser != null) {
            legendBrowser.setEditable(enabled);
        }
        if (ensembleToolBar != null) {
            ensembleToolBar.setEditable(enabled);
        }
        if (matrixNavigator != null) {
            matrixNavigator.setEditable(enabled);
        }

    }

    /**
     * Is the tool for the active tool mode ready to be acted against?
     */
    public boolean isWidgetReady() {
        boolean isReady = false;

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.NONE) {
            isReady = true;
        } else {
            if (mainToolsTabFolder == null || mainToolsTabFolder.isDisposed()) {
                // || !ensembleToolBar.isWidgetReady()) {
                isReady = false;
            } else {
                EnsembleToolMode mode = EnsembleTool.getInstance()
                        .getToolMode();
                if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                        || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                    isReady = legendBrowser.isWidgetReady();
                } else {
                    /*
                     * when the matrix navigator is still being initialized it
                     * is still considered ready.
                     */
                    if (matrixNavigator == null
                            && MatrixNavigatorComposite.isInitializing) {
                        isReady = true;
                    } else {
                        isReady = matrixNavigator.isWidgetReady();
                    }
                }
            }
        }
        return isReady;
    }

    /**
     * The plan view and time series editors are brand new so call inner mgrs to
     * inform.
     */
    public void prepareForNewToolInput() {

        EnsembleToolMode mode = EnsembleTool.getInstance().getToolMode();
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.prepareForNewToolInput();
        }
    }

    /**
     * Given a tool layer, refresh the input of the respective inner tool.
     */
    public void refreshInput(final List<AbstractResourceHolder> list) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                if (isWidgetReady()) {

                    EnsembleToolMode mode = EnsembleTool.getInstance()
                            .getToolMode();

                    if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                            || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        legendBrowser.refreshInput(list);
                        mainToolsTabFolder.setSelection(itemLegendsTabItem);
                    } else if (mode == EnsembleToolMode.MATRIX) {
                        if (matrixNavigator != null) {
                            mainToolsTabFolder.setSelection(itemMatrixTabItem);
                            matrixNavigator.updateControls();
                        }
                    }
                }
                if (isWidgetReady()) {
                    mainToolsTabFolder.getSelection().reskin(SWT.ALL);
                }
            }
        });
    }

    /**
     * Called from a class that is listening to the frameChange event. Let the
     * inner tools know of the event.
     */
    public void frameChanged(FramesInfo framesInfo) {

        EnsembleToolMode mode = EnsembleTool.getInstance().getToolMode();
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.frameChanged(framesInfo);

        } else if (mode == EnsembleToolMode.MATRIX) {
            matrixNavigator.frameChanged(framesInfo);
        }
    }

    /**
     * Clear all resources of the active tool.
     */
    public void clearAllByActiveMode() {

        clearAllByMode(EnsembleTool.getInstance().getToolMode());

    }

    /**
     * Clear all resources for the given mode.
     */
    public void clearAllByMode(EnsembleToolMode mode) {

        switch (mode) {
        case LEGENDS_PLAN_VIEW:
        case LEGENDS_TIME_SERIES:
            legendBrowser.clearAll();
            break;
        case MATRIX:
            if (matrixNavigator != null) {
                matrixNavigator.clearAllResources();
            }
            break;
        default:
            break;
        }
    }

    /* Fixed in association with AWIPS2_GSD repository Issue #29204 */
    public void resetMatrixNavigator() {
        if (matrixNavigator != null) {
            matrixNavigator.dispose();
            matrixNavigator = null;
        }
    }

    public static Color getEnabledForegroundColor() {
        return GlobalColor.get(GlobalColor.BLACK);
    }

    public static Color getDisabledForegroundColor() {
        return GlobalColor.get(GlobalColor.MEDIUM_GRAY);
    }

    /**
     * Set the focus to the inner tool depending upon the tool mode. Abstract in
     * WorkbenchPart.
     */
    @Override
    public void setFocus() {
        EnsembleToolMode mode = EnsembleTool.getInstance().getToolMode();
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.setFocus();
        }
        if (mode == EnsembleToolMode.MATRIX) {
            matrixNavigator.setFocus();
        }
    }

    /**
     * The navigation operation is one of: UP_ARROW, DOWN_ARROW, RIGHT_ARROW, or
     * LEFT_ARROW. It is generated by the ensemble tool plugin extensions.
     * Delegation method for the matrix navigation inner tool.
     * 
     * @param operationmode
     */
    public void matrixNavigationRequest(
            MatrixNavigationOperation operationmode) {
        if (matrixNavigator != null) {
            matrixNavigator.matrixNavigationRequest(operationmode);
        }
    }

    public DistributionViewerComposite getDistributionViewer() {
        return legendBrowser.getDistributionViewer();
    }

    /**
     * Returns the active resource time so the active ensemble tool layer legend
     * displays the time as it is displayed in the current frame of the active
     * editor.
     */
    public String getActiveRscTime() {
        String rscTime = null;
        if (EnsembleTool.getInstance()
                .getToolMode() == EnsembleToolMode.MATRIX) {
            rscTime = matrixNavigator.getActiveRscTime();
        }
        return rscTime;
    }

    @Override
    public void doSave(IProgressMonitor monitor) {
        // TODO Not yet needed
    }

    @Override
    public void doSaveAs() {
        // TODO Not yet needed
    }

    /**
     * TODO: Until there is an actual save capability in the Ensemble Tool,
     * always make the view dirty so as to make sure Eclipse calls the saving
     * framework (i.e. promptToSaveOnClose).
     */
    @Override
    public boolean isDirty() {
        return true;
    }

    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    /**
     * TODO: As of this release, there is no way to save the state of the active
     * editor and viewer. Therefore, it is imperative to always prompt to make
     * sure the user really wants to close the viewer, because closing the
     * viewer will, for convenience, close the tool layer in the active editor.
     */
    @Override
    public boolean isSaveOnCloseNeeded() {
        return true;
    }

    /**
     * The user has requested to close this view (by pressing the 'x' button in
     * the view tab). Check to make sure that's okay. If not then cancel the
     * request by returning ISaveablePart2.CANCEL.
     */
    @Override
    public int promptToSaveOnClose() {
        return EnsembleTool.getInstance().verifyCloseTool();
    }

    /**
     * When the matrix editor is closed by the user then dispose of the inner
     * matrix tool.
     */
    public void removeMatrixComponent() {

        if (matrixNavigator != null && !matrixNavigator.isDisposed()) {
            matrixNavigator.dispose();
            matrixNavigator = null;
        }
    }

    public void updateElementInTree(AbstractResourceHolder arh) {
        EnsembleToolMode mode = EnsembleTool.getInstance().getToolMode();
        if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.updateElementInTree(arh);
        }
        if (mode == EnsembleToolMode.MATRIX) {
            if (EnsembleTool.getInstance().getToolLayer() != null) {
                /* no need to repopulate on refresh */
                EnsembleTool.getInstance().getToolLayer().forceRefresh(false);
            }
        }
    }

}
