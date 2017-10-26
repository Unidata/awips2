package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.DistributionViewerComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.legend.LegendBrowserComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.MatrixNavigatorComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.VizMatrixEditor.MatrixNavigationOperation;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
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
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 6) Allow users to display meta-information for a selected resource via an
 * Information pane.
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

    private boolean isDisabled = false;

    private EnsembleToolViewerPartListener viewPartListener = null;

    protected Composite matrixRootContainerComposite = null;

    protected ScrolledComposite matrixTreeContainerScrolledComposite = null;

    protected TreeItem calculationSelectedTreeItem = null;

    /**
     * Static members
     */
    // public static EnsembleToolMode currentToolMode =
    // EnsembleToolMode.LEGENDS_PLAN_VIEW;

    public static final String NAME = "Ensembles";

    protected static boolean WAS_MAP_CHECKBOX_SELECTION = false;;

    protected static Composite rootComposite = null;

    private static Font smallViewFont = null;

    private static Font viewFont = null;

    public static int FEATURE_SET_COLUMN_INDEX = 1;

    public static int VISIBLE_COLUMN_INDEX = 2;

    public static int SAMPLING_COLUMN_INDEX = 3;

    public static AbstractVizResource<?, ?> LAST_HIGHLIGHTED_RESOURCE = null;

    public static RGB LAST_HIGHLIGHTED_RESOURCE_RGB = null;

    public static int LAST_HIGHLIGHTED_RESOURCE_WIDTH = 1;

    public static boolean LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = false;

    public static Cursor selectionModeCursor = null;

    public static Cursor normalCursor = null;

    public static Cursor waitCursor = null;

    // public static ResourceType editorResourceType = ResourceType.PLAN_VIEW;

    private static boolean isDisposing = false;

    private static boolean viewEditable = true;

    public EnsembleToolViewer() {
    }

    /**
     * This is a call-back that will allow us to create the viewer and
     * initialize it.
     * 
     * @param parent
     */
    public void createPartControl(Composite parent) {

        /* the layout of the parent must be a Grid Layout */
        setupRoot(parent);

        /* fonts, images, and cursors */
        setupResources();

        /* this upper sash main tab folder is for all major ET tools */
        createMainToolsTabFolder();

        /*
         * this tool bar fits nicely inside the main tab folder and should
         * change depending upon which tool tab has been chosen
         */
        createMainToolsTabToolBar();

        /* create the contents of the Legend Browser tab */
        createLegendToolContents();

        /* create the contents of the Matrix Navigator tab */
        createMatrixToolContents();

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
        GridData rootComposite_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        rootComposite.setLayoutData(rootComposite_gd);

        /* Set the grid layout for the root of all composites/controls */
        GridLayout rootComposite_gl = new GridLayout(1, false);
        rootComposite_gl.marginHeight = 0;
        rootComposite_gl.marginWidth = 0;
        rootComposite.setLayout(rootComposite_gl);

        rootComposite.setBackground(GlobalColor.get(GlobalColor.YELLOW));
    }

    private void setupResources() {

        /* Create the defaut icons when the view is initially opened */
        EnsembleToolImageStore.constructImages();

        /* Create fonts */
        viewFont = SWTResourceManager.getFont("Dialog", 9, SWT.NONE);
        smallViewFont = SWTResourceManager.getFont("Dialog", 8, SWT.NONE);

        /* Create cursors */
        selectionModeCursor = rootComposite.getDisplay().getSystemCursor(
                SWT.CURSOR_HAND);

        normalCursor = rootComposite.getDisplay().getSystemCursor(
                SWT.CURSOR_ARROW);

        waitCursor = rootComposite.getDisplay()
                .getSystemCursor(SWT.CURSOR_WAIT);

    }

    private void createLegendToolContents() {
        legendBrowser = new LegendBrowserComposite(mainToolsTabFolder,
                SWT.NONE, this, itemLegendsTabItem);
    }

    private void createMainToolsTabToolBar() {
        ensembleToolBar = new EnsembleToolBar(mainToolsTabFolder, SWT.None,
                this);
    }

    private void createMainToolsTabFolder() {
        /*
         * Main tab container to allow user to switch between legends, matrix,
         * and tools of the future...
         */
        mainToolsTabFolder = new CTabFolder(rootComposite, SWT.NONE);
        mainToolsTabFolder.setFont(viewFont);
        mainToolsTabFolder.setSelectionBackground(GlobalColor
                .get(GlobalColor.PALE_DULL_AZURE));
        mainToolsTabFolder.setTabHeight(42);
        mainToolsTabFolder.setBorderVisible(false);
        mainToolsTabFolder.setFont(SWTResourceManager.getFont("SansSerif", 10,
                SWT.NONE));
        mainToolsTabFolder
                .addSelectionListener(new MainToolsTabSelectionListener());

        GridData mainToolsTabFolder_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        mainToolsTabFolder.setLayoutData(mainToolsTabFolder_gd);

        /* Tab item for Legend browse tool */
        itemLegendsTabItem = new CTabItem(mainToolsTabFolder, SWT.NONE);
        itemLegendsTabItem
                .setImage(EnsembleToolImageStore.TAB_LEGENDS_ENABLED_UNSELECTED_IMG);

        /* Tab item for Matrix navigator tool */
        itemMatrixTabItem = new CTabItem(mainToolsTabFolder, SWT.NONE);
        itemMatrixTabItem
                .setImage(EnsembleToolImageStore.TAB_MATRIX_ENABLED_UNSELECTED_IMG);

    }

    private void handleChangeInTool() {

        mainToolsTabFolder.addSelectionListener(new SelectionAdapter() {

            public void widgetSelected(SelectionEvent e) {

                IDisplayPaneContainer idpc = null;
                String s = null;
                if (mainToolsTabFolder.getSelection() == itemLegendsTabItem) {
                    s = "Legend";
                    idpc = (IDisplayPaneContainer) EnsembleTool.getInstance()
                            .getCurrentLegendsToolEditor();
                }
                if (mainToolsTabFolder.getSelection() == itemMatrixTabItem) {
                    s = "Matrix";
                    try {
                        idpc = (IDisplayPaneContainer) matrixNavigator
                                .getMatrixEditor();
                    } catch (InstantiationException e1) {

                        MessageDialog.openInformation(rootComposite.getShell(),
                                "Matrix Editor Error",
                                "Unable to create the Matrix editor. Returning to the Legend browser.");

                        mainToolsTabFolder.setSelection(itemLegendsTabItem);
                    }
                }

                if (idpc != null) {
                    EnsembleTool.getInstance().setEditor(idpc);
                    EnsembleTool.getInstance().refreshTool(true);
                    EnsembleTool.getInstance().showEditor(
                            ((AbstractEditor) idpc));
                } else {
                    MessageDialog.openInformation(rootComposite.getShell(),
                            "Editor Error", "Happened in " + s + " mode.");

                }

            }

        });

    }

    public void setToolMode(EnsembleTool.EnsembleToolMode mode) {
        if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW
                | mode == EnsembleTool.EnsembleToolMode.LEGENDS_TIME_SERIES) {
            mainToolsTabFolder.setSelection(itemLegendsTabItem);
            legendBrowser.setToolMode(mode);
        } else if (mode == EnsembleTool.EnsembleToolMode.MATRIX) {
            mainToolsTabFolder.setSelection(itemMatrixTabItem);
        }
        ensembleToolBar.setToolbarMode(mode);
    }

    private void createMatrixToolContents() {

        matrixNavigator = new MatrixNavigatorComposite(mainToolsTabFolder,
                SWT.BORDER, itemMatrixTabItem);

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

    /*
     * Clean up all resources, fonts, providers, etc.
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {

        super.dispose();

        EnsembleToolViewer.setDisposing(true);

        legendBrowser.dispose();

        matrixNavigator.dispose();

        SWTResourceManager.dispose();

        EnsembleToolViewer.setDisposing(false);

    }

    public static boolean isDisposing() {
        return isDisposing;
    }

    public static void setDisposing(boolean id) {
        isDisposing = id;
    }

    @Override
    public void doSave(IProgressMonitor monitor) {
        /**
         * TODO Currently we don't have any way to Save the loaded products as a
         * bundle, that is in a way that would allow the user to reopen the
         * products back into the Ensemble Tool.
         */
    }

    @Override
    public void doSaveAs() {
        /**
         * TODO Currently we don't have any way to Save As the loaded products
         * as a bundle, that is in a way that would allow the user to reopen the
         * products back into the Ensemble Tool.
         */
    }

    @Override
    public boolean isDirty() {

        if (PlatformUI.getWorkbench().isClosing()) {
            return false;
        }
        return true;
    }

    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    @Override
    public boolean isSaveOnCloseNeeded() {
        boolean saveOnClose = true;
        if (PlatformUI.getWorkbench().isClosing()) {
            saveOnClose = false;
        }
        return saveOnClose;
    }

    @Override
    public int promptToSaveOnClose() {

        if (PlatformUI.getWorkbench().isClosing()) {
            return ISaveablePart2.NO;
        } else if (isDisabled) {
            return ISaveablePart2.CANCEL;
        }

        int userResponseToClose = EnsembleTool.getInstance().verifyCloseTool();
        return userResponseToClose;

    }

    public static boolean isViewEditable() {
        return viewEditable;
    }

    public void toggleViewEditable() {
        viewEditable = !viewEditable;
        EnsembleTool.getInstance().setEditable(viewEditable);
    }

    public void disableTool() {
        setViewEditable(false);
        isDisabled = true;

    }

    public boolean isDisabled() {
        return isDisabled;
    }

    synchronized public boolean isEnabled() {
        return viewEditable;
    }

    /*
     * The ViewPart should be enabled when an ensemble tool layer is editable in
     * the currently active editor, and disabled when the tool layer is set to
     * 'not editable'.
     */
    synchronized public void setViewEditable(boolean enabled) {

        isDisabled = false;
        viewEditable = enabled;

        legendBrowser.setViewEditable(enabled);
        ensembleToolBar.setViewEditable(enabled);
        matrixNavigator.setViewEditable(enabled);

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (viewEditable) {
                    mainToolsTabFolder.setSelectionBackground(GlobalColor
                            .get(GlobalColor.PALE_DULL_AZURE));

                    if (mainToolsTabFolder.getSelection() == itemLegendsTabItem) {
                        itemLegendsTabItem
                                .setImage(EnsembleToolImageStore.TAB_LEGENDS_ENABLED_SELECTED_IMG);
                        itemMatrixTabItem
                                .setImage(EnsembleToolImageStore.TAB_MATRIX_ENABLED_UNSELECTED_IMG);
                    } else if (mainToolsTabFolder.getSelection() == itemMatrixTabItem) {
                        mainToolsTabFolder.setSelection(itemMatrixTabItem);
                        itemLegendsTabItem
                                .setImage(EnsembleToolImageStore.TAB_LEGENDS_ENABLED_UNSELECTED_IMG);
                        itemMatrixTabItem
                                .setImage(EnsembleToolImageStore.TAB_MATRIX_ENABLED_SELECTED_IMG);
                    }
                } else {
                    mainToolsTabFolder.setSelectionBackground(GlobalColor
                            .get(GlobalColor.GRAY));
                    if (mainToolsTabFolder.getSelection() == itemLegendsTabItem) {
                        itemLegendsTabItem
                                .setImage(EnsembleToolImageStore.TAB_LEGENDS_DISABLED_SELECTED_IMG);
                        itemMatrixTabItem
                                .setImage(EnsembleToolImageStore.TAB_MATRIX_DISABLED_IMG);
                    } else if (mainToolsTabFolder.getSelection() == itemMatrixTabItem) {
                        itemLegendsTabItem
                                .setImage(EnsembleToolImageStore.TAB_LEGENDS_DISABLED_IMG);
                        itemMatrixTabItem
                                .setImage(EnsembleToolImageStore.TAB_MATRIX_DISABLED_SELECTED_IMG);
                    }
                }

                if (isViewerTreeReady()) {
                    mainToolsTabFolder.getSelection().reskin(SWT.ALL);
                    mainToolsTabFolder.redraw();
                }
            }
        });
    }

    public RGB getStartColor(RGB inColor) {

        RGB maxHue = null;
        float[] hsb = inColor.getHSB();
        maxHue = new RGB(hsb[0], 1.0f, 0.35f);

        return maxHue;
    }

    // wouldn't it be nice if when we attempt to automatically
    // assign a color to a resource it would be somewhat dis-
    // tinct from any previously assigned color?
    public RGB getNextShadeColor(RGB inColor) {

        RGB shadeDown = null;

        float[] hsb = inColor.getHSB();

        float brightness = hsb[2];
        float saturation = hsb[1];

        brightness = (float) Math.min(1.0f, (brightness + 0.05f));
        saturation = (float) Math.max(0.15f, (saturation - 0.05f));

        shadeDown = new RGB(hsb[0], saturation, brightness);

        return shadeDown;
    }

    public boolean isViewerTreeReady() {
        boolean isReady = false;

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            isReady = legendBrowser.isViewerTreeReady();
        } else {
            isReady = matrixNavigator.isViewerTreeReady();
        }
        return isReady;
    }

    public void prepareForNewToolInput() {

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.prepareForNewToolInput();
        }
    }

    synchronized public void refreshInput(EnsembleToolLayer toolLayer) {

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.refreshInput(toolLayer);
        } else if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            matrixNavigator.refreshInput(toolLayer);
        }
    }

    /*
     * Called from a class that is listening to the frameChange event.
     */
    public void frameChanged(FramesInfo framesInfo) {

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.frameChanged(framesInfo);

        } else if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            matrixNavigator.frameChanged(framesInfo);
        }
    }

    public void clearAll() {

        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.clearAll();
        } else if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            matrixNavigator.clearAllResources();
        }
    }

    protected class MainToolsTabSelectionListener extends SelectionAdapter {

        @Override
        public void widgetSelected(SelectionEvent e) {

            CTabItem ti = mainToolsTabFolder.getSelection();
            if (viewEditable) {
                if (ti == itemLegendsTabItem) {
                    itemLegendsTabItem
                            .setImage(EnsembleToolImageStore.TAB_LEGENDS_ENABLED_SELECTED_IMG);
                    itemMatrixTabItem
                            .setImage(EnsembleToolImageStore.TAB_MATRIX_ENABLED_UNSELECTED_IMG);
                }
                if (ti == itemMatrixTabItem) {
                    itemLegendsTabItem
                            .setImage(EnsembleToolImageStore.TAB_LEGENDS_ENABLED_UNSELECTED_IMG);
                    itemMatrixTabItem
                            .setImage(EnsembleToolImageStore.TAB_MATRIX_ENABLED_SELECTED_IMG);
                }
            } else {
                if (mainToolsTabFolder.getSelection() == itemLegendsTabItem) {
                    itemLegendsTabItem
                            .setImage(EnsembleToolImageStore.TAB_LEGENDS_DISABLED_SELECTED_IMG);
                    itemMatrixTabItem
                            .setImage(EnsembleToolImageStore.TAB_MATRIX_DISABLED_IMG);
                } else if (mainToolsTabFolder.getSelection() == itemMatrixTabItem) {
                    itemLegendsTabItem
                            .setImage(EnsembleToolImageStore.TAB_LEGENDS_DISABLED_IMG);
                    itemMatrixTabItem
                            .setImage(EnsembleToolImageStore.TAB_MATRIX_DISABLED_SELECTED_IMG);
                }
            }
        }
    }

    public static Color getEnabledForegroundColor() {
        return GlobalColor.get(GlobalColor.BLACK);
    }

    public static Color getDisabledForegroundColor() {
        return GlobalColor.get(GlobalColor.MEDIUM_GRAY);
    }

    public void setFocus() {
        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            legendBrowser.setFocus();
        }
        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            matrixNavigator.setFocus();
        }
    }

    public void matchLikeResources() {
        if (matrixNavigator != null) {
            matrixNavigator.matchLikeResources();
        }
    }

    public void matrixNavigationRequest(MatrixNavigationOperation operationmode) {
        if (matrixNavigator != null) {
            matrixNavigator.matrixNavigationRequest(operationmode);
        }
    }

    public DistributionViewerComposite getDistributionViewer() {
        return legendBrowser.getDistributionViewer();
    }

    public void refreshResources() {
        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            /**
             * TODO: Save for future use
             */
        } else if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            matrixNavigator.refreshResources();
        }

    }

    public String getActiveRscTime() {
        String rscTime = null;
        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            rscTime = matrixNavigator.getActiveRscTime();
        }
        return rscTime;
    }

}
