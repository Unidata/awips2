package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.calculate.RangeType;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedTimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.TimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolManager;
import gov.noaa.gsd.viz.ensemble.util.ChosenGEFSColors;
import gov.noaa.gsd.viz.ensemble.util.ChosenSREFColors;
import gov.noaa.gsd.viz.ensemble.util.EnsembleGEFSColorChooser;
import gov.noaa.gsd.viz.ensemble.util.EnsembleSREFColorChooser;
import gov.noaa.gsd.viz.ensemble.util.ImageResourceManager;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;

/**
 * This class represents the Ensemble Tool navigator widget, which is an RCP
 * View (ViewPart). It contains all user interface control logic for those
 * features which are otherwise accomplished by the user through the main CAVE
 * (primary) editor pane (e.g. Map editor, TimeSeries editor, etc.).
 * 
 * Features in this class include the ability to:
 * 
 * 1) Behave as proxy for the current active CAVE "editable" ensemble tool; this
 * class is a RCP CAVE view that is coupled to the EnsembleToolManager which
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
 * Oct 8, 2014    5056      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class EnsembleToolViewer extends ViewPart implements IRefreshListener {

    public static final String ID = "gov.noaa.gsd.viz.ensemble.tool.viewer";

    public static final String NAME = "Ensembles";

    protected static boolean WAS_MAP_CHECKBOX_SELECTION = false;;

    private static int CLIENT_BODY_AREA_HEIGHT = 739;

    private TreeViewer ensemblesTreeViewer = null;

    private Tree ensembleTree = null;

    private ArrayList<ColumnLabelProvider> columnLabelProviders = new ArrayList<ColumnLabelProvider>();

    private ScrolledComposite tabContainer = null;

    private CTabFolder tabEnsemblesMain = null;

    private ToolBar toolBar = null;

    private Font viewFont = null;

    private TabFolder tabFolder_lowerSash = null;

    private TabItem tabPreferences = null;

    private TabItem tabResourceInfo = null;

    private TabItem tabERFLayerControl = null;

    private TabItem lastSelectedNonTransientTabItem = null;

    private Text lowerRangeEntryTextBox_1 = null;

    private Text lowerRangeEntryTextBox_2 = null;

    private Text lowerRangeEntryTextBox_3 = null;

    private Text lowerRangeEntryTextBox_4 = null;

    private Text upperRangeEntryTextBox_1 = null;

    private Text upperRangeEntryTextBox_2 = null;

    private Button radioChooserRange_1 = null;

    private Button radioChooserRange_2 = null;

    private Button radioChooserRange_3 = null;

    private Button radioChooserRange_4 = null;

    private Button btn_cancelERF = null;

    private Button btn_computeERF = null;

    private MenuItem addERFLayerMenuItem = null;

    private Label label_ensembleProductName = null;

    private EnsembleViewPartListener viewPartListener = null;

    private boolean viewEnabled = true;

    private ResourceType editorType = ResourceType.PLAN_VIEW;

    private SashForm sashForm = null;

    static private Image GEAR_ICON = null;

    static private Image ALPHANUMERIC_SORT_ICON = null;

    static private Image INFO_ICON = null;

    public static int FEATURE_SET_COLUMN_INDEX = 1;

    public static int VISIBLE_COLUMN_INDEX = 2;

    public static int SAMPLING_COLUMN_INDEX = 3;

    private static String BUNDLE_CMD_TITLE = "Bundle";

    private AbstractVizResource<?, ?> currentEnsembleRsc = null;

    private static AbstractVizResource<?, ?> LAST_HIGHLIGHTED_RESOURCE = null;

    private static RGB LAST_HIGHLIGHTED_RESOURCE_RGB = null;

    private static int LAST_HIGHLIGHTED_RESOURCE_WIDTH = 1;

    private static boolean LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = false;

    private Color thickenOnSelectionColor = SWTResourceManager.PASTEL_LIGHT_BLUE;

    private boolean thickenOnSelection = true;

    private boolean useResourceColorOnThicken = true;

    private int thickenWidth = 4;

    private boolean volumeBrowserJustOpened = false;

    private long ignoreFocusStartTime = 0;

    private static final long IGNORE_FOCUS_PERIOD_MILLIS = 4000;

    final TransferFocusListener trackMouseEntryExit = new TransferFocusListener();

    protected Composite owner = null;

    final private ITreeViewerListener expandCollapseListener = new EnsembleTreeExpandCollapseListener();

    private static final Color ENABLED_FOREGROUND_COLOR = SWTResourceManager.BLACK;

    private static final Color DISABLED_FOREGROUND_COLOR = SWTResourceManager.MEDIUM_GRAY;

    private Label label_frameTimeUsingBasis = null;

    private Label label_TimeMatchResourceLabel = null;

    private Cursor selectionModeCursor = null;

    private Cursor normalCursor = null;

    private Cursor waitCursor = null;

    private static boolean isDisposing = false;

    public static boolean isDisposing() {
        return isDisposing;
    }

    public static void setDisposing(boolean id) {
        isDisposing = id;
    }

    public EnsembleToolViewer() {

    }

    /**
     * This is a call-back that will allow us to create the viewer and
     * initialize it.
     * 
     * @param parent
     */

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    public void createPartControl(Composite parent) {

        // create the defaut icons when the view is initially opened
        constructImages();

        owner = parent;
        viewFont = new Font(parent.getDisplay(), "Dialog", 9, SWT.NONE);

        GridLayout gridLayout1 = new GridLayout(1, false);
        parent.setLayout(gridLayout1);

        GridData gridData1 = new GridData(SWT.FILL, SWT.CENTER, true, true, 2,
                3);
        parent.setLayoutData(gridData1);

        // spacer component
        Composite dummySpacerContainer = new Composite(parent, SWT.NONE);
        GridLayout gl_dummySpacerContainer = new GridLayout();
        dummySpacerContainer.setLayout(gl_dummySpacerContainer);

        // the main container holds the resource "legends" and is scrollable
        tabContainer = new ScrolledComposite(parent, SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL);

        // this is the root grid layout of all grid layouts
        GridLayout gl_scrolledContainer = new GridLayout();
        GridData gd_tabContainer = new GridData(SWT.FILL, SWT.FILL, true, true,
                1, 1);
        gd_tabContainer.heightHint = CLIENT_BODY_AREA_HEIGHT;
        tabContainer.setLayout(gl_scrolledContainer);
        tabContainer.setLayoutData(gd_tabContainer);

        // main tab container to allow user to switch between legends, matrix,
        // and future ideas ...
        tabEnsemblesMain = new CTabFolder(tabContainer, SWT.TOP | SWT.BORDER);
        tabEnsemblesMain.setFont(viewFont);
        tabEnsemblesMain
                .setSelectionBackground(SWTResourceManager.PALE_LIGHT_AZURE);

        // here's the toolbar
        Composite toolbarContainer = new Composite(tabEnsemblesMain, SWT.NONE);
        toolbarContainer.setBackground(SWTResourceManager.WHITE);

        FillLayout fl_toolbarContainer = new FillLayout(SWT.HORIZONTAL);
        fl_toolbarContainer.marginWidth = 1;
        fl_toolbarContainer.marginHeight = 1;
        toolbarContainer.setLayout(fl_toolbarContainer);

        // fill the tool bar
        toolBar = makeToolBar(toolbarContainer);
        Rectangle r = toolbarContainer.getBounds();
        r.height = r.height + 32;
        toolbarContainer.setBounds(r);

        tabEnsemblesMain.setTabHeight(42);
        tabEnsemblesMain.setTopRight(toolbarContainer);
        tabEnsemblesMain.setFont(SWTResourceManager.getFont("SansSerif", 10,
                SWT.NONE));

        // tab entry for Legends
        CTabItem tbtmLegends = new CTabItem(tabEnsemblesMain, SWT.NONE);
        tbtmLegends.setText("  Legends  ");

        // tab entry for Matrix
        CTabItem tbtmMatrix = new CTabItem(tabEnsemblesMain, SWT.NONE);
        tbtmMatrix.setText("  Matrix  ");

        // let's have an upper sash and lower sash that the user can
        // resize vertically (see SWT concept of sash)
        sashForm = new SashForm(tabEnsemblesMain, SWT.BORDER);
        sashForm.setOrientation(SWT.VERTICAL);

        tabContainer.setContent(tabEnsemblesMain);
        tabContainer.setMinSize(tabEnsemblesMain.computeSize(SWT.DEFAULT,
                SWT.DEFAULT));

        // upper sash contains the resource "legend" tree.
        ensembleTree = new Tree(sashForm, SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION);
        ensembleTree.setLinesVisible(true);
        ensembleTree.setHeaderVisible(false);
        ensemblesTreeViewer = new TreeViewer(ensembleTree);
        createColumns(ensemblesTreeViewer);

        // keep track of the collapse/expand state
        ensemblesTreeViewer.addTreeListener(expandCollapseListener);

        // recognize when the user clicks on something in the tree
        ensembleTree.addMouseListener(new EnsembleTreeMouseListener());

        // the lower sash contains a composite which itself contains
        // a tab folder (i.e. set of tabs in one container) ...
        Composite lowerSash = new Composite(sashForm, SWT.BORDER_SOLID);
        lowerSash.setBackground(SWTResourceManager.LIGHT_GRAY);
        GridLayout gl_lowerSash = new GridLayout();
        lowerSash.setLayout(gl_lowerSash);
        GridData gd_lowerSash = new GridData(SWT.FILL, SWT.FILL, true, true, 1,
                1);
        gd_lowerSash.horizontalIndent = 1;
        gd_lowerSash.verticalIndent = 1;
        lowerSash.setLayoutData(gd_lowerSash);

        // put the Info, ERF, and Preferences tabs in the lower sash
        // container
        fillLowerSash(lowerSash);

        // what is the best ratio of visibility of upper vs. lower sash
        sashForm.setWeights(new int[] { 60, 40 });

        tbtmLegends.setControl(sashForm);

        // keep track of which lower tab was last selected
        tabFolder_lowerSash
                .addSelectionListener(new LowerSashTabSelectionListener());

        // fill the contents of the tree with
        ensemblesTreeViewer
                .setContentProvider(new EnsembleTreeContentProvider());
        ensemblesTreeViewer.setSorter(new EnsembleTreeSorter());

        tabContainer.setExpandHorizontal(true);
        tabContainer.setExpandVertical(true);

        tabEnsemblesMain.setSelection(tbtmLegends);

        parent.pack();

        final EnsembleToolViewer viewer = this;
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (viewPartListener == null) {
                    viewPartListener = new EnsembleViewPartListener(viewer);
                    IWorkbenchWindow window = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                    IPartService service = window.getPartService();
                    service.addPartListener(viewPartListener);
                }
            }
        });

        ensemblesTreeViewer.getControl().addMouseTrackListener(
                trackMouseEntryExit);

        selectionModeCursor = owner.getDisplay().getSystemCursor(
                SWT.CURSOR_HAND);

        normalCursor = owner.getDisplay().getSystemCursor(SWT.CURSOR_ARROW);

        waitCursor = owner.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        ensembleTree.addKeyListener(new EnsembleTreeKeyListener());

        updateCursor(normalCursor);

    }

    /*
     * Let's keep track of only the Tab items which contain long-standing
     * information (currently the meta-data and preferences tab items. Ignore,
     * for example, the probability tab which is used as a either a creation
     * widget (i.e. to create ERF products) or as a info widget (only when the
     * ERF product is specifically selected).
     */

    private class LowerSashTabSelectionListener implements SelectionListener {

        @Override
        public void widgetSelected(SelectionEvent e) {
            TabItem[] tabItem = tabFolder_lowerSash.getSelection();
            TabItem cti = tabItem[0];
            if ((cti == tabPreferences) || (cti == tabResourceInfo)) {
                lastSelectedNonTransientTabItem = cti;
            }
        }

        @Override
        public void widgetDefaultSelected(SelectionEvent e) {
            TabItem[] tabItem = tabFolder_lowerSash.getSelection();
            TabItem cti = tabItem[0];
            if ((cti == tabPreferences) || (cti == tabResourceInfo)) {
                lastSelectedNonTransientTabItem = cti;
            }
        }

    }

    /*
     * Keep track of the resource type of the current editor
     */
    public void setEditorType(ResourceType rt) {
        editorType = rt;

    }

    /*
     * Grab all root items and all descendants from the tree and return true if
     * any entry is "toggled on" meaning that the rsc.isVisible() == true
     */
    protected boolean anyChildrenToggleOn(String productName) {
        boolean anyChildrenToggledOn = false;

        TreeItem parentItem = this.findTreeItemByLabelName(productName);

        List<TreeItem> descendants = new ArrayList<TreeItem>();
        getAllDescendants(parentItem, descendants);

        // TODO: this test is due to the fact that for some
        // reason the ensemble children aren't being initially
        // recognized as actual members of the ensemble root
        // TreeItem even though they are really there.
        if (descendants.size() == 0) {
            return true;
        }

        for (TreeItem ti : descendants) {
            Object data = ti.getData();
            if (data == null) {
                anyChildrenToggledOn = true;
                break;
            }
            if (data instanceof GenericResourceHolder) {
                GenericResourceHolder gr = (GenericResourceHolder) data;
                AbstractVizResource<?, ?> rsc = gr.getRsc();
                if (rsc.getProperties().isVisible()) {
                    anyChildrenToggledOn = true;
                    break;
                }
            }

        }
        return anyChildrenToggledOn;
    }

    /*
     * The tree items in the tree are grouped by a top level ensemble name whose
     * children are all ensemble members (viz-resources). Visibility defines
     * whether the resource is visible on the main CAVE map, or not.
     * 
     * If any child tree item (viz resource) is visible then the parent tree
     * item (ensemble name) must also be NOT-grayed-out. Likewise, if all
     * children resources of a given parent are invisible then the parent tree
     * item should be grayed out.
     */
    protected void matchParentToChildrenVisibility(TreeItem childItem) {

        final TreeItem parentItem = childItem.getParentItem();
        if (parentItem == null)
            return;

        final Object d = parentItem.getData();
        if (d instanceof String) {

            List<TreeItem> descendants = new ArrayList<TreeItem>();
            getAllDescendants(parentItem, descendants);

            boolean ai = true;

            for (TreeItem ti : descendants) {
                Object data = ti.getData();
                if (data instanceof GenericResourceHolder) {
                    GenericResourceHolder gr = (GenericResourceHolder) data;
                    AbstractVizResource<?, ?> rsc = gr.getRsc();
                    if (rsc.getProperties().isVisible()) {
                        ai = false;
                        break;
                    }
                }
            }

            final boolean allInvisible = ai;

            // if all invisible then make sure the parent is grayed-out.
            if (allInvisible) {
                parentItem
                        .setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            }
            // otherwise, if any one item is visible then make sure the parent
            // is normalized.
            else {
                parentItem
                        .setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            }

        }
    }

    /*
     * This searches only root level items to see if a root item of a given name
     * has any children (ensemble members) and, if so, returns those children.
     */
    private TreeItem findTreeItemByLabelName(String name) {

        TreeItem[] allRoots = ensembleTree.getItems();
        TreeItem foundItem = null;

        for (TreeItem ti : allRoots) {
            if (String.class.isAssignableFrom(ti.getData().getClass())) {
                String treeItemLabelName = (String) ti.getData();
                if (treeItemLabelName.compareTo(name) == 0) {
                    foundItem = ti;
                    break;
                }
            }
        }
        return foundItem;
    }

    /*
     * Return the first tree item that equals the passed in
     * GenericResourceHolder.
     */
    private TreeItem findTreeItemByResource(GenericResourceHolder rsc) {

        TreeItem foundItem = null;
        TreeItem[] allRoots = ensembleTree.getItems();

        for (TreeItem ti : allRoots) {
            if (foundItem != null)
                break;
            Object tio = ti.getData();
            if ((tio != null) && (tio instanceof GenericResourceHolder)) {
                GenericResourceHolder gr = (GenericResourceHolder) tio;
                if (gr == rsc) {
                    foundItem = ti;
                    break;
                }
            } else if ((tio != null) && (tio instanceof String)) {
                TreeItem[] children = ti.getItems();
                for (TreeItem treeItem : children) {
                    Object o = treeItem.getData();
                    if ((o != null) && (o instanceof GenericResourceHolder)) {
                        GenericResourceHolder gr = (GenericResourceHolder) o;
                        if (gr == rsc) {
                            foundItem = treeItem;
                            break;
                        }
                    }
                }
            }
        }
        return foundItem;
    }

    private void createColumns(TreeViewer ensembleTableViewer) {

        TreeViewerColumn column = new TreeViewerColumn(ensembleTableViewer,
                SWT.LEFT);
        column.getColumn().setWidth(263);
        column.getColumn().setMoveable(false);
        column.getColumn().setText("   Legends");
        column.getColumn().setAlignment(SWT.LEFT);

        EnsembleTreeColumnLabelProvider clp = new EnsembleTreeColumnLabelProvider();
        columnLabelProviders.add(clp);
        column.setLabelProvider(clp);

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

        if (columnLabelProviders != null) {
            for (ColumnLabelProvider clp : columnLabelProviders) {
                clp.dispose();
            }
        }
        if ((ensembleTree != null)
                && (!ensemblesTreeViewer.getTree().isDisposed())) {
            ensembleTree.removeAll();
            ensembleTree.dispose();
            ensembleTree = null;
        }
        if (ensemblesTreeViewer != null) {
            ensemblesTreeViewer = null;
        }
        if (viewFont != null) {
            viewFont.dispose();
            viewFont = null;
        }

        EnsembleToolViewer.setDisposing(false);

    }

    /*
     * This allows the tree widget to take the focus when, for example, the user
     * moves the mouse pointer anywhere over this ViewPart.
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {

        if (ensemblesTreeViewer != null) {
            ensemblesTreeViewer.getControl().setFocus();
        }
    }

    /*
     * We implemented this so that we would (re-)grab the focus after the Volume
     * Browser was opened.
     */
    public void grabFocus() {
        ensemblesTreeViewer.getControl().forceFocus();
    }

    synchronized public boolean isEnabled() {
        return viewEnabled;
    }

    /*
     * The ViewPart should be enabled when an ensemble tool layer is editable in
     * the currently active editor.
     */
    synchronized public void setEnabled(boolean enabled) {

        viewEnabled = enabled;
        tabEnsemblesMain.setEnabled(enabled);
        toolBar.setEnabled(enabled);

    }

    /*
     * Given a tree item, find the item in the tree and toggle it's visibility
     * state.
     */
    private void toggleItemVisible(final TreeItem item) {

        final Object mousedItem = item.getData();
        if (mousedItem instanceof String) {

            Color fg = item.getForeground();
            boolean iv = false;

            // Awkward way of seeing if the item has been already
            // grayed-out. This needs to be further evaluated for a
            // better solution.
            if (fg.getRGB().equals(DISABLED_FOREGROUND_COLOR.getRGB())) {
                iv = false;
            } else {
                iv = true;
            }

            final boolean isVisible = iv;

            // if it was on turn it off
            if (isVisible) {
                item.setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            }
            // if it was off turn it on
            else {
                item.setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            }
            ensemblesTreeViewer.refresh(true);

        } else if (mousedItem instanceof GenericResourceHolder) {

            GenericResourceHolder gr = (GenericResourceHolder) mousedItem;
            // toggle visibility
            gr.getRsc().getProperties()
                    .setVisible(!gr.getRsc().getProperties().isVisible());
            gr.getRsc().issueRefresh();

            // update tree item to reflect new state
            if (gr.getRsc().getProperties().isVisible()) {
                item.setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            } else {
                item.setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                ensemblesTreeViewer.getTree().deselectAll();
            }
            ensemblesTreeViewer.refresh(true);
        }

    }

    /*
     * Given a tree item and a visibility state, find the item in the tree and
     * set it's visibility to the given state.
     */
    private void setItemVisible(TreeItem item, final boolean isVisible) {

        if ((!item.isDisposed()) && (item != null)) {
            final Object mousedItem = item.getData();
            final TreeItem givenItem = item;
            if (mousedItem instanceof String) {

                if (isVisible) {
                    givenItem
                            .setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                    ensemblesTreeViewer.getTree().deselectAll();
                } else {
                    givenItem
                            .setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                    ensemblesTreeViewer.getTree().deselectAll();
                }
                ensemblesTreeViewer.refresh();

            } else if (mousedItem instanceof GenericResourceHolder) {

                GenericResourceHolder gr = (GenericResourceHolder) mousedItem;

                // toggle visibility
                gr.getRsc().getProperties().setVisible(isVisible);
                gr.getRsc().issueRefresh();

                // update tree item to reflect new state
                if (gr.getRsc().getProperties().isVisible()) {
                    givenItem
                            .setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                    ensemblesTreeViewer.getTree().deselectAll();
                } else {
                    givenItem
                            .setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                    ensemblesTreeViewer.getTree().deselectAll();
                }
                ensemblesTreeViewer.refresh();
            }
        }
    }

    /*
     * Return all descendants of a given root tree item.
     */
    private List<TreeItem> getAllDescendants(TreeItem rootItem,
            List<TreeItem> descendants) {

        TreeItem[] children = rootItem.getItems();
        List<TreeItem> immediateChildren = Arrays.asList(children);
        for (TreeItem child : immediateChildren) {
            descendants = getAllDescendants(child, descendants);
            descendants.add(child);
        }
        return descendants;
    }

    /*
     * This is the content provider for the tree.
     */
    private class EnsembleTreeContentProvider implements ITreeContentProvider {

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // do nothing
        }

        @Override
        @SuppressWarnings("unchecked")
        public Object[] getElements(Object inputElement) {

            if (!EnsembleToolManager.getInstance().isReady()) {
                return new Object[0];
            }

            // The root elements of the tree are all strings representing
            // the grid product name ... we get the map from the Ensemble-
            // ToolManager. Create a primitive array of Objects having
            // those names.

            Map<String, List<GenericResourceHolder>> ensembles = (Map<String, List<GenericResourceHolder>>) inputElement;
            if ((ensembles == null) || (ensembles.size() == 0)) {
                return new Object[0];
            }

            Object[] members = new Object[ensembles.size()];

            List<GenericResourceHolder> currResources = null;
            Set<String> ensembleNames = ensembles.keySet();
            Iterator<String> iterator = ensembleNames.iterator();
            String currName = null;
            for (int i = 0; iterator.hasNext(); i++) {
                currName = iterator.next();
                currResources = ensembles.get(currName);
                // is this resource empty?
                if (currResources.size() == 0) {
                    continue;
                }
                // is this an individual resource?
                else if (currResources.size() == 1) {
                    members[i] = currResources.get(0);
                }
                // otherwise it is an ensemble resource (1-to-many ensemble
                // members)
                else {
                    members[i] = currName;
                }
            }

            return members;
        }

        @Override
        public Object[] getChildren(Object parentElement) {

            if (!EnsembleToolManager.getInstance().isReady()) {
                return new Object[0];
            }

            // The children of a top-level product are currently only
            // perturbation members of an ensemble product. Find all
            // children, which are guaranteed to be of Class type
            // GenericResourceHolder, and return them in a primitive
            // array of Objects.
            Map<String, List<GenericResourceHolder>> ensembles = EnsembleToolManager
                    .getInstance().getEnsembleResources();

            if ((ensembles == null) || (ensembles.size() == 0)) {
                return new Object[0];
            }

            Object[] members = null;

            if (String.class.isAssignableFrom(parentElement.getClass())) {

                String ensembleName = (String) parentElement;
                List<GenericResourceHolder> resources = ensembles
                        .get(ensembleName);
                members = new Object[resources.size()];
                int i = 0;
                for (GenericResourceHolder rsc : resources) {
                    members[i++] = rsc;
                }
            } else if (GenericResourceHolder.class
                    .isAssignableFrom(parentElement.getClass())) {
                members = new Object[0];
            }
            return members;
        }

        @Override
        public Object getParent(Object element) {

            if (!EnsembleToolManager.getInstance().isReady()) {
                return null;
            }
            // Given an item from the tree, return its parent
            // in the tree. Currently, only perturbation members
            // can have parents, so the Object that gets returned
            // is guaranteed to be an ensemble product. This is
            // not critical to the logic that follows but just
            // an FYI.
            String parentEnsembleName = null;
            boolean parentFound = false;
            if (GenericResourceHolder.class
                    .isAssignableFrom(element.getClass())) {
                GenericResourceHolder targetRsc = (GenericResourceHolder) element;
                Map<String, List<GenericResourceHolder>> ensembles = EnsembleToolManager
                        .getInstance().getEnsembleResources();
                Set<Entry<String, List<GenericResourceHolder>>> entries = ensembles
                        .entrySet();
                Iterator<Entry<String, List<GenericResourceHolder>>> iterator = entries
                        .iterator();
                Entry<String, List<GenericResourceHolder>> currChild = null;
                for (int i = 0; iterator.hasNext() && !parentFound; i++) {
                    currChild = iterator.next();
                    List<GenericResourceHolder> resources = currChild
                            .getValue();
                    for (GenericResourceHolder gr : resources) {
                        if (gr == targetRsc) {
                            parentFound = true;
                            parentEnsembleName = currChild.getKey();
                        }
                    }
                }
            }
            return parentEnsembleName;
        }

        @Override
        public boolean hasChildren(Object element) {

            if (!EnsembleToolManager.getInstance().isReady()) {
                return false;
            }
            // Currently, if the given element has children than it is
            // an ensemble product. Get the map of resources from the
            // EnsembleToolManager and see whether the element
            // given is an ensemble product (by having an associated
            // "not empty" list ...
            boolean hasChildren = false;
            Map<String, List<GenericResourceHolder>> ensembles = EnsembleToolManager
                    .getInstance().getEnsembleResources();
            if (String.class.isAssignableFrom(element.getClass())) {
                String ensembleName = (String) element;
                List<GenericResourceHolder> resources = ensembles
                        .get(ensembleName);
                if ((resources != null) && (resources.size() > 0)) {
                    hasChildren = true;
                }
            } else {
                // currently no other class types have children
            }
            return hasChildren;
        }

        @Override
        public void dispose() {
            // nothing needs to be disposed
        }
    }

    /*
     * Create the ViewPart's main tool bar.
     */
    private ToolBar makeToolBar(Composite parent) {

        final ToolBar toolBar = new ToolBar(parent, SWT.BORDER_SOLID);

        final ToolItem browserItem = new ToolItem(toolBar, SWT.PUSH);
        browserItem.setImage(INFO_ICON);
        browserItem.setToolTipText("Volume Browser");
        browserItem.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                volumeBrowserJustOpened = true;

                IServiceLocator serviceLocator = PlatformUI.getWorkbench();
                ICommandService commandService = (ICommandService) serviceLocator
                        .getService(ICommandService.class);

                try {
                    Command command = commandService
                            .getCommand("com.raytheon.viz.volumebrowser.volumeBrowserRef");

                    // Optionally pass a ExecutionEvent instance, default
                    // (empty)
                    // signature creates blank event
                    command.executeWithChecks(new ExecutionEvent());

                } catch (Exception ex) {
                    ex.printStackTrace();
                }
                ignoreFocusStartTime = System.currentTimeMillis();

            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

        });

        ToolItem separator_00 = new ToolItem(toolBar, SWT.SEPARATOR);
        separator_00.setWidth(2);

        final ToolItem runItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        runItem.setImage(GEAR_ICON);
        runItem.setToolTipText("Run Calculation");

        ToolItem separator_3 = new ToolItem(toolBar, SWT.SEPARATOR);
        separator_3.setWidth(0);

        EnsembleToolItemActionListener ecl = new EnsembleToolItemActionListener(
                runItem);
        ecl.add(BUNDLE_CMD_TITLE, false);
        ecl.addSeparator();
        ecl.add(Calculation.MEAN.getTitle(), true);
        ecl.add(Calculation.MIN.getTitle(), true);
        ecl.add(Calculation.MAX.getTitle(), true);
        ecl.add(Calculation.MEDIAN.getTitle(), true);
        ecl.add(Calculation.MODE.getTitle(), false);
        ecl.add(Calculation.RANGE.getTitle(), true);
        ecl.add(Calculation.SUMMATION.getTitle(), true);
        ecl.add(Calculation.STANDARD_DEVIATION.getTitle(), true);
        ecl.add(Calculation.AVG_MINUS_STD_DEV.getTitle(), false);
        ecl.add(Calculation.AVG_PLUS_STD_DEV.getTitle(), false);
        ecl.add(Calculation.COMBINED_ENS_REL_FREQ.getTitle(), false);
        ecl.add(Calculation.TRIPLET_ENS_REL_FREQ.getTitle(), false);
        ecl.add(Calculation.ENSEMBLE_RELATIVE_FREQUENCY.getTitle(), false);
        ecl.add(Calculation.HISTOGRAM_SAMPLING.getTitle(), true);
        ecl.add(Calculation.HISTOGRAM_TEXT.getTitle(), true);

        runItem.addSelectionListener(ecl);

        parent.pack();

        return toolBar;
    }

    private void constructImages() {

        if (ALPHANUMERIC_SORT_ICON == null) {
            ALPHANUMERIC_SORT_ICON = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/a-to-z-sort.gif");
        }
        if (GEAR_ICON == null) {
            GEAR_ICON = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/calculate-gear-40x24px.gif");
        }
        if (INFO_ICON == null) {
            INFO_ICON = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/volume-browser-lower-case-40x24px.gif");
        }
    }

    /*
     * Here's how we control the items displayed in the tree.
     */
    private class EnsembleTreeColumnLabelProvider extends ColumnLabelProvider {

        public Font getFont(Object element) {
            Font f = SWTResourceManager.getFont("courier new", 10, SWT.BOLD);
            return f;
        }

        public Image getImage(Object element) {
            Image image = null;
            if (element instanceof String) {
                String productName = (String) element;
                int imageWidth = 46;
                int imageHeight = 18;
                ImageData imageData = new ImageData(imageWidth, imageHeight,
                        24, new PaletteData(255, 255, 255));
                imageData.transparentPixel = imageData.palette
                        .getPixel(new RGB(255, 255, 255));
                image = new Image(owner.getDisplay(), imageData);
                GC gc = new GC(image);
                gc.setBackground(SWTResourceManager.WHITE);
                gc.fillRectangle(0, 0, imageWidth, imageHeight);

                // if any ensemble members are visible then the root tree item
                // should be "toggled on" ...
                if (anyChildrenToggleOn(productName)) {
                    gc.setBackground(SWTResourceManager.BLACK);
                }
                // otherwise, the root tree item should appear "toggled off" ...
                else {
                    gc.setBackground(SWTResourceManager.DARKER_GRAY);
                }

                int listEntryLineUpperLeft_x = 4;
                int listEntryTopLineUpperLeft_y = 4;
                int listEntryMiddleLineUpperLeft_y = 8;
                int listEntryBottomLineUpperLeft_y = 12;
                int listEntryWidth = 23;
                int listEntryHeight = 2;

                // the icon for an ensemble product is three black
                // horizontal bars which is an attempt to represent
                // a list of the ensemble's pertubation members.
                gc.fillRectangle(listEntryLineUpperLeft_x,
                        listEntryTopLineUpperLeft_y, listEntryWidth,
                        listEntryHeight);
                gc.fillRectangle(listEntryLineUpperLeft_x,
                        listEntryMiddleLineUpperLeft_y, listEntryWidth,
                        listEntryHeight);
                gc.fillRectangle(listEntryLineUpperLeft_x,
                        listEntryBottomLineUpperLeft_y, listEntryWidth,
                        listEntryHeight);

                int bulletSize = 3;
                int bulletUpperLeftMargin_x = 15;
                int bulletUpperLeft_y = 9;

                // then put a nice hyphen
                gc.fillRectangle(listEntryWidth + bulletUpperLeftMargin_x,
                        bulletUpperLeft_y, bulletSize + 2, bulletSize - 1);
                gc.dispose();

            } else if (element instanceof GenericResourceHolder) {

                GenericResourceHolder gr = (GenericResourceHolder) element;
                RGB color = gr.getRsc()
                        .getCapability(ColorableCapability.class).getColor();

                int imageWidth = 46;
                int imageHeight = 18;
                int colorWidth = 24;
                int colorHeight = 14;
                int innerColorWidth = 20;
                int innerColorHeight = 10;
                int bulletSize = 3;
                int bulletUpperLeftMargin_x = 13;
                int bulletUpperLeft_y = 9;

                ImageData imageData = new ImageData(imageWidth, imageHeight,
                        24, new PaletteData(255, 255, 255));
                imageData.transparentPixel = imageData.palette
                        .getPixel(new RGB(255, 255, 255));
                image = new Image(owner.getDisplay(), imageData);
                GC gc = new GC(image);
                gc.setBackground(SWTResourceManager.WHITE);
                gc.fillRectangle(0, 0, imageWidth, imageHeight);
                if (gr.getRsc().getProperties().isVisible()) {

                    // need the following tweaking integers which cosmetic
                    // center things nicely
                    gc.setBackground(SWTResourceManager.BLACK);

                    // the icon for a visible individual grid resources put the
                    // color of the resource inside a black bordered rectangle.
                    gc.fillRectangle(4, imageHeight - colorHeight - 2,
                            colorWidth, colorHeight);
                    gc.setBackground(SWTResourceManager.getColor(color));
                    gc.fillRectangle(4 + ((colorWidth - innerColorWidth) / 2),
                            (imageHeight - colorHeight)
                                    + ((colorHeight - innerColorHeight) / 2)
                                    - 2, innerColorWidth, innerColorHeight);

                    // then put a nice hyphen
                    gc.setBackground(SWTResourceManager.BLACK);
                    gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                            bulletUpperLeft_y, bulletSize + 2, bulletSize - 1);
                } else {
                    // need the following tweaking integers which cosmetic
                    // center things nicely
                    gc.setBackground(SWTResourceManager.DARKER_GRAY);

                    // the icon for a hidden individual grid resources put the
                    // color of the resource inside a greyed bordered rectangle.
                    gc.fillRectangle(4, imageHeight - colorHeight - 2,
                            colorWidth, colorHeight);
                    gc.setBackground(SWTResourceManager.getColor(color));
                    gc.fillRectangle(4 + ((colorWidth - innerColorWidth) / 2),
                            (imageHeight - colorHeight)
                                    + ((colorHeight - innerColorHeight) / 2)
                                    - 2, innerColorWidth, innerColorHeight);

                    gc.setBackground(SWTResourceManager.LIGHT_GRAY);
                    gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                            bulletUpperLeft_y, bulletSize + 2, bulletSize - 1);
                }
                gc.dispose();

            }
            return image;
        }

        public String getText(Object element) {

            String nodeLabel = null;
            if (element instanceof String) {
                nodeLabel = (String) element;
            } else if (GeneratedGridResourceHolder.class
                    .isAssignableFrom(element.getClass())
                    || (GeneratedTimeSeriesResourceHolder.class
                            .isAssignableFrom(element.getClass()))) {
                GenericResourceHolder gr = (GenericResourceHolder) element;
                nodeLabel = gr.getUniqueName();
            } else if (element instanceof GridResourceHolder) {

                GridResourceHolder gr = (GridResourceHolder) element;
                if ((gr.getEnsembleId() != null)
                        && (gr.getEnsembleId().length() > 0)) {
                    nodeLabel = gr.getEnsembleId();
                } else {
                    nodeLabel = gr.getUniqueName();
                }
            } else if (TimeSeriesResourceHolder.class.isAssignableFrom(element
                    .getClass())) {

                TimeSeriesResourceHolder tsr = (TimeSeriesResourceHolder) element;
                if ((tsr.getEnsembleId() != null)
                        && (tsr.getEnsembleId().length() > 0)) {
                    nodeLabel = tsr.getEnsembleId();
                } else {
                    nodeLabel = tsr.getUniqueName();
                }
            } else if (element instanceof HistogramGridResourceHolder) {

                HistogramGridResourceHolder gr = (HistogramGridResourceHolder) element;
                if ((gr.getEnsembleId() != null)
                        && (gr.getEnsembleId().length() > 0)) {
                    nodeLabel = gr.getEnsembleId();
                } else {
                    nodeLabel = gr.getUniqueName();
                }
            }
            // update the visibility status cosmetically (i.e. normal text
            // versus graying-out)
            if (element instanceof GenericResourceHolder) {
                GenericResourceHolder gr = (GenericResourceHolder) element;

                TreeItem treeItem = findTreeItemByResource(gr);
                if (treeItem != null) {
                    if (gr.getRsc().getProperties().isVisible()) {
                        treeItem.setForeground(EnsembleToolViewer.ENABLED_FOREGROUND_COLOR);
                    } else {
                        treeItem.setForeground(EnsembleToolViewer.DISABLED_FOREGROUND_COLOR);
                    }
                    matchParentToChildrenVisibility(treeItem);
                }
            }

            return nodeLabel;
        }

    }

    // TODO we need come up with a more intuitive solution
    // to sorting than what is provided here.
    private class EnsembleTreeSorter extends ViewerSorter {

        public int compare(Viewer v, Object av1, Object av2) {

            boolean compareResultFound = false;

            int compareResult = 0;

            if ((av1 instanceof String) && (av2 instanceof String)) {
                String n1 = (String) av1;
                String n2 = (String) av2;
                compareResult = n1.compareTo(n2);
            } else if ((av1 instanceof GenericResourceHolder)
                    && (av2 instanceof GenericResourceHolder)) {

                GenericResourceHolder gr1 = (GenericResourceHolder) av1;
                GenericResourceHolder gr2 = (GenericResourceHolder) av2;

                AbstractVizResource<?, ?> vr1 = gr1.getRsc();
                AbstractVizResource<?, ?> vr2 = gr2.getRsc();

                if ((av1 instanceof GridResourceHolder)
                        && (av2 instanceof GridResourceHolder)) {

                    GridResourceHolder grh1 = (GridResourceHolder) av1;
                    GridResourceHolder grh2 = (GridResourceHolder) av2;

                    // If there are perturbation names then let's compare those
                    // ...
                    String av1_pert = grh1.getEnsembleId();
                    String av2_pert = grh2.getEnsembleId();

                    if ((av1_pert != null) && (av1_pert.length() > 0)
                            && (av2_pert != null) && (av2_pert.length() > 0)) {

                        compareResult = av1_pert.compareTo(av2_pert);
                        compareResultFound = true;
                    } else {
                        String ts_fullName_1 = vr1.getName();
                        String ts_fullName_2 = vr2.getName();

                        if ((ts_fullName_1 != null) && (ts_fullName_2 != null)) {

                            compareResult = ts_fullName_1
                                    .compareTo(ts_fullName_2);
                            compareResultFound = true;
                        }
                    }
                }

                if (!compareResultFound) {

                    if (TimeSeriesResourceHolder.class.isAssignableFrom(av1
                            .getClass())
                            && (TimeSeriesResourceHolder.class
                                    .isAssignableFrom(av2.getClass()))) {

                        TimeSeriesResourceHolder tsr1 = (TimeSeriesResourceHolder) av1;
                        TimeSeriesResourceHolder tsr2 = (TimeSeriesResourceHolder) av2;

                        // If there are perturbation names then let's compare
                        // those ...
                        String pert_1 = tsr1.getEnsembleId();
                        String pert_2 = tsr2.getEnsembleId();

                        if ((pert_1 != null) && (pert_1.length() > 0)
                                && (pert_2 != null) && (pert_2.length() > 0)) {

                            compareResult = pert_1.compareTo(pert_2);
                            compareResultFound = true;
                        } else {

                            String ts_fullName_1 = vr1.getName();
                            String ts_fullName_2 = vr2.getName();

                            if ((ts_fullName_1 != null)
                                    && (ts_fullName_2 != null)) {

                                compareResult = ts_fullName_1
                                        .compareTo(ts_fullName_2);
                                compareResultFound = true;
                            }
                        }
                    }
                }
                if (!compareResultFound) {

                    if ((vr1 != null) && (vr1.getName() != null)
                            && (vr2 != null) && (vr2.getName() != null)) {

                        compareResult = vr1.getName().compareTo(vr2.getName());
                        compareResultFound = true;
                    }
                }
            }

            return compareResult;
        }
    }

    private class EnsembleToolItemActionListener extends SelectionAdapter {
        private ToolItem dropdown;

        private Menu calculationMenu;

        public EnsembleToolItemActionListener(ToolItem dd) {
            dropdown = dd;
            calculationMenu = new Menu(dropdown.getParent().getShell());
        }

        public void addSeparator() {
            new MenuItem(calculationMenu, SWT.SEPARATOR);
        }

        public void add(String item, boolean isEnabled) {
            MenuItem mi = new MenuItem(calculationMenu, SWT.NONE);
            mi.setText(item);
            mi.setEnabled(isEnabled);
            mi.addSelectionListener(new SelectionAdapter() {

                public void widgetSelected(SelectionEvent event) {
                    MenuItem selected = (MenuItem) event.widget;
                    if (selected.getText().compareTo(
                            Calculation.MEAN.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.MEAN);
                    } else if (selected.getText().compareTo(
                            Calculation.MIN.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.MIN);
                    } else if (selected.getText().compareTo(
                            Calculation.MAX.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.MAX);
                    } else if (selected.getText().compareTo(
                            Calculation.SUMMATION.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.SUMMATION);
                    } else if (selected.getText().compareTo(
                            Calculation.MEDIAN.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.MEDIAN);
                    } else if (selected.getText().compareTo(
                            Calculation.MODE.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.MODE);
                    } else if (selected.getText().compareTo(
                            Calculation.STANDARD_DEVIATION.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.STANDARD_DEVIATION);
                    } else if (selected.getText().compareTo(
                            Calculation.RANGE.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.RANGE);
                    } else if (selected.getText().compareTo(
                            Calculation.AVG_MINUS_STD_DEV.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.AVG_MINUS_STD_DEV);
                    } else if (selected.getText().compareTo(
                            Calculation.AVG_PLUS_STD_DEV.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.AVG_PLUS_STD_DEV);
                    } else if (selected.getText().compareTo(
                            Calculation.HISTOGRAM_SAMPLING.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.HISTOGRAM_SAMPLING);
                    } else if (selected.getText().compareTo(
                            Calculation.HISTOGRAM_TEXT.getTitle()) == 0) {
                        EnsembleToolManager.getInstance().calculate(
                                Calculation.HISTOGRAM_TEXT);
                    }
                }
            });
        }

        public void widgetSelected(SelectionEvent event) {
            // display the menu
            if ((event.detail == SWT.ARROW) || (event.detail == SWT.MENU_MOUSE)) {
                ToolItem item = (ToolItem) event.widget;
                Rectangle rect = item.getBounds();
                Point pt = item.getParent()
                        .toDisplay(new Point(rect.x, rect.y));
                calculationMenu.setLocation(pt.x, pt.y + rect.height);
                calculationMenu.setVisible(true);
            }
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            widgetSelected(e);
        }
    }

    static boolean CTRL_KEY_DEPRESSED = false;

    /*
     * This key listener class is used to set the mouse pointer to either the
     * HAND cursor, when the Ctrl key is depressed, or to the default ARROW
     * cursor, when the Ctrl key is released.
     * 
     * When the user presses CTRL-MB1 (e.g. Ctrl LEFT-CLICK) over a resource in
     * the navigator, the cursor will first change to the HAND cursor, and the
     * item will be selected (e.g. the contour highlighted).
     * 
     * When the user does not have the Ctrl key depressed, the mouse pointer
     * remains an ARROW cursor. When the user presses MB1 over (e.g. LEFT-CLICK)
     * a resource in the navigator that resource will have its visibility
     * toggled.
     */

    private class EnsembleTreeKeyListener implements KeyListener {

        @Override
        public void keyPressed(KeyEvent e) {

            if ((e.keyCode & SWT.CTRL) == SWT.CTRL) {
                CTRL_KEY_DEPRESSED = true;
                updateCursor(selectionModeCursor);
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {

            if ((e.keyCode & SWT.CTRL) == SWT.CTRL) {
                CTRL_KEY_DEPRESSED = false;
                updateCursor(normalCursor);
            }
        }

    }

    private class EnsembleTreeMouseListener implements MouseListener {

        @Override
        public void mouseDoubleClick(MouseEvent event) {
            // TODO No need for a double click event just yet.
        }

        @Override
        public void mouseDown(MouseEvent event) {

            // The mouseDown event is what initiates the
            // displaying of a context-sensitive popup
            // menu for either ensemble products or
            // individual grid products ...

            // if the ensemble tool isn't open then ignore
            // user mouse clicks ...
            if (!EnsembleToolManager.getInstance().isReady()) {
                return;
            }

            // get the tree item that was clicked on ...
            Point point = new Point(event.x, event.y);
            final TreeItem item = ensembleTree.getItem(point);

            // is this a mouse-button-3 (e.g. typical RIGHT-CLICK)
            // over a tree item?
            if ((item != null) && (event.button == 3)) {
                // let's put up a context-sensitive menu similar to cave legend
                // pop-up menu ...
                final Object mousedItem = item.getData();

                // Currently, top level tree items (which also contain child
                // tree items) are always Ensemble names (unique strings). So
                // if the user clicks on this item then display the popup menu
                // for the Ensemble product.
                if (mousedItem instanceof String) {
                    final Menu legendMenu = new Menu(owner.getShell(),
                            SWT.POP_UP);
                    final String mousedEnsembleName = (String) mousedItem;

                    // relative frequency menu item allows the user to generate
                    // a probability display demonstrating the chance a value
                    // p(x) lies within a range, outside a range, above a
                    // threshold, or below a threshold.
                    addERFLayerMenuItem = new MenuItem(legendMenu, SWT.PUSH);
                    addERFLayerMenuItem.setText("Relative Frequency");

                    // only enable the RF menu item if we are in plan view
                    if (editorType == ResourceType.TIME_SERIES) {
                        addERFLayerMenuItem.setEnabled(false);
                    }
                    if (editorType == ResourceType.PLAN_VIEW) {
                        addERFLayerMenuItem.setEnabled(true);
                    }

                    addERFLayerMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    VizApp.runAsync(new Runnable() {

                                        @Override
                                        public void run() {

                                            updateCursor(waitCursor);
                                            startAddERFLayer(mousedEnsembleName);
                                            updateCursor(normalCursor);

                                        }
                                    });

                                }
                            });

                    // TODO Need to have a easy access Mean calculation on
                    // an Ensemble product ...

                    // this menu item allows the user to choose a color
                    // gradient for either the SREF or GEFS ensemble
                    // products.
                    MenuItem ensembleColorizeMenuItem = new MenuItem(
                            legendMenu, SWT.PUSH);
                    ensembleColorizeMenuItem.setText("Color Gradient");

                    ensembleColorizeMenuItem.addListener(SWT.Selection,
                            new Listener() {
                                public void handleEvent(Event event) {

                                    VizApp.runAsync(new Runnable() {

                                        @Override
                                        public void run() {

                                            updateCursor(waitCursor);
                                            updateColorsOnResource(mousedEnsembleName);
                                            updateCursor(normalCursor);

                                        }
                                    });
                                }
                            });

                    // this menu item allows the user to remove the ensemble and
                    // all of its members.
                    MenuItem unloadRscMenuItem = new MenuItem(legendMenu,
                            SWT.PUSH);
                    unloadRscMenuItem.setText("Unload Members");
                    unloadRscMenuItem.addListener(SWT.Selection,
                            new Listener() {
                                public void handleEvent(Event event) {

                                    boolean proceed = MessageDialog.openConfirm(
                                            owner.getShell(),
                                            "Unload Ensemble Members",
                                            "Are you sure you want to unload all members of "
                                                    + mousedEnsembleName + "?");

                                    if (proceed) {

                                        VizApp.runAsync(new Runnable() {

                                            @Override
                                            public void run() {
                                                updateCursor(waitCursor);
                                                EnsembleToolManager
                                                        .getInstance()
                                                        .unloadResourcesByName(
                                                                item.getText());
                                                updateCursor(normalCursor);
                                            }
                                        });

                                    }
                                }
                            });

                    // this menu item allows the user to hide/show an ensemble
                    // product and all of its members.
                    final MenuItem toggleVisibilityMenuItem = new MenuItem(
                            legendMenu, SWT.CHECK);
                    toggleVisibilityMenuItem.setText("Display Product");
                    toggleVisibilityMenuItem.setEnabled(false);
                    toggleVisibilityMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    VizApp.runAsync(new Runnable() {

                                        @Override
                                        public void run() {

                                            updateCursor(waitCursor);
                                            setItemVisible(item,
                                                    toggleVisibilityMenuItem
                                                            .getSelection());
                                            updateCursor(normalCursor);

                                        }
                                    });
                                }
                            });

                    legendMenu.setVisible(true);

                    // show the last selected non-transient tab ... the
                    // only transient tab is the ERF tab. The other two
                    // tabs are Info and Preferences. If one of the latter
                    // two tabs were previously chosen, then reopen that
                    // previously selected tab.
                    if (lastSelectedNonTransientTabItem != null) {
                        tabFolder_lowerSash
                                .setSelection(lastSelectedNonTransientTabItem);
                    } else {
                        tabFolder_lowerSash.setSelection(tabResourceInfo);
                    }

                } else if (mousedItem instanceof GenericResourceHolder) {

                    // if the tree item the user clicked on was an individual
                    // grid product then show the context-sensitive popup menu
                    // for it ...
                    final GenericResourceHolder gr = (GenericResourceHolder) mousedItem;

                    MenuManager menuMgr = new MenuManager("#PopupMenu");
                    menuMgr.setRemoveAllWhenShown(true);

                    // the popup menu is generated by the ContextMenuManager
                    menuMgr.addMenuListener(new IMenuListener() {
                        public void menuAboutToShow(IMenuManager manager) {
                            ResourcePair rp = EnsembleToolManager.getInstance()
                                    .getResourcePair(gr.getRsc());
                            if (rp != null) {
                                com.raytheon.viz.ui.cmenu.ContextMenuManager
                                        .fillContextMenu(manager, rp, gr
                                                .getRsc()
                                                .getResourceContainer());
                            }
                        }

                    });

                    final Menu legendMenu = menuMgr.createContextMenu(owner);
                    legendMenu.setVisible(true);

                    ensemblesTreeViewer.refresh();

                    // disable the ui components on the ERF tab ...
                    disableERFTabWidgets();
                    // show the last selected non-transient tab ... the
                    // only transient tab is the ERF tab. The other two
                    // tabs are Info and Preferences. If one of the latter
                    // two tabs were previously chosen, then reopen that
                    // previously selected tab.
                    if (lastSelectedNonTransientTabItem != null) {
                        tabFolder_lowerSash
                                .setSelection(lastSelectedNonTransientTabItem);
                    } else {
                        tabFolder_lowerSash.setSelection(tabResourceInfo);
                    }

                }
                ensembleTree.deselect(item);
            }
        }

        @Override
        public void mouseUp(MouseEvent event) {

            Point point = new Point(event.x, event.y);
            final TreeItem userClickedTreeItem = ensembleTree.getItem(point);

            // the mouse up event currently only acts on items in the tree
            // so if the tree item is null then just return ...
            if (userClickedTreeItem == null)
                return;

            // keep track of the last highlighted resource and make sure
            // it is still displayed properly ...
            if (EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE != null) {
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE.getCapability(
                        OutlineCapability.class).setOutlineWidth(
                        LAST_HIGHLIGHTED_RESOURCE_WIDTH);
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE.getCapability(
                        ColorableCapability.class).setColor(
                        LAST_HIGHLIGHTED_RESOURCE_RGB);
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE
                        .getCapability(OutlineCapability.class)
                        .setOutlineOn(
                                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED);
            }

            // is this a CTRL-MB1 (e.g. CONTROL LEFT-CLICK) over a tree item?
            // ... then this is a UI SWT item selection
            if ((userClickedTreeItem != null) && (event.button == 1)
                    && ((event.stateMask & SWT.CTRL) != 0)) {

                final Object mousedItem = userClickedTreeItem.getData();

                // Ctrl-click on a item that is already selected deselects it.
                if (EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE != null) {

                    if (mousedItem instanceof GenericResourceHolder) {
                        GenericResourceHolder grh = (GenericResourceHolder) mousedItem;
                        if (grh.getRsc() == EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE) {
                            ensembleTree.deselect(userClickedTreeItem);
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = null;
                            return;
                        }
                    }
                } else if ((ensemblesTreeViewer != null)
                        && (ensemblesTreeViewer.getTree() != null)) {

                    // Ctrl-click on a item that is not selected selects it.
                    if (mousedItem instanceof GenericResourceHolder) {
                        GenericResourceHolder grh = (GenericResourceHolder) mousedItem;
                        EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = grh
                                .getRsc();
                        ensemblesTreeViewer.getTree().deselectAll();
                        ensemblesTreeViewer.getTree().select(
                                userClickedTreeItem);
                    }
                    ensemblesTreeViewer.getTree().update();
                }

                // always display either the last opened tab or the information
                // tab (i.e. only display the ERF tab when we the user is
                // actively entering an ERF probablility).
                disableERFTabWidgets();
                if (lastSelectedNonTransientTabItem != null) {
                    tabFolder_lowerSash
                            .setSelection(lastSelectedNonTransientTabItem);
                } else {
                    tabFolder_lowerSash.setSelection(tabResourceInfo);
                }

                // if the user selects an generated ERF product then update
                // the ERF tabs ...
                if (mousedItem instanceof GeneratedGridResourceHolder) {
                    GeneratedGridResourceHolder grh = (GeneratedGridResourceHolder) mousedItem;
                    if (grh.getCalculation() == Calculation.ENSEMBLE_RELATIVE_FREQUENCY) {
                        tabFolder_lowerSash.setSelection(tabERFLayerControl);
                        setERFFields(grh.getRange(), grh.getUniqueName());
                    }
                }

                // is this an individual grid product?
                if (mousedItem instanceof GridResourceHolder) {
                    GridResourceHolder grh = (GridResourceHolder) mousedItem;

                    // only highlight a visible resource
                    if ((grh.getRsc() != null)
                            && (grh.getRsc().getProperties().isVisible())) {

                        currentEnsembleRsc = grh.getRsc();

                        // then highlight the resource in the primary pane
                        // and keep track of the resource as the most recently
                        // highlighted resource.
                        VizApp.runSync(new Runnable() {

                            public void run() {
                                if ((currentEnsembleRsc != null)
                                        && (thickenOnSelection)) {
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = currentEnsembleRsc;
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_WIDTH = currentEnsembleRsc
                                            .getCapability(
                                                    OutlineCapability.class)
                                            .getOutlineWidth();
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_RGB = currentEnsembleRsc
                                            .getCapability(
                                                    ColorableCapability.class)
                                            .getColor();
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = currentEnsembleRsc
                                            .getCapability(
                                                    OutlineCapability.class)
                                            .isOutlineOn();
                                    currentEnsembleRsc.getCapability(
                                            OutlineCapability.class)
                                            .setOutlineOn(true);
                                    currentEnsembleRsc.getCapability(
                                            OutlineCapability.class)
                                            .setOutlineWidth(thickenWidth);
                                    if (!useResourceColorOnThicken) {
                                        currentEnsembleRsc.getCapability(
                                                ColorableCapability.class)
                                                .setColor(
                                                        thickenOnSelectionColor
                                                                .getRGB());
                                    }
                                    currentEnsembleRsc.issueRefresh();
                                }
                            }
                        });
                    }
                }
                // is this a generated product (e.g. a user-requested
                // calculation)
                if (mousedItem instanceof GeneratedGridResourceHolder) {
                    GeneratedGridResourceHolder grh = (GeneratedGridResourceHolder) mousedItem;

                    // only highlight a visible resource
                    if ((grh.getRsc() != null)
                            && (grh.getRsc().getProperties().isVisible())) {

                        currentEnsembleRsc = grh.getRsc();

                        // then highlight the resource in the primary pane
                        // and keep track of the resource as the most recently
                        // highlighted resource.
                        VizApp.runSync(new Runnable() {

                            public void run() {
                                if ((currentEnsembleRsc != null)
                                        && (thickenOnSelection)) {
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = currentEnsembleRsc;
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_WIDTH = currentEnsembleRsc
                                            .getCapability(
                                                    OutlineCapability.class)
                                            .getOutlineWidth();
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_RGB = currentEnsembleRsc
                                            .getCapability(
                                                    ColorableCapability.class)
                                            .getColor();
                                    EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = currentEnsembleRsc
                                            .getCapability(
                                                    OutlineCapability.class)
                                            .isOutlineOn();
                                    currentEnsembleRsc.getCapability(
                                            OutlineCapability.class)
                                            .setOutlineOn(true);
                                    currentEnsembleRsc.getCapability(
                                            OutlineCapability.class)
                                            .setOutlineWidth(thickenWidth);
                                    if (!useResourceColorOnThicken) {
                                        currentEnsembleRsc.getCapability(
                                                ColorableCapability.class)
                                                .setColor(
                                                        thickenOnSelectionColor
                                                                .getRGB());
                                    }
                                    currentEnsembleRsc.issueRefresh();
                                }
                            }
                        });
                    }
                }

            }
            // is this a simple left-click (MB1) over a tree item?
            else if ((userClickedTreeItem != null) && (event.button == 1)) {

                // by default, left-clicking on a tree item in the
                // tree will toggle that product's visibility.
                final Object mousedItem = userClickedTreeItem.getData();

                // A string means it is an ensemble product name
                if (mousedItem instanceof String) {
                    // TODO: need a performant solution for toggling visibility
                    // of all the resources in one ensemble
                } else if (mousedItem instanceof GenericResourceHolder) {

                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {

                            updateCursor(waitCursor);
                            toggleItemVisible(userClickedTreeItem);

                            // if this was the last item to be toggled off,
                            // for example, in an ensemble group, then you
                            // need to toggle the parent tree item off also
                            matchParentToChildrenVisibility(userClickedTreeItem);
                            updateCursor(normalCursor);
                        }

                    });

                }
            }

        }
    }

    /*
     * This method will update color on a given ensemble resource. Underlying
     * methods know how to colorize the contained perturbation members (eg. for
     * SREF, breaks colors into three categories for NMM, NMB and EM).
     * 
     * We need a better way of determining what type of resource we have.
     */
    protected void updateColorsOnResource(String ensembleName) {

        final List<GenericResourceHolder> children = getEnsembleMemberGenericResources(ensembleName);

        // TODO: poor-man's way of knowing what type of flavor this ensemble is
        if ((ensembleName.indexOf("GEFS") >= 0)
                || (ensembleName.indexOf("GFS Ensemble") >= 0)) {
            updateGEFSEnsembleColors(ensembleName, children);
        } else if (ensembleName.indexOf("SREF") >= 0) {
            updateSREFColors(ensembleName, children);
        }

    }

    private void updateSREFColors(String ensembleName,
            final List<GenericResourceHolder> children) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                updateCursor(waitCursor);

                EnsembleSREFColorChooser cd = new EnsembleSREFColorChooser(
                        owner.getShell());
                cd.setBlockOnOpen(true);
                if (cd.open() == Window.OK) {
                    Color currColor = null;
                    for (GenericResourceHolder gRsc : children) {
                        if (gRsc instanceof GridResourceHolder) {
                            AbstractVizResource<?, ?> rsc = gRsc.getRsc();
                            String ensId = gRsc.getEnsembleIdRaw();
                            if ((ensId != null) && (ensId.length() > 1)) {
                                currColor = ChosenSREFColors.getInstance()
                                        .getGradientByEnsembleId(ensId);
                                rsc.getCapability(ColorableCapability.class)
                                        .setColor(currColor.getRGB());
                                rsc.issueRefresh();
                            }
                        }
                    }
                    ensemblesTreeViewer.refresh();
                }

                updateCursor(normalCursor);

            }
        });

    }

    private void updateGEFSEnsembleColors(String ensembleName,
            final List<GenericResourceHolder> children) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                updateCursor(waitCursor);

                EnsembleGEFSColorChooser cd = new EnsembleGEFSColorChooser(
                        owner.getShell());
                cd.setBlockOnOpen(true);
                if (cd.open() == Window.OK) {

                    // EditorUtil.getActiveEditor().
                    Color currColor = null;
                    int count = 0;
                    for (GenericResourceHolder gRsc : children) {
                        if (gRsc instanceof GridResourceHolder) {
                            count++;
                            AbstractVizResource<?, ?> rsc = gRsc.getRsc();
                            if (count == 1) {
                                currentEnsembleRsc = rsc;
                            }
                            String ensId = gRsc.getEnsembleIdRaw();
                            if ((ensId != null) && (ensId.length() > 1)) {
                                currColor = ChosenGEFSColors.getInstance()
                                        .getGradientByEnsembleId(ensId);
                                rsc.getCapability(ColorableCapability.class)
                                        .setColor(currColor.getRGB());
                                rsc.getCapability(OutlineCapability.class);
                                rsc.issueRefresh();
                            }
                        }
                    }
                    ensemblesTreeViewer.refresh();
                }

                updateCursor(normalCursor);
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

    /*
     * Returns all resources (perturbation members) associated with an ensemble
     * resource name.
     */
    protected List<GenericResourceHolder> getEnsembleMemberGenericResources(
            String ensembleName) {

        TreeItem parentItem = findTreeItemByLabelName(ensembleName);

        List<TreeItem> descendants = new ArrayList<TreeItem>();
        getAllDescendants(parentItem, descendants);

        List<GenericResourceHolder> childResources = new ArrayList<GenericResourceHolder>();

        if (descendants.size() > 0) {
            for (TreeItem ti : descendants) {
                Object data = ti.getData();
                if (data == null) {
                    continue;
                }
                if (data instanceof GenericResourceHolder) {
                    GenericResourceHolder gr = (GenericResourceHolder) data;
                    childResources.add(gr);
                }
            }
        }
        return childResources;
    }

    protected List<AbstractVizResource<?, ?>> getEnsembleMemberVizResources(
            String ensembleName) {

        TreeItem parentItem = findTreeItemByLabelName(ensembleName);

        List<TreeItem> descendants = new ArrayList<TreeItem>();
        getAllDescendants(parentItem, descendants);

        List<AbstractVizResource<?, ?>> childResources = new ArrayList<AbstractVizResource<?, ?>>();

        // TODO: this test is due to the fact that for some reason the
        // ensemble children aren't being initially recognized as actual
        // members of the ensemble root TreeItem even though they are
        // really there!

        if (descendants.size() > 0) {
            for (TreeItem ti : descendants) {
                Object data = ti.getData();
                if (data == null) {
                    continue;
                }
                if (data instanceof GenericResourceHolder) {
                    GenericResourceHolder gr = (GenericResourceHolder) data;
                    AbstractVizResource<?, ?> rsc = gr.getRsc();
                    childResources.add(rsc);
                }
            }
        }
        return childResources;
    }

    public void prepareForNewToolInput() {
        VizApp.runAsync(new Runnable() {
            public void run() {
                ensemblesTreeViewer.setInput(null);
            }
        });
    }

    public void refreshInput() {

        // TODO: Is there a better way to initialize e.g. listeners;
        // a better place to detect when the view part is uninitialized?
        if (viewPartListener == null) {
            viewPartListener = new EnsembleViewPartListener(this);
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            IPartService service = window.getPartService();
            service.addPartListener(viewPartListener);
        }

        if ((!EnsembleToolManager.getInstance().isReady())
                && (ensemblesTreeViewer != null)) {
            ensemblesTreeViewer.refresh(false);
            return;
        }

        VizApp.runAsync(new Runnable() {
            @SuppressWarnings("unchecked")
            public void run() {

                if (ensemblesTreeViewer == null)
                    return;

                Map<String, List<GenericResourceHolder>> ensembleResourcesMap = EnsembleToolManager
                        .getInstance().getEnsembleResources();
                Map<String, List<GenericResourceHolder>> previousResourcesMap = null;

                // this method only acts on an instance of a Map ...
                if (ensembleResourcesMap != null) {
                    // only change the tree's input reference (via setInput)
                    // when the ensembleResourcesMap reference is new or has
                    // changed.
                    if ((ensemblesTreeViewer.getInput() == null)
                            || (ensembleResourcesMap != ensemblesTreeViewer
                                    .getInput())) {
                        // unregister previous model's resources
                        if (ensemblesTreeViewer.getInput() != null) {
                            previousResourcesMap = (Map<String, List<GenericResourceHolder>>) ensemblesTreeViewer
                                    .getInput();
                            unregisterResources(previousResourcesMap);
                        }
                        ensemblesTreeViewer.setInput(ensembleResourcesMap);
                        ensemblesTreeViewer.refresh(true);
                        // register new model's resources
                        registerResources(ensembleResourcesMap);
                    } else {
                        ensemblesTreeViewer.refresh(true);
                        registerResources(ensembleResourcesMap);
                    }

                    GenericResourceHolder grh = null;
                    String ensembleRscName = null;
                    if ((ensemblesTreeViewer != null)
                            && (EnsembleToolManager.getInstance().isReady())) {
                        TreeItem[] selectedItems = ensembleTree.getSelection();
                        if ((selectedItems != null)
                                && (selectedItems.length > 0)
                                && (selectedItems[0] != null)) {
                            Object o = selectedItems[0].getData();
                            if (o instanceof GenericResourceHolder) {
                                grh = (GenericResourceHolder) o;
                            } else if (o instanceof String) {
                                ensembleRscName = (String) o;
                            }
                        }

                        updateTimeBasisInfo(EnsembleToolManager.getInstance()
                                .getTimeBasisResourceName(),
                                EnsembleToolManager.getInstance()
                                        .getTimeBasisLegendTime());

                        setTreeExpansion(EnsembleToolManager.getInstance()
                                .getActiveToolLayer().getExpandedElements());

                        if ((selectedItems != null)
                                && (selectedItems.length > 0)
                                && (selectedItems[0] != null)) {
                            if (grh != null) {
                                TreeItem ti = findTreeItemByResource(grh);
                                if (ti != null)
                                    ensembleTree.select(ti);
                            } else if (ensembleRscName != null) {
                                TreeItem ti = findTreeItemByLabelName(ensembleRscName);
                                if (ti != null)
                                    ensembleTree.select(ti);
                            }
                        }
                    }
                    // always refresh the tree ...
                    ensemblesTreeViewer.refresh(true);
                }

            }
        });

    }

    private void registerResources(
            Map<String, List<GenericResourceHolder>> ensembleResourcesMap) {

        List<AbstractVizResource<?, ?>> allResources = getAllResources(ensembleResourcesMap);
        for (AbstractVizResource<?, ?> vr : allResources) {
            vr.registerListener(this);
        }

    }

    private void unregisterResources(
            Map<String, List<GenericResourceHolder>> previousResourcesMap) {

        List<AbstractVizResource<?, ?>> allResources = getAllResources(previousResourcesMap);
        for (AbstractVizResource<?, ?> vr : allResources) {
            vr.unregisterListener(this);
        }
    }

    private List<AbstractVizResource<?, ?>> getAllResources(
            Map<String, List<GenericResourceHolder>> previousResourcesMap) {

        List<AbstractVizResource<?, ?>> allRscs = new ArrayList<AbstractVizResource<?, ?>>();
        List<GenericResourceHolder> currList = null;

        Set<String> keys = previousResourcesMap.keySet();
        for (String k : keys) {
            currList = previousResourcesMap.get(k);
            for (GenericResourceHolder grh : currList) {
                allRscs.add(grh.getRsc());
            }
        }
        return allRscs;
    }

    private void setTreeExpansion(List<String> expandedElements) {
        TreeItem ti = null;
        for (String s : expandedElements) {
            ti = this.findTreeItemByLabelName(s);
            if (ti != null) {
                ti.setExpanded(true);
            }
        }
    }

    private List<String> getTreeExpansion() {

        List<String> expandedItems = new ArrayList<String>();
        TreeItem[] children = ensemblesTreeViewer.getTree().getItems();
        List<TreeItem> immediateChildren = Arrays.asList(children);
        for (TreeItem ti : immediateChildren) {
            if (ti.getData() instanceof String) {
                String s = (String) ti.getData();
                if (ti.getExpanded()) {
                    expandedItems.add(s);
                }
            }
        }
        return expandedItems;
    }

    /*
     * This method is so we can keep track of the expansion state of the tree.
     */
    public void updateExpansionState() {
        EnsembleToolManager.getInstance().getActiveToolLayer()
                .setExpandedElements(getTreeExpansion());
    }

    protected TreeItem calculationItemSelected = null;

    private void startAddERFLayer(String mousedEnsembleName) {

        enableDefaultERFTabWidgetState(mousedEnsembleName);
        tabFolder_lowerSash.setSelection(tabERFLayerControl);
        lowerRangeEntryTextBox_1.forceFocus();
        lowerRangeEntryTextBox_1.insert("");

    }

    /*
     * The lower sash currently contains three tabs: a Info tab, a
     * RelativeFrequency tab, and a Preferences tab.
     */
    private void fillLowerSash(Composite lowerSash) {

        tabFolder_lowerSash = new TabFolder(lowerSash, SWT.NONE);

        fillMetaDataInfoTab(tabFolder_lowerSash);
        fillRelativeFrequency(tabFolder_lowerSash);
        fillPreferencesTab(tabFolder_lowerSash);
        tabFolder_lowerSash.setSelection(tabResourceInfo);
        disableERFTabWidgets();
    }

    /*
     * This method constructs the ui for the the relative frequency tab. This
     * tab allows the user to choose one of four radio button row-based widget
     * selections which allows access to one of four range-filter row-widget
     * selections for the ERF. These are row-based widgets composed of range
     * filters including 1) choosing x within a range 2) choosing x outside of a
     * range 3) choosing x above a threshold and 4) choosing x below a
     * threshold.
     */
    private void fillRelativeFrequency(final TabFolder tabFolder_lowerSash) {

        Composite mainPanel = new Composite(tabFolder_lowerSash, SWT.BORDER);
        GridData gd_rootCalculatorPanel = new GridData(SWT.FILL, SWT.FILL,
                false, true, 1, 1);
        gd_rootCalculatorPanel.widthHint = 220;
        gd_rootCalculatorPanel.heightHint = 240;
        mainPanel.setLayoutData(gd_rootCalculatorPanel);
        mainPanel.setLayout(new GridLayout(5, false));

        tabERFLayerControl = new TabItem(tabFolder_lowerSash, SWT.NONE);
        tabERFLayerControl.setText(" Rel Freq ");
        tabERFLayerControl.setControl(mainPanel);

        Label lbl_dummySpacer_A = new Label(mainPanel, SWT.NONE);
        GridData gd_dummySpacer_A = new GridData(SWT.CENTER, SWT.CENTER, false,
                false, 5, 1);
        gd_dummySpacer_A.heightHint = 5;
        lbl_dummySpacer_A.setLayoutData(gd_dummySpacer_A);

        // this is the title box containing the ensemble name ... it has a
        // a slightly different background to highlight the title. It is also
        // centered.
        label_ensembleProductName = new Label(mainPanel, SWT.BORDER);
        label_ensembleProductName.setFont(SWTResourceManager.getFont("Dialog",
                9, SWT.BOLD));
        label_ensembleProductName.setAlignment(SWT.CENTER);
        label_ensembleProductName.setForeground(SWTResourceManager.getColor(0,
                0, 0));
        label_ensembleProductName
                .setBackground(SWTResourceManager.LIGHT_YELLOW);
        GridData gd_label_frameTimeUsingBasis = new GridData(SWT.CENTER,
                SWT.CENTER, true, false, 5, 1);
        gd_label_frameTimeUsingBasis.heightHint = 23;
        gd_label_frameTimeUsingBasis.widthHint = 256;
        label_ensembleProductName.setLayoutData(gd_label_frameTimeUsingBasis);

        // height spacer
        Label lbl_dummySpacer_B = new Label(mainPanel, SWT.NONE);
        GridData gd_dummySpacer_B = new GridData(SWT.CENTER, SWT.CENTER, false,
                false, 5, 1);
        gd_dummySpacer_B.heightHint = 5;
        lbl_dummySpacer_B.setLayoutData(gd_dummySpacer_B);

        //
        // This is the beginning of the WITHIN A RANGE row-widget
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRoot_1 = new Composite(mainPanel, SWT.BORDER);
        rangeToolRoot_1.setLayout(new GridLayout(7, false));
        rangeToolRoot_1
                .setToolTipText("ERF probability P(x) is within a range (%)");
        GridData gd_rangeToolRoot_1 = new GridData(SWT.LEFT, SWT.CENTER, true,
                false, 5, 1);
        gd_rangeToolRoot_1.heightHint = 38;
        gd_rangeToolRoot_1.widthHint = 260;
        gd_rangeToolRoot_1.verticalIndent = 1;
        gd_rangeToolRoot_1.horizontalIndent = 1;
        rangeToolRoot_1.setLayoutData(gd_rangeToolRoot_1);

        // Are you choosing the WITHIN A RANGE row-widget?
        radioChooserRange_1 = new Button(rangeToolRoot_1, SWT.RADIO);
        radioChooserRange_1.setSelection(true);

        // All the ui components for WITHIN A RANGE will have tool tip hints.

        // Put the "probability of x" label ...
        Label lblProbabilityOfX_1 = new Label(rangeToolRoot_1, SWT.NONE);
        GridData gd_lblProbabilityOfX_1 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_lblProbabilityOfX_1.widthHint = 50;
        lblProbabilityOfX_1.setLayoutData(gd_lblProbabilityOfX_1);
        lblProbabilityOfX_1.setFont(SWTResourceManager.getFont("Serif", 11,
                SWT.BOLD | SWT.ITALIC));
        lblProbabilityOfX_1.setText("P(x):   ");
        lblProbabilityOfX_1
                .setToolTipText("ERF probability P(x) is within a range (%)");

        // There's a lower bound text entry to this WITHIN A RANGE row-widget.
        lowerRangeEntryTextBox_1 = new Text(rangeToolRoot_1, SWT.CENTER
                | SWT.BORDER);
        GridData gd_lowerRangeEntryTextBox_1 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 1, 1);
        gd_lowerRangeEntryTextBox_1.widthHint = 30;
        lowerRangeEntryTextBox_1.setLayoutData(gd_lowerRangeEntryTextBox_1);
        lowerRangeEntryTextBox_1
                .setToolTipText("This must be the minimum value for 'x'");

        Label lblLowerConditional_1 = new Label(rangeToolRoot_1, SWT.NONE);
        GridData gd_lblLowerConditional_1 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 3, 1);
        lblLowerConditional_1.setFont(SWTResourceManager.getFont("Serif", 12,
                SWT.BOLD));
        gd_lblLowerConditional_1.heightHint = 22;
        gd_lblLowerConditional_1.widthHint = 65;
        lblLowerConditional_1.setLayoutData(gd_lblLowerConditional_1);
        lblLowerConditional_1.setText("   <  x  <");
        lblLowerConditional_1
                .setToolTipText("ERF probability P(x) is within a range (%)");

        // There's an upper bound text entry to this WITHIN A RANGE row-widget.
        upperRangeEntryTextBox_1 = new Text(rangeToolRoot_1, SWT.CENTER
                | SWT.BORDER);
        GridData gd_upperRangeEntryTextBox_1 = new GridData(SWT.LEFT,
                SWT.CENTER, false, false, 1, 1);
        gd_upperRangeEntryTextBox_1.widthHint = 30;
        upperRangeEntryTextBox_1.setLayoutData(gd_upperRangeEntryTextBox_1);
        upperRangeEntryTextBox_1
                .setToolTipText("This must be the maximum value for 'x'");

        // This is the beginning of the OUTSIDE A RANGE widget row.
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRoot_2 = new Composite(mainPanel, SWT.BORDER);
        GridData gd_rangeToolRoot_2 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 5, 1);
        gd_rangeToolRoot_2.widthHint = 262;
        rangeToolRoot_2.setLayoutData(gd_rangeToolRoot_2);
        rangeToolRoot_2.setLayout(new GridLayout(7, false));
        rangeToolRoot_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // Are you choosing the OUTSIDE A RANGE row-widget?
        radioChooserRange_2 = new Button(rangeToolRoot_2, SWT.RADIO);
        radioChooserRange_2.setSelection(false);

        // All the ui components for OUTSIDE A RANGE will have tool tip hints.

        // Put the "probability of x" label ...
        Label lblProbabilityOfX_2 = new Label(rangeToolRoot_2, SWT.NONE);
        GridData gd_lblProbabilityOfX_2 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_lblProbabilityOfX_2.widthHint = 50;
        lblProbabilityOfX_2.setLayoutData(gd_lblProbabilityOfX_2);
        lblProbabilityOfX_2.setText("P(x):");
        lblProbabilityOfX_2.setFont(SWTResourceManager.getFont("Serif", 11,
                SWT.BOLD | SWT.ITALIC));
        lblProbabilityOfX_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // There's a lower bound text entry to this OUTSIDE A RANGE row-widget.
        lowerRangeEntryTextBox_2 = new Text(rangeToolRoot_2, SWT.BORDER
                | SWT.CENTER);
        GridData gd_lowerRangeEntryTextBox_2 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 1, 1);
        gd_lowerRangeEntryTextBox_2.widthHint = 30;
        lowerRangeEntryTextBox_2.setLayoutData(gd_lowerRangeEntryTextBox_2);
        lowerRangeEntryTextBox_2
                .setToolTipText("This must be the minimum value for 'x'");

        Label lblLowerConditional_2 = new Label(rangeToolRoot_2, SWT.NONE);
        GridData gd_lblLowerConditional_2 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 3, 1);
        lblLowerConditional_2.setFont(SWTResourceManager.getFont("Serif", 12,
                SWT.BOLD));
        gd_lblLowerConditional_2.heightHint = 22;
        gd_lblLowerConditional_2.widthHint = 65;
        lblLowerConditional_2.setLayoutData(gd_lblLowerConditional_2);
        lblLowerConditional_2.setText("   >  x  >");
        lblLowerConditional_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // There's an upper bound text entry to this OUTSIDE A RANGE row-widget.
        upperRangeEntryTextBox_2 = new Text(rangeToolRoot_2, SWT.BORDER
                | SWT.CENTER);
        GridData gd_upperRangeEntryTextBox_2 = new GridData(SWT.LEFT,
                SWT.CENTER, false, false, 1, 1);
        gd_upperRangeEntryTextBox_2.widthHint = 30;
        upperRangeEntryTextBox_2.setLayoutData(gd_upperRangeEntryTextBox_2);
        upperRangeEntryTextBox_2
                .setToolTipText("This must be the maximum value for 'x'");

        // This is the beginning of the ABOVE A THRESHOLD row-widget.
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRoot_3 = new Composite(mainPanel, SWT.BORDER);
        GridData gd_rangeToolRoot_3 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 5, 1);
        gd_rangeToolRoot_3.widthHint = 262;
        rangeToolRoot_3.setLayoutData(gd_rangeToolRoot_3);
        rangeToolRoot_3.setLayout(new GridLayout(7, false));
        rangeToolRoot_3.setToolTipText("ERF probability P(x) is above (%)");

        // Are you choosing the ABOVE A THRESHOLD widget row?
        radioChooserRange_3 = new Button(rangeToolRoot_3, SWT.RADIO);
        radioChooserRange_3.setSelection(false);

        // All the ui components for ABOVE A THRESHOLD will have tool tip hints.

        // Put the "probability of x" label ...
        Label lblProbabilityOfX_3 = new Label(rangeToolRoot_3, SWT.NONE);
        GridData gd_lblProbabilityOfX_3 = new GridData(SWT.RIGHT, SWT.CENTER,
                false, false, 1, 1);
        gd_lblProbabilityOfX_3.widthHint = 42;
        lblProbabilityOfX_3.setLayoutData(gd_lblProbabilityOfX_3);
        lblProbabilityOfX_3.setText("P(x): ");
        lblProbabilityOfX_3.setFont(SWTResourceManager.getFont("Serif", 11,
                SWT.BOLD | SWT.ITALIC));

        // There's an upper bound text entry to this ABOVE A THRESHOLD row-
        // widget.
        Label lblValueOfX_3 = new Label(rangeToolRoot_3, SWT.NONE);
        lblValueOfX_3.setAlignment(SWT.CENTER);
        GridData gd_lblValueOfX_3 = new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 2, 1);
        gd_lblValueOfX_3.widthHint = 45;
        lblValueOfX_3.setLayoutData(gd_lblValueOfX_3);
        lblValueOfX_3.setFont(SWTResourceManager.getFont("Serif", 12, SWT.BOLD
                | SWT.ITALIC));
        lblValueOfX_3.setText("x   > ");
        lblValueOfX_3.setToolTipText("ERF probability P(x) is above (%)");

        lowerRangeEntryTextBox_3 = new Text(rangeToolRoot_3, SWT.BORDER
                | SWT.CENTER);
        GridData gd_lowerRangeEntryTextBox_3 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 1, 1);
        gd_lowerRangeEntryTextBox_3.widthHint = 30;
        lowerRangeEntryTextBox_3.setLayoutData(gd_lowerRangeEntryTextBox_3);
        lowerRangeEntryTextBox_3
                .setToolTipText("The threshold that 'x' is above");

        new Label(rangeToolRoot_3, SWT.NONE);
        new Label(rangeToolRoot_3, SWT.NONE);

        // This is the beginning of the BELOW A THRESHOLD row-widget.
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRoot_4 = new Composite(mainPanel, SWT.BORDER);
        GridData gd_rangeToolRoot_4 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 5, 1);
        gd_rangeToolRoot_4.widthHint = 262;
        rangeToolRoot_4.setLayoutData(gd_rangeToolRoot_4);
        rangeToolRoot_4.setLayout(new GridLayout(7, false));
        rangeToolRoot_4.setToolTipText("ERF probability P(x) is below (%)");

        // Are you choosing the BELOW A THRESHOLD row-widget?
        radioChooserRange_4 = new Button(rangeToolRoot_4, SWT.RADIO);
        radioChooserRange_4.setSelection(false);

        // All the ui components for BELOW A THRESHOLD will have tool tip hints.

        // Put the "probability of x" label ...
        Label lblProbabilityOfX_4 = new Label(rangeToolRoot_4, SWT.NONE);
        GridData gd_lblProbabilityOfX_4 = new GridData(SWT.RIGHT, SWT.CENTER,
                false, false, 1, 1);
        gd_lblProbabilityOfX_4.widthHint = 42;
        lblProbabilityOfX_4.setLayoutData(gd_lblProbabilityOfX_4);
        lblProbabilityOfX_4.setText("P(x): ");
        lblProbabilityOfX_4.setFont(SWTResourceManager.getFont("Serif", 11,
                SWT.BOLD | SWT.ITALIC));
        lblProbabilityOfX_4.setToolTipText("Probability P(x) is below");

        // There's a lower bound text entry to this BELOW A THRESHOLD row-
        // widget.
        Label lblValueOfX_4 = new Label(rangeToolRoot_4, SWT.NONE);
        lblValueOfX_4.setAlignment(SWT.CENTER);
        GridData gd_lblValueOfX_4 = new GridData(SWT.RIGHT, SWT.CENTER, false,
                false, 2, 1);
        gd_lblValueOfX_4.widthHint = 45;
        lblValueOfX_4.setLayoutData(gd_lblValueOfX_4);
        lblValueOfX_4.setFont(SWTResourceManager.getFont("Serif", 12, SWT.BOLD
                | SWT.ITALIC));
        lblValueOfX_4.setText("x   < ");
        lblValueOfX_4.setToolTipText("ERF probability P(x) is below (%)");

        lowerRangeEntryTextBox_4 = new Text(rangeToolRoot_4, SWT.BORDER
                | SWT.CENTER);
        GridData gd_lowerRangeEntryTextBox_4 = new GridData(SWT.CENTER,
                SWT.CENTER, false, false, 1, 1);
        gd_lowerRangeEntryTextBox_4.widthHint = 30;
        lowerRangeEntryTextBox_4.setLayoutData(gd_lowerRangeEntryTextBox_4);
        lowerRangeEntryTextBox_4
                .setToolTipText("The threshold that 'x' is below");

        new Label(rangeToolRoot_4, SWT.NONE);
        new Label(rangeToolRoot_4, SWT.NONE);

        // Height spacer
        Label lbl_dummySpacer_0 = new Label(mainPanel, SWT.NONE);
        GridData gd_dummySpacer_0 = new GridData(SWT.CENTER, SWT.CENTER, false,
                false, 3, 1);
        gd_dummySpacer_0.heightHint = 5;
        lbl_dummySpacer_0.setLayoutData(gd_dummySpacer_0);

        Label lbl_dummySpacer_1 = new Label(mainPanel, SWT.NONE);
        GridData gd_dummySpacer_1 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1);
        gd_dummySpacer_1.widthHint = 78;
        lbl_dummySpacer_1.setLayoutData(gd_dummySpacer_1);

        // only one range-filter row-widget is enabled at a time
        lowerRangeEntryTextBox_1.setEnabled(true);
        upperRangeEntryTextBox_1.setEnabled(true);

        lowerRangeEntryTextBox_2.setEnabled(false);
        upperRangeEntryTextBox_2.setEnabled(false);

        lowerRangeEntryTextBox_3.setEnabled(false);
        lowerRangeEntryTextBox_4.setEnabled(false);

        // horizontal spacer
        Label lbl_dummySpacer_3 = new Label(mainPanel, SWT.NONE);
        GridData gd_dummySpacer_3 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1);
        gd_dummySpacer_3.widthHint = 75;
        lbl_dummySpacer_3.setLayoutData(gd_dummySpacer_3);

        // Cancel button
        btn_cancelERF = new Button(mainPanel, SWT.PUSH);
        GridData gd_cancelProbability = new GridData(SWT.RIGHT, SWT.CENTER,
                false, false, 1, 1);
        btn_cancelERF.setFont(SWTResourceManager.getFont("Sans", 10, SWT.NONE));
        btn_cancelERF.setBackground(SWTResourceManager.LIGHTER_GRAY);
        gd_cancelProbability.heightHint = 28;
        gd_cancelProbability.widthHint = 58;
        btn_cancelERF.setLayoutData(gd_cancelProbability);
        btn_cancelERF.setText("Cancel");

        btn_cancelERF.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                disableERFTabWidgets();
                if (lastSelectedNonTransientTabItem != null) {
                    tabFolder_lowerSash
                            .setSelection(lastSelectedNonTransientTabItem);
                } else {
                    tabFolder_lowerSash.setSelection(tabResourceInfo);
                }
            }
        });

        // Compute ERF button
        btn_computeERF = new Button(mainPanel, SWT.PUSH);
        GridData gd_computeProbability = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        btn_computeERF
                .setFont(SWTResourceManager.getFont("Sans", 10, SWT.NONE));
        btn_computeERF.setBackground(SWTResourceManager.PALE_DULL_AZURE);
        gd_computeProbability.heightHint = 28;
        gd_computeProbability.widthHint = 120;
        btn_computeERF.setLayoutData(gd_computeProbability);
        btn_computeERF.setText("Compute ERF");

        // compute the ERF based on a range
        btn_computeERF.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                computeERF();
            }
        });

        // select the WITHIN A RANGE row-widget and deselect the others.
        radioChooserRange_1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                radioChooserRange_1.setSelection(true);
                radioChooserRange_2.setSelection(false);
                radioChooserRange_3.setSelection(false);
                radioChooserRange_4.setSelection(false);

                lowerRangeEntryTextBox_1.setText("");
                lowerRangeEntryTextBox_2.setText("");
                lowerRangeEntryTextBox_3.setText("");
                lowerRangeEntryTextBox_4.setText("");
                upperRangeEntryTextBox_1.setText("");
                upperRangeEntryTextBox_2.setText("");

                lowerRangeEntryTextBox_1.setEnabled(true);
                lowerRangeEntryTextBox_2.setEnabled(false);
                lowerRangeEntryTextBox_3.setEnabled(false);
                lowerRangeEntryTextBox_4.setEnabled(false);
                upperRangeEntryTextBox_1.setEnabled(true);
                upperRangeEntryTextBox_2.setEnabled(false);

            }
        });

        // select the OUTSIDE A RANGE row-widget and deselect the others.
        radioChooserRange_2.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                radioChooserRange_1.setSelection(false);
                radioChooserRange_2.setSelection(true);
                radioChooserRange_3.setSelection(false);
                radioChooserRange_4.setSelection(false);

                lowerRangeEntryTextBox_1.setText("");
                lowerRangeEntryTextBox_2.setText("");
                lowerRangeEntryTextBox_3.setText("");
                lowerRangeEntryTextBox_4.setText("");
                upperRangeEntryTextBox_1.setText("");
                upperRangeEntryTextBox_2.setText("");

                lowerRangeEntryTextBox_1.setEnabled(false);
                lowerRangeEntryTextBox_2.setEnabled(true);
                lowerRangeEntryTextBox_3.setEnabled(false);
                lowerRangeEntryTextBox_4.setEnabled(false);
                upperRangeEntryTextBox_1.setEnabled(false);
                upperRangeEntryTextBox_2.setEnabled(true);

            }
        });

        // select the ABOVE A THRESHOLD and deselect the others.
        radioChooserRange_3.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                radioChooserRange_1.setSelection(false);
                radioChooserRange_2.setSelection(false);
                radioChooserRange_3.setSelection(true);
                radioChooserRange_4.setSelection(false);

                lowerRangeEntryTextBox_1.setText("");
                lowerRangeEntryTextBox_2.setText("");
                lowerRangeEntryTextBox_3.setText("");
                lowerRangeEntryTextBox_4.setText("");
                upperRangeEntryTextBox_1.setText("");
                upperRangeEntryTextBox_2.setText("");

                lowerRangeEntryTextBox_1.setEnabled(false);
                lowerRangeEntryTextBox_2.setEnabled(false);
                lowerRangeEntryTextBox_3.setEnabled(true);
                lowerRangeEntryTextBox_4.setEnabled(false);
                upperRangeEntryTextBox_1.setEnabled(false);
                upperRangeEntryTextBox_2.setEnabled(false);

            }
        });

        // select the BELOW A THRESHOLD and deselect the others.
        radioChooserRange_4.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                radioChooserRange_1.setSelection(false);
                radioChooserRange_2.setSelection(false);
                radioChooserRange_3.setSelection(false);
                radioChooserRange_4.setSelection(true);

                lowerRangeEntryTextBox_1.setText("");
                lowerRangeEntryTextBox_2.setText("");
                lowerRangeEntryTextBox_3.setText("");
                lowerRangeEntryTextBox_4.setText("");
                upperRangeEntryTextBox_1.setText("");
                upperRangeEntryTextBox_2.setText("");

                lowerRangeEntryTextBox_1.setEnabled(false);
                lowerRangeEntryTextBox_2.setEnabled(false);
                lowerRangeEntryTextBox_3.setEnabled(false);
                lowerRangeEntryTextBox_4.setEnabled(true);
                upperRangeEntryTextBox_1.setEnabled(false);
                upperRangeEntryTextBox_2.setEnabled(false);

            }
        });

    }

    // extract and validate the values from the chosen ERF range
    protected void computeERF() {

        if (radioChooserRange_1.getSelection()) {
            String lns = lowerRangeEntryTextBox_1.getText();
            double lowerValue = 0;
            try {
                lowerValue = Double.parseDouble(lns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Lower range entry must be a valid real number.");
                return;
            }
            String hns = upperRangeEntryTextBox_1.getText();
            double higherValue = 0;
            try {
                higherValue = Double.parseDouble(hns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Higher range entry must be a valid real number.");
                return;
            }
            if (lowerValue >= higherValue) {
                MessageDialog
                        .openError(owner.getShell(), "Invalid Range",
                                "Lower range entry must be smaller than higher range entry.");
                return;
            } else {
                Range range = new Range(RangeType.INNER_RANGE);
                range.setRange(lowerValue, higherValue);
                EnsembleToolManager.getInstance().calculate(
                        Calculation.ENSEMBLE_RELATIVE_FREQUENCY, range);
            }
        }

        else if (radioChooserRange_2.getSelection()) {
            String lns = lowerRangeEntryTextBox_2.getText();
            double lowerValue = 0;
            try {
                lowerValue = Double.parseDouble(lns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Lower range entry must be a valid real number.");
                return;
            }
            String hns = upperRangeEntryTextBox_2.getText();
            double higherValue = 0;
            try {
                higherValue = Double.parseDouble(hns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Higher range entry must be a valid real number.");
                return;
            }
            if (lowerValue >= higherValue) {
                MessageDialog
                        .openError(owner.getShell(), "Invalid Range",
                                "Lower range entry must be smaller than higher range entry.");
                return;
            } else {
                Range range = new Range(RangeType.OUTER_RANGE);
                range.setRange(lowerValue, higherValue);
                EnsembleToolManager.getInstance().calculate(
                        Calculation.ENSEMBLE_RELATIVE_FREQUENCY, range);
            }
        }

        else if (radioChooserRange_3.getSelection()) {
            String lns = lowerRangeEntryTextBox_3.getText();
            double lowerValue = 0;
            try {
                lowerValue = Double.parseDouble(lns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Entry must be a valid number.");
                return;
            }
            Range range = new Range(RangeType.ABOVE_THRESHOLD);
            range.setThreshold(lowerValue);
            EnsembleToolManager.getInstance().calculate(
                    Calculation.ENSEMBLE_RELATIVE_FREQUENCY, range);
        }

        else if (radioChooserRange_4.getSelection()) {
            String lns = lowerRangeEntryTextBox_3.getText();
            double lowerValue = 0;
            try {
                lowerValue = Double.parseDouble(lns);
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(owner.getShell(), "Invalid Number",
                        "Entry must be a valid number.");
                return;
            }
            Range range = new Range(RangeType.BELOW_THRESHOLD);
            range.setThreshold(lowerValue);
            EnsembleToolManager.getInstance().calculate(
                    Calculation.ENSEMBLE_RELATIVE_FREQUENCY, range);
        }

        if (EnsembleToolManager.getInstance().isReady()) {
            EnsembleToolManager.getInstance().getActiveToolLayer()
                    .transferFocusToEditor();
        }

        disableERFTabWidgets();
        if (lastSelectedNonTransientTabItem != null) {
            tabFolder_lowerSash.setSelection(lastSelectedNonTransientTabItem);
        } else {
            tabFolder_lowerSash.setSelection(tabResourceInfo);
        }
    }

    // disable the ERF widgets
    protected void disableERFTabWidgets() {

        clearProbabilityFields();

        radioChooserRange_1.setEnabled(false);
        radioChooserRange_2.setEnabled(false);
        radioChooserRange_3.setEnabled(false);
        radioChooserRange_4.setEnabled(false);

        lowerRangeEntryTextBox_1.setEnabled(false);
        lowerRangeEntryTextBox_2.setEnabled(false);
        lowerRangeEntryTextBox_3.setEnabled(false);
        lowerRangeEntryTextBox_4.setEnabled(false);
        upperRangeEntryTextBox_1.setEnabled(false);
        upperRangeEntryTextBox_2.setEnabled(false);

        btn_cancelERF.setEnabled(false);
        btn_computeERF.setEnabled(false);
    }

    // enable the ERF widgets default state
    protected void enableDefaultERFTabWidgetState(String rscName) {

        clearProbabilityFields();
        label_ensembleProductName.setText(rscName);

        radioChooserRange_1.setEnabled(true);
        radioChooserRange_2.setEnabled(true);
        radioChooserRange_3.setEnabled(true);
        radioChooserRange_4.setEnabled(true);

        lowerRangeEntryTextBox_1.setEnabled(true);
        lowerRangeEntryTextBox_2.setEnabled(false);
        lowerRangeEntryTextBox_3.setEnabled(false);
        lowerRangeEntryTextBox_4.setEnabled(false);
        upperRangeEntryTextBox_1.setEnabled(true);
        upperRangeEntryTextBox_2.setEnabled(false);

        btn_cancelERF.setEnabled(true);
        btn_computeERF.setEnabled(true);
    }

    // set the ERF tab widgets to enable the row-widget based
    // on the Range filter type. Other row-widgets are then
    // disabled.
    private void setERFFields(Range range, String rscName) {

        clearProbabilityFields();
        label_ensembleProductName.setText(rscName);

        radioChooserRange_1.setEnabled(true);
        radioChooserRange_2.setEnabled(true);
        radioChooserRange_3.setEnabled(true);
        radioChooserRange_4.setEnabled(true);
        btn_cancelERF.setEnabled(true);
        btn_computeERF.setEnabled(true);

        if (range.getRangeType() == RangeType.INNER_RANGE) {
            radioChooserRange_1.setSelection(true);
            radioChooserRange_2.setSelection(false);
            radioChooserRange_3.setSelection(false);
            radioChooserRange_4.setSelection(false);

            String lowerStr = Double.toString(range.getLowerRangeThreshold());
            String upperStr = Double.toString(range.getUpperRangeThreshold());
            lowerRangeEntryTextBox_1.setText(lowerStr);
            upperRangeEntryTextBox_1.setText(upperStr);

            lowerRangeEntryTextBox_1.setEnabled(true);
            lowerRangeEntryTextBox_2.setEnabled(false);
            lowerRangeEntryTextBox_3.setEnabled(false);
            lowerRangeEntryTextBox_4.setEnabled(false);
            upperRangeEntryTextBox_1.setEnabled(true);
            upperRangeEntryTextBox_2.setEnabled(false);

        } else if (range.getRangeType() == RangeType.OUTER_RANGE) {
            radioChooserRange_1.setSelection(false);
            radioChooserRange_2.setSelection(true);
            radioChooserRange_3.setSelection(false);
            radioChooserRange_4.setSelection(false);

            String lowerStr = Double.toString(range.getLowerRangeThreshold());
            String upperStr = Double.toString(range.getUpperRangeThreshold());
            lowerRangeEntryTextBox_2.setText(lowerStr);
            upperRangeEntryTextBox_2.setText(upperStr);

            lowerRangeEntryTextBox_1.setEnabled(false);
            lowerRangeEntryTextBox_2.setEnabled(true);
            lowerRangeEntryTextBox_3.setEnabled(false);
            lowerRangeEntryTextBox_4.setEnabled(false);
            upperRangeEntryTextBox_1.setEnabled(false);
            upperRangeEntryTextBox_2.setEnabled(true);

        } else if (range.getRangeType() == RangeType.ABOVE_THRESHOLD) {
            radioChooserRange_1.setSelection(false);
            radioChooserRange_2.setSelection(false);
            radioChooserRange_3.setSelection(true);
            radioChooserRange_4.setSelection(false);

            String threshold = Double.toString(range.getThreshold());
            lowerRangeEntryTextBox_3.setText(threshold);

            lowerRangeEntryTextBox_1.setEnabled(false);
            lowerRangeEntryTextBox_2.setEnabled(false);
            lowerRangeEntryTextBox_3.setEnabled(true);
            lowerRangeEntryTextBox_4.setEnabled(false);
            upperRangeEntryTextBox_1.setEnabled(false);
            upperRangeEntryTextBox_2.setEnabled(false);

        } else if (range.getRangeType() == RangeType.BELOW_THRESHOLD) {
            radioChooserRange_1.setSelection(false);
            radioChooserRange_2.setSelection(false);
            radioChooserRange_3.setSelection(false);
            radioChooserRange_4.setSelection(true);

            String threshold = Double.toString(range.getThreshold());
            lowerRangeEntryTextBox_4.setText(threshold);

            lowerRangeEntryTextBox_1.setEnabled(false);
            lowerRangeEntryTextBox_2.setEnabled(false);
            lowerRangeEntryTextBox_3.setEnabled(false);
            lowerRangeEntryTextBox_4.setEnabled(true);
            upperRangeEntryTextBox_1.setEnabled(false);
            upperRangeEntryTextBox_2.setEnabled(false);

        }

    }

    // clear all user-entered probability field ranges.
    private void clearProbabilityFields() {
        lowerRangeEntryTextBox_1.setText("");
        lowerRangeEntryTextBox_2.setText("");
        lowerRangeEntryTextBox_3.setText("");
        lowerRangeEntryTextBox_4.setText("");
        upperRangeEntryTextBox_1.setText("");
        upperRangeEntryTextBox_2.setText("");
        label_ensembleProductName.setText("");

    }

    // The preference tab contains the preference widgets including
    // "thicken on selection", how thick a selection should be, and whether to
    // use the resource color or user-defined color for selection.
    private void fillPreferencesTab(TabFolder tabFolder_lowerSash) {

        Composite composite = new Composite(tabFolder_lowerSash, SWT.BORDER);
        composite.setLayout(new GridLayout(10, true));
        GridData gd_composite = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 1, 1);
        gd_composite.heightHint = 215;
        gd_composite.widthHint = 371;
        composite.setLayoutData(gd_composite);
        composite.setBackground(SWTResourceManager.MEDIUM_GRAY);
        tabPreferences = new TabItem(tabFolder_lowerSash, SWT.NONE);
        tabPreferences.setText(" Prefs ");
        tabPreferences.setControl(composite);

        Composite composite_ThickenOnSelection = new Composite(composite,
                SWT.SHADOW_ETCHED_IN);
        composite_ThickenOnSelection.setLayout(new GridLayout(5, false));
        GridData gd_composite_ThickenOnSelection = new GridData(SWT.LEFT,
                SWT.CENTER, false, false, 1, 1);
        gd_composite_ThickenOnSelection.widthHint = 166;
        gd_composite_ThickenOnSelection.heightHint = 130;
        composite_ThickenOnSelection
                .setLayoutData(gd_composite_ThickenOnSelection);

        final Button btnThickenOnSelection = new Button(
                composite_ThickenOnSelection, SWT.CHECK);
        btnThickenOnSelection.setSelection(true);
        thickenOnSelection = true;
        btnThickenOnSelection.setText("Thicken On Selection");
        btnThickenOnSelection.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 4, 1));
        btnThickenOnSelection.setFont(viewFont);

        new Label(composite_ThickenOnSelection, SWT.NONE);

        Label label_1 = new Label(composite_ThickenOnSelection, SWT.SEPARATOR
                | SWT.HORIZONTAL);
        GridData gd_label_1 = new GridData(SWT.LEFT, SWT.CENTER, false, false,
                4, 1);
        gd_label_1.widthHint = 218;
        label_1.setLayoutData(gd_label_1);

        final Button btnUseResourceColor = new Button(
                composite_ThickenOnSelection, SWT.RADIO);
        btnUseResourceColor.setSelection(true);
        btnUseResourceColor.setText("Use Resource Color");
        btnUseResourceColor.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 4, 1));
        btnUseResourceColor.setFont(viewFont);

        new Label(composite_ThickenOnSelection, SWT.NONE);

        final Button btnChooseColor = new Button(composite_ThickenOnSelection,
                SWT.RADIO);
        GridData gd_btnChooseColor = new GridData(SWT.LEFT, SWT.TOP, false,
                false, 3, 1);
        gd_btnChooseColor.widthHint = 115;
        btnChooseColor.setLayoutData(gd_btnChooseColor);
        btnChooseColor.setFont(viewFont);
        btnChooseColor.setText("Choose Color ");
        btnChooseColor.setSelection(false);

        final Label label_ColorChooser = new Label(
                composite_ThickenOnSelection, SWT.BORDER);
        label_ColorChooser.setBackground(thickenOnSelectionColor);
        label_ColorChooser.setFont(SWTResourceManager.getFont("Dialog", 14,
                SWT.NONE));
        label_ColorChooser.setAlignment(SWT.CENTER);
        GridData gd_label = new GridData(SWT.LEFT, SWT.CENTER, false, false, 2,
                1);
        gd_label.widthHint = 25;
        label_ColorChooser.setLayoutData(gd_label);
        label_ColorChooser.setEnabled(false);
        label_ColorChooser.setBackground(SWTResourceManager.LIGHT_GRAY);
        label_ColorChooser.setText("X");

        // new Label(composite_ThickenOnSelection, SWT.NONE);

        final Label labelThicknessChooser = new Label(
                composite_ThickenOnSelection, SWT.BORDER);
        GridData gd_labelThicknessChooser = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        gd_labelThicknessChooser.widthHint = 60;
        labelThicknessChooser.setLayoutData(gd_labelThicknessChooser);
        labelThicknessChooser.setText("Thickness: ");
        labelThicknessChooser.setFont(viewFont);
        labelThicknessChooser.setAlignment(SWT.CENTER);

        final Spinner spinnerThicknessChooser = new Spinner(
                composite_ThickenOnSelection, SWT.BORDER);
        spinnerThicknessChooser.setValues(thickenWidth, 2, 7, 0, 1, 1);
        GridData gd_spinnerThicknessChooser = new GridData(SWT.LEFT,
                SWT.CENTER, false, false, 2, 1);
        gd_spinnerThicknessChooser.widthHint = 28;
        spinnerThicknessChooser.setLayoutData(gd_spinnerThicknessChooser);
        spinnerThicknessChooser.setFont(viewFont);
        spinnerThicknessChooser.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                thickenWidth = ((Spinner) e.getSource()).getSelection();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                thickenWidth = ((Spinner) e.getSource()).getSelection();
            }

        });

        label_ColorChooser.addMouseListener(new MouseAdapter() {

            public void mouseUp(MouseEvent e) {
                ColorDialog cd = new ColorDialog(owner.getShell());
                cd.setRGB(thickenOnSelectionColor.getRGB());
                cd.setText("Choose Selection Color");
                RGB result = cd.open();
                if (result != null) {
                    Color c = SWTResourceManager.getColor(result);
                    thickenOnSelectionColor = c;
                    label_ColorChooser.setBackground(c);
                }
            }

        });

        btnThickenOnSelection.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isResourceColorBeingUsed = btnUseResourceColor
                        .getSelection();
                boolean isChecked = btnThickenOnSelection.getSelection();
                if (isChecked) {
                    btnUseResourceColor.setEnabled(true);
                    btnChooseColor.setEnabled(true);
                    label_ColorChooser.setEnabled(true);
                    if (isResourceColorBeingUsed) {
                        label_ColorChooser
                                .setBackground(SWTResourceManager.LIGHT_GRAY);
                        label_ColorChooser.setText("X");
                    } else {
                        label_ColorChooser
                                .setBackground(thickenOnSelectionColor);
                        label_ColorChooser.setText("");
                    }
                    labelThicknessChooser.setEnabled(true);
                    spinnerThicknessChooser.setEnabled(true);
                    thickenOnSelection = true;
                } else {
                    btnUseResourceColor.setEnabled(false);
                    btnChooseColor.setEnabled(false);
                    label_ColorChooser.setEnabled(false);
                    label_ColorChooser
                            .setBackground(SWTResourceManager.LIGHT_GRAY);
                    label_ColorChooser.setText("X");
                    labelThicknessChooser.setEnabled(false);
                    spinnerThicknessChooser.setEnabled(false);
                    thickenOnSelection = false;
                }
            }
        });

        btnUseResourceColor.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isSelected = ((Button) e.getSource()).getSelection();
                if (isSelected) {
                    label_ColorChooser
                            .setBackground(SWTResourceManager.LIGHT_GRAY);
                    label_ColorChooser.setText("X");
                    label_ColorChooser.setEnabled(false);
                    useResourceColorOnThicken = true;
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {

            }

        });

        btnChooseColor.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isSelected = ((Button) e.getSource()).getSelection();
                if (isSelected) {
                    label_ColorChooser.setBackground(thickenOnSelectionColor);
                    label_ColorChooser.setText("");
                    label_ColorChooser.setEnabled(true);
                    useResourceColorOnThicken = false;
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {

            }

        });

    }

    private void fillMetaDataInfoTab(TabFolder tabFolder_lowerSash) {

        Composite metaDataInfoComposite = new Composite(tabFolder_lowerSash,
                SWT.BORDER);
        GridLayout gl_infoComposite = new GridLayout(3, false);
        gl_infoComposite.horizontalSpacing = 2;
        gl_infoComposite.verticalSpacing = 3;
        metaDataInfoComposite.setLayout(gl_infoComposite);
        GridData gd = new GridData(GridData.FILL_BOTH);
        tabFolder_lowerSash.setLayoutData(gd);

        tabResourceInfo = new TabItem(tabFolder_lowerSash, SWT.NONE);
        tabResourceInfo.setText("  Info  ");
        tabResourceInfo.setControl(metaDataInfoComposite);

        Label lblPrimaryRscTime = new Label(metaDataInfoComposite, SWT.BORDER);
        lblPrimaryRscTime.setFont(SWTResourceManager.getFont("Dialog", 9,
                SWT.NONE));
        lblPrimaryRscTime.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                false, false, 1, 1));
        lblPrimaryRscTime.setText(" Time: ");

        label_frameTimeUsingBasis = new Label(metaDataInfoComposite, SWT.BORDER);
        label_frameTimeUsingBasis.setFont(SWTResourceManager.getFont("Dialog",
                8, SWT.NORMAL));
        label_frameTimeUsingBasis.setAlignment(SWT.CENTER);
        label_frameTimeUsingBasis.setForeground(SWTResourceManager.getColor(0,
                0, 0));
        label_frameTimeUsingBasis
                .setBackground(SWTResourceManager.LIGHT_YELLOW);
        GridData gd_label_frameTimeUsingBasis = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 2, 1);
        gd_label_frameTimeUsingBasis.heightHint = 16;
        gd_label_frameTimeUsingBasis.widthHint = 150;
        label_frameTimeUsingBasis.setLayoutData(gd_label_frameTimeUsingBasis);
        // label_frameTimeUsingBasis.setText(" 20.09 - 187hr Fri 18:00z 21-Mar-14");

        Label lblPrimaryRsc = new Label(metaDataInfoComposite, SWT.BORDER);
        lblPrimaryRsc.setFont(SWTResourceManager.getFont("Dialog", 9,
                SWT.NORMAL));
        lblPrimaryRsc.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 1, 1));
        lblPrimaryRsc.setText(" Basis: ");

        label_TimeMatchResourceLabel = new Label(metaDataInfoComposite,
                SWT.BORDER);
        label_TimeMatchResourceLabel.setFont(SWTResourceManager.getFont(
                "Dialog", 8, SWT.NORMAL));
        label_TimeMatchResourceLabel
                .setBackground(SWTResourceManager.LIGHT_YELLOW);
        label_TimeMatchResourceLabel.setAlignment(SWT.CENTER);
        GridData gd_label_TimeMatchResourceLabel = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 2, 1);
        gd_label_TimeMatchResourceLabel.widthHint = 130;
        label_TimeMatchResourceLabel
                .setLayoutData(gd_label_TimeMatchResourceLabel);
        // label_TimeMatchResourceLabel.setText("GFS Ensemble 500 MB");

        // need to fill some space
        Composite filler = new Composite(metaDataInfoComposite, SWT.NONE);
        filler.setSize(20, 150);
        GridData gd_filler = new GridData(SWT.FILL, SWT.CENTER, true, false, 3,
                1);
        gd_filler.widthHint = 150;
        gd_filler.heightHint = 20;
        filler.setLayoutData(gd_filler);

    }

    protected void updateTimeBasisInfo(String timeBasisRscName, String datatime) {
        if (!label_TimeMatchResourceLabel.isDisposed()) {
            label_TimeMatchResourceLabel.setText(timeBasisRscName);
            label_frameTimeUsingBasis.setText(datatime);
        }
    }

    public void updateLegendTimeInfo() {

        VizApp.runAsync(new Runnable() {
            public void run() {
                updateTimeBasisInfo(EnsembleToolManager.getInstance()
                        .getTimeBasisResourceName(), EnsembleToolManager
                        .getInstance().getTimeBasisLegendTime());
            }
        });

    }

    /*
     * The focus listener transfers focus between the EnsembleToolViewer view
     * and the CAVE application's active editor. However, this focus listener
     * ignores focus events just after the Volume Browser is opened (for a
     * period of IGNORE_FOCUS_PERIOD milliseconds. This was done so the VB
     * didn't lose focus immediately after opening which automatically pushes
     * the dialog to behind the CAVE application.
     */
    private class TransferFocusListener implements MouseTrackListener {

        @Override
        public void mouseEnter(MouseEvent e) {
            if (volumeBrowserJustOpened) {
                long currentTime = System.currentTimeMillis();
                long timeElapsed = currentTime - ignoreFocusStartTime;
                if (timeElapsed > IGNORE_FOCUS_PERIOD_MILLIS) {
                    volumeBrowserJustOpened = false;
                }
            }
            if (!volumeBrowserJustOpened) {
                grabFocus();
            }
        }

        @Override
        public void mouseExit(MouseEvent e) {
            if (EnsembleToolManager.getInstance().isReady()) {
                if (volumeBrowserJustOpened) {
                    long currentTime = System.currentTimeMillis();
                    long timeElapsed = currentTime - ignoreFocusStartTime;
                    if (timeElapsed > IGNORE_FOCUS_PERIOD_MILLIS) {
                        volumeBrowserJustOpened = false;
                    }
                }
                if (!volumeBrowserJustOpened) {
                    EnsembleToolManager.getInstance().getActiveToolLayer()
                            .transferFocusToEditor();
                }
            }
        }

        @Override
        public void mouseHover(MouseEvent e) {
        }

    }

    /*
     * Keep track of the expansion state of the tree.
     */
    private class EnsembleTreeExpandCollapseListener implements
            ITreeViewerListener {

        @Override
        public void treeCollapsed(TreeExpansionEvent event) {

            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    updateExpansionState();
                }
            });
        }

        @Override
        public void treeExpanded(TreeExpansionEvent event) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    updateExpansionState();
                }
            });
        }
    }

    @Override
    public void refresh() {

        if ((ensemblesTreeViewer != null)
                && (EnsembleToolManager.getInstance().isReady())) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    ensemblesTreeViewer.refresh(true);
                }
            });
        }
    }

    private void updateCursor(Cursor c) {
        ensembleTree.setCursor(c);
    }

}
