package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.legend;

import gov.noaa.gsd.viz.ensemble.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedTimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.TimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.ContextMenuManager;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.DistributionViewerComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.GlobalPreferencesComposite;
import gov.noaa.gsd.viz.ensemble.util.ChosenGEFSColors;
import gov.noaa.gsd.viz.ensemble.util.ChosenSREFColors;
import gov.noaa.gsd.viz.ensemble.util.EnsembleGEFSColorChooser;
import gov.noaa.gsd.viz.ensemble.util.EnsembleSREFColorChooser;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
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
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;

/***
 * 
 * This class is a Composite which contains the widget/contents of the Legend
 * browser. It is soley coupled to and a sub-component of the Ensemble Tool main
 * ViewPart (<code>EnsembleToolViewer</code>).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer      Description
 * ------------ ---------- ----------- --------------------------
 * Oct 15, 2015   12565      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class LegendBrowserComposite extends Composite {

    private EnsembleToolViewer mainToolView = null;

    private DistributionViewerComposite distributionViewerComposite = null;

    private SashForm rootSashForm = null;

    private Composite rootComposite = null;

    private CTabFolder ensembleToolTabFolder = null;

    private CTabItem legendsTabItem = null;

    private TreeViewer legendsTreeViewer = null;

    private Tree legendsTree = null;

    private ScrolledComposite legendRootContainerScrolledComposite = null;

    final private ITreeViewerListener expandCollapseListener = new LegendTreeExpandCollapseListener();

    private ERFProductDialog erfDialog = null;

    protected TreeItem foundTreeItem = null;

    protected TreeItem[] directDescendants = null;

    private ArrayList<ColumnLabelProvider> columnLabelProviders = new ArrayList<ColumnLabelProvider>();

    private MenuItem addERFLayerMenuItem = null;

    private TreeViewerColumn column0 = null;

    private TreeViewerColumn column1 = null;

    private final long elegantWaitPeriod = 100;

    private AbstractVizResource<?, ?> currentEnsembleRsc = null;

    private boolean CTRL_KEY_DEPRESSED = false;

    private List<Image> imageCache = null;

    // private FramesInfo currentFramesInfo;

    // If an item in the tree is toggled, need not clear the distribution viewer
    // when refreshing the tree.
    boolean isItemToggled = false;

    public LegendBrowserComposite(Composite parentTabFolder, int style,
            EnsembleToolViewer ownerView, CTabItem itemLegendsTabItem) {
        super(parentTabFolder, style);
        ensembleToolTabFolder = (CTabFolder) parentTabFolder;
        mainToolView = ownerView;
        legendsTabItem = itemLegendsTabItem;
        imageCache = new ArrayList<>();
        createBody();
    }

    private void createBody() {

        /*
         * The legends tab contains an upper and lower section controlled with a
         * sash form. The upper section will be used to contain a main toolbar
         * and main tab folder, which contain the different ET tools (Legend
         * Browse tool, Matrix Navigator tool, etc.) The lower section will
         * contain a tab folder displaying a distribution viewer tool.
         */
        rootComposite = new Composite(ensembleToolTabFolder, SWT.NONE);
        GridData rootComposite_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        rootComposite.setLayoutData(rootComposite_gd);
        GridLayout rootComposite_gl = new GridLayout(1, true);
        rootComposite_gl.marginWidth = 0;
        rootComposite_gl.marginHeight = 0;
        rootComposite.setLayout(rootComposite_gl);

        createSashForm();

        createLegendViewerControl();

        createDistributionViewer();

        /* ratio of size of upper v. lower sash components */
        rootSashForm.setWeights(new int[] { 66, 33 });

    }

    private void createSashForm() {

        rootSashForm = new SashForm(rootComposite, SWT.BORDER);
        rootSashForm.setOrientation(SWT.VERTICAL);

        GridData rootSashForm_gd = new GridData(SWT.FILL, SWT.FILL, true, true,
                1, 1);
        rootSashForm.setLayoutData(rootSashForm_gd);

        GridLayout rootSashForm_gl = new GridLayout(1, true);
        rootSashForm.setLayout(rootSashForm_gl);

        rootSashForm.setBackground(GlobalColor.get(GlobalColor.WHITE));

    }

    private void createLegendViewerControl() {

        /* The upper sash holds the resource "legends" and is scrollable */
        legendRootContainerScrolledComposite = new ScrolledComposite(
                rootSashForm, SWT.NONE | SWT.H_SCROLL | SWT.V_SCROLL);

        /*
         * This is the grid layout for the root composite of the Legends tab.
         */
        GridLayout legendsRootContainerComposite_gl = new GridLayout();
        GridData legendsRootContainerComposite_gd = new GridData(SWT.LEFT,
                SWT.TOP, true, true, 1, 1);
        legendRootContainerScrolledComposite
                .setLayout(legendsRootContainerComposite_gl);
        legendRootContainerScrolledComposite
                .setLayoutData(legendsRootContainerComposite_gd);
        legendRootContainerScrolledComposite.setExpandHorizontal(true);
        legendRootContainerScrolledComposite.setExpandVertical(true);

        /* Legend tab contains a tree of legends */
        legendsTree = new Tree(legendRootContainerScrolledComposite, SWT.BORDER
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION);
        legendsTree.setLinesVisible(true);
        legendsTree.setHeaderVisible(true);
        legendsTreeViewer = new TreeViewer(legendsTree);
        createColumns(legendsTreeViewer);

        /* put the tree into the scrolled composite */
        legendRootContainerScrolledComposite.setContent(legendsTree);

        /* keep track of the collapse/expand state */
        legendsTreeViewer.addTreeListener(expandCollapseListener);

        /* recognize when the user clicks on something in the tree */
        legendsTree.addMouseListener(new LegendTreeMouseListener());

        /* now put the root legend composite into the tab item */
        legendsTabItem.setControl(rootComposite);

        legendsTreeViewer.setContentProvider(new LegendTreeContentProvider());
        legendsTreeViewer.setSorter(new EnsembleTreeSorter());

        legendsTree.addKeyListener(new LegendTreeKeyListener());

    }

    private void createColumns(TreeViewer legendsTableViewer) {

        column0 = new TreeViewerColumn(legendsTableViewer, SWT.LEFT);
        column0.getColumn().setWidth(220);
        column0.getColumn().setMoveable(false);
        column0.getColumn().setText("   Grid Products");
        column0.getColumn().setAlignment(SWT.LEFT);

        LegendNameTreeColumnLabelProvider cnlp = new LegendNameTreeColumnLabelProvider();
        columnLabelProviders.add(cnlp);
        column0.setLabelProvider(cnlp);

        column1 = new TreeViewerColumn(legendsTableViewer, SWT.LEFT);
        column1.getColumn().setWidth(220);
        column1.getColumn().setMoveable(false);
        column1.getColumn().setText("   Time");
        column1.getColumn().setAlignment(SWT.LEFT);

        LegendTimeTreeColumnLabelProvider ctlp = new LegendTimeTreeColumnLabelProvider();
        columnLabelProviders.add(ctlp);
        column1.setLabelProvider(ctlp);

    }

    /*
     * Create the Distribution Viewer.
     */
    private void createDistributionViewer() {

        distributionViewerComposite = new DistributionViewerComposite(
                rootSashForm, SWT.NONE);

        // Initial as an example PDFCDF chart in the Distribution Viewer.
        // It's a test and developing tool for adding new chart tool, and fixing
        // bug. Keep it!
        // distributionViewerComposite.getGhGUI()
        // .updateDisplay(
        // DistributionDisplay.makeTestraphicsHistogramInfo());

        // Clear Distribution Viewer
        distributionViewerComposite.getGhGUI().getDisp()
                .clearDistributionViewer();

    }

    @Override
    public void dispose() {

        for (Image img : imageCache) {
            img.dispose();
        }

        if (columnLabelProviders != null) {
            for (ColumnLabelProvider clp : columnLabelProviders) {
                clp.dispose();
            }
        }
        if (erfDialog != null) {
            erfDialog.close();
        }

        if (isViewerTreeReady()) {

            legendsTree.removeAll();
            legendsTree.dispose();
            legendsTree = null;
        }
        if (legendsTreeViewer != null) {
            legendsTreeViewer = null;
        }
    }

    /*
     * Grab all root items and all descendants from the tree and return true if
     * any entry is "toggled on" meaning that the rsc.isVisible() == true
     */
    protected boolean anyChildrenToggleOn(String productName) {
        boolean anyChildrenToggledOn = false;

        TreeItem parentItem = findTreeItemByLabelName(productName);

        List<TreeItem> descendants = new ArrayList<TreeItem>();
        getAllDescendants(parentItem, descendants);

        /*
         * TODO: this test is due to the fact that for some reason the ensemble
         * children aren't being initially recognized as actual members of the
         * ensemble root TreeItem even though they are really there.
         */
        if (descendants.size() == 0) {
            return true;
        }

        for (TreeItem ti : descendants) {
            Object data = ti.getData();
            if (data == null) {
                anyChildrenToggledOn = true;
                break;
            }
            if (data instanceof AbstractResourceHolder) {
                AbstractResourceHolder gr = (AbstractResourceHolder) data;
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
    protected void matchParentToChildrenVisibility(TreeItem ci) {

        final TreeItem childItem = ci;
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (!isViewerTreeReady()) {
                    return;
                }
                if (childItem == null) {
                    return;
                }
                if (childItem.isDisposed()) {
                    return;
                }
                if (childItem.getParentItem() == null) {
                    return;
                }
                if (childItem.getParentItem().isDisposed()) {
                    return;
                }
                final TreeItem parentItem = childItem.getParentItem();

                final Object d = parentItem.getData();
                if (d instanceof String) {

                    List<TreeItem> descendants = new ArrayList<TreeItem>();
                    getAllDescendants(parentItem, descendants);

                    boolean ai = true;

                    for (TreeItem ti : descendants) {
                        Object data = ti.getData();
                        if (data instanceof AbstractResourceHolder) {
                            AbstractResourceHolder gr = (AbstractResourceHolder) data;
                            AbstractVizResource<?, ?> rsc = gr.getRsc();
                            if (rsc.getProperties().isVisible()) {
                                ai = false;
                                break;
                            }
                        }
                    }

                    final boolean allInvisible = ai;

                    /* if all invisible then make sure the parent is grayed-out. */
                    if (allInvisible) {
                        parentItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                        if (isViewerTreeReady()) {
                            legendsTreeViewer.getTree().deselectAll();
                        }
                    }
                    /*
                     * otherwise, if any one item is visible then make sure the
                     * parent is normalized.
                     */

                    else {
                        parentItem.setForeground(EnsembleToolViewer
                                .getEnabledForegroundColor());
                        if (isViewerTreeReady()) {
                            legendsTreeViewer.getTree().deselectAll();
                        }
                    }
                }
            }
        });
    }

    /*
     * This searches only root level items to see if a root item of a given name
     * has any children (ensemble members) and, if so, returns those children.
     */
    private TreeItem findTreeItemByLabelName(final String name) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                TreeItem[] allRoots = legendsTree.getItems();

                for (TreeItem ti : allRoots) {
                    if (ti.getData() instanceof String) {
                        String treeItemLabelName = (String) ti.getData();
                        if (treeItemLabelName.equals(name)) {
                            foundTreeItem = ti;
                            break;
                        }
                    }
                }
            }
        });
        return foundTreeItem;

    }

    /*
     * Return the first tree item that equals the passed in
     * GenericResourceHolder.
     */
    private TreeItem findTreeItemByResource(AbstractResourceHolder rsc) {

        TreeItem foundItem = null;
        TreeItem[] allRoots = legendsTree.getItems();

        for (TreeItem ti : allRoots) {
            if (foundItem != null)
                break;
            Object tio = ti.getData();
            if ((tio != null) && (tio instanceof AbstractResourceHolder)) {
                AbstractResourceHolder gr = (AbstractResourceHolder) tio;
                if (gr == rsc) {
                    foundItem = ti;
                    break;
                }
            } else if ((tio != null) && (tio instanceof String)) {
                TreeItem[] children = ti.getItems();
                for (TreeItem treeItem : children) {
                    Object o = treeItem.getData();
                    if ((o != null) && (o instanceof AbstractResourceHolder)) {
                        AbstractResourceHolder gr = (AbstractResourceHolder) o;
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

    public void prepareForNewToolInput() {
        VizApp.runAsync(new Runnable() {
            public void run() {
                if (isViewerTreeReady()) {
                    legendsTreeViewer.setInput(EnsembleTool.getInstance()
                            .getEmptyResourceMap());
                }
            }
        });
    }

    synchronized public void setViewEditable(final boolean enabled) {

        if (isViewerTreeReady()) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    if (!enabled) {
                        legendsTreeViewer.getTree().deselectAll();
                    }

                    legendsTree.setEnabled(enabled);
                    distributionViewerComposite.setViewEditable(enabled);

                }
            });
        }

    }

    private void setTreeExpansion(List<String> expandedElements) {
        TreeItem ti = null;
        for (String s : expandedElements) {
            ti = this.findTreeItemByLabelName(s);
            if (ti != null && !ti.isDisposed()) {
                ti.setExpanded(true);
            }
        }
    }

    private List<String> getTreeExpansion() {

        List<String> expandedItems = new ArrayList<String>();

        if (isViewerTreeReady()) {
            TreeItem[] children = legendsTreeViewer.getTree().getItems();
            List<TreeItem> immediateChildren = Arrays.asList(children);
            for (TreeItem ti : immediateChildren) {
                if (ti != null && !ti.isDisposed()) {
                    if (ti.getData() instanceof String) {
                        String s = (String) ti.getData();
                        if (ti.getExpanded()) {
                            expandedItems.add(s);
                        }
                    }
                }
            }
        }
        return expandedItems;
    }

    /*
     * This method is so we can keep track of the expansion state of the tree.
     */
    public void updateExpansionState() {
        EnsembleTool.getInstance().setExpandedElements(getTreeExpansion());
    }

    private void startAddERFLayer(String mousedEnsembleName) {

        erfDialog = new ERFProductDialog(rootComposite.getShell(),
                mousedEnsembleName);
        if (erfDialog.open() == Window.OK) {
            erfDialog.close();
            erfDialog = null;
        }
    }

    public void refreshInput(EnsembleToolLayer tl) {

        if (!isViewerTreeReady()) {
            return;
        }

        final Map<String, List<AbstractResourceHolder>> ensembleResourcesMap = EnsembleTool
                .getInstance().getCurrentToolLayerResources();

        if (ensembleResourcesMap == null) {
            return;
        }

        VizApp.runAsync(new Runnable() {
            public void run() {

                if (!isViewerTreeReady()) {
                    return;
                }

                // this method only acts on an instance of a Map ...
                if (ensembleResourcesMap != null) {

                    legendsTree.clearAll(true);

                    // only change the tree's input reference (via setInput)
                    // when the ensembleResourcesMap reference is different.

                    if ((legendsTreeViewer.getInput() == null)
                            || (ensembleResourcesMap != legendsTreeViewer
                                    .getInput())) {
                        legendsTreeViewer.setInput(ensembleResourcesMap);
                    }
                    legendsTreeViewer.refresh(true);

                    // TODO:A simple solution for this release. Clear the
                    // distribution viewer when refreshing
                    // Should do clean for related change only. implement it
                    // later.
                    // See code in the
                    // EnsembleResourceManager:updateGenerated(AbstractResourceHolder
                    // rh).
                    if (!isItemToggled) {
                        getDistributionViewer().getGhGUI().getDisp()
                                .clearDistributionViewer();
                    }
                    isItemToggled = false;
                    
                    AbstractResourceHolder grh = null;
                    String ensembleRscName = null;
                    if (isViewerTreeReady()) {
                        TreeItem[] selectedItems = legendsTree.getSelection();
                        if ((selectedItems != null)
                                && (selectedItems.length > 0)
                                && (selectedItems[0] != null)) {
                            Object o = selectedItems[0].getData();
                            if (o instanceof AbstractResourceHolder) {
                                grh = (AbstractResourceHolder) o;
                            } else if (o instanceof String) {
                                ensembleRscName = (String) o;
                            }
                        }

                        setTreeExpansion(EnsembleTool.getInstance()
                                .getExpandedElements());

                        if ((selectedItems != null)
                                && (selectedItems.length > 0)
                                && (selectedItems[0] != null)) {
                            if (grh != null) {
                                TreeItem ti = findTreeItemByResource(grh);
                                if (ti != null)
                                    legendsTree.select(ti);
                            } else if (ensembleRscName != null) {
                                TreeItem ti = findTreeItemByLabelName(ensembleRscName);
                                if (ti != null)
                                    legendsTree.select(ti);
                            }
                        }
                    }
                    legendsTreeViewer.refresh(false);
                }
            }
        });
    }

    /*
     * Keep track of the expansion state of the tree.
     */
    private class LegendTreeExpandCollapseListener implements
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

    public void setTabDefaultState() {
        updateCursor(EnsembleToolViewer.normalCursor);
    }

    protected void updateCursor(final Cursor c) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (isViewerTreeReady()) {
                    legendsTree.setCursor(c);
                }
            }

        });
    }

    public boolean isViewerTreeReady() {
        boolean isReady = false;

        if (legendsTreeViewer != null && legendsTree != null
                && legendsTreeViewer.getTree() != null
                && !legendsTreeViewer.getTree().isDisposed()) {
            isReady = true;
        }

        return isReady;
    }

    @Override
    public boolean setFocus() {
        legendsTreeViewer.getControl().setFocus();
        return true;
    }

    /*
     * Given a tree item, find the item in the tree and toggle it's visibility
     * state.
     */
    private void toggleItemVisible(TreeItem item) {

        final TreeItem finalItem = item;

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (finalItem.isDisposed()) {
                    return;
                }
                final Object mousedItem = finalItem.getData();
                if (mousedItem instanceof String) {

                    Color fg = finalItem.getForeground();
                    boolean isVisible = false;

                    /*
                     * TODO: Awkward way of seeing if the item has been already
                     * grayed-out. This needs to be further evaluated for a
                     * better solution.
                     */
                    if (fg.getRGB().equals(
                            EnsembleToolViewer.getEnabledForegroundColor()
                                    .getRGB())) {
                        isVisible = false;
                    } else {
                        isVisible = true;
                    }

                    /* if it was on turn it off */
                    if (isVisible) {
                        finalItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                        if (isViewerTreeReady()) {
                            legendsTreeViewer.getTree().deselectAll();
                        }
                    }
                    /* if it was off turn it on */
                    else {
                        finalItem.setForeground(EnsembleToolViewer
                                .getEnabledForegroundColor());
                        if (isViewerTreeReady()) {
                            legendsTreeViewer.getTree().deselectAll();
                        }
                    }

                } else if (mousedItem instanceof AbstractResourceHolder) {

                    AbstractResourceHolder gr = (AbstractResourceHolder) mousedItem;
                    boolean isVisible = gr.getRsc().getProperties().isVisible();
                    /* toggle visibility */
                    isVisible = !isVisible;
                    gr.getRsc().getProperties().setVisible(isVisible);
                    gr.getRsc().issueRefresh();
                    /* update tree item to reflect new state */
                    if (isVisible) {
                        finalItem.setForeground(EnsembleToolViewer
                                .getEnabledForegroundColor());
                    } else {
                        finalItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                    }
                    if (isViewerTreeReady()) {
                        legendsTreeViewer.getTree().deselectAll();
                    }

                    // Toggle process for sample, text histogram and
                    // distribution viewer.
                    if (gr instanceof HistogramGridResourceHolder) {
                        // Turns off other same mode histogram tools.
                        // Keeps only one work at same time.
                        if (isVisible) {
                            EnsembleResourceManager.getInstance()
                                    .turnOffOtherHistograms(
                                            (HistogramGridResourceHolder) gr);
                        }

                        // Clear the Distribution Viewer
                        if (((HistogramResource<?>) (gr.getRsc())).getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTGRAM
                                && getDistributionViewer() != null) {
                            getDistributionViewer().getGhGUI().getDisp()
                                    .clearDistributionViewer();
                        }
                        // Toggle process for loaded grid resource in Map
                        // Update related generated resource.
                    } else if (!gr.isGenerated()
                            && gr.getRsc() instanceof D2DGridResource
                            && gr.getRsc().getDescriptor() instanceof MapDescriptor) {
                        // Update related generated resource(s).
                        EnsembleResourceManager.getInstance().updateGenerated(
                                gr);
                    }

                    // Set flag to prevent clear the distribution viewer when
                    // refreshing the tree
                    isItemToggled = true;
                }
            }
        });
    }

    protected TreeItem[] getDirectDescendants(TreeItem ri) {

        final TreeItem rootItem = ri;
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                directDescendants = rootItem.getItems();
            }

        });

        return directDescendants;
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
     * Returns all resources (perturbation members) associated with an ensemble
     * resource name.
     */
    protected List<AbstractResourceHolder> getEnsembleMemberGenericResources(
            String ensembleName) {

        TreeItem parentItem = findTreeItemByLabelName(ensembleName);

        List<TreeItem> descendants = new ArrayList<TreeItem>();
        getAllDescendants(parentItem, descendants);

        List<AbstractResourceHolder> childResources = new ArrayList<AbstractResourceHolder>();

        if (descendants.size() > 0) {
            for (TreeItem ti : descendants) {
                Object data = ti.getData();
                if (data == null) {
                    continue;
                }
                if (data instanceof AbstractResourceHolder) {
                    AbstractResourceHolder gr = (AbstractResourceHolder) data;
                    childResources.add(gr);
                }
            }
        }
        return childResources;
    }

    private List<AbstractResourceHolder> perturbationMembers = null;

    public List<AbstractResourceHolder> getPerturbationMembers() {
        return perturbationMembers;
    }

    public void setPerturbationMembers(
            List<AbstractResourceHolder> perturbationMembers) {
        this.perturbationMembers = perturbationMembers;
    }

    /*
     * This method will update color on a given ensemble resource. Underlying
     * methods know how to colorize the contained perturbation members (eg. for
     * SREF, breaks colors into three categories for NMM, NMB and EM).
     * 
     * We need a better way of determining what type of resource we have.
     */
    protected void updateColorsOnEnsembleResource(final String ensembleName) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                setPerturbationMembers(getEnsembleMemberGenericResources(ensembleName));
            }
        });

        // TODO: poor-man's way of knowing what type of flavor this ensemble is
        if ((ensembleName.indexOf("GEFS") >= 0)
                || (ensembleName.indexOf("GFS Ensemble") >= 0)) {
            updateGEFSEnsembleColors(ensembleName, getPerturbationMembers());
        } else if (ensembleName.indexOf("SREF") >= 0) {
            updateSREFColors(ensembleName, getPerturbationMembers());
        }
    }

    private void updateSREFColors(String ensembleName,
            final List<AbstractResourceHolder> children) {

        EnsembleSREFColorChooser cd = new EnsembleSREFColorChooser(
                rootComposite.getShell());
        cd.setBlockOnOpen(true);
        if (cd.open() == Window.OK) {

            cd.close();

            SREFMembersColorChangeJob ccj = new SREFMembersColorChangeJob(
                    "Changing SREF Ensemble Members Colors");
            ccj.setPriority(Job.INTERACTIVE);
            ccj.schedule();

        }

    }

    private void updateGEFSEnsembleColors(String ensembleName,
            final List<AbstractResourceHolder> children) {

        EnsembleGEFSColorChooser cd = new EnsembleGEFSColorChooser(
                rootComposite.getShell());
        cd.setBlockOnOpen(true);
        if (cd.open() == Window.OK) {
            cd.close();

            GEFSMembersColorChangeJob ccj = new GEFSMembersColorChangeJob(
                    "Changing GEFS Ensemble Members Colors");
            ccj.setPriority(Job.INTERACTIVE);
            ccj.schedule();
        }

    }

    /*
     * Called from a class that is listening to the frameChange event.
     */
    public void frameChanged(FramesInfo framesInfo) {

        VizApp.runAsync(new Runnable() {
            public void run() {
                if (isViewerTreeReady()) {
                    legendsTreeViewer.refresh(true);
                }

            }
        });

    }

    private class LegendTreeMouseListener implements MouseListener {

        @Override
        public void mouseDoubleClick(MouseEvent event) {
            // TODO No need for a double click event just yet.
        }

        @Override
        public void mouseDown(MouseEvent event) {

            /**
             * The mouseDown event is what initiates the displaying of a
             * context-sensitive popup menu for either ensemble products or
             * individual grid products ...
             */

            /*
             * if the ensemble tool isn't open then ignore user mouse clicks ...
             */
            if (!EnsembleTool.getInstance().isToolEditable()
                    || !isViewerTreeReady()) {
                return;
            }

            // get the tree item that was clicked on ...
            Point point = new Point(event.x, event.y);

            final TreeItem userClickedTreeItem = legendsTree.getItem(point);
            if (userClickedTreeItem == null || userClickedTreeItem.isDisposed()) {
                return;
            }

            /*
             * is this a mouse-button-3 (e.g. typical RIGHT-CLICK) over a tree
             * item?
             */
            if ((userClickedTreeItem != null) && (event.button == 3)) {
                /*
                 * let's put up a context-sensitive menu similar to cave legend
                 * pop-up menu ...
                 */
                final Object mousedItem = userClickedTreeItem.getData();

                /*
                 * Currently, top level tree items (which also contain child
                 * tree items) are always Ensemble names (unique strings). So if
                 * the user clicks on this item then display the popup menu for
                 * the Ensemble product.
                 */
                if (mousedItem instanceof String) {
                    final Menu legendMenu = new Menu(rootComposite.getShell(),
                            SWT.POP_UP);
                    final String mousedEnsembleName = (String) mousedItem;

                    /*
                     * Relative frequency menu item allows the user to generate
                     * a probability display demonstrating the chance a value
                     * p(x) lies within a range, outside a range, above a
                     * threshold, or below a threshold.
                     */
                    addERFLayerMenuItem = new MenuItem(legendMenu, SWT.PUSH);
                    addERFLayerMenuItem.setText("Relative Frequency");

                    /* only enable the RF menu item if we are in plan view */
                    if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        addERFLayerMenuItem.setEnabled(false);
                    }
                    if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                        addERFLayerMenuItem.setEnabled(true);
                    }

                    addERFLayerMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    updateCursor(EnsembleToolViewer.waitCursor);
                                    startAddERFLayer(mousedEnsembleName);
                                    updateCursor(EnsembleToolViewer.normalCursor);

                                }
                            });

                    /*
                     * This menu item allows the user to choose a color gradient
                     * for either the SREF or GEFS ensemble products.
                     */
                    MenuItem ensembleColorizeMenuItem = new MenuItem(
                            legendMenu, SWT.PUSH);
                    ensembleColorizeMenuItem.setText("Color Gradient");

                    ensembleColorizeMenuItem.addListener(SWT.Selection,
                            new Listener() {
                                public void handleEvent(Event event) {

                                    /*
                                     * TODO: SWT bug prevents getting children
                                     * on a collapsed tree item. Must expand
                                     * first for color gradient change method to
                                     * "see" children.
                                     */
                                    boolean isExpanded = userClickedTreeItem
                                            .getExpanded();
                                    if (!isExpanded) {
                                        userClickedTreeItem.setExpanded(true);
                                    }

                                    updateColorsOnEnsembleResource(mousedEnsembleName);

                                }
                            });

                    /*
                     * this menu item allows the user to remove the ensemble and
                     * all of its members.
                     */
                    MenuItem unloadRscMenuItem = new MenuItem(legendMenu,
                            SWT.PUSH);
                    unloadRscMenuItem.setText("Unload Members");
                    unloadRscMenuItem.addListener(SWT.Selection,
                            new Listener() {
                                public void handleEvent(Event event) {

                                    boolean proceed = MessageDialog.openConfirm(
                                            rootComposite.getShell(),
                                            "Unload Ensemble Members",
                                            "Are you sure you want to unload all members of "
                                                    + mousedEnsembleName + "?");

                                    if (proceed) {

                                        EnsembleTool.getInstance()
                                                .unloadResourcesByName(
                                                        userClickedTreeItem
                                                                .getText());
                                    }

                                }
                            });

                    /*
                     * this menu item allows the user to hide/show an ensemble
                     * product and all of its members.
                     */
                    final MenuItem toggleVisibilityMenuItem = new MenuItem(
                            legendMenu, SWT.CHECK);
                    toggleVisibilityMenuItem.setText("Display Product");
                    toggleVisibilityMenuItem.setEnabled(false);
                    toggleVisibilityMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    ToggleProductVisiblityJob ccj = new ToggleProductVisiblityJob(
                                            "Toggle Product Visibility");
                                    ccj.setPriority(Job.INTERACTIVE);
                                    ccj.setTargetTreeItem(userClickedTreeItem);
                                    ccj.schedule(elegantWaitPeriod);

                                }
                            });

                    legendMenu.setVisible(true);

                } else if (mousedItem instanceof AbstractResourceHolder) {

                    /*
                     * If the tree item the user clicked on was an individual
                     * grid product then show the context-sensitive popup menu
                     * for it ...
                     */
                    final AbstractResourceHolder gr = (AbstractResourceHolder) mousedItem;

                    MenuManager menuMgr = new MenuManager("#PopupMenu");
                    menuMgr.setRemoveAllWhenShown(true);

                    /* The popup menu is generated by the ContextMenuManager */
                    menuMgr.addMenuListener(new IMenuListener() {
                        public void menuAboutToShow(IMenuManager manager) {
                            ResourcePair rp = EnsembleTool.getInstance()
                                    .getResourcePair(gr.getRsc());
                            if (rp != null) {
                                ContextMenuManager.fillContextMenu(manager, rp,
                                        gr.getRsc().getResourceContainer());
                            }
                        }
                    });

                    final Menu legendMenu = menuMgr
                            .createContextMenu(rootComposite);
                    legendMenu.setVisible(true);

                }
                legendsTree.deselect(userClickedTreeItem);
            }
            /*
             * Is this a simple left-click (MB1) over a tree item? Then
             * show/hide. Also, make certain this isn't a Ctrl-MB1 (as that key
             * sequence is the selection feature and is handled in the mouseUp
             * event.
             */
            else if ((userClickedTreeItem != null) && (event.button == 1)
                    && ((event.stateMask & SWT.CTRL) == 0)) {
                /*
                 * By default, left-clicking on a tree item in the tree will
                 * toggle that product's visibility.
                 */

                final Object mousedItem = userClickedTreeItem.getData();

                /* A string means it is an ensemble product name */
                if (mousedItem instanceof String) {

                    String ensembleName = (String) mousedItem;

                    /*
                     * The way that the TreeItem appears to work is that it
                     * returns the correct number of child items when the
                     * TreeItem is expanded and always the wrong number of child
                     * items (usually 1) when the TreeItem is collapsed.
                     * 
                     * Therefore, always expand a collpased tree item before
                     * asking for starting a job that will attempt to get its
                     * child items.
                     * 
                     * TODO: Fix this in the future so we don't force the root
                     * item to be expanded just to toggle the child items
                     * visibility. This place to make this change will not be
                     * here, but instead in the getDirectDescendants method.
                     */

                    boolean isExpanded = userClickedTreeItem.getExpanded();
                    if (!isExpanded) {
                        userClickedTreeItem.setExpanded(true);
                    }
                    ToggleEnsembleVisiblityJob ccj = new ToggleEnsembleVisiblityJob(
                            "Toggle Ensemble Members Visibility");
                    ccj.setPriority(Job.INTERACTIVE);
                    ccj.setTargetEnsembleProduct(ensembleName);
                    ccj.schedule(elegantWaitPeriod);

                } else if (mousedItem instanceof AbstractResourceHolder) {

                    ToggleProductVisiblityJob ccj = new ToggleProductVisiblityJob(
                            "Toggle Product Visibility");
                    ccj.setPriority(Job.INTERACTIVE);
                    ccj.setTargetTreeItem(userClickedTreeItem);
                    ccj.schedule(elegantWaitPeriod);
                }
            }
        }

        @Override
        public void mouseUp(MouseEvent event) {

            Point point = new Point(event.x, event.y);

            if (!EnsembleTool.getInstance().isToolEditable()
                    || !isViewerTreeReady()) {
                return;
            }

            final TreeItem userClickedTreeItem = legendsTree.getItem(point);

            /*
             * The mouse up event currently only acts on items in the tree so if
             * the tree item is null then just return ...
             */
            if (userClickedTreeItem == null || userClickedTreeItem.isDisposed()) {
                return;
            }

            /*
             * keep track of the last highlighted resource and make sure it is
             * still displayed properly ...
             */
            if (EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE != null) {
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE.getCapability(
                        OutlineCapability.class).setOutlineWidth(
                        EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_WIDTH);
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE.getCapability(
                        ColorableCapability.class).setColor(
                        EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_RGB);
                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE
                        .getCapability(OutlineCapability.class)
                        .setOutlineOn(
                                EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED);
            }

            /*
             * Is this a CTRL-MB1 (e.g. CONTROL LEFT-CLICK) over a tree item?
             * ... then this is a UI SWT item selection
             */
            if ((event.button == 1) && ((event.stateMask & SWT.CTRL) != 0)) {

                final Object mousedObject = userClickedTreeItem.getData();

                /* Ctrl-click on a item that is already selected deselects it. */
                if (EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE != null) {

                    if (mousedObject instanceof AbstractResourceHolder) {
                        AbstractResourceHolder grh = (AbstractResourceHolder) mousedObject;
                        if (grh.getRsc() == EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE) {
                            legendsTree.deselect(userClickedTreeItem);
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = null;
                            return;
                        }
                    }
                } else if (isViewerTreeReady()) {

                    /* Ctrl-click on a item that is not selected selects it. */
                    if (mousedObject instanceof AbstractResourceHolder) {
                        AbstractResourceHolder grh = (AbstractResourceHolder) mousedObject;
                        EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = grh
                                .getRsc();
                        legendsTreeViewer.getTree().deselectAll();
                        legendsTreeViewer.getTree().select(userClickedTreeItem);
                    }
                    legendsTreeViewer.getTree().update();

                }

                /*
                 * if the user selects an generated ERF product then update the
                 * ERF tabs ...
                 */
                if (mousedObject instanceof GeneratedGridResourceHolder) {
                    GeneratedGridResourceHolder grh = (GeneratedGridResourceHolder) mousedObject;
                    if (grh.getCalculation() == Calculation.ENSEMBLE_RELATIVE_FREQUENCY) {
                        /**
                         * TODO: Do we want to bring up the ERF dialog with the
                         * existing values to populate?
                         */
                    }
                }

                /* is this an individual grid product? */
                if (mousedObject instanceof GridResourceHolder) {
                    GridResourceHolder grh = (GridResourceHolder) mousedObject;

                    /* only highlight a visible resource */
                    if ((grh.getRsc() != null)
                            && (grh.getRsc().getProperties().isVisible())) {

                        currentEnsembleRsc = grh.getRsc();

                        /*
                         * Then highlight the resource in the primary pane and
                         * keep track of the resource as the most recently
                         * highlighted resource.
                         */
                        if ((currentEnsembleRsc != null)
                                && (GlobalPreferencesComposite
                                        .isThickenOnSelectionPreference())) {
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = currentEnsembleRsc;
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_WIDTH = currentEnsembleRsc
                                    .getCapability(OutlineCapability.class)
                                    .getOutlineWidth();
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_RGB = currentEnsembleRsc
                                    .getCapability(ColorableCapability.class)
                                    .getColor();
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = currentEnsembleRsc
                                    .getCapability(OutlineCapability.class)
                                    .isOutlineOn();
                            currentEnsembleRsc.getCapability(
                                    OutlineCapability.class).setOutlineOn(true);
                            currentEnsembleRsc.getCapability(
                                    OutlineCapability.class).setOutlineWidth(
                                    GlobalPreferencesComposite
                                            .getThickenWidthPreference());
                            if (!GlobalPreferencesComposite
                                    .isUseResourceColorOnThickenPreference()) {
                                currentEnsembleRsc
                                        .getCapability(
                                                ColorableCapability.class)
                                        .setColor(
                                                GlobalPreferencesComposite
                                                        .getThickenOnSelectionColorPreference()
                                                        .getRGB());
                            }
                        }
                    }
                }
                /*
                 * Is this a generated product (e.g. a user-requested
                 * calculation)?
                 */
                if (mousedObject instanceof GeneratedGridResourceHolder) {
                    GeneratedGridResourceHolder grh = (GeneratedGridResourceHolder) mousedObject;

                    /* only highlight a visible resource */
                    if ((grh.getRsc() != null)
                            && (grh.getRsc().getProperties().isVisible())) {

                        currentEnsembleRsc = grh.getRsc();

                        /*
                         * Then highlight the resource in the primary pane and
                         * keep track of the resource as the most recently
                         * highlighted resource.
                         */
                        if ((currentEnsembleRsc != null)
                                && (GlobalPreferencesComposite
                                        .isThickenOnSelectionPreference())) {
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE = currentEnsembleRsc;
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_WIDTH = currentEnsembleRsc
                                    .getCapability(OutlineCapability.class)
                                    .getOutlineWidth();
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_RGB = currentEnsembleRsc
                                    .getCapability(ColorableCapability.class)
                                    .getColor();
                            EnsembleToolViewer.LAST_HIGHLIGHTED_RESOURCE_OUTLINE_ASSERTED = currentEnsembleRsc
                                    .getCapability(OutlineCapability.class)
                                    .isOutlineOn();
                            currentEnsembleRsc.getCapability(
                                    OutlineCapability.class).setOutlineOn(true);
                            currentEnsembleRsc.getCapability(
                                    OutlineCapability.class).setOutlineWidth(
                                    GlobalPreferencesComposite
                                            .getThickenWidthPreference());
                            if (!GlobalPreferencesComposite
                                    .isUseResourceColorOnThickenPreference()) {
                                currentEnsembleRsc
                                        .getCapability(
                                                ColorableCapability.class)
                                        .setColor(
                                                GlobalPreferencesComposite
                                                        .getThickenOnSelectionColorPreference()
                                                        .getRGB());
                            }
                        }
                    }
                }
            }
        }
    }

    /*
     * This job toggles visibility for all members of an ensemble product.
     */
    protected class ToggleEnsembleVisiblityJob extends Job {

        private String ensembleName = null;

        public ToggleEnsembleVisiblityJob(String name) {
            super(name);
        }

        public void setTargetEnsembleProduct(String en) {
            ensembleName = en;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = null;

            if (ensembleName == null) {
                status = Status.CANCEL_STATUS;
            } else {
                updateCursor(EnsembleToolViewer.waitCursor);
                TreeItem ensembleRootItem = findTreeItemByLabelName(ensembleName);

                TreeItem[] descendants = getDirectDescendants(ensembleRootItem);
                TreeItem ti = null;
                int numDescendants = descendants.length;
                for (int i = 0; i < numDescendants; i++) {
                    ti = descendants[i];
                    toggleItemVisible(ti);

                    if (i == numDescendants - 1) {
                        /*
                         * if this was the last item to be toggled off, for the
                         * ensemble group, then you need to toggle the parent
                         * tree item off also
                         */
                        matchParentToChildrenVisibility(ti);
                    }

                }
                updateCursor(EnsembleToolViewer.normalCursor);
                status = Status.OK_STATUS;
            }
            return status;
        }

    }

    /*
     * This is the content provider for the tree.
     */
    private class LegendTreeContentProvider implements ITreeContentProvider {

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // do nothing
        }

        @Override
        @SuppressWarnings("unchecked")
        public Object[] getElements(Object inputElement) {

            // The root elements of the tree are all strings representing
            // the grid product name ... we get the map from the Ensemble-
            // ToolManager. Create a primitive array of Objects having
            // those names.

            Map<String, List<AbstractResourceHolder>> allLoadedProducts = (Map<String, List<AbstractResourceHolder>>) inputElement;
            if ((allLoadedProducts == null) || (allLoadedProducts.size() == 0)) {
                return new Object[0];
            }

            Object[] members = new Object[allLoadedProducts.size()];

            List<AbstractResourceHolder> currResources = null;
            Set<String> loadedProductNames = allLoadedProducts.keySet();
            Iterator<String> iterator = loadedProductNames.iterator();
            String currName = null;
            for (int i = 0; iterator.hasNext(); i++) {
                currName = iterator.next();
                if (currName == null || currName.equals("")) {
                    continue;
                }
                currResources = allLoadedProducts.get(currName);
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

            // The children of a top-level product are currently only
            // perturbation members of an ensemble product. Find all
            // children, which are guaranteed to be of Class type
            // GenericResourceHolder, and return them in a primitive
            // array of Objects.
            Map<String, List<AbstractResourceHolder>> ensembles = EnsembleTool
                    .getInstance().getCurrentToolLayerResources();

            if ((ensembles == null) || (ensembles.size() == 0)
                    || parentElement == null) {
                return new Object[0];
            }

            Object[] members = null;
            if (String.class.isAssignableFrom(parentElement.getClass())) {

                String ensembleName = (String) parentElement;
                if (ensembleName.length() == 0) {
                    return new Object[0];
                }
                List<AbstractResourceHolder> resources = ensembles
                        .get(ensembleName);
                members = new Object[resources.size()];
                int i = 0;
                for (AbstractResourceHolder rsc : resources) {
                    members[i++] = rsc;
                }
            } else if (AbstractResourceHolder.class
                    .isAssignableFrom(parentElement.getClass())) {
                members = new Object[0];
            }
            return members;
        }

        @Override
        public Object getParent(Object element) {

            // Given an item from the tree, return its parent
            // in the tree. Currently, only perturbation members
            // can have parents, so the Object that gets returned
            // is guaranteed to be an ensemble product. This is
            // not critical to the logic that follows but just
            // an FYI.
            String parentEnsembleName = null;
            boolean parentFound = false;
            if (AbstractResourceHolder.class.isAssignableFrom(element
                    .getClass())) {
                AbstractResourceHolder targetRsc = (AbstractResourceHolder) element;
                Map<String, List<AbstractResourceHolder>> ensembles = EnsembleTool
                        .getInstance().getCurrentToolLayerResources();
                Set<Entry<String, List<AbstractResourceHolder>>> entries = ensembles
                        .entrySet();
                Iterator<Entry<String, List<AbstractResourceHolder>>> iterator = entries
                        .iterator();
                Entry<String, List<AbstractResourceHolder>> currChild = null;
                while (iterator.hasNext() && !parentFound) {
                    currChild = iterator.next();
                    List<AbstractResourceHolder> resources = currChild
                            .getValue();
                    for (AbstractResourceHolder gr : resources) {
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

            // Currently, if the given element has children than it is
            // an ensemble product. Get the map of resources from the
            // EnsembleTool and see whether the element
            // given is an ensemble product (by having an associated
            // "not empty" list ...
            boolean hasChildren = false;
            Map<String, List<AbstractResourceHolder>> ensembles = EnsembleTool
                    .getInstance().getCurrentToolLayerResources();
            if (String.class.isAssignableFrom(element.getClass())) {
                String ensembleName = (String) element;
                List<AbstractResourceHolder> resources = ensembles
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
        }
    }

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

    private class LegendTreeKeyListener implements KeyListener {

        @Override
        public void keyPressed(KeyEvent e) {

            if ((e.keyCode & SWT.CTRL) == SWT.CTRL) {
                CTRL_KEY_DEPRESSED = true;
                updateCursor(EnsembleToolViewer.selectionModeCursor);
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {

            if ((e.keyCode & SWT.CTRL) == SWT.CTRL) {
                CTRL_KEY_DEPRESSED = false;
                updateCursor(EnsembleToolViewer.normalCursor);
            }
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
            } else if ((av1 instanceof AbstractResourceHolder)
                    && (av2 instanceof AbstractResourceHolder)) {

                AbstractResourceHolder gr1 = (AbstractResourceHolder) av1;
                AbstractResourceHolder gr2 = (AbstractResourceHolder) av2;

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
                    if (!compareResultFound) {

                        if ((vr1 != null) && (vr1.getName() != null)
                                && (vr2 != null) && (vr2.getName() != null)) {

                            compareResult = vr1.getName().compareTo(
                                    vr2.getName());
                        }
                    }
                }
            }
            /*
             * Finally, check for apples to oranges comparisons.
             */
            else if ((av1 instanceof String)
                    && (av2 instanceof AbstractResourceHolder)) {
                compareResult = 1;
            } else if ((av1 instanceof AbstractResourceHolder)
                    && (av2 instanceof String)) {
                compareResult = -1;
            }

            return compareResult;
        }
    }

    private Font setLegendLabelFont(Object element) {

        return SWTResourceManager.getFont("courier new", 10, SWT.BOLD);

    }

    public void setToolMode(EnsembleTool.EnsembleToolMode mode) {
        if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW) {
            column1.getColumn().setText("   Valid Time");
        } else if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_TIME_SERIES) {
            column1.getColumn().setText("   Cycle Time");
        }
    }

    private class LegendTimeTreeColumnLabelProvider extends ColumnLabelProvider {

        public Font getFont(Object element) {
            return setLegendLabelFont(element);
        }

        public Image getImage(Object element) {
            Image image = null;
            return image;
        }

        public String getText(Object element) {

            String nodeLabel = null;
            if (element instanceof String) {
                nodeLabel = "";
                /**
                 * Find a child perturbation member resource (tree item?) and
                 * get the time to set in this text.
                 */
            } else if (element instanceof GridResourceHolder) {

                GridResourceHolder gr = (GridResourceHolder) element;
                nodeLabel = gr.getDataTime();
            } else if (TimeSeriesResourceHolder.class.isAssignableFrom(element
                    .getClass())) {
                TimeSeriesResourceHolder tsr = (TimeSeriesResourceHolder) element;
                nodeLabel = tsr.getDataTime();
            }
            // update the visibility status cosmetically (i.e. normal text
            // versus graying-out)
            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder gr = (AbstractResourceHolder) element;

                TreeItem treeItem = findTreeItemByResource(gr);
                if (treeItem != null) {
                    if (gr.getRsc().getProperties().isVisible()) {
                        treeItem.setForeground(EnsembleToolViewer
                                .getEnabledForegroundColor());
                    } else {
                        treeItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                    }
                    matchParentToChildrenVisibility(treeItem);
                }
            }
            return nodeLabel;
        }

    }

    /*
     * Here's how we control the items displayed in the tree.
     */
    private class LegendNameTreeColumnLabelProvider extends ColumnLabelProvider {

        public Font getFont(Object element) {
            return setLegendLabelFont(element);
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
                image = new Image(rootComposite.getDisplay(), imageData);
                GC gc = new GC(image);
                gc.setBackground(GlobalColor.get(GlobalColor.WHITE));
                gc.fillRectangle(0, 0, imageWidth, imageHeight);

                // if any ensemble members are visible then the root tree item
                // should be "toggled on" ...
                if (anyChildrenToggleOn(productName)) {
                    gc.setBackground(GlobalColor.get(GlobalColor.BLACK));
                }
                // otherwise, the root tree item should appear "toggled off" ...
                else {
                    gc.setBackground(GlobalColor.get(GlobalColor.GRAY));
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

            } else if (element instanceof AbstractResourceHolder) {

                AbstractResourceHolder gr = (AbstractResourceHolder) element;
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
                image = new Image(rootComposite.getDisplay(), imageData);
                GC gc = new GC(image);
                gc.setBackground(GlobalColor.get(GlobalColor.WHITE));
                gc.fillRectangle(0, 0, imageWidth, imageHeight);
                if (gr.getRsc().getProperties().isVisible()) {

                    // need the following tweaking integers which cosmetic
                    // center things nicely
                    gc.setBackground(GlobalColor.get(GlobalColor.BLACK));

                    // the icon for a visible individual grid resources put the
                    // color of the resource inside a black bordered rectangle.
                    gc.fillRectangle(4, imageHeight - colorHeight - 2,
                            colorWidth, colorHeight);

                    if (element instanceof GeneratedGridResourceHolder) {
                        color = Utilities.brighten(color);
                    }
                    gc.setBackground(SWTResourceManager.getColor(color));
                    gc.fillRectangle(4 + ((colorWidth - innerColorWidth) / 2),
                            (imageHeight - colorHeight)
                                    + ((colorHeight - innerColorHeight) / 2)
                                    - 2, innerColorWidth, innerColorHeight);

                    // then put a nice hyphen
                    gc.setBackground(GlobalColor.get(GlobalColor.BLACK));
                    gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                            bulletUpperLeft_y, bulletSize + 2, bulletSize - 1);
                } else {

                    // need the following tweaking integers which cosmetic
                    // center things nicely
                    gc.setBackground(GlobalColor.get(GlobalColor.GRAY));

                    // the icon for a hidden individual grid resources put the
                    // color of the resource inside a greyed bordered rectangle.
                    gc.fillRectangle(4, imageHeight - colorHeight - 2,
                            colorWidth, colorHeight);
                    gc.setBackground(SWTResourceManager.getColor(Utilities
                            .desaturate(color)));
                    gc.fillRectangle(4 + ((colorWidth - innerColorWidth) / 2),
                            (imageHeight - colorHeight)
                                    + ((colorHeight - innerColorHeight) / 2)
                                    - 2, innerColorWidth, innerColorHeight);

                    gc.setBackground(GlobalColor.get(GlobalColor.GRAY));
                    gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                            bulletUpperLeft_y, bulletSize + 2, bulletSize - 1);
                }
                gc.dispose();

            }
            if (image != null) {
                imageCache.add(image);
            }
            return image;
        }

        public String getText(Object element) {

            String nodeLabel = null;
            if (element instanceof String) {
                nodeLabel = (String) element;
            } else if (GeneratedGridResourceHolder.class
                    .isAssignableFrom(element.getClass())) {
                AbstractResourceHolder gr = (AbstractResourceHolder) element;
                nodeLabel = gr.getSpecificName();
            } else if (element instanceof GridResourceHolder) {

                GridResourceHolder gr = (GridResourceHolder) element;
                if ((gr.getEnsembleId() != null)
                        && (gr.getEnsembleId().length() > 0)) {
                    nodeLabel = gr.getEnsembleId();
                } else {
                    nodeLabel = gr.getSpecificName();
                }
            } else if (TimeSeriesResourceHolder.class.isAssignableFrom(element
                    .getClass())) {

                TimeSeriesResourceHolder tsr = (TimeSeriesResourceHolder) element;
                if ((tsr.getEnsembleId() != null)
                        && (tsr.getEnsembleId().length() > 0)) {
                    nodeLabel = tsr.getEnsembleId();
                } else {
                    if (element instanceof GeneratedTimeSeriesResourceHolder) {
                        nodeLabel = tsr.getSpecificName();
                    } else {
                        nodeLabel = tsr.getGeneralName();
                    }
                }
            } else if (element instanceof HistogramGridResourceHolder) {

                HistogramGridResourceHolder gr = (HistogramGridResourceHolder) element;
                if ((gr.getEnsembleId() != null)
                        && (gr.getEnsembleId().length() > 0)) {
                    nodeLabel = gr.getEnsembleId();
                } else {
                    nodeLabel = gr.getSpecificName();
                }
            }
            // update the visibility status cosmetically (i.e. normal text
            // versus graying-out)
            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder gr = (AbstractResourceHolder) element;

                TreeItem treeItem = findTreeItemByResource(gr);
                if (treeItem != null) {
                    if (gr.getRsc().getProperties().isVisible()) {
                        treeItem.setForeground(EnsembleToolViewer
                                .getEnabledForegroundColor());
                    } else {
                        treeItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                    }
                    matchParentToChildrenVisibility(treeItem);
                }
                // if (nodeLabel != null) {
                // nodeLabel = nodeLabel.trim();
                // if (gr != null && gr.requiresLoadCheck()
                // && !gr.isLoadedAtFrame()) {
                // nodeLabel = nodeLabel.concat(" (Not Loaded)");
                // }
                // }
            }
            return nodeLabel;
        }
    }

    /*
     * Allow user to change SREF member colors based on a chosen color pattern
     * map.
     */
    protected class SREFMembersColorChangeJob extends Job {

        public SREFMembersColorChangeJob(String name) {
            super(name);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = null;

            Color currColor = null;
            for (AbstractResourceHolder gRsc : getPerturbationMembers()) {
                if ((gRsc instanceof GridResourceHolder)
                        || (gRsc instanceof TimeSeriesResourceHolder)) {
                    AbstractVizResource<?, ?> rsc = gRsc.getRsc();
                    String ensId = gRsc.getEnsembleIdRaw();
                    if ((ensId != null) && (ensId.length() > 1)) {
                        currColor = ChosenSREFColors.getInstance()
                                .getGradientByEnsembleId(ensId);
                        rsc.getCapability(ColorableCapability.class).setColor(
                                currColor.getRGB());
                    }
                }
            }
            status = Status.OK_STATUS;

            return status;
        }

    }

    /*
     * Allow user to change GEFS member colors based on a chosen color pattern
     * map.
     */
    protected class GEFSMembersColorChangeJob extends Job {

        public GEFSMembersColorChangeJob(String name) {
            super(name);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = null;

            Color currColor = null;
            int count = 0;

            for (AbstractResourceHolder gRsc : getPerturbationMembers()) {
                if ((gRsc instanceof GridResourceHolder)
                        || (gRsc instanceof TimeSeriesResourceHolder)) {
                    count++;
                    AbstractVizResource<?, ?> rsc = gRsc.getRsc();
                    if (count == 1) {
                        currentEnsembleRsc = rsc;
                    }
                    String ensId = gRsc.getEnsembleIdRaw();
                    if ((ensId != null) && (ensId.length() > 1)) {
                        currColor = ChosenGEFSColors.getInstance()
                                .getGradientByEnsembleId(ensId);
                        rsc.getCapability(ColorableCapability.class).setColor(
                                currColor.getRGB());
                        rsc.getCapability(OutlineCapability.class);
                    }
                }
            }

            status = Status.OK_STATUS;
            return status;
        }

    }

    /*
     * This job toggles visibility for an individual product or ensemble member.
     */
    protected class ToggleProductVisiblityJob extends Job {

        private TreeItem treeItem = null;

        public ToggleProductVisiblityJob(String name) {
            super(name);
        }

        public void setTargetTreeItem(TreeItem ti) {
            treeItem = ti;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = null;

            if (treeItem == null) {
                status = Status.CANCEL_STATUS;
            } else {
                toggleItemVisible(treeItem);

                /*
                 * if this was the last item to be toggled off, for example, in
                 * an ensemble group, then you need to toggle the parent tree
                 * item off also
                 */

                matchParentToChildrenVisibility(treeItem);

                status = Status.OK_STATUS;
            }
            return status;
        }

    }

    public DistributionViewerComposite getDistributionViewer() {
        return distributionViewerComposite;
    }

    public void clearAll() {

        if (isViewerTreeReady()) {
            legendsTreeViewer.getTree().clearAll(true);
            legendsTreeViewer.refresh();
        }

    };

}
