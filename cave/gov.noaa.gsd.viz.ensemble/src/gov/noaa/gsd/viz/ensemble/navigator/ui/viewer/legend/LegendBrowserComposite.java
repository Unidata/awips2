package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.legend;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.EnsembleMembersHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedTimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.TimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.contour.ContourControlDialog;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.ContextMenuManager;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.DistributionViewerComposite;
import gov.noaa.gsd.viz.ensemble.util.ChosenGEFSColors;
import gov.noaa.gsd.viz.ensemble.util.ChosenSREFColors;
import gov.noaa.gsd.viz.ensemble.util.EnsembleGEFSColorChooser;
import gov.noaa.gsd.viz.ensemble.util.EnsembleSREFColorChooser;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

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
 * Dec 14, 2016   19443      polster     added isWidgetReady method
 * Dec 29, 2016   19325      jing        Legend for an image member
 * Feb 17, 2017   19325      jing        Added ERF image capability
 * Mar 01, 2017   19443      polster     Clear all method force clears to empty map
 * Mar 17  2017   19443      jing        Resource group behavior added
 * Mar 31, 2017   19598      jing        Contour control feature
 * Jan 10, 2018   20524      polster     Fixed time series get legend name
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class LegendBrowserComposite extends Composite {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LegendBrowserComposite.class);

    private DistributionViewerComposite distributionViewerComposite = null;

    private SashForm rootSashForm = null;

    private Composite rootComposite = null;

    private CTabFolder ensembleToolTabFolder = null;

    private CTabItem legendsTabItem = null;

    private TreeViewer legendsTreeViewer = null;

    private Tree legendsTree = null;

    private ScrolledComposite legendRootContainerScrolledComposite = null;

    final private ITreeViewerListener expandCollapseListener = new LegendTreeExpandCollapseListener();

    private ContourControlDialog contourDialog = null;

    private ERFProductDialog erfDialog = null;

    protected TreeItem foundTreeItem = null;

    protected TreeItem[] directDescendants = null;

    private ArrayList<ColumnLabelProvider> columnLabelProviders = new ArrayList<ColumnLabelProvider>();

    private LegendNameTreeColumnLabelProvider columnNameLabelProvider = null;

    private LegendTimeTreeColumnLabelProvider columnTimeLabelProvider = null;

    private MenuItem addERFLayerMenuItem = null;

    private MenuItem addERFImageLayerMenuItem = null;

    private MenuItem contourMenuItem = null;

    private TreeViewerColumn column0 = null;

    private TreeViewerColumn column1 = null;

    private final long elegantWaitPeriod = 100;

    private final Font legendTimeFont = SWTResourceManager
            .getFont("courier new", 8, SWT.BOLD);

    private final Font legendNameFont = SWTResourceManager
            .getFont("courier new", 8, SWT.BOLD);

    /*
     * Padding used as tree viewer column headers are not very cosmetically
     * controllable.
     */
    private static final String HEADER_GRID_PRODUCTS = "Products";

    private static final String HEADER_VALID_TIME = "Time";

    private static final String HEADER_CYCLE_TIME = "Cycle Time";

    /*
     * If an item in the tree is toggled, need not clear the distribution viewer
     * when refreshing the tree.
     */
    boolean isItemToggled = false;

    public LegendBrowserComposite(Composite parentTabFolder, int style,
            EnsembleToolViewer ownerView, CTabItem itemLegendsTabItem) {
        super(parentTabFolder, style);

        ensembleToolTabFolder = (CTabFolder) parentTabFolder;
        legendsTabItem = itemLegendsTabItem;
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
        GridData rootComposite_gd = new GridData(SWT.FILL, SWT.FILL, true, true,
                1, 1);
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
        legendsTree = new Tree(legendRootContainerScrolledComposite,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE
                        | SWT.FULL_SELECTION);
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
        legendsTreeViewer.setSorter(new LegendTreeSorter());

        legendsTree.addKeyListener(new LegendTreeKeyListener());

    }

    private void createColumns(TreeViewer legendsTableViewer) {

        column0 = new TreeViewerColumn(legendsTableViewer, SWT.LEFT);
        column0.getColumn().setWidth(265);
        column0.getColumn().setMoveable(false);
        column0.getColumn().setText(HEADER_GRID_PRODUCTS);
        column0.getColumn().setAlignment(SWT.LEFT);

        columnNameLabelProvider = new LegendNameTreeColumnLabelProvider();
        columnLabelProviders.add(columnNameLabelProvider);
        column0.setLabelProvider(columnNameLabelProvider);

        column1 = new TreeViewerColumn(legendsTableViewer, SWT.LEFT);
        column1.getColumn().setWidth(200);
        column1.getColumn().setMoveable(false);
        column1.getColumn().setText(HEADER_VALID_TIME);
        column1.getColumn().setAlignment(SWT.LEFT);

        columnTimeLabelProvider = new LegendTimeTreeColumnLabelProvider();
        columnLabelProviders.add(columnTimeLabelProvider);
        column1.setLabelProvider(columnTimeLabelProvider);

    }

    /*
     * Create the Distribution Viewer.
     */
    private void createDistributionViewer() {

        distributionViewerComposite = new DistributionViewerComposite(
                rootSashForm, SWT.NONE);

        // TODO Initial as an example PDFCDF chart in the Distribution Viewer.
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

        if (columnNameLabelProvider != null) {
            columnNameLabelProvider.dispose();
        }

        if (columnTimeLabelProvider != null) {
            columnTimeLabelProvider.dispose();
        }

        if (columnLabelProviders != null) {
            for (ColumnLabelProvider clp : columnLabelProviders) {
                clp.dispose();
            }
        }
        if (erfDialog != null) {
            erfDialog.close();
        }

        if (isWidgetReady()) {

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

        TreeItem parentItem = findTreeItemByLabelName(productName, true);

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
     * This method wraps the method of the same name which is either called from
     * the main (GUI) thread or not. See method matchParentToChildrenVisibility
     * below this method for a description of what these methods do.
     * 
     */
    protected void matchParentToChildrenVisibility(TreeItem ci,
            boolean useMainThread) {

        final TreeItem childItem = ci;
        if (useMainThread) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    matchParentToChildrenVisibility(childItem);
                }
            });

        } else {
            matchParentToChildrenVisibility(childItem);
        }
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
     * 
     * This call must be called from a method which is running on the main
     * thread.
     */
    protected void matchParentToChildrenVisibility(TreeItem ci) {

        final TreeItem childItem = ci;
        if (!isWidgetReady() || childItem == null || childItem.isDisposed()
                || childItem.getParentItem() == null
                || childItem.getParentItem().isDisposed()) {
            return;
        }

        final TreeItem parentItem = childItem.getParentItem();

        final Object d = parentItem.getData();
        if (d instanceof EnsembleMembersHolder) {

            List<TreeItem> descendants = new ArrayList<TreeItem>();
            getAllDescendants(parentItem, descendants);

            boolean ai = true;

            for (TreeItem ti : descendants) {
                Object data = ti.getData();
                if (data instanceof EnsembleMembersHolder) {
                    continue;
                }
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

            /*
             * if all invisible then make sure the parent is grayed-out.
             */
            if (allInvisible) {
                parentItem.setForeground(
                        EnsembleToolViewer.getDisabledForegroundColor());
                legendsTreeViewer.getTree().deselectAll();
            }
            /*
             * otherwise, if any one item is visible then make sure the parent
             * is normalized.
             */

            else {
                parentItem.setForeground(
                        EnsembleToolViewer.getEnabledForegroundColor());
                legendsTreeViewer.getTree().deselectAll();
            }
        }
    }

    /*
     * This method wraps the method of the same name which is either called from
     * the main (GUI) thread or not. See method findTreeItemByLabelName below
     * this method for a description of what these methods do.
     * 
     */
    private TreeItem findTreeItemByLabelName(final String name,
            final boolean useMainThread) {

        if (isWidgetReady()) {
            /*
             * If we are not already on the ui main thread
             */
            if (useMainThread) {
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {
                        foundTreeItem = findTreeItemByLabelName(name);
                    }
                });
            }
            /*
             * otherwise, we are already on the ui main thread
             */
            else {
                foundTreeItem = findTreeItemByLabelName(name);
            }
        }
        return foundTreeItem;

    }

    /*
     * This searches only root level items to see if a root item of a given name
     * has any children (ensemble members) and, if so, returns that tree item.
     * 
     * This call must be called from a method which is running on the main
     * thread.
     */
    private TreeItem findTreeItemByLabelName(final String name) {

        TreeItem[] allRoots = legendsTree.getItems();

        for (TreeItem ti : allRoots) {
            if (ti.getData() instanceof AbstractResourceHolder) {

                AbstractResourceHolder arh = (AbstractResourceHolder) ti
                        .getData();
                /* Root items have no parents */
                if (arh.getParent() == null) {
                    if (arh instanceof EnsembleMembersHolder) {
                        if (arh.getGroupName().equals(name)) {
                            foundTreeItem = ti;
                            break;
                        }
                    } else if (arh.getRsc().getName().equals(name)) {
                        foundTreeItem = ti;
                        break;
                    }
                }
            }
        }
        return foundTreeItem;

    }

    /*
     * Return the first tree item that equals the passed in
     * AbstractResourceHolder.
     */
    private TreeItem findTreeItemByResource(AbstractResourceHolder rsc) {

        TreeItem foundItem = null;
        TreeItem[] allRoots = legendsTree.getItems();

        for (TreeItem ti : allRoots) {
            if (foundItem != null)
                break;
            Object tio = ti.getData();
            if (tio != null) {
                continue;
            }
            if (tio instanceof EnsembleMembersHolder) {
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
            } else if (tio instanceof AbstractResourceHolder
                    && !(tio instanceof EnsembleMembersHolder)) {
                AbstractResourceHolder gr = (AbstractResourceHolder) tio;
                if (gr == rsc) {
                    foundItem = ti;
                    break;
                }
            }

        }
        return foundItem;
    }

    public void prepareForNewToolInput() {
        VizApp.runAsync(new Runnable() {
            public void run() {
                if (isWidgetReady()) {
                    legendsTreeViewer.setInput(
                            EnsembleTool.getInstance().getEmptyResourceList());
                }
            }
        });
    }

    public void setEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (isWidgetReady()) {
                    if (!enabled) {
                        legendsTreeViewer.getTree().deselectAll();
                    }

                    legendsTree.setEnabled(enabled);
                    distributionViewerComposite.setEditable(enabled);
                }
            }
        });

    }

    private void setTreeExpansion(
            List<EnsembleMembersHolder> expandedElements) {
        TreeItem ti = null;
        String ensName = null;
        for (EnsembleMembersHolder emh : expandedElements) {
            ensName = emh.getGroupName();
            ti = this.findTreeItemByLabelName(ensName);
            if (ti != null && !ti.isDisposed()) {
                ti.setExpanded(true);
            }
        }
    }

    private List<EnsembleMembersHolder> getTreeExpansion() {

        List<EnsembleMembersHolder> expandedItems = new ArrayList<>();

        if (isWidgetReady()) {
            TreeItem[] children = legendsTreeViewer.getTree().getItems();
            List<TreeItem> immediateChildren = Arrays.asList(children);
            for (TreeItem ti : immediateChildren) {
                if (ti != null && !ti.isDisposed()) {
                    if (ti.getData() instanceof EnsembleMembersHolder) {
                        EnsembleMembersHolder emh = (EnsembleMembersHolder) ti
                                .getData();
                        if (ti.getExpanded()) {
                            expandedItems.add(emh);
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

    private void startAddERFLayer(String mousedEnsembleName, boolean isImage) {

        erfDialog = new ERFProductDialog(rootComposite.getShell(),
                mousedEnsembleName, isImage);
        if (erfDialog.open() == Window.OK) {
            erfDialog.close();
            erfDialog = null;
        }
    }

    private void startContourControl(String mousedEnsembleName)
            throws StyleException {
        List<AbstractResourceHolder> rhs = getEnsembleMemberGenericResources(
                mousedEnsembleName);
        if (rhs == null || rhs.isEmpty()
                || !(rhs.get(0).getRsc() instanceof AbstractGridResource)) {
            return;
        }
        List<AbstractGridResource<?>> rscList = new ArrayList<AbstractGridResource<?>>();
        for (AbstractResourceHolder rh : rhs) {
            rscList.add((AbstractGridResource<?>) rh.getRsc());
        }
        contourDialog = new ContourControlDialog(rootComposite.getShell(),
                mousedEnsembleName, rscList);

        /*
         * TODO: This code tests the stylePreferences and label preferences
         * objs. All member resources share one stylePreferences and one label
         * preference object in the baseline code, which is not right. It maybe
         * a fundamental problem for D2D data display. For dealing with the
         * sharing problem, we clone the original StylePreferences object and
         * assign a new object to the grid resources related to the
         * "Contour Control". See the code in the
         * EnsembleResourceManager::registerResource() and registerGenerated().
         * 
         * It requires adding two methods in ufcore:
         * 
         * AbstractGridResource.java, public AbstractStylePreferences
         * getStylePreferences() { return stylePreferences; } public void
         * setStylePreferences(AbstractStylePreferences stylePreferences) {
         * this.stylePreferences = stylePreferences; }
         * 
         * For long term, AWIPS2 team should fix the baseline bug which may
         * impact the contour and other display. Need to keep this code at this
         * location for ET until the bug in base line is fixed.
         * 
         * for (AbstractResourceHolder rh : rhs){ AbstractGridResource rsc =
         * (AbstractGridResource) (rh.getRsc()); LabelingPreferences
         * labelingPreferences = null; AbstractStylePreferences stylePreferences
         * = rsc .getStylePreferences(); if (stylePreferences instanceof
         * ContourPreferences) { labelingPreferences = ((ContourPreferences)
         * stylePreferences) .getContourLabeling(); float incrementOrig =
         * labelingPreferences.getIncrement(); // valuesOrig
         * =labelingPreferences.getValues(); }
         */

        if (contourDialog.open() == Window.OK) {
            contourDialog.close();
            contourDialog = null;
        }
    }

    public void refreshInput(final List<AbstractResourceHolder> rscList) {

        VizApp.runSync(new Runnable() {
            public void run() {

                if (!isWidgetReady()) {
                    return;
                }

                if (rscList == null) {
                    return;
                }

                legendsTree.clearAll(true);

                /*
                 * only change the tree's input reference (via setInput) when
                 * the ensembleResourcesMap reference is different.
                 */

                legendsTreeViewer.setInput(rscList);
                legendsTreeViewer.refresh(true);

                /*
                 * TODO: A simple solution for this release. Clear the
                 * distribution viewer when refreshing Should do clean for
                 * related change only. implement it later. See code in the
                 * EnsembleResourceManager:updateGenerated(
                 * AbstractResourceHolder rh).
                 */
                if (!isItemToggled) {
                    getDistributionViewer().getGhGUI().getDisp()
                            .clearDistributionViewer();
                }
                isItemToggled = false;

                AbstractResourceHolder grh = null;
                TreeItem[] selectedItems = legendsTree.getSelection();
                if ((selectedItems != null) && (selectedItems.length > 0)
                        && (selectedItems[0] != null)) {
                    Object o = selectedItems[0].getData();
                    if (o instanceof AbstractResourceHolder) {
                        grh = (AbstractResourceHolder) o;
                    }
                }

                setTreeExpansion(
                        EnsembleTool.getInstance().getExpandedElements());

                if ((selectedItems != null) && (selectedItems.length > 0)
                        && (selectedItems[0] != null)) {
                    if (grh != null) {
                        TreeItem ti = findTreeItemByResource(grh);
                        if (ti != null)
                            legendsTree.select(ti);
                    }
                }
                legendsTreeViewer.refresh(false);
            }
        });
    }

    /*
     * Keep track of the expansion state of the tree.
     */
    private class LegendTreeExpandCollapseListener
            implements ITreeViewerListener {

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
        updateCursor(EnsembleToolViewer.getNormalCursor());
    }

    protected void updateCursor(final Cursor c) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (isWidgetReady()) {
                    legendsTree.setCursor(c);
                }
            }

        });
    }

    public boolean isWidgetReady() {
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
     * This method wraps the method of the same name which is either called from
     * the main (GUI) thread or not. See method toggleItemVisible below this
     * method for a description of what these methods do.
     * 
     */
    private void toggleItemVisible(final TreeItem item,
            final boolean useMainThread) {

        if (useMainThread) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    toggleItemVisible(item);
                }
            });
        } else {
            toggleItemVisible(item);
        }
    }

    /*
     * Given a tree item, find the item in the tree and toggle it's visibility
     * state.
     * 
     * This call must be called from a method which is running on the main
     * thread.
     */
    private void toggleItemVisible(final TreeItem item) {

        if (item.isDisposed()) {
            return;
        }
        final Object mousedItem = item.getData();
        if (mousedItem instanceof EnsembleMembersHolder) {

            Color fg = item.getForeground();
            boolean isVisible = false;

            /*
             * TODO: Awkward way of seeing if the item has been already
             * grayed-out. This needs to be further evaluated for a better
             * solution.
             */
            if (fg.getRGB().equals(
                    EnsembleToolViewer.getEnabledForegroundColor().getRGB())) {
                isVisible = false;
            } else {
                isVisible = true;
            }

            /* if it was on turn it off */
            if (isVisible) {
                item.setForeground(
                        EnsembleToolViewer.getDisabledForegroundColor());
                if (isWidgetReady()) {
                    legendsTreeViewer.getTree().deselectAll();
                }
            }
            /* if it was off turn it on */
            else {
                item.setForeground(
                        EnsembleToolViewer.getEnabledForegroundColor());
                if (isWidgetReady()) {
                    legendsTreeViewer.getTree().deselectAll();
                }
            }

        } else if (mousedItem instanceof AbstractResourceHolder
                && !(mousedItem instanceof EnsembleMembersHolder)) {

            AbstractResourceHolder gr = (AbstractResourceHolder) mousedItem;
            boolean isVisible = gr.getRsc().getProperties().isVisible();
            /* toggle visibility */
            isVisible = !isVisible;
            gr.getRsc().getProperties().setVisible(isVisible);
            gr.getRsc().issueRefresh();
            if (EnsembleTool.getInstance().getToolLayer() != null) {
                EnsembleTool.getInstance().getToolLayer().issueRefresh();
            }
            /* update tree item to reflect new state */
            if (isVisible) {
                item.setForeground(
                        EnsembleToolViewer.getEnabledForegroundColor());
            } else {
                item.setForeground(
                        EnsembleToolViewer.getDisabledForegroundColor());
            }
            if (isWidgetReady()) {
                legendsTreeViewer.getTree().deselectAll();
            }

            /*
             * Toggle process for sampling, text histogram and distribution
             * viewer.
             */
            if (gr instanceof HistogramGridResourceHolder) {
                /*
                 * Turns off other same mode histogram tools. Keeps only one
                 * working at same time.
                 */
                if (isVisible) {
                    EnsembleTool.getInstance().turnOffOtherHistograms(
                            (HistogramGridResourceHolder) gr);
                }

                /* Clear the Distribution Viewer */
                if (((HistogramResource<?>) (gr.getRsc()))
                        .getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTOGRAM
                        && getDistributionViewer() != null) {
                    getDistributionViewer().getGhGUI().getDisp()
                            .clearDistributionViewer();
                }
                /*
                 * Toggle process for loaded grid resource in Map Update related
                 * generated resource.
                 */
            } else if (!gr.isGenerated()
                    && gr.getRsc() instanceof D2DGridResource
                    && gr.getRsc().getDescriptor() instanceof MapDescriptor) {

                /* Update related generated resource(s). */
                EnsembleTool.getInstance().updateGenerated(gr);
            }

            /*
             * Set flag to prevent clear the distribution viewer when refreshing
             * the tree
             */
            isItemToggled = true;
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
     * Returns all resources (perturbation members) associated with an ensemble
     * resource name.
     */
    protected List<AbstractResourceHolder> getEnsembleMemberGenericResources(
            String ensembleName) {

        TreeItem parentItem = findTreeItemByLabelName(ensembleName, true);

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
                setPerturbationMembers(
                        getEnsembleMemberGenericResources(ensembleName));
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
                if (isWidgetReady()) {
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
                    || !isWidgetReady()) {
                return;
            }

            // get the tree item that was clicked on ...
            Point point = new Point(event.x, event.y);

            final TreeItem userClickedTreeItem = legendsTree.getItem(point);
            if (userClickedTreeItem == null
                    || userClickedTreeItem.isDisposed()) {
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

                if (mousedItem instanceof EnsembleMembersHolder) {

                    EnsembleMembersHolder emh = (EnsembleMembersHolder) mousedItem;

                    String ensMemberName = emh.getGroupName();

                    final Menu legendMenu = new Menu(rootComposite.getShell(),
                            SWT.POP_UP);

                    /*
                     * Relative frequency menu item allows the user to generate
                     * a probability display demonstrating the chance a value
                     * p(x) lies within a range, outside a range, above a
                     * threshold, or below a threshold.
                     */
                    addERFLayerMenuItem = new MenuItem(legendMenu, SWT.PUSH);
                    addERFLayerMenuItem.setText("Relative Frequency");

                    /* only enable the ERF menu item if we are in plan view */
                    EnsembleToolMode mode = EnsembleTool.getInstance()
                            .getToolMode();
                    if (mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        addERFLayerMenuItem.setEnabled(false);
                    } else if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                        addERFLayerMenuItem.setEnabled(true);
                    }

                    addERFLayerMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    startAddERFLayer(ensMemberName, false);

                                }
                            });

                    /*
                     * Contour control for ensemble product.
                     */
                    contourMenuItem = new MenuItem(legendMenu, SWT.PUSH);
                    contourMenuItem.setText("Contour Control");

                    if (mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        contourMenuItem.setEnabled(false);
                    } else if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                        contourMenuItem.setEnabled(true);
                    }

                    contourMenuItem.addListener(SWT.Selection, new Listener() {

                        public void handleEvent(Event event) {

                            try {
                                startContourControl(ensMemberName);
                            } catch (StyleException e) {
                                statusHandler.handle(Priority.WARN,
                                        e.getLocalizedMessage(), e);
                            }

                        }
                    });

                    /*
                     * Relative frequency image menu item allows the user to
                     * generate a probability image display demonstrating the
                     * chance a value p(x) lies within a range, outside a range,
                     * above a threshold, or below a threshold.
                     */
                    addERFImageLayerMenuItem = new MenuItem(legendMenu,
                            SWT.PUSH);
                    addERFImageLayerMenuItem
                            .setText("Relative Frequency Image");

                    /* only enable the ERF menu item if we are in plan view */
                    if (mode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                        addERFImageLayerMenuItem.setEnabled(false);
                    } else if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                        addERFImageLayerMenuItem.setEnabled(true);
                    }

                    addERFImageLayerMenuItem.addListener(SWT.Selection,
                            new Listener() {

                                public void handleEvent(Event event) {

                                    startAddERFLayer(ensMemberName, true);

                                }
                            });
                    /*
                     * This menu item allows the user to choose a color gradient
                     * for either the SREF or GEFS ensemble products.
                     */
                    MenuItem ensembleColorizeMenuItem = new MenuItem(legendMenu,
                            SWT.PUSH);
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

                                    updateColorsOnEnsembleResource(
                                            ensMemberName);

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
                                                    + ensMemberName + "?");

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

                } else if (mousedItem instanceof AbstractResourceHolder
                        && !(mousedItem instanceof EnsembleMembersHolder)) {

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
                            EnsembleToolLayer toolLayer = EnsembleTool
                                    .getToolLayer(gr.getRsc());
                            if (toolLayer == null) {
                                return;
                            }

                            ResourcePair rp = toolLayer
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

                if (mousedItem instanceof EnsembleMembersHolder)

                {

                    EnsembleMembersHolder emh = (EnsembleMembersHolder) mousedItem;
                    String ensembleName = emh.getGroupName();

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

                } else if (mousedItem instanceof AbstractResourceHolder
                        && !(mousedItem instanceof EnsembleMembersHolder))

                {

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
            // TODO No need for a mouse up event just yet.
        }
    }

    /*
     * This job toggles visibility for all members of an ensemble product.
     */
    protected class ToggleEnsembleVisiblityJob extends UIJob {

        private IStatus jobStatusViz = null;

        private String ensembleName = null;

        public ToggleEnsembleVisiblityJob(String name) {
            super(name);
        }

        public void setTargetEnsembleProduct(String en) {
            ensembleName = en;
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            jobStatusViz = Status.CANCEL_STATUS;

            if (ensembleName != null && isWidgetReady()) {
                final TreeItem ensembleRootItem = findTreeItemByLabelName(
                        ensembleName);
                TreeItem[] descendants = ensembleRootItem.getItems();
                TreeItem ti = null;
                int numDescendants = descendants.length;
                for (int i = 0; i < numDescendants; i++) {
                    ti = descendants[i];
                    toggleItemVisible(ti, false);

                    if (i == numDescendants - 1) {
                        /*
                         * if this was the last item to be toggled off, for the
                         * ensemble group, then you need to toggle the parent
                         * tree item off also
                         */
                        matchParentToChildrenVisibility(ti, false);
                    }

                }
                jobStatusViz = Status.OK_STATUS;
            }

            if (jobStatusViz == Status.OK_STATUS) {
                if (EnsembleTool.getInstance().getToolLayer() != null) {
                    EnsembleTool.getInstance().refreshEditor();
                }
            }

            return jobStatusViz;
        }

    }

    /*
     * This is the content provider for the tree. These inner methods are being
     * provided with elements of type AbstractResourceHolder.
     */
    private class LegendTreeContentProvider implements ITreeContentProvider {

        @Override
        public void inputChanged(Viewer viewer, Object oldInput,
                Object newInput) {
            // TODO: ignore for now
        }

        @Override
        public Object[] getElements(Object inputElement) {

            List<AbstractResourceHolder> products = EnsembleTool.getInstance()
                    .getResourceList();
            if (products == null || products.size() == 0) {
                return new Object[0];
            }
            int count = 0;
            for (AbstractResourceHolder arh : products) {
                if (arh.getParent() == null) {
                    count++;
                }
            }
            int index = 0;
            Object[] members = new Object[count];
            for (AbstractResourceHolder arh : products) {
                if (arh.getParent() == null) {
                    members[index++] = arh;
                }
            }
            return members;

        }

        @Override
        public Object[] getChildren(Object parentElement) {

            AbstractResourceHolder currHolder = null;
            Object[] members = null;
            if (parentElement instanceof AbstractResourceHolder) {
                currHolder = (AbstractResourceHolder) parentElement;
                if (currHolder.hasChildren()) {
                    members = currHolder.getChildren();
                } else {
                    members = new Object[0];
                }
            }
            return members;
        }

        @Override
        public Object getParent(Object element) {

            AbstractResourceHolder parent = null;
            if (element == null) {
                return null;
            }
            if (element instanceof AbstractResourceHolder) {

                AbstractResourceHolder targetRsc = (AbstractResourceHolder) element;
                parent = (AbstractResourceHolder) targetRsc.getParent();
            }
            return parent;
        }

        @Override
        public boolean hasChildren(Object element) {

            if (element == null) {
                return false;
            }
            /*
             * if the given element is an ensemble product then assume it has
             * children
             */
            boolean hasChildren = false;
            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder targetRsc = (AbstractResourceHolder) element;
                hasChildren = targetRsc.hasChildren();
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
                updateCursor(EnsembleToolViewer.getSelectionCursor());
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {

            if ((e.keyCode & SWT.CTRL) == SWT.CTRL) {
                updateCursor(EnsembleToolViewer.getNormalCursor());
            }
        }

    }

    /**
     * Product legends to be sorted are either individual products (e.g. NAM20,
     * HRRR, etc) or an ensemble of products (e.g. SREF, GEFS)
     */
    private class LegendTreeSorter extends ViewerSorter {

        public int compare(Viewer v, Object av1, Object av2) {

            int compareResult = 0;

            if ((av1 instanceof AbstractResourceHolder)
                    && (av2 instanceof AbstractResourceHolder)) {

                AbstractResourceHolder gr1 = (AbstractResourceHolder) av1;
                AbstractResourceHolder gr2 = (AbstractResourceHolder) av2;

                /*
                 * Compare one ensemble set group name to a root-level resource
                 */
                if (av1 instanceof EnsembleMembersHolder
                        && !(av2 instanceof EnsembleMembersHolder)) {

                    EnsembleMembersHolder eh1 = (EnsembleMembersHolder) av1;
                    String eh1_group = eh1.getGroupName().trim();
                    if (gr2.getRsc() != null) {
                        compareResult = eh1_group
                                .compareTo(gr2.getRsc().getName());
                    } else {
                        compareResult = 0;
                    }

                }
                /*
                 * Compare one root-level resource to an ensemble set group name
                 */
                else if (!(av1 instanceof EnsembleMembersHolder)
                        && av2 instanceof EnsembleMembersHolder) {

                    EnsembleMembersHolder eh2 = (EnsembleMembersHolder) av2;
                    String eh2_group = eh2.getGroupName().trim();
                    if (gr1.getRsc() != null) {
                        compareResult = eh2_group
                                .compareTo(gr1.getRsc().getName());
                    } else {
                        compareResult = 0;
                    }

                }
                /* Compare two ensemble sets by group name */
                else if (av1 instanceof EnsembleMembersHolder
                        && av2 instanceof EnsembleMembersHolder) {

                    EnsembleMembersHolder eh1 = (EnsembleMembersHolder) av1;
                    EnsembleMembersHolder eh2 = (EnsembleMembersHolder) av2;
                    String eh1_group = eh1.getGroupName().trim();
                    String eh2_group = eh2.getGroupName().trim();
                    compareResult = eh1_group.compareTo(eh2_group);

                }
                /* compare two ensemble members or two root-level resources */
                else if ((av1 instanceof GridResourceHolder)
                        && (av2 instanceof GridResourceHolder)) {

                    AbstractVizResource<?, ?> vr1 = gr1.getRsc();
                    AbstractVizResource<?, ?> vr2 = gr2.getRsc();

                    GridResourceHolder grh1 = (GridResourceHolder) av1;
                    GridResourceHolder grh2 = (GridResourceHolder) av2;

                    // If there are perturbation names then let's compare those
                    // if they are from the same ensemble set
                    if (grh1.getEnsembleId() != null
                            && grh1.getEnsembleId().length() > 0
                            && grh2.getEnsembleId() != null
                            && grh2.getEnsembleId().length() > 0
                            && grh1.getGroupName() != null
                            && grh1.getGroupName().length() > 0
                            && grh2.getGroupName() != null
                            && grh2.getGroupName().length() > 0) {
                        String av1_pert = grh1.getEnsembleId().trim();
                        String av2_pert = grh2.getEnsembleId().trim();
                        String av1_group = grh1.getGroupName().trim();
                        String av2_group = grh2.getGroupName().trim();

                        /*
                         * In order to sort an ensemble resource (i.e.
                         * perturbation or member) make sure the ensemble id and
                         * the group name are valid first. Otherwise just sort
                         * alphabetically by resource name.
                         */
                        if ((av1_pert != null) && (av1_pert.length() > 0)
                                && (av2_pert != null) && (av2_pert.length() > 0)
                                && (av1_group != null && av1_group.length() > 0)
                                && (av2_group != null
                                        && av2_group.length() > 0)) {
                            String av1_pertNumStr = null;
                            String av2_pertNumStr = null;
                            Integer av1_pertNum = null;
                            Integer av2_pertNum = null;

                            if (grh1.getGroupName()
                                    .equals(grh2.getGroupName())) {
                                /*
                                 * If this is a GEFS perturbation/member then
                                 * sort by the numeric portion of the id so the
                                 * members are sorted in ascending order (.i.e.
                                 * p1, p2, p3, ... p20, p21)
                                 */
                                if (grh1.getGroupName().startsWith("GEFS ")) {
                                    if (av1_pert.startsWith("p")
                                            && av2_pert.startsWith("p")) {
                                        av1_pertNumStr = av1_pert.substring(1,
                                                av1_pert.length());
                                        av2_pertNumStr = av2_pert.substring(1,
                                                av2_pert.length());

                                        try {
                                            av1_pertNum = new Integer(
                                                    av1_pertNumStr);
                                            av2_pertNum = new Integer(
                                                    av2_pertNumStr);

                                            compareResult = av1_pertNum
                                                    .compareTo(av2_pertNum);

                                        } catch (NumberFormatException nfe) {

                                            compareResult = av1_pertNumStr
                                                    .compareTo(av2_pertNumStr);

                                        }

                                    }
                                }
                                /*
                                 * Otherwise, this will be an ensemble
                                 * perturbation member that is not part of the
                                 * GEFS. Just sort by resource name.
                                 */
                                else {
                                    String fullName_1 = vr1.getName();
                                    String fullName_2 = vr2.getName();

                                    if ((fullName_1 != null)
                                            && (fullName_2 != null)) {

                                        compareResult = fullName_1
                                                .compareTo(fullName_2);
                                    }
                                }
                            }
                        }
                    }
                    /*
                     * Otherwise, one or the other entries are not ensemble
                     * members (i.e. have no ensemble id) and so are root level
                     * resources so just sort by resource name.
                     */
                    else {
                        String fullName_1 = vr1.getName();
                        String fullName_2 = vr2.getName();

                        if ((fullName_1 != null) && (fullName_2 != null)) {

                            compareResult = fullName_1.compareTo(fullName_2);
                        }
                    }

                } else if (gr1 instanceof TimeSeriesResourceHolder
                        && gr2 instanceof TimeSeriesResourceHolder) {

                    TimeSeriesResourceHolder tsr1 = (TimeSeriesResourceHolder) av1;
                    TimeSeriesResourceHolder tsr2 = (TimeSeriesResourceHolder) av2;

                    // If there are perturbation names then let's compare those
                    // if they are from the same ensemble set
                    String av1_pert = tsr1.getEnsembleId().trim();
                    String av2_pert = tsr2.getEnsembleId().trim();
                    String av1_group = tsr1.getGroupName().trim();
                    String av2_group = tsr2.getGroupName().trim();

                    /*
                     * In order to sort an ensemble resource (i.e. perturbation
                     * or member) make sure the ensemble id and the group name
                     * are valid first. Otherwise just sort alphabetically by
                     * resource name.
                     */
                    if ((av1_pert != null) && (av1_pert.length() > 0)
                            && (av2_pert != null) && (av2_pert.length() > 0)
                            && (av1_group != null && av1_group.length() > 0)
                            && (av2_group != null && av2_group.length() > 0)) {
                        String av1_pertNumStr = null;
                        String av2_pertNumStr = null;
                        Integer av1_pertNum = null;
                        Integer av2_pertNum = null;

                        if (tsr1.getGroupName().equals(tsr2.getGroupName())) {
                            /*
                             * If this is a GEFS perturbation/member then sort
                             * by the numeric portion of the id so the members
                             * are sorted in ascending order (.i.e. p1, p2, p3,
                             * ... p20, p21)
                             */
                            if (tsr1.getGroupName().startsWith("GEFS ")) {
                                if (av1_pert.startsWith("p")
                                        && av2_pert.startsWith("p")) {
                                    av1_pertNumStr = av1_pert.substring(1,
                                            av1_pert.length());
                                    av2_pertNumStr = av2_pert.substring(1,
                                            av2_pert.length());

                                    try {
                                        av1_pertNum = new Integer(
                                                av1_pertNumStr);
                                        av2_pertNum = new Integer(
                                                av2_pertNumStr);

                                        compareResult = av1_pertNum
                                                .compareTo(av2_pertNum);

                                    } catch (NumberFormatException nfe) {

                                        compareResult = av1_pertNumStr
                                                .compareTo(av2_pertNumStr);

                                    }
                                }
                            }
                            /*
                             * Otherwise, this will be an ensemble perturbation
                             * member that is not part of the GEFS. Just sort by
                             * resource name.
                             */
                            else {

                                AbstractVizResource<?, ?> ts1 = gr1.getRsc();
                                AbstractVizResource<?, ?> ts2 = gr2.getRsc();

                                if (ts1 != null && ts1.getName().length() > 0
                                        && ts2 != null
                                        && ts2.getName().length() > 0) {

                                    String fullName_1 = ts1.getName();
                                    String fullName_2 = ts2.getName();

                                    if ((fullName_1 != null)
                                            && (fullName_2 != null)) {
                                        compareResult = fullName_1
                                                .compareTo(fullName_2);
                                    }
                                } else {
                                    compareResult = 0;
                                }
                            }
                        }
                    }
                }
                /*
                 * otherwise, just see if both holders have a valid resource and
                 * compare those
                 */
                else {
                    AbstractVizResource<?, ?> rsc1 = gr1.getRsc();
                    AbstractVizResource<?, ?> rsc2 = gr2.getRsc();
                    if (rsc1 != null && rsc1.getName().length() > 0
                            && rsc2 != null && rsc2.getName().length() > 0) {

                        String fullName_1 = rsc1.getName();
                        String fullName_2 = rsc2.getName();
                        compareResult = fullName_1.compareTo(fullName_2);

                    }
                    /*
                     * if we reach here, just assume the two holders are equal
                     */
                    else {
                        compareResult = 0;
                    }
                }
            }
            return compareResult;
        }

    }

    public void setToolMode(EnsembleTool.EnsembleToolMode mode) {
        if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW) {
            column1.getColumn().setText(HEADER_VALID_TIME);
        } else if (mode == EnsembleTool.EnsembleToolMode.LEGENDS_TIME_SERIES) {
            column1.getColumn().setText(HEADER_CYCLE_TIME);
        }
    }

    private class LegendTimeTreeColumnLabelProvider
            extends ColumnLabelProvider {

        public Font getFont(Object element) {
            return legendTimeFont;
        }

        public Image getImage(Object element) {
            Image image = null;
            return image;
        }

        public String getText(Object element) {

            String nodeLabel = null;
            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder gr = (AbstractResourceHolder) element;
                nodeLabel = gr.getDataTime();

                /*
                 * update the visibility status cosmetically (i.e. normal text
                 * versus graying-out)
                 */
                TreeItem treeItem = findTreeItemByResource(gr);
                if (treeItem != null) {
                    if (gr.getRsc().getProperties().isVisible()) {
                        treeItem.setForeground(
                                EnsembleToolViewer.getEnabledForegroundColor());
                    } else {
                        treeItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                    }
                    matchParentToChildrenVisibility(treeItem, false);
                }
            }
            return nodeLabel;
        }

    }

    /*
     * Here's how we control the items displayed in the tree.
     */
    private class LegendNameTreeColumnLabelProvider
            extends ColumnLabelProvider {

        public Font getFont(Object element) {
            return legendNameFont;
        }

        public Image getImage(Object element) {
            Image image = null;

            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder rscHolder = (AbstractResourceHolder) element;

                /* Is this an ensemble member? */
                if (rscHolder instanceof EnsembleMembersHolder) {
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

                    // if any ensemble members are visible then the root tree
                    // item
                    // should be "toggled on" ...
                    if (anyChildrenToggleOn(rscHolder.getGroupName())) {
                        gc.setBackground(GlobalColor.get(GlobalColor.BLACK));
                    }
                    // otherwise, the root tree item should appear "toggled off"
                    // ...
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
                } else {
                    RGB color = rscHolder.getRsc()
                            .getCapability(ColorableCapability.class)
                            .getColor();

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
                    if (rscHolder.getRsc().getProperties().isVisible()) {

                        /*
                         * Need the following tweaking integers which cosmetic
                         * center things nicely
                         */
                        gc.setBackground(GlobalColor.get(GlobalColor.BLACK));

                        /*
                         * The icon for a visible individual grid resources put
                         * the color of the resource inside a black bordered
                         * rectangle.
                         */
                        gc.fillRectangle(4, imageHeight - colorHeight - 2,
                                colorWidth, colorHeight);

                        if (element instanceof GeneratedGridResourceHolder) {
                            color = Utilities.brighten(color);
                        }
                        gc.setBackground(SWTResourceManager.getColor(color));
                        gc.fillRectangle(
                                4 + ((colorWidth - innerColorWidth) / 2),
                                (imageHeight - colorHeight)
                                        + ((colorHeight - innerColorHeight) / 2)
                                        - 2,
                                innerColorWidth, innerColorHeight);

                        /* then put a nice hyphen */
                        gc.setBackground(GlobalColor.get(GlobalColor.BLACK));
                        gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                                bulletUpperLeft_y, bulletSize + 2,
                                bulletSize - 1);
                    } else {

                        /*
                         * Need the following tweaking integers which cosmetic
                         * center things nicely
                         */
                        gc.setBackground(GlobalColor.get(GlobalColor.GRAY));

                        /*
                         * The icon for a hidden individual grid resources put
                         * the color of the resource inside a greyed bordered
                         * rectangle.
                         */
                        gc.fillRectangle(4, imageHeight - colorHeight - 2,
                                colorWidth, colorHeight);
                        gc.setBackground(SWTResourceManager
                                .getColor(Utilities.desaturate(color)));
                        gc.fillRectangle(
                                4 + ((colorWidth - innerColorWidth) / 2),
                                (imageHeight - colorHeight)
                                        + ((colorHeight - innerColorHeight) / 2)
                                        - 2,
                                innerColorWidth, innerColorHeight);

                        gc.setBackground(GlobalColor.get(GlobalColor.GRAY));
                        gc.fillRectangle(colorWidth + bulletUpperLeftMargin_x,
                                bulletUpperLeft_y, bulletSize + 2,
                                bulletSize - 1);
                    }
                    gc.dispose();
                }
            }
            return image;
        }

        public String getText(Object element) {

            String nodeLabel = null;

            if (element instanceof AbstractResourceHolder) {
                AbstractResourceHolder gr = (AbstractResourceHolder) element;

                if (element instanceof EnsembleMembersHolder) {
                    nodeLabel = gr.getGroupName();
                } else if (element instanceof GeneratedGridResourceHolder) {
                    nodeLabel = gr.getSpecificName();
                } else if (element instanceof GridResourceHolder) {
                    if ((gr.getEnsembleId() != null)
                            && (gr.getEnsembleId().length() > 0)
                            && ((AbstractGridResource<?>) (gr.getRsc()))
                                    .getDisplayType() != DisplayType.IMAGE) {
                        nodeLabel = gr.getEnsembleId();
                    } else {
                        nodeLabel = gr.getSpecificName();
                    }
                } else if (element instanceof TimeSeriesResourceHolder
                        && !(element instanceof GeneratedTimeSeriesResourceHolder)) {
                    TimeSeriesResourceHolder tsr = (TimeSeriesResourceHolder) element;
                    if ((tsr.getEnsembleId() != null)
                            && (tsr.getEnsembleId().length() > 0)) {
                        nodeLabel = tsr.getEnsembleId();
                    } else {
                        nodeLabel = tsr.getSpecificName();
                    }
                } else if (element instanceof GeneratedTimeSeriesResourceHolder) {
                    nodeLabel = gr.getSpecificName();
                } else if (element instanceof HistogramGridResourceHolder) {
                    HistogramGridResourceHolder hr = (HistogramGridResourceHolder) element;
                    if ((hr.getEnsembleId() != null)
                            && (hr.getEnsembleId().length() > 0)) {
                        nodeLabel = hr.getEnsembleId();
                    } else {
                        nodeLabel = hr.getSpecificName();
                    }
                }

                /*
                 * Update the visibility status cosmetically (i.e. normal text
                 * versus graying-out)
                 */

                TreeItem treeItem = findTreeItemByResource(gr);
                if (treeItem != null) {
                    if (gr.getRsc().getProperties().isVisible()) {
                        treeItem.setForeground(
                                EnsembleToolViewer.getEnabledForegroundColor());
                    } else {
                        treeItem.setForeground(EnsembleToolViewer
                                .getDisabledForegroundColor());
                    }
                    matchParentToChildrenVisibility(treeItem, false);
                }
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
                        rsc.getCapability(ColorableCapability.class)
                                .setColor(currColor.getRGB());
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

            for (AbstractResourceHolder gRsc : getPerturbationMembers()) {
                if ((gRsc instanceof GridResourceHolder)
                        || (gRsc instanceof TimeSeriesResourceHolder)) {
                    AbstractVizResource<?, ?> rsc = gRsc.getRsc();
                    String ensId = gRsc.getEnsembleIdRaw();
                    if ((ensId != null) && (ensId.length() > 1)) {
                        currColor = ChosenGEFSColors.getInstance()
                                .getGradientByEnsembleId(ensId);
                        rsc.getCapability(ColorableCapability.class)
                                .setColor(currColor.getRGB());
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
                toggleItemVisible(treeItem, true);

                /*
                 * if this was the last item to be toggled off, for example, in
                 * an ensemble group, then you need to toggle the parent tree
                 * item off also
                 */

                matchParentToChildrenVisibility(treeItem, true);

                status = Status.OK_STATUS;
            }
            return status;
        }

    }

    public DistributionViewerComposite getDistributionViewer() {
        return distributionViewerComposite;
    }

    public void clearAll() {

        if (isWidgetReady()) {
            /*
             * In association with VLab AWIPS2_GSD Issue #29204
             * 
             * Cosmetic clear for performance perception.
             */
            legendsTreeViewer.setInput(
                    EnsembleTool.getInstance().getEmptyResourceList());
            legendsTreeViewer.refresh();
        }

    }

    public void updateElementInTree(AbstractResourceHolder arh) {

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (isWidgetReady()) {
                    legendsTreeViewer.update(arh, null);
                }
            }

        });
    };

}
