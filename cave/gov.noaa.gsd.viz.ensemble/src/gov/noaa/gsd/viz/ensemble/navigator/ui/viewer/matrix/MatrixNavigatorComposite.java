package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.FieldPlanePairChooserControl.FieldPlanePairControl;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.VizMatrixEditor.MatrixNavigationOperation;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.d2d.core.map.D2DColorBarResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.FramesHandler;

/***
 * 
 * This composite contains all of the controls/widgets of the matrix navigator.
 * 
 * The upper portion of this control is a selectable list of model sources
 * selected in the model family dialog.
 * 
 * The bottom portion of this control is a group of field/plane pair components
 * (<code>FieldPlanePairChooserControl</code>) that are associated with the
 * model family chosen from the model family dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer      Description
 * ------------ ---------- ----------- --------------------------
 * Oct 15, 2015   12565      polster     Initial creation
 * Nov 13, 2015   13211      polster     Matrix editor commonalities
 * Jun 27, 2016   19975      bsteffen    Eclipse 4: Fix NPE when focus event
 *                                       arrives before selection event.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class MatrixNavigatorComposite extends Composite implements
        IModelFamilyListener, IFieldPlanePairVisibilityChangedListener,
        IModelSourceSelectionProvider, IMemoryUsageProvider,
        IMatrixEditorFocusProvider {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MatrixNavigatorComposite.class);

    private static ModelSources primaryTimeBasisModelSource = null;

    protected static int totalExpectedResources = 0;

    private static VizMatrixEditor matrixEditor = null;

    private static final String MODEL_VARIABLE = "modelName";

    private static final String TOTAL_PRECIP_VARIABLE = "TP";

    private static final String FRAME_COUNT_VARIABLE = "frameCount";

    private static final int initialLoadCount = 1;

    protected static final int defaultMaxFrameCount = 5;

    protected static final int maxFrameCount = 64;

    private static boolean isStepLoadComplete = false;

    private final String spacerPadding = "  ";

    private CTabItem matrixTabItem = null;

    private TreeViewer modelSourceTreeViewer = null;

    private Tree modelSourceTree = null;

    private ScrolledComposite modelSourceRootContainerScrolledComp = null;

    private FieldPlanePairChooserControl fieldPlaneSelector = null;

    private LoadControlStatusComposite loaderControl = null;

    private ArrayList<ColumnLabelProvider> columnLabelProviders = new ArrayList<ColumnLabelProvider>();

    private ResolvedModelFamily currentModelFamily = null;

    private List<Image> imageCache = null;

    private ModelSources lastSelectedSource = null;

    private FramesHandler framesHandler = null;

    private long startMemory = 0;

    private long memoryPerLoad = 0;

    private static WaitForLoadingToComplete updateMemoryStats = null;

    /**
     * The constructor for the Matrix navigator control.
     * 
     * @param parentTabFolder
     *            the composite of the tab item in the
     *            <code>EnsembleToolViewer</code> which contains this control.
     * @param style
     *            the SWT style mask.
     * @param ownerView
     *            the owning GUI controller of this control.
     * @param itemMatrixTabItem
     *            the tab item inside which this control resides.
     */
    public MatrixNavigatorComposite(Composite parentTabFolder, int style,
            CTabItem itemMatrixTabItem) {
        super(parentTabFolder, style);
        matrixTabItem = itemMatrixTabItem;
        imageCache = new ArrayList<>();
        framesHandler = new FramesHandler();
        createContents();
        updateMemoryStats = new WaitForLoadingToComplete(
                (IMemoryUsageProvider) this);
    }

    /**
     * Currently, there is only one VizMatrixEditor associated with the Ensemble
     * Tool's Matrix feature. This method acts like a singleton accessor for
     * this matrix editor.
     * 
     * @return the existing or created matrix editor.
     * @throws InstantiationException
     */
    public VizMatrixEditor getMatrixEditor() throws InstantiationException {

        VizMatrixEditor me = MatrixNavigatorComposite.matrixEditor;
        if (MatrixNavigatorComposite.matrixEditor == null) {
            MatrixNavigatorComposite.matrixEditor = (VizMatrixEditor) VizMatrixEditor
                    .create();
            if (MatrixNavigatorComposite.matrixEditor == null) {
                throw new InstantiationException();
            }
            me = MatrixNavigatorComposite.matrixEditor;
            EnsembleTool.getInstance().getEditorPartListener()
                    .addEditor(MatrixNavigatorComposite.matrixEditor);
        }
        return me;
    }

    /**
     * Creates the entire contents of the matrix navigator control.
     */
    private void createContents() {

        configureRootArea();

        createModelSourcesControlArea();

        createFieldPlanesControlArea();

        createLoaderControl();

        /* put this control into tab item */
        matrixTabItem.setControl(this);

    }

    private void createLoaderControl() {
        loaderControl = new LoadControlStatusComposite(this, this);
        loaderControl.setEnabled(false);
    }

    /**
     * Configures the layout and layout data for the control's root area.
     */
    private void configureRootArea() {
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        setLayout(new GridLayout(1, false));
    }

    /**
     * Creates the model sources area where the list of model sources are
     * displayed and manipulated by the user.
     */
    private void createModelSourcesControlArea() {

        modelSourceRootContainerScrolledComp = new ScrolledComposite(this,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData matrixRootContainerComposite_gd = new GridData(SWT.FILL,
                SWT.FILL, true, true, 1, 1);
        modelSourceRootContainerScrolledComp
                .setLayoutData(matrixRootContainerComposite_gd);

        GridLayout matrixRootContainerComposite_gl = new GridLayout();
        modelSourceRootContainerScrolledComp
                .setLayout(matrixRootContainerComposite_gl);
        modelSourceRootContainerScrolledComp.setExpandHorizontal(true);
        modelSourceRootContainerScrolledComp.setExpandVertical(true);

        /* Legend tab contains a tree of legends */
        modelSourceTree = new Tree(modelSourceRootContainerScrolledComp,
                SWT.BORDER | SWT.SINGLE);
        modelSourceTree.setLinesVisible(false);
        modelSourceTree.setHeaderVisible(false);
        modelSourceTreeViewer = new TreeViewer(modelSourceTree);
        createColumns(modelSourceTreeViewer);

        /* put the tree into the scrolled composite */
        modelSourceRootContainerScrolledComp.setContent(modelSourceTree);

        /* recognize when the user clicks on something in the tree */
        modelSourceTree.addMouseListener(new ModelSourcesTreeMouseListener());

        modelSourceTreeViewer
                .setContentProvider(new ModelSourcesTreeContentProvider());
        modelSourceTreeViewer.setSorter(new ModelSourcesTreeSorter());
        modelSourceTreeViewer
                .addSelectionChangedListener(new ModelSourceTreeSelectionListener());

    }

    /**
     * Creates the field/plane area where the group of pairs are displayed and
     * manipulated by the user.
     */
    private void createFieldPlanesControlArea() {
        fieldPlaneSelector = new FieldPlanePairChooserControl(this, SWT.BORDER,
                this, this, this);
    }

    /**
     * Creates the column definitions for the model source tree. Currently there
     * is only one column in the tree.
     * 
     * @param modelSourceTreeViewer
     *            the tree viewer against which the columns are composed.
     */
    private void createColumns(TreeViewer modelSourceTreeViewer) {

        TreeViewerColumn column = new TreeViewerColumn(modelSourceTreeViewer,
                SWT.LEFT);
        column.getColumn().setWidth(250);
        column.getColumn().setMoveable(false);
        column.getColumn().setAlignment(SWT.LEFT);
        column.setLabelProvider(new ModelSourcesTreeColumnLabelProvider());

    }

    /**
     * Dispose of all SWT resources associated with this control.
     */
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
        if (isViewerTreeReady()) {

            modelSourceTree.removeAll();
            modelSourceTree.dispose();
            modelSourceTree = null;
        }
        if (modelSourceTreeViewer != null) {
            modelSourceTreeViewer = null;
        }

        if (MatrixNavigatorComposite.matrixEditor != null
                && MatrixNavigatorComposite.matrixEditor.isCloseable()) {
            MatrixNavigatorComposite.matrixEditor.dispose();
            MatrixNavigatorComposite.matrixEditor = null;
        }

        if (fieldPlaneSelector != null) {
            fieldPlaneSelector.dispose();
        }
    }

    /**
     * Listen to mouse actions on the model source tree.
     */
    private class ModelSourcesTreeMouseListener implements MouseListener {

        @Override
        public void mouseDoubleClick(MouseEvent event) {
            /* ignore */
        }

        @Override
        public void mouseDown(MouseEvent event) {

            /*
             * The mouseDown event is what initiates the selection of a model
             * source.
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

            final TreeItem userClickedTreeItem = modelSourceTree.getItem(point);
            if (userClickedTreeItem == null || userClickedTreeItem.isDisposed()) {
                return;
            }

            /*
             * Is this a simple left-click (MB1) over a tree item? Then select
             * the model source. Selecting the model source will make certain
             * that only the resources of that model source will be displayed in
             * the matrix editor. All other resources will be hidden.
             */
            else if (userClickedTreeItem != null && event.button == 1) {

                select(userClickedTreeItem);
                giveEditorFocus();
            }
        }

        @Override
        public void mouseUp(MouseEvent event) {
            /* ignore */
        }
    }

    /**
     * Change the mouse cursor.
     * 
     * @param cursor
     */
    protected void updateCursor(final Cursor cursor) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (isViewerTreeReady()) {
                    modelSourceTree.setCursor(cursor);
                }
            }

        });
    }

    /**
     * Convenience method to make certain all tree components are not null and
     * not disposed.
     * 
     * @return
     */
    public boolean isViewerTreeReady() {
        boolean isReady = false;

        if (modelSourceTreeViewer != null && modelSourceTree != null
                && modelSourceTreeViewer.getTree() != null
                && !modelSourceTreeViewer.getTree().isDisposed()) {
            isReady = true;
        }

        return isReady;
    }

    /**
     * Called from the controlling ViewPart ensemble viewer.
     * 
     * TODO: Currently this does not act on this control but is kept here for
     * future use.
     * 
     * @param toolLayer
     *            the tool layer which is the administrator for this control.
     */
    public void refreshInput(EnsembleToolLayer toolLayer) {

        Map<String, List<AbstractResourceHolder>> map = toolLayer
                .getEnsembleResources();
        if (map.size() == 0) {
        }

    }

    /**
     * The method to reset controls and state varialbles to an initial state.
     */
    public void clearAllResources() {

        modelSourceTree.clearAll(true);
        loaderControl.clearAll();
        fieldPlaneSelector.clearFieldPlanePairControls();
        currentModelFamily = null;
        modelSourceTreeViewer.refresh();
    }

    /**
     * Give focus back to the matrix editor so key bindings will work.
     */
    @Override
    public void giveEditorFocus() {
        /*
         * The editor will be null on initial selection if SWT sends the focus
         * event before the selection event.
         */
        if (matrixEditor != null) {
            matrixEditor.getEditorSite().getPart().setFocus();
        }
    }

    /**
     * Returns the first tree item that contains (via <code>item.getData</code>)
     * the passed in model source.
     * 
     * TODO: Currently this method is not used but is kept here for future use.
     */
    private TreeItem findTreeItemBySource(ModelSources targetSrc) {

        TreeItem foundItem = null;
        TreeItem[] allRoots = modelSourceTree.getItems();

        for (TreeItem ti : allRoots) {
            if (foundItem != null)
                break;
            Object tio = ti.getData();
            if ((tio != null) && (tio instanceof ModelSources)) {
                ModelSources src = (ModelSources) tio;
                if (src == targetSrc) {
                    foundItem = ti;
                    break;
                }
            }
        }
        return foundItem;
    }

    /**
     * Here's how we control how the items are displayed in the tree.
     */
    private class ModelSourcesTreeColumnLabelProvider extends
            ColumnLabelProvider {

        public Font getFont(Object element) {
            Font f = SWTResourceManager.getFont("courier new", 11, SWT.BOLD);
            return f;
        }

        public Image getImage(Object element) {
            Image image = null;
            if (element instanceof ModelSources) {
                image = EnsembleToolImageStore.DOT_IMG;
            }
            return image;
        }

        public String getText(Object element) {
            String nodeLabel = null;
            ModelSources src = null;
            if (element instanceof ModelSources) {
                src = (ModelSources) element;
                nodeLabel = src.getModelName();
            }
            return spacerPadding + nodeLabel;
        }
    }

    /**
     * This is the content provider for the model sources tree.
     */
    private class ModelSourcesTreeContentProvider implements
            ITreeContentProvider {

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // do nothing
        }

        /**
         * All top-level elements in the tree are model sources obtained from
         * the loaded model family.
         */
        @Override
        public Object[] getElements(Object inputElement) {

            Object[] members = null;

            if (currentModelFamily != null) {
                List<ModelSources> sources = currentModelFamily.getSources();
                members = new Object[sources.size()];
                int i = 0;
                for (ModelSources src : sources) {
                    members[i++] = src;
                }
            } else {
                members = new Object[0];
            }

            return members;
        }

        /**
         * There are currently no child tree items
         */
        @Override
        public Object[] getChildren(Object parentElement) {

            Object[] members = new Object[0];

            return members;
        }

        /**
         * There are currently no parents (as there are no children) in the
         * tree.
         */
        @Override
        public Object getParent(Object treeElement) {
            return null;
        }

        @Override
        public boolean hasChildren(Object element) {

            boolean hasChildren = false;
            if (element instanceof String) {
                hasChildren = true;
            }
            return hasChildren;
        }

        @Override
        public void dispose() {
        }
    }

    /**
     * Selects a model source from the tree by known tree item.
     * 
     * @param treeItem
     *            the tree item which contains the model source to select.
     * @param ensembleResourcesMap
     *            the map which contains the resources loaded into the matrix
     *            editor.
     */
    synchronized private void select(final TreeItem treeItem) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (treeItem.getData() instanceof ModelSources) {

                    getShell().setCursor(EnsembleToolViewer.waitCursor);
                    Map<String, List<AbstractResourceHolder>> ensembleResourcesMap = EnsembleTool
                            .getInstance().getCurrentToolLayerResources();

                    if (ensembleResourcesMap != null) {
                        modelSourceTree.select(treeItem);
                        ModelSources currSrc = (ModelSources) treeItem
                                .getData();
                        if (lastSelectedSource != null
                                && lastSelectedSource == currSrc) {
                            return;
                        }

                        refreshDisplayBySelectedSource(currSrc,
                                ensembleResourcesMap);
                        lastSelectedSource = currSrc;

                        getShell().setCursor(EnsembleToolViewer.normalCursor);

                        modelSourceTreeViewer.refresh();
                        getShell().setCursor(EnsembleToolViewer.normalCursor);
                    }
                }
            }
        });

    }

    /**
     * Hides (in the matrix editor) all resources contained in the formal
     * argument resource map. The map is contained of a string key for the
     * resource name and a list of one to many resources associated with that
     * key (e.g. many resources are associated with an ensemble resource).
     * 
     * @param rscMap
     *            the map of resources to hide.
     */
    private void hideAllResources(
            final Map<String, List<AbstractResourceHolder>> rscMap) {

        if (rscMap == null || rscMap.isEmpty()) {
            return;
        }

        List<AbstractResourceHolder> currResources = null;
        Set<String> loadedProductNames = rscMap.keySet();
        Iterator<String> iterator = loadedProductNames.iterator();
        String currName = null;
        while (iterator.hasNext()) {
            /*
             * Either a top-level ensemble name (e.g. SREF) or an individual
             * product name (e.g. NAM40 500MB Height)
             */
            currName = iterator.next();
            currResources = rscMap.get(currName);
            if (currResources != null) {
                for (AbstractResourceHolder arh : currResources) {
                    arh.getRsc().getProperties().setVisible(false);
                    arh.getRsc().issueRefresh();
                }
            }

        }

    }

    /**
     * If the user chooses a different model source in the list, then refresh
     * the display by hiding all existing resources displayed (i.e. of the
     * previously selected source) and then update the display with the new
     * model sources resources.
     * 
     * @param selectedSrc
     *            the newly selected model source
     * @param ensembleResourcesMap
     *            the map which contains the resources loaded into the matrix
     *            editor.
     */
    public void refreshDisplayBySelectedSource(ModelSources selectedSrc,
            Map<String, List<AbstractResourceHolder>> ensembleResourcesMap) {

        hideAllResources(ensembleResourcesMap);
        updateElementsBySelectedSource(selectedSrc, ensembleResourcesMap);

    }

    /**
     * Show the Elements (field/plane pairs) that are currently asserted for the
     * currently selected Source.
     * 
     * @param ensembleResourcesMap
     */
    private void updateElementsBySelectedSource(final ModelSources currSrc,
            final Map<String, List<AbstractResourceHolder>> rscMap) {

        if (rscMap == null || rscMap.isEmpty()) {
            return;
        }

        String currSrcName = currSrc.getModelName();
        List<FieldPlanePair> elements = fieldPlaneSelector.getFieldPlanePairs();

        List<AbstractResourceHolder> currResources = null;
        Set<String> loadedProductNames = rscMap.keySet();
        Iterator<String> iterator = loadedProductNames.iterator();
        RequestableResourceMetadata rrmd = null;
        String fieldAbbrev = null;
        String plane = null;
        String currName = null;
        while (iterator.hasNext()) {
            /*
             * Either a top-level ensemble name (e.g. SREF) or an individual
             * product name (e.g. NAM40 500MB Height)
             */
            currName = iterator.next();
            currResources = rscMap.get(currName);
            if (currResources != null) {
                for (AbstractResourceHolder arh : currResources) {
                    /*
                     * Only look at resources having the same Source as the
                     * selected Source
                     */
                    if (arh.getRsc().getName().trim()
                            .startsWith(currSrcName.trim())) {
                        for (FieldPlanePair e : elements) {
                            if (e.isVisible()) {
                                if (arh.getRsc().getResourceData() instanceof AbstractRequestableResourceData) {
                                    AbstractRequestableResourceData ard = (AbstractRequestableResourceData) arh
                                            .getRsc().getResourceData();
                                    rrmd = new RequestableResourceMetadata(ard);
                                    fieldAbbrev = rrmd.getFieldAbbrev();
                                    plane = rrmd.getPlane();
                                    if (e.getFieldAbbrev().equals(fieldAbbrev)
                                            && e.getPlane().equals(plane)) {
                                        arh.getRsc().getProperties()
                                                .setVisible(true);
                                        arh.getRsc().issueRefresh();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        EditorUtil.getActiveVizContainer().refresh();

    }

    /**
     * Show the Elements (field/plane pairs) that are currently asserted for the
     * currently selected Source.
     */
    private void updateChangeInElementVisibility(
            final ModelSources selectedSrc, final FieldPlanePair changedElement) {

        final Map<String, List<AbstractResourceHolder>> rscMap = EnsembleTool
                .getInstance().getCurrentToolLayerResources();

        if (rscMap == null || rscMap.isEmpty()) {
            return;
        }

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                String currSrcName = selectedSrc.getModelName();

                List<AbstractResourceHolder> currResources = null;
                Set<String> loadedProductNames = rscMap.keySet();
                Iterator<String> iterator = loadedProductNames.iterator();
                RequestableResourceMetadata rrmd = null;
                String fieldAbbrev = null;
                String plane = null;
                String currName = null;
                while (iterator.hasNext()) {
                    /*
                     * The following name is either a top-level ensemble name
                     * (e.g. SREF 500MB Height) or an individual product name
                     * (e.g. NAM40 500MB Height)
                     */
                    currName = iterator.next();
                    currResources = rscMap.get(currName);
                    for (AbstractResourceHolder arh : currResources) {
                        /*
                         * Only look at resources having the same Source as the
                         * selected Source
                         */
                        if (arh.getRsc().getName().trim()
                                .startsWith(currSrcName.trim())) {
                            if (arh.getRsc().getResourceData() instanceof AbstractRequestableResourceData) {
                                AbstractRequestableResourceData ard = (AbstractRequestableResourceData) arh
                                        .getRsc().getResourceData();

                                rrmd = new RequestableResourceMetadata(ard);
                                fieldAbbrev = rrmd.getFieldAbbrev();
                                plane = rrmd.getPlane();

                                if (changedElement.getFieldAbbrev().equals(
                                        fieldAbbrev)
                                        && changedElement.getPlane().equals(
                                                plane)) {

                                    arh.getRsc()
                                            .getProperties()
                                            .setVisible(
                                                    changedElement.isVisible());

                                    if (arh.getRsc().hasCapability(
                                            DisplayTypeCapability.class)) {
                                        DisplayType dt = arh
                                                .getRsc()
                                                .getCapability(
                                                        DisplayTypeCapability.class)
                                                .getDisplayType();
                                        if (dt == DisplayType.IMAGE) {
                                            /**
                                             * TODO: This apparently is not how
                                             * color bars work.
                                             */
                                            if (arh.getRsc().getProperties()
                                                    .isVisible()) {
                                                /* show color bar */
                                                setColorBarVisible(true);
                                            } else {
                                                /* hide color bar */
                                                setColorBarVisible(false);
                                            }

                                        }
                                        arh.getRsc().issueRefresh();
                                    }
                                }

                            }
                        }
                    }
                }
                EditorUtil.getActiveVizContainer().refresh();
            }
        });

    }

    /**
     * TODO: HELP!! Why doesn't this work? Need a DR for 16.2.2!
     * 
     * @param isVisible
     */
    protected void setColorBarVisible(boolean isVisible) {

        ResourceList rscList = EditorUtil.getActiveVizContainer()
                .getActiveDisplayPane().getDescriptor().getResourceList();
        for (ResourcePair rp : rscList) {
            if (rp.getResource() instanceof D2DColorBarResource) {
                D2DColorBarResource colorBar = (D2DColorBarResource) rp
                        .getResource();
                colorBar.getProperties().setVisible(isVisible);
                colorBar.issueRefresh();
            }
        }
    }

    /**
     * The model source tree is sorted in alphabetical order.
     * 
     * TODO: It may be better to display the model sources in the order they are
     * loaded, which is shortest time-step to longest time-step.
     */
    private class ModelSourcesTreeSorter extends ViewerSorter {

        public int compare(Viewer v, Object av1, Object av2) {

            int compareResult = 0;

            if ((av1 instanceof String) && (av2 instanceof String)) {
                String n1 = (String) av1;
                String n2 = (String) av2;
                compareResult = n1.compareTo(n2);
            }

            return compareResult;
        }
    }

    private void loadModelFamily(final ResolvedModelFamily modelFamily) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                getShell().setCursor(EnsembleToolViewer.waitCursor);

                if (modelFamily.getSources() != null
                        && modelFamily.getSources().size() > 0) {

                    currentModelFamily = modelFamily;
                    fieldPlaneSelector.addModelFamily(modelFamily);

                    totalExpectedResources = modelFamily.getSources().size()
                            * fieldPlaneSelector.getNumberOfFieldPlanePairs();

                    layout();
                    modelSourceTreeViewer.setInput(currentModelFamily);
                    modelSourceTreeViewer.expandAll();

                }
                getShell().setCursor(EnsembleToolViewer.normalCursor);

            }
        });

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                loadModels(currentModelFamily);
            }
        });

    }

    private void selectInitialModelSource() {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                select(modelSourceTree.getItem(0));
            }
        });

    }

    @Override
    public void updateMemoryUsage() {
        Runtime runtime = Runtime.getRuntime();
        System.gc();
        memoryPerLoad = runtime.totalMemory()
                - (runtime.freeMemory() + startMemory);
        loaderControl.setMemoryUsage(memoryPerLoad);
    }

    /**
     * Reinitializes this entire control with a newly selected model family.
     */
    @Override
    public void addModelFamily(final ResolvedModelFamily modelFamily) {

        clearAllResources();
        System.gc();
        Runtime runtime = Runtime.getRuntime();
        startMemory = runtime.totalMemory() - runtime.freeMemory();
        primaryTimeBasisModelSource = modelFamily.getSources().get(0);

        if (updateMemoryStats.getState() == Job.RUNNING) {
            updateMemoryStats.cancel();
        }
        updateMemoryStats.setPriority(Job.LONG);
        updateMemoryStats.schedule(2000);

        LoadModelFamily lmfJob = new LoadModelFamily(modelFamily);
        lmfJob.setPriority(Job.SHORT);
        lmfJob.schedule();

        /**
         * TODO: Need to wait until all resources are loaded via preceding job
         * (see method loadModels and the job MatchResourceCapabilityState) or
         * once a delay expires, whichever comes first.
         * 
         * For now, waitfor an arbitrary but extended period of time to start
         * the "select first item" job; this is obviously not scalable for any
         * arbitrary user-requested model family. Not sure how to solve this for
         * the 16.2.2 release.
         */

        int numModelSources = modelFamily.getSources().size();
        final int delayTimePerModelSource = 11000;

        SelectInitialModelSource selectSrcJob = new SelectInitialModelSource();
        selectSrcJob.setPriority(Job.SHORT);
        selectSrcJob.schedule(numModelSources * delayTimePerModelSource);

        DisableControlsForUnloadedResources disableControlsJob = new DisableControlsForUnloadedResources();
        disableControlsJob.setPriority(Job.SHORT);
        disableControlsJob.schedule(numModelSources * delayTimePerModelSource);

        loaderControl.setEnabled(true);

    }

    /**
     * Load the resources associated with the given resolved model family which
     * will therefore request all elements (field/plane pairs) for each model
     * source contained in therein.
     * 
     * @param rmf
     *            the resolved model family containing the specific resources to
     *            associate with the family's field/plane pairs.
     */
    private void loadModels(ResolvedModelFamily rmf) {

        /* reset the previously selected resource variable */
        lastSelectedSource = null;

        List<ModelSources> sources = rmf.getSources();

        Map<String, String> variableTranslations = new HashMap<>();
        ModelFamilyDefinitions mfd = null;
        String modelDbId = null; // e.g. GFS215
        String totalPrecipValue = null;

        Bundle bundle = null;
        getShell().setCursor(EnsembleToolViewer.waitCursor);
        for (ModelSources src : sources) {
            modelDbId = src.getModelId();
            mfd = rmf.getCurrFamilyDefinition();
            totalPrecipValue = mfd.getTotalPrecip();
            variableTranslations.put(TOTAL_PRECIP_VARIABLE, totalPrecipValue);
            variableTranslations.put(MODEL_VARIABLE, modelDbId);
            variableTranslations.put(FRAME_COUNT_VARIABLE,
                    Integer.toString(initialLoadCount));
            try {
                bundle = BundleLoader.getBundle(mfd.getFamilyFileName(),
                        variableTranslations, BundleInfoType.FILE_LOCATION);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to load bundle: " + rmf.bundle.getName() + "; "
                                + e.getLocalizedMessage(), e);
                continue;

            }
            /* load synchronously */
            new BundleProductLoader(matrixEditor, bundle).run();

        }

        MatchResourceCapabilityState matchSimilarResources = null;
        matchSimilarResources = new MatchResourceCapabilityState();
        matchSimilarResources.setPriority(Job.SHORT);
        matchSimilarResources.schedule(4000);

        getShell().setCursor(EnsembleToolViewer.normalCursor);

        return;
    }

    /**
     * Loop through all the Matrix (.i.e. resolved model family) resources and
     */
    public void associateResourcesWithFPPControl() {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                getShell().setCursor(EnsembleToolViewer.waitCursor);

                Map<String, List<AbstractResourceHolder>> rscMap = EnsembleTool
                        .getInstance().getCurrentToolLayerResources();

                List<AbstractResourceHolder> currResources = null;
                List<FieldPlanePairControl> controls = null;
                Set<String> loadedProductNames = rscMap.keySet();
                Iterator<String> iterator = null;
                AbstractVizResource<?, ?> currRsc = null;
                String currName = null;

                FieldPlanePair fpp = null;
                String fppField = null;
                String fppPlane = null;
                String matchToRscField = null;
                String matchToRscPlane = null;
                RequestableResourceMetadata matchToRscMetaData = null;

                controls = fieldPlaneSelector.getFieldPlanePairControls();
                for (FieldPlanePairControl fppc : controls) {
                    fpp = fppc.getFieldPlanePair();
                    fppField = fpp.getFieldAbbrev();
                    fppPlane = fpp.getPlane();
                    iterator = loadedProductNames.iterator();
                    while (iterator.hasNext()) {
                        /*
                         * Either a top-level ensemble name (e.g. SREF) or an
                         * individual product name (e.g. NAM40 500MB Height)
                         */
                        currName = iterator.next();
                        currResources = rscMap.get(currName);

                        for (AbstractResourceHolder arh : currResources) {

                            currRsc = arh.getRsc();
                            if (currRsc.getResourceData() instanceof AbstractRequestableResourceData) {

                                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) currRsc
                                        .getResourceData();
                                matchToRscMetaData = new RequestableResourceMetadata(
                                        arrd);

                                matchToRscField = matchToRscMetaData
                                        .getFieldAbbrev();
                                matchToRscPlane = matchToRscMetaData.getPlane();

                                if (matchToRscField == null
                                        || matchToRscField.length() == 0
                                        || matchToRscPlane == null
                                        || matchToRscField.length() == 0) {
                                    continue;
                                }

                                if (matchToRscField.equals(fppField)
                                        && matchToRscPlane.equals(fppPlane)) {
                                    // fppc.register(currRsc);
                                    currRsc.getResourceData()
                                            .addChangeListener(
                                                    (IResourceDataChanged) fppc);
                                }
                            }
                        }
                    }

                }
                getShell().setCursor(EnsembleToolViewer.normalCursor);
            }
        });

    }

    /**
     * Starts a job to match resources, having shared field/plane pairs, to have
     * the same color and density.
     */
    public void matchLikeResources() {
        MatchResourceCapabilityState matchSimilarResources = null;
        matchSimilarResources = new MatchResourceCapabilityState();
        matchSimilarResources.setPriority(Job.SHORT);
        matchSimilarResources.schedule();
    }

    public ModelFamily getCurrentModelFamily() {
        return currentModelFamily;
    }

    /**
     * Updates the visibility associated with the given element for the current
     * selected model source.
     */
    @Override
    public void visibilityChanged(FieldPlanePair e) {
        /* Refresh the display to match the change in Element visibility */
        TreeItem[] selectedItems = modelSourceTree.getSelection();
        TreeItem currItem = selectedItems[0];
        if (currItem.getData() instanceof ModelSources) {
            ModelSources currSrc = (ModelSources) currItem.getData();
            updateChangeInElementVisibility(currSrc, e);
        }
    }

    @Override
    public ModelSources getSelected() {
        return lastSelectedSource;
    }

    /**
     * Set the enabled/disabled state of all of the gui components associated
     * with this control.
     * 
     * @param enabled
     *            the boolean enabled/disabled flag
     */
    public void setViewEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                modelSourceTree.setEnabled(enabled);
                fieldPlaneSelector.setViewEditable(enabled);
            }
        });
    }

    public static ModelSources getPrimaryTimeBasisModelSource() {
        return primaryTimeBasisModelSource;
    }

    public boolean setFocus() {
        giveEditorFocus();
        return true;
    }

    /**
     * This job loads the resources associated with the given resolved model
     * family.
     */
    private class LoadModelFamily extends Job {

        private IStatus status = Status.OK_STATUS;

        private ResolvedModelFamily resolvedModelFamily = null;

        public LoadModelFamily(ResolvedModelFamily rmf) {
            super("Load Model Family");
            resolvedModelFamily = rmf;
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            status = Status.OK_STATUS;
            if (!monitor.isCanceled()) {
                loadModelFamily(resolvedModelFamily);

            } else {
                status = Status.CANCEL_STATUS;
            }

            return status;

        }
    }

    /**
     * This job programmatically selects the first source in the model source
     * tree.
     */
    private class SelectInitialModelSource extends Job {

        private IStatus status = Status.OK_STATUS;

        public SelectInitialModelSource() {
            super("Select Initial Model Family");
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            status = Status.OK_STATUS;
            if (!monitor.isCanceled()) {
                selectInitialModelSource();
            } else {
                status = Status.CANCEL_STATUS;
            }

            return status;

        }
    }

    private class DisableControlsForUnloadedResources extends Job {

        private IStatus status = Status.OK_STATUS;

        public DisableControlsForUnloadedResources() {
            super("Check for unloaded resources");
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            /**
             * TODO: Strange place to turn off memory usage updater job. Remove
             * this when the final solution for the WaitForLoadingComplete is
             * refactored for the 16.2.2 release.
             */
            if (updateMemoryStats.getState() == Job.RUNNING) {
                updateMemoryStats.cancel();
            }

            status = Status.OK_STATUS;
            if (!monitor.isCanceled()) {
                fieldPlaneSelector.disableEmptyFieldPlanePairControls();
            } else {
                status = Status.CANCEL_STATUS;
            }
            EditorUtil.getActiveVizContainer().refresh();
            return status;

        }

    }

    /**
     * This job waits for resources to be available then attempts to match the
     * resource Color and Density across all model sources.
     * 
     * Once the resources have been matched the Job exits.
     */
    private class MatchResourceCapabilityState extends Job {

        private IStatus status = Status.OK_STATUS;

        public MatchResourceCapabilityState() {
            super("Match A-B Resource");
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            status = Status.OK_STATUS;
            if (!monitor.isCanceled()) {
                List<AbstractVizResource<?, ?>> foundRscs = findSourceToMatchToResources();
                matchProductCapability(foundRscs, ColorableCapability.class);
                matchProductCapability(foundRscs, DensityCapability.class);
                EditorUtil.getActiveVizContainer().refresh();
            } else {
                status = Status.CANCEL_STATUS;
            }

            return status;

        }

        /**
         * Finds the shortest time-step resources against which all other
         * resources will be matched.
         * 
         * @return the list of found resources
         */
        private List<AbstractVizResource<?, ?>> findSourceToMatchToResources() {

            List<AbstractVizResource<?, ?>> foundRscs = new ArrayList<>();
            IDisplayPane pane = null;
            /* Currently only deal with single display families */
            if (matrixEditor != null) {
                pane = matrixEditor.getActiveDisplayPane();
                ModelSources modelSource = currentModelFamily.getSources().get(
                        0);

                ResourceList rscList = pane.getDescriptor().getResourceList();
                for (int i = 0; i < rscList.size(); i++) {
                    ResourcePair rp = rscList.get(i);
                    if (rp != null && rp.getResource() != null
                            && rp.getResource().getName() != null) {
                        rp.getResource();
                        if (rp.getResource().getName()
                                .startsWith(modelSource.getModelName())) {
                            foundRscs.add(rp.getResource());
                        }
                    }
                }
            }

            return foundRscs;
        }

        /**
         * Given a list of source resources and a capability class, walk through
         * all target resources and set the target capability equal to the
         * source.
         * 
         * @param matchToTheseRscs
         *            the resources to match to
         * @param capabilityClass
         *            the capability to match
         */
        private void matchProductCapability(
                List<AbstractVizResource<?, ?>> matchToTheseRscs,
                Class<? extends AbstractCapability> capabilityClass) {

            IDisplayPane pane = null;
            if (matchToTheseRscs.size() == 0) {
                return;
            }

            /**
             * A model family is a family of resources resolved by a model
             * source.
             * 
             * The first model source contains the resources against which all
             * other resources will be matched.
             */
            ModelSources matchToModelSource = currentModelFamily.getSources()
                    .get(0);

            /**
             * The other model sources contain the family of resources that need
             * to have their capabilities matched.
             */
            List<ModelSources> toBeMatchedSources = new ArrayList<>();
            for (int i = 1; i < currentModelFamily.getSources().size(); i++) {
                toBeMatchedSources.add(currentModelFamily.getSources().get(i));
            }

            AbstractVizResource<?, ?> rsc = null;

            /**
             * TODO: only working with single-display families for now
             */
            pane = matrixEditor.getActiveDisplayPane();
            ResourceList rscList = pane.getDescriptor().getResourceList();

            /**
             * Get the resources that need to match the color and density of the
             * match-to model source resources.
             */
            List<AbstractVizResource<?, ?>> rscsNeedingToBeMatched = new ArrayList<>();
            for (ResourcePair rp : rscList) {
                if (rp == null) {
                    continue;
                }
                /**
                 * This rsc can be any flavor found in the editor.
                 * 
                 * Ignore the match-to model source resources.
                 */
                rsc = rp.getResource();
                if (rsc != null && rsc.getName() != null) {
                    if (rsc.getName().startsWith(
                            matchToModelSource.getModelName())) {
                        continue;
                    } else {
                        /**
                         * Check to make sure the resource starts with the
                         * to-be-matched model source names
                         */
                        for (ModelSources ms : toBeMatchedSources) {
                            if (rsc.getName().startsWith(ms.getModelName())) {
                                rscsNeedingToBeMatched.add(rsc);
                                break;
                            }
                        }
                    }
                }
            }

            String toBeMatchedField = null;
            String toBeMatchedPlane = null;
            String matchToField = null;
            String matchToPlane = null;
            RequestableResourceMetadata toBeMatchedRscMetaData = null;
            RequestableResourceMetadata matchToRscMetaData = null;
            for (AbstractVizResource<?, ?> matchToRsc : matchToTheseRscs) {
                if (matchToRsc == null) {
                    continue;
                }
                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) matchToRsc
                        .getResourceData();
                matchToRscMetaData = new RequestableResourceMetadata(arrd);

                matchToField = matchToRscMetaData.getFieldAbbrev();
                matchToPlane = matchToRscMetaData.getPlane();

                if (matchToField == null || matchToField.length() == 0
                        || matchToPlane == null || matchToField.length() == 0) {
                    continue;
                }
                /**
                 * Loop through the to-be-matched resources and find a
                 * field/plane pair match and then set the capability on the
                 * to-be-matched resource to be equal to the same capability on
                 * the match-to resource.
                 */
                for (AbstractVizResource<?, ?> rscToBeMatched : rscsNeedingToBeMatched) {

                    /* both field and plane need to match */
                    AbstractRequestableResourceData tbmr = (AbstractRequestableResourceData) rscToBeMatched
                            .getResourceData();

                    toBeMatchedRscMetaData = new RequestableResourceMetadata(
                            tbmr);

                    toBeMatchedField = toBeMatchedRscMetaData.getFieldAbbrev();
                    toBeMatchedPlane = toBeMatchedRscMetaData.getPlane();

                    if (toBeMatchedField == null
                            || toBeMatchedField.length() == 0
                            || toBeMatchedPlane == null
                            || toBeMatchedPlane.length() == 0) {
                        continue;
                    }

                    if (!matchToField.equals(toBeMatchedField)
                            || !matchToPlane.equals(toBeMatchedPlane)) {
                        continue;
                    }
                    if (matchToRsc.hasCapability(capabilityClass)
                            && rscToBeMatched.hasCapability(capabilityClass)) {
                        if (capabilityClass == ColorableCapability.class) {
                            rscToBeMatched.getCapability(
                                    ColorableCapability.class).setColor(
                                    matchToRsc.getCapability(
                                            ColorableCapability.class)
                                            .getColor());

                            rscToBeMatched.issueRefresh();
                        }
                        if (capabilityClass == DensityCapability.class) {
                            rscToBeMatched.getCapability(
                                    DensityCapability.class).setDensity(
                                    matchToRsc.getCapability(
                                            DensityCapability.class)
                                            .getDensity());
                            rscToBeMatched.issueRefresh();
                        }
                    }
                }
            }
        }
    }

    private class ModelSourceTreeSelectionListener implements
            ISelectionChangedListener {

        @Override
        public void selectionChanged(SelectionChangedEvent event) {
            getShell().setCursor(EnsembleToolViewer.waitCursor);
            TreeItem[] tis = modelSourceTree.getSelection();

            if (tis != null && tis.length > 0 && tis[0] != null
                    && !tis[0].isDisposed()
                    && tis[0].getData() instanceof ModelSources) {

                select(tis[0]);

            }
            getShell().setCursor(EnsembleToolViewer.normalCursor);
        }
    }

    public void matrixNavigationRequest(MatrixNavigationOperation operation) {
        switch (operation) {
        case DOWN_MODEL_SOURCE:
        case UP_MODEL_SOURCE:
            navigateModelSources(operation);
            break;
        case LEFT_FRAME:
        case RIGHT_FRAME:
            navigateFrames(operation);
            break;
        }
    }

    private void navigateModelSources(MatrixNavigationOperation operation) {
        /**
         * Programmatically move up or down one model source in the tree.
         */
        TreeItem treeItem = (TreeItem) modelSourceTree.getSelection()[0];
        if (treeItem != null) {
            int currentTreeItemCount = modelSourceTree.indexOf(treeItem);
            if (operation == MatrixNavigationOperation.UP_MODEL_SOURCE) {
                if (currentTreeItemCount > 0) {
                    select(modelSourceTree.getItem(currentTreeItemCount - 1));
                }
            }
            if (operation == MatrixNavigationOperation.DOWN_MODEL_SOURCE) {
                if (currentTreeItemCount < modelSourceTree.getItemCount() - 1) {
                    select(modelSourceTree.getItem(currentTreeItemCount + 1));
                }
            }
        }

    }

    private void navigateFrames(MatrixNavigationOperation operation) {
        matrixEditor.getLoopProperties().setLooping(false);

        int currentFrame = matrixEditor.getActiveDisplayPane().getDescriptor()
                .getFramesInfo().getFrameIndex();
        int currentMaxFrame = matrixEditor.getActiveDisplayPane()
                .getDescriptor().getFramesInfo().getFrameCount();
        /* normalize for zero-offset */
        currentFrame += 1;
        if (operation == MatrixNavigationOperation.LEFT_FRAME) {
            if (currentFrame == 1) {
                matrixEditor
                        .getActiveDisplayPane()
                        .getDescriptor()
                        .getFrameCoordinator()
                        .changeFrame(
                                IFrameCoordinator.FrameChangeOperation.LAST,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            } else {
                matrixEditor
                        .getActiveDisplayPane()
                        .getDescriptor()
                        .getFrameCoordinator()
                        .changeFrame(
                                IFrameCoordinator.FrameChangeOperation.PREVIOUS,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            }
        }
        if (operation == MatrixNavigationOperation.RIGHT_FRAME) {

            /**
             * If the current frame is less than the maximum loaded frame, then
             * just step forward one frame.
             */
            if (currentFrame < currentMaxFrame) {
                matrixEditor
                        .getActiveDisplayPane()
                        .getDescriptor()
                        .getFrameCoordinator()
                        .changeFrame(
                                IFrameCoordinator.FrameChangeOperation.NEXT,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            }
            /**
             * Otherwise, if the current frame is equal to the maximum loaded
             * frame, then check to see if the user requested maximum frame is
             * reached. If it isn't yet reached, then request to load another
             * frame. If it is reached then go to the first frame.
             */
            else if (currentFrame == currentMaxFrame) {
                if (currentFrame < loaderControl.getMaxFrameCount()) {

                    getShell().setCursor(EnsembleToolViewer.waitCursor);

                    Map<String, String> params = new HashMap<String, String>();
                    params.put("frameCount", Integer.toString(currentFrame + 1));
                    ExecutionEvent eEvent = new ExecutionEvent(null, params,
                            null, null);
                    try {
                        framesHandler.execute(eEvent);
                    } catch (ExecutionException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error executing frame handler", e);
                    }

                    try {
                        Thread.sleep(200);
                    } catch (InterruptedException e) {
                        /* ignore */
                    }
                    isStepLoadComplete = false;

                    BlockUserInteraction waitForCompleteJob = new BlockUserInteraction();
                    waitForCompleteJob.setPriority(Job.SHORT);
                    waitForCompleteJob.setUser(true);
                    waitForCompleteJob.schedule();

                    matrixEditor
                            .getActiveDisplayPane()
                            .getDescriptor()
                            .getFrameCoordinator()
                            .changeFrame(
                                    IFrameCoordinator.FrameChangeOperation.LAST,
                                    IFrameCoordinator.FrameChangeMode.TIME_ONLY);

                } else if (currentFrame == loaderControl.getMaxFrameCount()) {
                    matrixEditor
                            .getActiveDisplayPane()
                            .getDescriptor()
                            .getFrameCoordinator()
                            .changeFrame(
                                    IFrameCoordinator.FrameChangeOperation.FIRST,
                                    IFrameCoordinator.FrameChangeMode.TIME_ONLY);
                }
            }

        }
    }

    public void frameChanged(FramesInfo framesInfo) {

        isStepLoadComplete = true;
    }

    synchronized public boolean isStepLoadComplete() {
        return isStepLoadComplete;
    }

    private class BlockUserInteraction extends Job {

        private IStatus status = Status.OK_STATUS;

        public BlockUserInteraction() {
            super("Waiting for step");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            final int maxWait = 5000;
            final int sleepStep = 200;
            int counter = 0;
            while (!isStepLoadComplete) {
                try {
                    Thread.sleep(sleepStep);
                    counter += sleepStep;
                    if (counter > maxWait) {
                        break;
                    }
                } catch (InterruptedException e) {
                    /* ignore */
                }
            }
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    getShell().setCursor(EnsembleToolViewer.normalCursor);
                }
            });
            return status;
        }

    }

    public void refreshResources() {
        DisableControlsForUnloadedResources disableControlsJob = new DisableControlsForUnloadedResources();
        disableControlsJob.setPriority(Job.SHORT);
        disableControlsJob.schedule();
    }

    public String getActiveRscTime() {
        String rscTime = "";
        IDisplayPane pane = null;

        AbstractVizResource<?, ?> rsc = null;

        pane = matrixEditor.getActiveDisplayPane();
        ResourceList rscList = pane.getDescriptor().getResourceList();

        for (ResourcePair rp : rscList) {
            if (rp == null) {
                continue;
            }
            /**
             * TODO: we need to be able to match against any currently visible
             * resource.
             */
            GridNameGenerator.IGridNameResource currRsc = null;
            if (rp.getResource() instanceof GridNameGenerator.IGridNameResource) {
                currRsc = (GridNameGenerator.IGridNameResource) rp
                        .getResource();
                if (rp.getResource().getProperties().isVisible()) {
                    if ((currRsc.getLegendParameters() == null)
                            || (currRsc.getLegendParameters().dataTime == null)) {
                        rscTime = "";
                        continue;
                    } else {
                        LegendParameters legendParams = currRsc
                                .getLegendParameters();
                        rscTime = legendParams.dataTime.getLegendString();
                        break;
                    }
                }
            }
        }
        return rscTime;
    }

}
