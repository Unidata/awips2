package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager.ManagedMapScale;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.d2d.core.map.D2DColorBarResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.MatrixNavigationOperation;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.FieldPlanePairChooserControl.FieldPlanePairControl;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

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
 * Oct 12, 2016   19443      polster     Make sure matrix editor can be swapped
 * Mar 01, 2017   19443      polster     Safer way to hide color bar
 * Dec 01, 2017   41520      polster     Now uses actual VizMatrixEditor
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class MatrixNavigatorComposite extends Composite
        implements IModelFamilyListener,
        IFieldPlanePairVisibilityChangedListener, IModelSourceSelectionProvider,
        IMatrixEditorFocusProvider, IMatrixResourceLoadProvider,
        IMatrixResourceMatcher, IModelFamilyProvider {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MatrixNavigatorComposite.class);

    public static final String MATRIX_EDITOR_TAB_TITLE = "Matrix";

    private static final String DEFAULT_SCALES_DIR = "bundles"
            + IPathManager.SEPARATOR + "scales";

    private static final String DEFAULT_SCALES_FILE = "ET_scalesInfo.xml";

    private VizMapEditor matrixEditor = null;

    private static boolean isExtant = false;

    private static ModelFamilyBrowserDialog modelFamilyDialog = null;

    public static boolean isInitializing = false;

    private final String spacerPadding = "  ";

    private CTabItem matrixTabItem = null;

    private TreeViewer modelSourceTreeViewer = null;

    private static Tree modelSourceTree = null;

    private ScrolledComposite modelSourceRootContainerScrolledComp = null;

    private FieldPlanePairChooserControl fieldPlaneChooserControl = null;

    private ArrayList<ColumnLabelProvider> columnLabelProviders = new ArrayList<>();

    private ResolvedModelFamily currentModelFamily = null;

    private List<Image> imageCache = null;

    private ModelSourceKind lastSelectedSource = null;

    private EnsembleToolLayer matrixToolLayer = null;

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
     * @throws InstantiationException
     */
    public MatrixNavigatorComposite(Composite parentTabFolder, int style,
            CTabItem itemMatrixTabItem) throws InstantiationException {
        super(parentTabFolder, style);

        isInitializing = true;
        matrixTabItem = itemMatrixTabItem;
        imageCache = new ArrayList<>();

        EnsembleTool.getInstance().ignorePartActivatedEvent(true);
        try {
            matrixEditor = (VizMatrixEditor) create();
        } catch (Exception e) {
            throw new InstantiationException(
                    "Unable to create a Matrix Navigator editor: "
                            + e.getMessage());
        } finally {
            EnsembleTool.getInstance().ignorePartActivatedEvent(false);
        }

        modelFamilyDialog = new ModelFamilyBrowserDialog(getShell(), this);

        createContents();

        isInitializing = false;

    }

    /**
     * Create the Matrix editor which is a VizMapEditor with a unique name.
     * 
     * @return the created editor
     * @throws SerializationException
     */
    public AbstractEditor create() throws SerializationException {
        AbstractEditor matrixEditor = null;

        MapScalesManager mgr = new MapScalesManager(DEFAULT_SCALES_DIR,
                DEFAULT_SCALES_FILE);
        ManagedMapScale editorScale = mgr.getScaleByName("CONUS");

        if (editorScale != null) {
            try {
                Bundle b = editorScale.getScaleBundle();

                matrixEditor = UiUtil.createEditor(VizMatrixEditor.EDITOR_ID,
                        b.getDisplays());

                matrixEditor.setPartName(MATRIX_EDITOR_TAB_TITLE);
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to load bundle for scale " + editorScale
                                + " to screen",
                        e);
            }
        } else {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to find an editor based map scale");
            matrixEditor = null;
        }
        EnsembleToolLayer toolLayer = null;
        if (matrixEditor != null) {
            try {
                toolLayer = EnsembleTool.getInstance()
                        .createToolLayer(matrixEditor, EnsembleToolMode.MATRIX);
            } catch (VizException e1) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to create tool layer for Matrix Navigator", e1);
            }
            matrixToolLayer = toolLayer;
            EnsembleTool.getInstance().setEditor(matrixEditor);
            EnsembleTool.getInstance().setHideLegendsMode();
            EnsembleTool.getInstance().refreshTool(true);
        }
        MatrixNavigatorComposite.isExtant = true;
        return matrixEditor;
    }

    public static boolean isEmpty() {
        return (modelSourceTree.getItemCount() == 0);
    }

    public static boolean isExtant() {
        return isExtant;
    }

    /**
     * Get the matrix editor reference.
     * 
     */
    public VizMapEditor getMatrixEditor() {

        return matrixEditor;
    }

    /**
     * Creates the housing contents of the matrix navigator control.
     */
    private void createContents() {

        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        setLayout(new GridLayout(1, false));

        createModelSourcesControlArea();

        createFieldPlanesControlArea();

        /* put this control into tab item */
        matrixTabItem.setControl(this);

    }

    /**
     * Creates the model sources area where the list of model sources are
     * displayed and manipulated by the user.
     */
    private void createModelSourcesControlArea() {

        modelSourceRootContainerScrolledComp = new ScrolledComposite(this,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData matrixRootContainerComposite_gd = new GridData(SWT.FILL,
                SWT.FILL, true, false, 1, 1);
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
        modelSourceTreeViewer.addSelectionChangedListener(
                new ModelSourceTreeSelectionListener());
        modelSourceTree.addMouseListener(new IgnoreFocusListener());
    }

    /**
     * Creates the field/plane area where the group of pairs are displayed and
     * manipulated by the user.
     */
    private void createFieldPlanesControlArea() {
        fieldPlaneChooserControl = new FieldPlanePairChooserControl(
                (Composite) this, SWT.BORDER, matrixToolLayer,
                (IFieldPlanePairVisibilityChangedListener) this,
                (IModelSourceSelectionProvider) this,
                (IMatrixEditorFocusProvider) this,
                (IMatrixResourceLoadProvider) this,
                (IMatrixResourceMatcher) this, (IModelFamilyProvider) this,
                (IMatrixEditorFocusProvider) this);
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

        MatrixNavigatorComposite.isExtant = false;

        for (Image img : imageCache) {
            img.dispose();
        }

        if (columnLabelProviders != null) {
            for (ColumnLabelProvider clp : columnLabelProviders) {
                clp.dispose();
            }
        }
        if (isWidgetReady()) {

            modelSourceTree.removeAll();
            modelSourceTree.dispose();
            modelSourceTree = null;
        }
        if (modelSourceTreeViewer != null) {
            modelSourceTreeViewer = null;
        }

        if (matrixEditor != null) {
            matrixEditor.dispose();
            matrixEditor = null;
        }

        if (fieldPlaneChooserControl != null) {
            fieldPlaneChooserControl.dispose();
            fieldPlaneChooserControl = null;
        }

        modelFamilyDialog = null;

        /*
         * The following cleanup is necessary for times when the user wishes to
         * unload the matrix tool layer and reopen it at a later time.
         */
        if (!PlatformUI.getWorkbench().isClosing()) {
            Control[] children = getChildren();
            for (Control c : children) {
                c.dispose();
            }
        }
    }

    /**
     * Listen to mouse actions on the model source tree.
     */
    private class ModelSourcesTreeMouseListener extends MouseAdapter {

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
                    || !isWidgetReady()) {
                return;
            }

            // get the tree item that was clicked on ...
            Point point = new Point(event.x, event.y);

            final TreeItem userClickedTreeItem = modelSourceTree.getItem(point);
            if (userClickedTreeItem == null
                    || userClickedTreeItem.isDisposed()) {
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
            }
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
                if (isWidgetReady()) {
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
    public boolean isWidgetReady() {
        boolean isReady = false;

        if (modelSourceTreeViewer != null && modelSourceTree != null
                && modelSourceTreeViewer.getTree() != null
                && !modelSourceTreeViewer.getTree().isDisposed()) {
            isReady = true;
        }

        return isReady;
    }

    /**
     * The method to reset controls and state variables to an initial state.
     */
    public void clearAllResources() {

        if (isWidgetReady()) {
            modelSourceTree.clearAll(true);
            fieldPlaneChooserControl.clearFieldPlanePairControls();
            hideSupportResources();
            currentModelFamily = null;
            modelSourceTreeViewer.refresh();

            matrixEditor.refresh();

        }
    }

    /**
     * Hide any supporting resources.
     * 
     * Currently only the color bar needs to be hidden.
     */
    private void hideSupportResources() {

        List<D2DColorBarResource> colorBarList = matrixEditor
                .getActiveDisplayPane().getDescriptor().getResourceList()
                .getResourcesByTypeAsType(D2DColorBarResource.class);
        /* only one color bar in any given editor. */
        if (!colorBarList.isEmpty()) {
            D2DColorBarResource cbr = colorBarList.get(0);
            cbr.getProperties().setVisible(false);
        }

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
        if (matrixEditor != null && matrixEditor.getEditorSite() != null
                && matrixEditor.getEditorSite().getPart() != null) {
            matrixEditor.getEditorSite().getPart().setFocus();
        }
    }

    /**
     * Here's how we control what items are displayed in the tree.
     */
    private class ModelSourcesTreeColumnLabelProvider
            extends ColumnLabelProvider {

        public Font getFont(Object element) {
            Font f = SWTResourceManager.getFont("courier new", 10, SWT.BOLD);
            return f;
        }

        public Image getImage(Object element) {
            Image image = null;
            if (element instanceof ModelSourceKind) {
                image = EnsembleToolImageStore.DOT_IMG;
            }
            return image;
        }

        public String getText(Object element) {
            String nodeLabel = null;
            ModelSourceKind src = null;
            if (element instanceof ModelSourceKind) {
                src = (ModelSourceKind) element;
                nodeLabel = src.getModelName();
            }
            return spacerPadding + nodeLabel;
        }
    }

    /**
     * This is the content provider for the model sources tree.
     */
    private class ModelSourcesTreeContentProvider
            implements ITreeContentProvider {

        @Override
        public void inputChanged(Viewer viewer, Object oldInput,
                Object newInput) {
            modelSourceRootContainerScrolledComp.layout();
        }

        /**
         * All top-level elements in the tree are model sources obtained from
         * the loaded model family.
         */
        @Override
        public Object[] getElements(Object inputElement) {

            Object[] members = null;

            if (currentModelFamily != null) {
                List<ModelSourceKind> sources = currentModelFamily.getSources();
                members = new Object[sources.size()];
                int i = 0;
                for (ModelSourceKind src : sources) {
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
    private void select(final TreeItem treeItem) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (treeItem == null || treeItem.isDisposed()
                        || !isWidgetReady()) {
                    return;
                }
                if (treeItem.getData() instanceof ModelSourceKind) {

                    getShell().setCursor(EnsembleToolViewer.getWaitCursor());
                    List<AbstractResourceHolder> rscHolderList = EnsembleTool
                            .getInstance().getResourceList();

                    if (rscHolderList != null) {
                        modelSourceTree.select(treeItem);
                        ModelSourceKind currSrc = (ModelSourceKind) treeItem
                                .getData();
                        if (lastSelectedSource != null
                                && lastSelectedSource == currSrc) {
                            return;
                        }

                        refreshDisplayBySelectedSource(currSrc, rscHolderList);
                        lastSelectedSource = currSrc;

                        getShell().setCursor(
                                EnsembleToolViewer.getNormalCursor());

                        modelSourceTreeViewer.refresh();
                        getShell().setCursor(
                                EnsembleToolViewer.getNormalCursor());
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
     * @param rscHolderList
     *            the map of resources to hide.
     */
    private void hideAllResources(
            final List<AbstractResourceHolder> rscHolderList) {

        if (rscHolderList == null || rscHolderList.isEmpty()) {
            return;
        }

        for (AbstractResourceHolder arh : rscHolderList) {
            if (arh.isIndivdualProduct()) {
                arh.getRsc().getProperties().setVisible(false);
                arh.getRsc().issueRefresh();
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
     * @param rscHolderList
     *            the map which contains the resources loaded into the matrix
     *            editor.
     */
    public void updateControls() {

        List<AbstractResourceHolder> rscHolderList = EnsembleTool.getInstance()
                .getResourceList();
        if (rscHolderList != null && !rscHolderList.isEmpty()
                && fieldPlaneChooserControl != null
                && !fieldPlaneChooserControl.isDisposed()) {
            fieldPlaneChooserControl.updateControls(rscHolderList);
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
     * @param rscHolderList
     *            the map which contains the resources loaded into the matrix
     *            editor.
     */
    public void refreshDisplayBySelectedSource(ModelSourceKind selectedSrc,
            List<AbstractResourceHolder> rscHolderList) {

        hideAllResources(rscHolderList);
        updateElementsBySelectedSource(selectedSrc, rscHolderList);
        EnsembleTool.getInstance().refreshEditor();

    }

    /**
     * Show the field/plane pairs that are currently asserted for the currently
     * selected Source.
     * 
     * @param ensembleResourcesMap
     */
    private void updateElementsBySelectedSource(final ModelSourceKind currSrc,
            final List<AbstractResourceHolder> rscHolderList) {

        if (rscHolderList == null || rscHolderList.isEmpty()) {
            return;
        }

        String currSrcName = currSrc.getModelName();
        List<FieldPlanePair> elements = fieldPlaneChooserControl
                .getActiveFieldPlanePairs();

        RequestableResourceMetadata rrmd = null;
        String fieldAbbrev = null;
        String plane = null;

        for (AbstractResourceHolder arh : rscHolderList) {

            if (arh.isIndivdualProduct() == false) {
                continue;
            }
            /*
             * Only look at resources having the same Source as the selected
             * Source
             */
            if (arh.getRsc().getName().trim().startsWith(currSrcName.trim())) {
                for (FieldPlanePair e : elements) {
                    if (e.isResourceVisible()) {
                        if (arh.getRsc()
                                .getResourceData() instanceof AbstractRequestableResourceData) {
                            AbstractRequestableResourceData ard = (AbstractRequestableResourceData) arh
                                    .getRsc().getResourceData();
                            rrmd = new RequestableResourceMetadata(ard);
                            fieldAbbrev = rrmd.getFieldAbbrev();
                            plane = rrmd.getPlane();
                            if (e.getFieldAbbrev().equals(fieldAbbrev)
                                    && e.getPlane().equals(plane)) {
                                arh.getRsc().getProperties().setVisible(true);
                                arh.getRsc().issueRefresh();
                            }
                        }
                    }
                }
            }
        }

    }

    /**
     * Show the Elements (field/plane pairs) that are currently asserted for the
     * currently selected Source.
     */
    private void updateChangeInElementVisibility(
            final ModelSourceKind selectedSrc,
            final FieldPlanePair changedElement) {

        final List<AbstractResourceHolder> rscHolderList = EnsembleTool
                .getInstance().getResourceList();

        if (rscHolderList == null || rscHolderList.isEmpty()) {
            return;
        }

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                String currSrcName = selectedSrc.getModelName();

                RequestableResourceMetadata rrmd = null;
                String fieldAbbrev = null;
                String plane = null;
                String currName = null;
                for (AbstractResourceHolder arh : rscHolderList) {

                    if (arh.isIndivdualProduct() == false) {
                        continue;
                    }

                    currName = arh.getRsc().getName();

                    /*
                     * Only look at resources having the same Source as the
                     * selected Source
                     */
                    if (currName.trim().startsWith(currSrcName.trim())) {
                        if (arh.getRsc()
                                .getResourceData() instanceof AbstractRequestableResourceData) {
                            AbstractRequestableResourceData ard = (AbstractRequestableResourceData) arh
                                    .getRsc().getResourceData();

                            rrmd = new RequestableResourceMetadata(ard);
                            fieldAbbrev = rrmd.getFieldAbbrev();
                            plane = rrmd.getPlane();

                            if (changedElement.getFieldAbbrev()
                                    .equals(fieldAbbrev)
                                    && changedElement.getPlane()
                                            .equals(plane)) {

                                arh.getRsc().getProperties().setVisible(
                                        changedElement.isResourceVisible());

                                if (arh.getRsc().hasCapability(
                                        DisplayTypeCapability.class)) {
                                    DisplayType dt = arh.getRsc()
                                            .getCapability(
                                                    DisplayTypeCapability.class)
                                            .getDisplayType();
                                    if (dt == DisplayType.IMAGE) {
                                        if (arh.getRsc().getProperties()
                                                .isVisible()) {
                                            setColorBarVisible(true);
                                        } else {
                                            setColorBarVisible(false);
                                        }
                                    }
                                    arh.getRsc().issueRefresh();
                                }
                            }
                        }
                    }
                }
                EnsembleTool.getInstance().refreshEditor();
            }
        });

    }

    /**
     * Set whether the color bar is visible or not.
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

    /**
     * Reinitializes this entire control with a newly selected model family.
     */
    @Override
    public void addModelFamily(final ResolvedModelFamily modelFamily) {

        boolean proceed = false;
        if (modelSourceTree.getItemCount() > 1) {
            proceed = MessageDialog.openConfirm(
                    matrixEditor.getActiveDisplayPane().getDisplay()
                            .getActiveShell(),
                    "Model family verification",
                    "Loading this model family will remove any already loaded family.\n"
                            + "Okay to proceed?");
        } else {
            proceed = true;
        }
        if (proceed) {
            clearAllResources();

            currentModelFamily = modelFamily;

            createModelFamilyControls(modelFamily);

            /* reset the previously selected resource variable */
            lastSelectedSource = null;
        }
    }

    /**
     * Load those resources of a certain field/plane pair across all model
     * sources.
     */
    @Override
    public void loadResources(FieldPlanePair fpp) {

        /*
         * If this is the first resource being loaded then use the default
         * number of frames from the map descriptor.
         */
        boolean loadFrameCountDefault = false;
        /*
         * If this is the first field/plane pair to become active the select the
         * first model source in the model source tree.
         */
        if (fieldPlaneChooserControl.getActiveFieldPlanePairCount() == 1) {
            if (modelSourceTree != null
                    && modelSourceTree.getItems().length > 0) {
                loadFrameCountDefault = true;
                modelSourceTree.select(modelSourceTree.getItem(0));
                lastSelectedSource = (ModelSourceKind) modelSourceTree
                        .getItem(0).getData();
            }
        }
        /*
         * Load the resources for the given field/plane pair for all the models
         * sources in the current resolved model family.
         */
        currentModelFamily.loadModelsByFieldPlanePairs(matrixEditor, fpp,
                loadFrameCountDefault);

    }

    /**
     * Unload those resources associated with a field/plane pair. This happens
     * when the field/plane pair control widget is deactivated (i.e. moved from
     * the upper composite/activated fpp list to the lower composite
     * not-activated list).
     * 
     * TODO: This will be associated with deactivating a field/plane pair
     * control.
     * 
     * @param fpp
     *            the resources associated with the given field/plane pair will
     *            be unloaded.
     */

    @Override
    public void unloadResources(FieldPlanePair fpp) {
        for (AbstractVizResource<?, ?> rsc : currentModelFamily
                .getAssociatedRscSet(fpp)) {
            removeResourceByResourcePair(rsc);
        }
        matrixEditor.refresh();
    }

    private void removeResourceByResourcePair(AbstractVizResource<?, ?> rsc) {

        EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(matrixEditor);
        if (toolLayer != null) {
            toolLayer.getResourceList().removeRsc(rsc);
            toolLayer.issueRefresh();
        }
    }

    /**
     * Populate the model source tree and create the field/plane pair
     * controllers.
     * 
     * @param modelFamily
     */
    private void createModelFamilyControls(
            final ResolvedModelFamily modelFamily) {

        fieldPlaneChooserControl.addModelFamily(modelFamily);

        modelSourceTreeViewer.setInput(currentModelFamily);
        modelSourceTreeViewer.refresh();
        modelSourceTreeViewer.expandAll();

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
        if (currItem.getData() instanceof ModelSourceKind) {
            ModelSourceKind currSrc = (ModelSourceKind) currItem.getData();
            updateChangeInElementVisibility(currSrc, e);
        }
    }

    @Override
    public ModelSourceKind getSelected() {

        if (lastSelectedSource == null) {
            if (isWidgetReady() && modelSourceTree.getItemCount() > 0) {
                lastSelectedSource = (ModelSourceKind) modelSourceTree
                        .getItem(0).getData();
                modelSourceTree.select(modelSourceTree.getItem(0));
            }
        }

        return lastSelectedSource;
    }

    /**
     * Set the enabled/disabled state of all of the gui components associated
     * with this control.
     * 
     * @param enabled
     *            the boolean enabled/disabled flag
     */
    public void setEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                if (isWidgetReady()) {
                    modelSourceTree.setEnabled(enabled);
                    if (fieldPlaneChooserControl != null
                            && !fieldPlaneChooserControl.isDisposed()) {
                        fieldPlaneChooserControl.setEditable(enabled);
                    }
                }
            }
        });
    }

    /**
     * When a model source is selected from the model source tree, update the
     * active editor with that source's resources and return focus to the active
     * editor.
     */
    private class ModelSourceTreeSelectionListener
            implements ISelectionChangedListener {

        @Override
        public void selectionChanged(SelectionChangedEvent event) {

            IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();

            /*
             * This check is here as during swapping we want to make sure the
             * editor hasn't changed out from under us.
             */
            if (editor != null && EnsembleTool.isMatrixEditor(editor)) {

                getShell().setCursor(EnsembleToolViewer.getWaitCursor());
                TreeItem[] tis = modelSourceTree.getSelection();

                if (tis != null && tis.length > 0 && tis[0] != null
                        && !tis[0].isDisposed()
                        && tis[0].getData() instanceof ModelSourceKind) {

                    select(tis[0]);

                }
                getShell().setCursor(EnsembleToolViewer.getNormalCursor());
                giveEditorFocus();
            }
        }
    }

    /**
     * The navigation operation is one of: UP_ARROW, DOWN_ARROW, RIGHT_ARROW, or
     * LEFT_ARROW. It is generated by the ensemble tool plugin extensions.
     * Delegation method for the matrix navigation inner tool.
     * 
     * @param operation
     */
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
        default:
            break;
        }
    }

    /**
     * Handle up and down arrows navigation in the model source tree.
     * 
     * @param operation
     */
    private void navigateModelSources(MatrixNavigationOperation operation) {

        if (matrixEditor == null) {
            return;
        }
        if (modelSourceTree != null && modelSourceTree.getItemCount() > 1
                && modelSourceTree.getSelection().length == 1) {
            /**
             * Programmatically move up or down one model source in the tree.
             */
            TreeItem treeItem = (TreeItem) modelSourceTree.getSelection()[0];
            if (treeItem != null) {
                int currentTreeItemCount = modelSourceTree.indexOf(treeItem);
                if (operation == MatrixNavigationOperation.UP_MODEL_SOURCE) {
                    if (currentTreeItemCount > 0) {
                        select(modelSourceTree
                                .getItem(currentTreeItemCount - 1));
                        fieldPlaneChooserControl
                                .disableEmptyFieldPlanePairControls();
                    }
                }
                if (operation == MatrixNavigationOperation.DOWN_MODEL_SOURCE) {
                    if (currentTreeItemCount < modelSourceTree.getItemCount()
                            - 1) {
                        select(modelSourceTree
                                .getItem(currentTreeItemCount + 1));
                        fieldPlaneChooserControl
                                .disableEmptyFieldPlanePairControls();
                    }
                }
            }
        }

    }

    /**
     * Handle left and right arrows navigation to move back and forward plan
     * view editor frames.
     * 
     * @param operation
     */
    private void navigateFrames(MatrixNavigationOperation operation) {

        if (matrixEditor == null) {
            return;
        }

        matrixEditor.getLoopProperties().setLooping(false);

        int currentFrame = matrixEditor.getActiveDisplayPane().getDescriptor()
                .getFramesInfo().getFrameIndex();
        int currentMaxFrame = matrixEditor.getActiveDisplayPane()
                .getDescriptor().getFramesInfo().getFrameCount();
        /* normalize for zero-offset */
        currentFrame += 1;
        if (operation == MatrixNavigationOperation.LEFT_FRAME) {
            if (currentFrame == 1) {
                matrixEditor.getActiveDisplayPane().getDescriptor()
                        .getFrameCoordinator().changeFrame(
                                IFrameCoordinator.FrameChangeOperation.LAST,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            } else {
                matrixEditor.getActiveDisplayPane().getDescriptor()
                        .getFrameCoordinator().changeFrame(
                                IFrameCoordinator.FrameChangeOperation.PREVIOUS,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            }
        }
        if (operation == MatrixNavigationOperation.RIGHT_FRAME) {

            if (currentFrame < currentMaxFrame) {
                matrixEditor.getActiveDisplayPane().getDescriptor()
                        .getFrameCoordinator().changeFrame(
                                IFrameCoordinator.FrameChangeOperation.NEXT,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            } else if (currentFrame == currentMaxFrame) {
                matrixEditor.getActiveDisplayPane().getDescriptor()
                        .getFrameCoordinator().changeFrame(
                                IFrameCoordinator.FrameChangeOperation.FIRST,
                                IFrameCoordinator.FrameChangeMode.TIME_ONLY);
            }
        }
    }

    /**
     * Called when a frameChange event is just completed. Assert the
     * step-frame-complete flag.
     * 
     * @param framesInfo
     */
    public void frameChanged(FramesInfo framesInfo) {
        fieldPlaneChooserControl.disableEmptyFieldPlanePairControls();
    }

    /**
     * Returns the time string of the first found visible resource in the active
     * editor. The time string is the same one found in the resource's legend.
     * 
     * @return The legend time string.
     */
    public String getActiveRscTime() {
        String rscTime = "";
        IDisplayPane pane = null;

        if (matrixEditor != null) {
            pane = matrixEditor.getActiveDisplayPane();
            ResourceList rscList = pane.getDescriptor().getResourceList();

            for (ResourcePair rp : rscList) {
                if (rp == null) {
                    continue;
                }
                GridNameGenerator.IGridNameResource currRsc = null;
                if (rp.getResource() instanceof GridNameGenerator.IGridNameResource) {
                    currRsc = (GridNameGenerator.IGridNameResource) rp
                            .getResource();
                    if (rp.getResource().getProperties().isVisible()) {
                        LegendParameters legendParams = currRsc
                                .getLegendParameters();
                        if (legendParams == null) {
                            rscTime = "";
                            continue;
                        } else if (legendParams.dataTime == null) {
                            rscTime = "";
                            continue;
                        } else {
                            rscTime = legendParams.dataTime.getLegendString();
                            break;
                        }
                    }
                }
            }
        }

        return rscTime;
    }

    /**
     * Opening this dialog allows a user to choose/load a model family.
     */
    public void openFamilyLoaderDialog() {

        modelFamilyDialog.setBlockOnOpen(true);
        if (modelFamilyDialog.open() == Window.OK) {
            modelFamilyDialog.close();
        }
    }

    /**
     * Given a field/plane pair and an expected capability instance, walk
     * through all associated model family resources (i.e. those that share the
     * same field/plane pairs) and set the given capability on those resources.
     * 
     * @param matchToTheseRscs
     *            the resources to match to
     * @param capabilityClass
     *            the capability to match
     * @return wasColorMatched
     */
    @Override
    public void matchProductCapabilityByFieldPlanePair(
            final FieldPlanePairControl fppc, final Object object) {

        final FieldPlanePair fpp = fppc.getFieldPlanePair();

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (object instanceof ColorableCapability) {
                    ColorableCapability cc = (ColorableCapability) object;
                    RGB rgb = cc.getColor();
                    for (AbstractVizResource<?, ?> r : currentModelFamily
                            .getAssociatedRscSet(fpp)) {
                        if (r.hasCapability(ColorableCapability.class)) {
                            if (r.getCapability(ColorableCapability.class)
                                    .getColor() != rgb) {
                                r.getResourceData().removeChangeListener(fppc);
                                r.getCapability(ColorableCapability.class)
                                        .setColor(rgb);
                                r.issueRefresh();
                                r.getResourceData().addChangeListener(fppc);
                            }
                        }
                    }
                }
                if (object instanceof DensityCapability) {
                    DensityCapability dc = (DensityCapability) object;
                    Double density = dc.getDensity();
                    for (AbstractVizResource<?, ?> r : currentModelFamily
                            .getAssociatedRscSet(fpp)) {
                        if (r.hasCapability(DensityCapability.class)) {
                            if (r.getCapability(DensityCapability.class)
                                    .getDensity() != density) {
                                r.getResourceData().removeChangeListener(fppc);
                                r.getCapability(DensityCapability.class)
                                        .setDensity(density);
                                r.issueRefresh();
                                r.getResourceData().addChangeListener(fppc);
                            }
                        }
                    }
                }

                if (object instanceof OutlineCapability) {
                    OutlineCapability oc = (OutlineCapability) object;
                    LineStyle style = oc.getLineStyle();
                    int width = oc.getOutlineWidth();
                    for (AbstractVizResource<?, ?> r : currentModelFamily
                            .getAssociatedRscSet(fpp)) {
                        if (r.hasCapability(OutlineCapability.class)) {
                            if (!(r.getCapability(OutlineCapability.class)
                                    .getLineStyle().equals(style))) {
                                r.getResourceData().removeChangeListener(fppc);
                                r.getCapability(OutlineCapability.class)
                                        .setLineStyle(style);
                                r.issueRefresh();
                                r.getResourceData().addChangeListener(fppc);
                            }
                            if (!(r.getCapability(OutlineCapability.class)
                                    .getOutlineWidth() != width)) {
                                r.getResourceData().removeChangeListener(fppc);
                                r.getCapability(OutlineCapability.class)
                                        .setOutlineWidth(width);
                                r.issueRefresh();
                                r.getResourceData().addChangeListener(fppc);
                            }
                        }
                    }
                }

                if (object instanceof MagnificationCapability) {
                    MagnificationCapability oc = (MagnificationCapability) object;
                    Double mag = oc.getMagnification();
                    for (AbstractVizResource<?, ?> r : currentModelFamily
                            .getAssociatedRscSet(fpp)) {
                        if (r.hasCapability(MagnificationCapability.class)) {
                            if (!(r.getCapability(MagnificationCapability.class)
                                    .getMagnification() == mag)) {
                                r.getResourceData().removeChangeListener(fppc);
                                r.getCapability(MagnificationCapability.class)
                                        .setMagnification(mag);
                                r.issueRefresh();
                                r.getResourceData().addChangeListener(fppc);
                            }
                        }
                    }
                }

            }
        });
    }

    /**
     * Given a resource and a field/plane pair, walk through all associated
     * resources and set their capability of color, density, line width, line
     * style, and magnification equal to the source's respective capability.
     * 
     * @param matchToTheseRscs
     *            the resources to match to
     * @param capabilityClass
     *            the capability to match
     * @return wasColorMatched
     */
    @Override
    public void matchProductCapabilityByResource(
            final AbstractVizResource<?, ?> matchToRsc,
            final Set<AbstractVizResource<?, ?>> assocRscs,
            final FieldPlanePairControl fppc) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                RGB rgb = null;
                Double density = null;
                Double mag = null;
                LineStyle lineStyle = null;
                int lineWidth = 0;

                for (AbstractVizResource<?, ?> currRsc : assocRscs) {

                    currRsc.getResourceData().removeChangeListener(fppc);
                    if (matchToRsc.hasCapability(ColorableCapability.class)
                            && currRsc
                                    .hasCapability(ColorableCapability.class)) {
                        rgb = matchToRsc
                                .getCapability(ColorableCapability.class)
                                .getColor();

                        if (rgb != null) {
                            currRsc.getCapability(ColorableCapability.class)
                                    .setColor(rgb);
                        }
                    }

                    if (matchToRsc.hasCapability(DensityCapability.class)
                            && currRsc.hasCapability(DensityCapability.class)) {
                        density = matchToRsc
                                .getCapability(DensityCapability.class)
                                .getDensity();
                        if (density != null) {
                            currRsc.getCapability(DensityCapability.class)
                                    .setDensity(density);
                        }
                    }

                    if (matchToRsc.hasCapability(OutlineCapability.class)
                            && currRsc.hasCapability(OutlineCapability.class)) {
                        lineStyle = matchToRsc
                                .getCapability(OutlineCapability.class)
                                .getLineStyle();
                        if (lineStyle != null) {
                            currRsc.getCapability(OutlineCapability.class)
                                    .setLineStyle(lineStyle);
                        }
                        lineWidth = matchToRsc
                                .getCapability(OutlineCapability.class)
                                .getOutlineWidth();
                        currRsc.getCapability(OutlineCapability.class)
                                .setOutlineWidth(lineWidth);
                    }

                    if (matchToRsc.hasCapability(MagnificationCapability.class)
                            && currRsc.hasCapability(
                                    MagnificationCapability.class)) {
                        mag = matchToRsc
                                .getCapability(MagnificationCapability.class)
                                .getMagnification();
                        if (mag != null) {
                            currRsc.getCapability(MagnificationCapability.class)
                                    .setMagnification(mag);
                        }
                    }

                    currRsc.getResourceData().addChangeListener(fppc);
                    currRsc.issueRefresh();
                }
            }
        });
    }

    /**
     * Given a field/plane pair, set all associated model family resources (i.e.
     * those that share the same field/plane pairs) to have the same
     * capabilities as the first on those resources.
     * 
     * @param makeTheseRscsMatchDefault
     *            the resources to match to
     * @param capabilityClass
     *            the capability to match
     */
    @Override
    public void matchDefaultProductCapability(FieldPlanePairControl fppc) {

        Set<AbstractVizResource<?, ?>> assocRscs = currentModelFamily
                .getAssociatedRscSet(fppc.getFieldPlanePair());

        /*
         * There should always be at least one resource in the associated
         * resources set so this next test is gratuitously defensive.
         */
        if (assocRscs.isEmpty() || matrixEditor == null) {
            return;
        }

        /*
         * The first resource found in the field/plane pair resource set can be
         * used to match resource capabilities.
         */
        Iterator<AbstractVizResource<?, ?>> rscSetIter = assocRscs.iterator();
        AbstractVizResource<?, ?> matchToRsc = rscSetIter.next();

        if (matchToRsc != null) {
            matchProductCapabilityByResource(matchToRsc, assocRscs, fppc);
        }

    }

    @Override
    public ResolvedModelFamily getActiveModelFamily() {
        return currentModelFamily;
    }

    /**
     * Is the given resource in the currently active model source?
     */
    @Override
    public boolean isResourceInVisibleSource(AbstractVizResource<?, ?> rsc) {
        boolean isOfVisibleSource = false;
        ModelSourceKind ms = getSelected();
        if (ms != null) {
            if (rsc.getName().contains(ms.getModelName())) {
                isOfVisibleSource = true;
            }
        }
        return isOfVisibleSource;
    }

    class IgnoreFocusListener extends MouseAdapter {

        @Override
        public void mouseUp(MouseEvent e) {
            giveEditorFocus();
        }

    }

}
