package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;

/**
 * This class represents the analog to the Volume Browser but to be used instead
 * by the Matrix Navigator. It allows the user to choose model families and
 * model sources to load into the Matrix navigator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 08, 2015  12371      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class ModelFamilyDialog extends CaveJFACEDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ModelFamilyDialog.class);

    private final static IPathManager pathMgr = PathManagerFactory
            .getPathManager();

    private Composite rootComposite = null;

    /**
     * Main dialog area composite
     */
    private Composite mainRootComposite = null;

    private Composite modelSourcesRootComposite = null;

    private Button loadBtn;

    private Button closeBtn;

    private ScrolledComposite modelSourcesScrolledComposite = null;

    private ScrolledComposite modelFamiliesScrolledComposite = null;

    private ScrolledComposite elementSetScrolledComposite = null;

    Composite elementSetsRootComposite = null;

    private Tree modelSourcesTree = null;

    private TreeViewer modelSourcesTreeViewer = null;

    private ToolItem newElementSetToolItem = null;

    private Tree modelFamiliesTree = null;

    private TreeViewer modelFamiliesTreeViewer = null;

    private Tree elementSetTree = null;

    private TreeViewer elementSetTreeViewer = null;

    private ToolBar modelFamiliesToolBar = null;

    private ToolItem clearSelectedModelFamiliesToolItem = null;

    private ToolItem clearAllModelFamiliesToolItem = null;

    private Composite modelFamiliesRootComposite = null;

    private FieldPlanePairSet currElementSet = null;

    private final FieldPlanePairSet emptyElementSet = new FieldPlanePairSet();

    final private int minimumTreeHeight = 180;

    final private int minimumTreeWidth = 350;

    final private int minimumSourcesTreeWidth = 140;

    private ModelFamily currFamily = null;

    private IModelFamilyListener familyLoadListener = null;

    private List<ModelSources> notAvailableOnServer = null;

    /**
     * Data Catalog Entry
     */
    private IDataCatalogEntry catalogEntry = null;

    /**
     * Create the dialog.
     * 
     * @param parent
     * @param style
     */
    public ModelFamilyDialog(Shell parent, IModelFamilyListener esl) {
        super(parent);

        setShellStyle(SWT.RESIZE | SWT.APPLICATION_MODAL);
        setBlockOnOpen(true);

        familyLoadListener = esl;
        notAvailableOnServer = new ArrayList<>();
    }

    /**
     * Create contents of the dialog.
     * 
     * @param parent
     */
    @Override
    protected Control createDialogArea(Composite parent) {

        rootComposite = parent;

        createMainDialogArea();

        createModelFamiliesArea();

        createModelSourcesArea();

        return parent;
    }

    /**
     * The main dialog area is rooted with a composite that must use a
     * GridLayout.
     * 
     * @param parent
     */
    private void createMainDialogArea() {

        mainRootComposite = new Composite(rootComposite, SWT.NONE);
        mainRootComposite.setLayout(new GridLayout(10, false));
        mainRootComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1));

    }

    /**
     * The Sources area has a header with a title. The main section of this area
     * is the Sources list.
     */
    private void createModelSourcesArea() {

        createModelSourcesRoot();

        createModelSourcesHeader();

        createModelSourcesMainBody();

        createModelSourcesTreeComponents();

    }

    /**
     * Configures the layout and layout data for the model sources root
     * composite.
     */
    private void createModelSourcesRoot() {
        modelSourcesRootComposite = new Composite(mainRootComposite, SWT.BORDER);
        modelSourcesRootComposite.setLayout(new GridLayout(1, false));
        GridData sourcesRootComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                false, true, 3, 1);
        modelSourcesRootComposite.setLayoutData(sourcesRootComposite_gd);

    }

    /**
     * Defines the contents of the title section of the model source component.
     */
    private void createModelSourcesHeader() {

        Composite sourcesHeaderComposite = new Composite(
                modelSourcesRootComposite, SWT.BORDER);
        sourcesHeaderComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
                false, false, 2, 1));
        GridLayout sourcesHeaderComposite_gl = new GridLayout(2, false);
        sourcesHeaderComposite_gl.marginHeight = 3;
        sourcesHeaderComposite_gl.marginWidth = 5;
        sourcesHeaderComposite_gl.marginLeft = 0;
        sourcesHeaderComposite_gl.marginRight = 0;
        sourcesHeaderComposite_gl.marginTop = 0;
        sourcesHeaderComposite_gl.marginBottom = 0;
        sourcesHeaderComposite_gl.horizontalSpacing = 0;
        sourcesHeaderComposite.setLayout(sourcesHeaderComposite_gl);
        sourcesHeaderComposite.setBackground(GlobalColor
                .get(GlobalColor.PALE_DULL_AZURE));

        Label sourcesHeaderLbl = new Label(sourcesHeaderComposite, SWT.NONE);
        sourcesHeaderLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                false, 1, 1));
        sourcesHeaderLbl.setBackground(GlobalColor
                .get(GlobalColor.LIGHTER_GRAY));
        sourcesHeaderLbl.setAlignment(SWT.CENTER);
        sourcesHeaderLbl.setText("Sources");

    }

    /**
     * Defines the scrolling composite of the model source component.
     */
    private void createModelSourcesMainBody() {
        modelSourcesScrolledComposite = new ScrolledComposite(
                modelSourcesRootComposite, SWT.BORDER | SWT.H_SCROLL
                        | SWT.V_SCROLL);
        GridLayout sourcesScrolledComposite_gl = new GridLayout();
        modelSourcesScrolledComposite.setLayout(sourcesScrolledComposite_gl);
        modelSourcesScrolledComposite.setMinHeight(minimumTreeHeight);
        modelSourcesScrolledComposite.setMinWidth(minimumSourcesTreeWidth);
        GridData sourcesScrolledComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 1);
        modelSourcesScrolledComposite
                .setLayoutData(sourcesScrolledComposite_gd);

        modelSourcesScrolledComposite.setExpandHorizontal(true);
        modelSourcesScrolledComposite.setExpandVertical(true);
    }

    /**
     * Defines the contents of the tree section of the model source component.
     */
    private void createModelSourcesTreeComponents() {

        modelSourcesTree = new Tree(modelSourcesScrolledComposite, SWT.BORDER
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
        modelSourcesTree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1));
        modelSourcesTree.setLinesVisible(false);
        modelSourcesTree.setHeaderVisible(false);

        modelSourcesTreeViewer = new TreeViewer(modelSourcesTree);

        createSourcesColumns(modelSourcesTreeViewer);
        modelSourcesTreeViewer
                .setContentProvider(new ModelSourcesTreeContentProvider());

        modelSourcesScrolledComposite.setContent(modelSourcesTree);
        modelSourcesTreeViewer.setInput(new Object());
        modelSourcesTreeViewer.refresh();
        modelSourcesTree.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                allowOnlyRelevantSelections();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

        });

    }

    /**
     * Check to see if any of the model sources are not available on the server
     * and add them to a class accessible list named
     * <code>notAvailableOnServer</code>.
     * 
     * The methods which use this "not avaible on server" list include the
     * codependent <code>onlyAllowRelevantSelections</code> and
     * <code>enableRelevantModelSources</code> methods.
     */
    private void checkForProductAvailability() {

        notAvailableOnServer.clear();
        TreeItem[] treeItems = modelFamiliesTree.getSelection();
        if (treeItems != null && treeItems.length > 0) {
            if (treeItems[0].getData() instanceof ModelFamilyType) {
                return;
            }
            if (treeItems[0].getData() instanceof ModelFamilySubType) {
                ModelFamilySubType mfst = (ModelFamilySubType) treeItems[0]
                        .getData();
                ModelFamilyDefinitions[] modelDefs = ModelFamilyDefinitions
                        .values();
                ModelFamilyDefinitions foundModelDef = null;
                for (ModelFamilyDefinitions modelDef : modelDefs) {
                    if (modelDef.getFamilySubType() == mfst) {
                        foundModelDef = modelDef;
                        break;
                    }
                }

                List<ModelSources> relevantModelSources = ModelFamilyDefinitions
                        .getModelSources(foundModelDef.getFamilyType(),
                                foundModelDef.getFamilySubType());

                SelectedData selectedData = null;
                ModelSources currModelSrc = null;
                treeItems = modelSourcesTree.getItems();
                for (TreeItem ti : treeItems) {
                    if (ti.getData() instanceof ModelSources) {
                        currModelSrc = (ModelSources) ti.getData();
                        if (relevantModelSources.contains(currModelSrc)) {

                            // Example SelectedData:
                            // ... sourcesText: GFS40
                            // ... sourcesKey: GFS212
                            // ... fieldsText: Height
                            // ... fieldsKey: GH
                            // ... planesText: 500MB
                            // ... planesKey: 500MB
                            // ... uniqueKey: GFS212::GH::500MB

                            /**
                             * TODO: We may need to come up with a better way to
                             * check for product availability on the server.
                             */

                            selectedData = new SelectedData(
                                    currModelSrc.getModelName(),
                                    currModelSrc.getModelId(), "Height", "GH",
                                    "500MB", "500MB", currModelSrc.getModelId()
                                            + "::GH::500MB");

                            catalogEntry = DataCatalogManager
                                    .getDataCatalogManager()
                                    .getDataCatalogEntry(selectedData);

                            if (catalogEntry == null) {
                                notAvailableOnServer.add(currModelSrc);
                                continue;
                            }

                        }
                    }
                }
            }
        }

    }

    /**
     * The Element Sets area has a header with a title and a simple Add button.
     * The main section of this area is the Element Sets list.
     */
    private void createModelFamiliesArea() {

        createModelFamiliesRoot();

        createModelFamiliesHeader();

        createModelFamiliesMainBody();

    }

    /**
     * Configures the layout and layout data for the model families root
     * composite.
     */
    private void createModelFamiliesRoot() {

        modelFamiliesRootComposite = new Composite(mainRootComposite,
                SWT.BORDER);
        GridData modelFamiliesRootComposite_gd = new GridData(SWT.FILL,
                SWT.FILL, true, true, 7, 1);
        modelFamiliesRootComposite.setLayoutData(modelFamiliesRootComposite_gd);
        GridLayout modelFamiliesRootComposite_gl = new GridLayout(10, false);
        modelFamiliesRootComposite_gl.horizontalSpacing = 2;
        modelFamiliesRootComposite_gl.verticalSpacing = 3;
        modelFamiliesRootComposite.setLayout(modelFamiliesRootComposite_gl);

    }

    /**
     * Defines the contents of the title section of the model families
     * component.
     */
    private void createModelFamiliesHeader() {

        Composite modelFamiliesHeaderComposite = new Composite(
                modelFamiliesRootComposite, SWT.BORDER);
        modelFamiliesHeaderComposite.setLayoutData(new GridData(SWT.FILL,
                SWT.TOP, false, false, 10, 1));
        GridLayout modelFamiliesHeaderComposite_gl = new GridLayout(1, false);
        modelFamiliesHeaderComposite_gl.marginHeight = 3;
        modelFamiliesHeaderComposite_gl.marginWidth = 6;
        modelFamiliesHeaderComposite.setLayout(modelFamiliesHeaderComposite_gl);

        modelFamiliesHeaderComposite.setBackground(GlobalColor
                .get(GlobalColor.PALE_DULL_AZURE));

        Label modelFamiliesHeaderLbl = new Label(modelFamiliesHeaderComposite,
                SWT.NONE);
        modelFamiliesHeaderLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, true, 1, 1));
        modelFamiliesHeaderLbl.setBackground(GlobalColor
                .get(GlobalColor.LIGHTER_GRAY));
        modelFamiliesHeaderLbl.setAlignment(SWT.CENTER);
        modelFamiliesHeaderLbl.setText("Model Families");

    }

    /**
     * The model families composite contains a model family tree selection
     * widget and an field/plane pairs tree selection widget.
     */
    private void createModelFamiliesMainBody() {

        createModelFamiliesTreeArea();

        createElementsSetTreeArea();

    }

    /**
     * Defines the contents of the tree section of the model families component.
     */
    private void createModelFamiliesTreeArea() {

        modelFamiliesScrolledComposite = new ScrolledComposite(
                modelFamiliesRootComposite, SWT.BORDER | SWT.H_SCROLL
                        | SWT.V_SCROLL);
        GridData modelFamiliesComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 6, 8);

        modelFamiliesScrolledComposite.setLayoutData(modelFamiliesComposite_gd);
        modelFamiliesScrolledComposite.setLayout(new GridLayout());
        modelFamiliesScrolledComposite.setMinHeight(minimumTreeHeight);
        modelFamiliesScrolledComposite.setMinWidth(minimumTreeWidth);

        modelFamiliesScrolledComposite.setExpandHorizontal(true);
        modelFamiliesScrolledComposite.setExpandVertical(true);

        modelFamiliesTree = new Tree(modelFamiliesScrolledComposite, SWT.BORDER
                | SWT.SINGLE);
        modelFamiliesTree.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, true,
                true, 1, 1));
        modelFamiliesTree.setLinesVisible(false);
        modelFamiliesTree.setHeaderVisible(false);

        modelFamiliesTree.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                doModelFamiliesTreeSelection();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

        });

        modelFamiliesTreeViewer = new TreeViewer(modelFamiliesTree);
        createModelFamiliesColumns(modelFamiliesTreeViewer);

        modelFamiliesTreeViewer
                .setSorter(new AlphaNumericModelFamiliesTreeSorter());
        modelFamiliesTreeViewer
                .setContentProvider(new ModelFamilyTreeContentProvider());
        modelFamiliesScrolledComposite.setContent(modelFamiliesTree);

        modelFamiliesTreeViewer.setInput(new Object());
        modelFamiliesTreeViewer.refresh();
        modelFamiliesTreeViewer.expandAll();
    }

    /**
     * Defines the contents of the tree section of the field/plane pairs
     * component.
     */
    private void createElementsSetTreeArea() {

        elementSetsRootComposite = new Composite(modelFamiliesRootComposite,
                SWT.BORDER);
        elementSetsRootComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
                true, true, 4, 8));
        elementSetsRootComposite.setBackground(GlobalColor
                .get(GlobalColor.GRAY));
        GridLayout elementSetsRootComposite_gl = new GridLayout(1, true);
        elementSetsRootComposite_gl.marginHeight = 0;
        elementSetsRootComposite_gl.marginTop = 0;
        elementSetsRootComposite_gl.marginLeft = 0;
        elementSetsRootComposite_gl.marginRight = 0;
        elementSetsRootComposite_gl.marginBottom = 0;
        elementSetsRootComposite_gl.marginWidth = 0;
        elementSetsRootComposite.setLayout(elementSetsRootComposite_gl);

        elementSetScrolledComposite = new ScrolledComposite(
                elementSetsRootComposite, SWT.H_SCROLL | SWT.V_SCROLL);
        GridData elementSetComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 1);
        elementSetScrolledComposite.setLayoutData(elementSetComposite_gd);
        GridLayout elementSetScrolledComposite_gl = new GridLayout(1, true);
        elementSetScrolledComposite_gl.marginHeight = 0;
        elementSetScrolledComposite_gl.marginWidth = 0;
        elementSetScrolledComposite.setLayout(elementSetScrolledComposite_gl);
        elementSetScrolledComposite.setMinWidth(160);
        elementSetScrolledComposite.setMinHeight(240);

        elementSetScrolledComposite.setExpandHorizontal(true);
        elementSetScrolledComposite.setExpandVertical(true);

        elementSetTree = new Tree(elementSetScrolledComposite, SWT.BORDER
                | SWT.MULTI | SWT.FULL_SELECTION);
        elementSetTree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1));
        elementSetTree.setLinesVisible(true);
        elementSetTree.setHeaderVisible(true);

        // genericTree.addMouseListener(new GenericTreeMouseListener());
        elementSetTreeViewer = new TreeViewer(elementSetTree);
        createElementSetColumns(elementSetTreeViewer);

        elementSetTreeViewer
                .setSorter(new AlphaNumericElementSetNodeTreeSorter());
        elementSetTreeViewer
                .setContentProvider(new ElementSetsTreeContentProvider());
        elementSetScrolledComposite.setContent(elementSetTree);
    }

    /**
     * Creates the column definitions for the model source tree. Currently there
     * is only one column in the tree.
     * 
     * @param modelSourceTreeViewer
     *            the tree viewer against which the columns are composed.
     */
    private void createSourcesColumns(TreeViewer modelSourceTreeViewer) {

        ColumnLabelProvider clpn = new ModelSourcesNameColumnLabelProvider();
        TreeViewerColumn familyName = new TreeViewerColumn(
                modelSourceTreeViewer, SWT.LEFT);
        familyName.getColumn().setWidth(145);
        familyName.getColumn().setMoveable(false);
        familyName.getColumn().setAlignment(SWT.LEFT);

        familyName.setLabelProvider(clpn);

    }

    /**
     * Creates the column definitions for the model families tree. Currently
     * there is only one column in the tree.
     * 
     * @param modelSourceTreeViewer
     *            the tree viewer against which the columns are composed.
     */
    private void createModelFamiliesColumns(TreeViewer modelFamiliesTreeViewer) {

        ColumnLabelProvider clpn = new ModelFamilyNameColumnLabelProvider();
        TreeViewerColumn familyName = new TreeViewerColumn(
                modelFamiliesTreeViewer, SWT.LEFT);
        familyName.getColumn().setWidth(235);
        familyName.getColumn().setMoveable(false);
        familyName.getColumn().setAlignment(SWT.LEFT);

        familyName.setLabelProvider(clpn);

    }

    /**
     * Creates the column definitions for the field/plane pairs tree. There are
     * two columns in the tree. One for a visibility icon, and the other for the
     * field/plane pair name.
     * 
     * @param modelSourceTreeViewer
     *            the tree viewer against which the columns are composed.
     */
    private void createElementSetColumns(TreeViewer fieldPlanePairsTreeViewer) {

        ColumnLabelProvider clpd = new ElementSetDummyColumnLabelProvider();
        TreeViewerColumn dummyColumn = new TreeViewerColumn(
                fieldPlanePairsTreeViewer, SWT.LEFT);
        dummyColumn.getColumn().setWidth(2);
        dummyColumn.getColumn().setMoveable(false);
        dummyColumn.getColumn().setAlignment(SWT.LEFT);
        dummyColumn.setLabelProvider(clpd);

        ColumnLabelProvider clpv = new ElementSetVisibilityColumnLabelProvider();
        TreeViewerColumn visibility = new TreeViewerColumn(
                fieldPlanePairsTreeViewer, SWT.CENTER);
        visibility.getColumn().setWidth(24);
        visibility.getColumn().setMoveable(false);
        visibility.getColumn().setAlignment(SWT.CENTER);
        visibility.getColumn().setImage(EnsembleToolImageStore.VISIBILITY_IMG);
        visibility.setLabelProvider(clpv);

        ColumnLabelProvider clpf = new ElementSetNameColumnLabelProvider();
        TreeViewerColumn fieldPlaneName = new TreeViewerColumn(
                fieldPlanePairsTreeViewer, SWT.LEFT);
        fieldPlaneName.getColumn().setWidth(140);
        fieldPlaneName.getColumn().setMoveable(false);
        fieldPlaneName.getColumn().setResizable(true);
        /* we only want the column header text centered, so pad with spaces */
        fieldPlaneName.getColumn().setText("            Plane - Field ");
        fieldPlaneName.setLabelProvider(clpf);

    }

    /**
     * When a model family is selected, this method fills the field/plane pairs
     * tree associated with that selected family.
     */
    private void doModelFamiliesTreeSelection() {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                getShell().setCursor(EnsembleToolViewer.waitCursor);

                TreeItem[] treeItems = modelFamiliesTree.getSelection();
                if (treeItems != null && treeItems.length > 0) {
                    if (treeItems[0].getData() instanceof ModelFamilyType) {
                        elementSetTreeViewer.setInput(emptyElementSet);
                    }

                    if (treeItems[0].getData() instanceof ModelFamilySubType) {
                        ModelFamilySubType mfst = (ModelFamilySubType) treeItems[0]
                                .getData();
                        /* Open model family */
                        ModelFamilyDefinitions[] modelDefs = ModelFamilyDefinitions
                                .values();
                        ModelFamilyDefinitions foundModelDef = null;
                        for (ModelFamilyDefinitions modelDef : modelDefs) {
                            if (modelDef.getFamilySubType() == mfst) {
                                foundModelDef = modelDef;
                                currFamily = null;
                                refreshModelFamilyElements(modelDef);
                                break;
                            }
                        }

                        checkForProductAvailability();

                        enableRelevantModelSources(
                                foundModelDef.getFamilyType(),
                                foundModelDef.getFamilySubType());
                    }
                }
                getShell().setCursor(EnsembleToolViewer.normalCursor);
            }
        });
    }

    /**
     * Model families are defined to work with known model sources. When the
     * user selects a model family, make certain only the relevant model sources
     * are selectable from the model source list.
     * 
     * Make sure that when a user clicks on a model source in the model source
     * tree, that if the model source is not valid for the current model family,
     * or that no products for that model source exist on the server, to
     * deselect the model source. Normally, a mouse click on a tree item would
     * automatically select the tree item. This method, in essence, makes the
     * tool *ignore the selection*.
     * 
     * This method is required as the <code>TreeItem</code> interface does not
     * support being enabled or disabled. This has to be instead done
     * programatically.
     * 
     * This method therefore works in conjunction with the method
     * <code>enableRelevantModelSources</code>.
     */
    private void allowOnlyRelevantSelections() {

        TreeItem[] treeItems = modelFamiliesTree.getSelection();
        if (treeItems != null && treeItems.length > 0) {
            if (treeItems[0].getData() instanceof ModelFamilyType) {
                return;
            }
            if (treeItems[0].getData() instanceof ModelFamilySubType) {
                ModelFamilySubType mfst = (ModelFamilySubType) treeItems[0]
                        .getData();
                ModelFamilyDefinitions[] modelDefs = ModelFamilyDefinitions
                        .values();
                ModelFamilyDefinitions foundModelDef = null;
                for (ModelFamilyDefinitions modelDef : modelDefs) {
                    if (modelDef.getFamilySubType() == mfst) {
                        foundModelDef = modelDef;
                        break;
                    }
                }

                List<ModelSources> relevantModelSources = ModelFamilyDefinitions
                        .getModelSources(foundModelDef.getFamilyType(),
                                foundModelDef.getFamilySubType());
                ModelSources currSrc = null;
                treeItems = modelSourcesTree.getItems();
                for (TreeItem ti : treeItems) {
                    if (ti.getData() instanceof ModelSources) {
                        currSrc = (ModelSources) ti.getData();
                        if (notAvailableOnServer.contains(currSrc)
                                || !relevantModelSources.contains(currSrc)) {
                            modelSourcesTree.deselect(ti);
                        }
                    }
                }
            }
        }
    }

    /**
     * Model families are defined to work with known model sources. When the
     * user selects a model family, make certain only the relevant model sources
     * (those that are referenced in a model family) and/or model sources which
     * are currently not available on the server, are selectable from the model
     * source list.
     * 
     * This method changes the foreground color of the model source tree item to
     * either black or gray, depending upon whether the model source is enabled
     * or disabled, respectively.
     * 
     * This method is required as the <code>TreeItem</code> interface does not
     * support being enabled or disabled. This has to be instead done
     * programatically.
     * 
     * This method therefore works in conjunction with the method
     * <code>onlyAllowRelevantSelections</code>.
     */
    private void enableRelevantModelSources(ModelFamilyType mft,
            ModelFamilySubType mfst) {
        List<ModelSources> relevantModelSources = ModelFamilyDefinitions
                .getModelSources(mft, mfst);
        ModelSources currSrc = null;
        modelSourcesTree.deselectAll();
        TreeItem[] treeItems = modelSourcesTree.getItems();
        for (TreeItem ti : treeItems) {
            if (ti.getData() instanceof ModelSources) {
                currSrc = (ModelSources) ti.getData();
                if (!notAvailableOnServer.contains(currSrc)
                        && relevantModelSources.contains(currSrc)) {
                    ti.setForeground(GlobalColor.get(GlobalColor.BLACK));
                } else if (notAvailableOnServer.contains(currSrc)) {
                    ti.setForeground(GlobalColor.get(GlobalColor.LIGHT_RED));
                } else if (!relevantModelSources.contains(currSrc)) {
                    ti.setForeground(GlobalColor.get(GlobalColor.MEDIUM_GRAY));
                }
            }
        }

    }

    /**
     * Refreshes the elements (field/plane pairs) tree when the user selects a
     * different model family.
     * 
     * @param family
     *            the model family selected by the user from the model family
     *            tree
     */
    private void refreshModelFamilyElements(ModelFamilyDefinitions family) {

        try {
            currFamily = new ModelFamily(family);
        } catch (IOException | VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        currElementSet = currFamily.getFieldPlanePairs();
        elementSetTreeViewer.setInput(currElementSet);

    }

    /**
     * The tool bar for this dialog. Currently there is not a need for this in
     * 16.2.2.
     * 
     * TODO: Toolbar reserved for future use
     */
    private void createModelFamiliesToolBar() {

        Composite modelFamiliesToolBarComposite = new Composite(
                modelFamiliesRootComposite, SWT.BORDER);
        modelFamiliesToolBarComposite.setLayoutData(new GridData(SWT.FILL,
                SWT.CENTER, false, false, 4, 1));
        GridLayout modelFamiliesToolBarComposite_gl = new GridLayout(10, false);
        modelFamiliesToolBarComposite
                .setLayout(modelFamiliesToolBarComposite_gl);

        modelFamiliesToolBarComposite.setBackground(GlobalColor
                .get(GlobalColor.GRAY));

        Composite dummySpacer0 = new Composite(modelFamiliesToolBarComposite,
                SWT.NONE);
        dummySpacer0.setBackground(GlobalColor.get(GlobalColor.GRAY));
        dummySpacer0.setLayout(new GridLayout());
        dummySpacer0.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false,
                false, 1, 1));

        modelFamiliesToolBar = new ToolBar(modelFamiliesToolBarComposite,
                SWT.BORDER_SOLID | SWT.RIGHT);
        modelFamiliesToolBar.setLayoutData(new GridData(SWT.LEFT, SWT.FILL,
                false, false, 4, 1));

        Composite dummySpacer1 = new Composite(modelFamiliesToolBarComposite,
                SWT.NONE);
        dummySpacer1.setBackground(GlobalColor.get(GlobalColor.GRAY));
        dummySpacer1.setLayout(new GridLayout());
        dummySpacer1.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true,
                false, 5, 1));

        newElementSetToolItem = new ToolItem(modelFamiliesToolBar, SWT.PUSH
                | SWT.BORDER);

        newElementSetToolItem.setImage(EnsembleToolImageStore.NEW_IMG);

        newElementSetToolItem.setToolTipText("Add an element set");

        newElementSetToolItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
            }

        });

        new ToolItem(modelFamiliesToolBar, SWT.SEPARATOR);
        clearSelectedModelFamiliesToolItem = new ToolItem(modelFamiliesToolBar,
                SWT.PUSH | SWT.BORDER);

        clearSelectedModelFamiliesToolItem
                .setImage(EnsembleToolImageStore.REMOVE_SELECTED_IMG);

        clearSelectedModelFamiliesToolItem
                .setToolTipText("Clear selected element sets");

        clearSelectedModelFamiliesToolItem
                .addSelectionListener(new SelectionListener() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                    }

                    @Override
                    public void widgetDefaultSelected(SelectionEvent e) {
                        widgetSelected(e);
                    }

                });

        clearAllModelFamiliesToolItem = new ToolItem(modelFamiliesToolBar,
                SWT.PUSH | SWT.BORDER);
        clearAllModelFamiliesToolItem
                .setImage(EnsembleToolImageStore.REMOVE_ALL_IMG);

        clearAllModelFamiliesToolItem.setToolTipText("Clear all element sets");

        clearAllModelFamiliesToolItem
                .addSelectionListener(new SelectionListener() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                    }

                    @Override
                    public void widgetDefaultSelected(SelectionEvent e) {
                        widgetSelected(e);
                    }

                });

    }

    /**
     * This is the content provider for the model families tree.
     */
    private class ModelFamilyTreeContentProvider implements
            ITreeContentProvider {

        @Override
        public Object[] getElements(Object inputElement) {

            ModelFamilyType[] allModelFamilyTypes = ModelFamilyType.values();
            return allModelFamilyTypes;
        }

        @Override
        public Object[] getChildren(Object parentElement) {

            ModelFamilySubType[] childItems = null;
            if (parentElement instanceof ModelFamilyType) {
                String topLevelMenuName = ((ModelFamilyType) parentElement)
                        .getName();
                if (topLevelMenuName != null && topLevelMenuName.length() > 0) {
                    ModelFamilySubType[] allSubFamilies = ModelFamilySubType
                            .values();
                    List<ModelFamilySubType> childItemsFancyList = new ArrayList<>();
                    int i = 0;
                    for (ModelFamilySubType mfst : allSubFamilies) {
                        if (topLevelMenuName.equals(mfst.getParentType()
                                .getName())) {
                            childItemsFancyList.add(mfst);
                            i++;
                        }
                    }
                    if (i > 0) {
                        childItems = new ModelFamilySubType[i];
                        for (int j = 0; j < i; j++) {
                            childItems[j] = childItemsFancyList.get(j);
                        }
                    } else {
                        childItems = new ModelFamilySubType[0];
                    }
                }
            }
            return childItems;
        }

        @Override
        public Object getParent(Object element) {

            ModelFamilyType parent = null;
            ModelFamilySubType child = null;
            if (element instanceof ModelFamilySubType) {
                child = (ModelFamilySubType) element;
                parent = child.getParentType();
            }
            return parent;
        }

        @Override
        public boolean hasChildren(Object element) {
            boolean hasChildren = false;
            /* TODO: Need to refactor away from enumerations */
            if (element instanceof ModelFamilyType) {
                hasChildren = true;
            } else if (element instanceof ModelFamilySubType) {
                hasChildren = false;
            }
            return hasChildren;
        }

        @Override
        public void dispose() {
            /* ignore */
        }

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            /* ignore */
        }

    }

    /**
     * This is the content provider for the element sets tree.
     */
    private class ElementSetsTreeContentProvider implements
            ITreeContentProvider {

        @Override
        public Object[] getElements(Object inputElement) {

            FieldPlanePair[] rootTreeItems = null;

            if (currElementSet == null) {
                rootTreeItems = new FieldPlanePair[0];
            } else {
                List<FieldPlanePair> nodes = currElementSet.getNodes();
                if (nodes != null && nodes.size() > 0) {
                    rootTreeItems = new FieldPlanePair[nodes.size()];
                    rootTreeItems = nodes.toArray(rootTreeItems);
                }
            }
            return rootTreeItems;
        }

        @Override
        public Object[] getChildren(Object parentElement) {
            return new Object[0];
        }

        @Override
        public Object getParent(Object element) {
            return null;
        }

        @Override
        public boolean hasChildren(Object element) {
            return false;
        }

        @Override
        public void dispose() {
            /* ignore */
        }

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            /* ignore */
        }

    }

    /**
     * This is the content provider for the model sources tree.
     */
    private class ModelSourcesTreeContentProvider implements
            ITreeContentProvider {

        @Override
        public Object[] getElements(Object inputElement) {
            ModelSources[] rootTreeElements = ModelSources.values();
            List<ModelSources> onlyActiveItems = new ArrayList<>();
            int i = 0;
            for (ModelSources modelSource : rootTreeElements) {
                if (modelSource.isActive()) {
                    onlyActiveItems.add(modelSource);
                    i++;
                }
            }
            ModelSources[] activeItems = null;
            if (i > 0) {
                activeItems = new ModelSources[i];
                for (int j = 0; j < i; j++) {
                    activeItems[j] = onlyActiveItems.get(j);
                }
            } else {
                activeItems = new ModelSources[0];
            }
            return activeItems;
        }

        @Override
        public Object[] getChildren(Object parentElement) {
            return new ModelSources[0];
        }

        @Override
        public Object getParent(Object element) {
            return null;
        }

        @Override
        public boolean hasChildren(Object element) {
            return false;
        }

        @Override
        public void dispose() {
            /* ignore */
        }

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            /* ignore */
        }

    }

    /**
     * Creates the Load an Close buttons as well as defines the behavior for
     * what to do when these buttons are selected.
     * 
     * The Load button will load whatever model family is selected for the model
     * sources that are selected.
     * 
     * The Close button will close the dialog directly.
     * 
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        loadBtn = createButton(parent, IDialogConstants.PROCEED_ID, "Load",
                false);
        closeBtn = createButton(parent, IDialogConstants.OK_ID, "Close", false);
        final Composite rootComposite = modelFamiliesRootComposite;
        loadBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                ModelSources currSource = null;
                TreeItem[] sourceItems = modelSourcesTree.getSelection();
                if (sourceItems.length < 2) {
                    MessageDialog.open(MessageDialog.INFORMATION,
                            rootComposite.getShell(), "Select a Source",
                            "You must select two or more sources to load.",
                            SWT.NONE);

                } else {
                    TreeItem[] selectedFamily = modelFamiliesTree
                            .getSelection();
                    if (selectedFamily.length == 0) {
                        MessageDialog.open(MessageDialog.INFORMATION,
                                rootComposite.getShell(),
                                "Select a Model Family",
                                "You must select one model family to load.",
                                SWT.NONE);
                    } else {
                        TreeItem ti = selectedFamily[0];
                        if (ti.getData() instanceof ModelFamilySubType) {
                            List<ModelSources> sourceList = new ArrayList<>();
                            for (TreeItem item : sourceItems) {
                                currSource = (ModelSources) item.getData();
                                sourceList.add(currSource);
                            }
                            ResolvedModelFamily resolvedFamily = null;
                            try {
                                resolvedFamily = new ResolvedModelFamily(
                                        currFamily, sourceList);
                            } catch (VizException e1) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e1.getLocalizedMessage(), e1);
                            }
                            if (resolvedFamily != null) {
                                familyLoadListener
                                        .addModelFamily(resolvedFamily);
                            }
                            close();
                        }
                    }
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

        });

        loadBtn.setEnabled(true);
        closeBtn.setEnabled(true);
    }

    /**
     * Give the dialog a pertinent title.
     */
    @Override
    protected void configureShell(final Shell shell) {
        super.configureShell(shell);
        shell.setText("Model Family Browser");
    }

    public IDataCatalog getDataCatalog(IDataCatalogEntry catalogEntry) {
        return DataCatalogManager.getDataCatalogManager().getDataCatalog(
                catalogEntry.getSelectedData());
    }

    /**
     * This class controls how items are displayed in the model sources tree.
     */
    private class ModelSourcesNameColumnLabelProvider extends
            ColumnLabelProvider {

        public Font getFont(Object element) {

            Font f = null;
            f = SWTResourceManager.getFont("dialog", 11, SWT.NONE);
            return f;
        }

        public Image getImage(Object element) {

            return null;
        }

        public String getText(Object element) {

            String retval = null;
            if (element instanceof ModelSources) {
                ModelSources d = (ModelSources) element;
                retval = d.getModelName();
            }
            return retval;
        }

    }

    /**
     * Here's how we control what model family names are displayed in the tree.
     */
    private class ModelFamilyNameColumnLabelProvider extends
            ColumnLabelProvider {

        public Font getFont(Object element) {

            Font f = null;
            f = SWTResourceManager.getFont("dialog", 11, SWT.NONE);
            return f;
        }

        public Image getImage(Object element) {

            return null;
        }

        public String getText(Object element) {

            String retval = null;
            if (element instanceof ModelFamilyType) {
                retval = ((ModelFamilyType) element).getName();
            } else if (element instanceof ModelFamilySubType) {
                retval = ((ModelFamilySubType) element).getSubtypeName();
            }
            return retval;
        }

    }

    /**
     * This class is needed give a *dummy* first (i.e. left-most) column the
     * element set tree. This cosmetically pads the tree so subsequent columns
     * don't look so squashed.
     */
    private class ElementSetDummyColumnLabelProvider extends
            ColumnLabelProvider {

        public Font getFont(Object element) {
            return null;
        }

        public Image getImage(Object element) {
            return null;
        }

        public String getText(Object element) {
            return " ";
        }
    }

    /**
     * Here's how we control the visibility column of the element set tree. If
     * the model has a default of making the element (field/plane pair) visible
     * then this class will place a dot image in the row entry.
     */
    private class ElementSetVisibilityColumnLabelProvider extends
            ColumnLabelProvider {

        public Font getFont(Object element) {
            return null;
        }

        public Image getImage(Object element) {
            Image image = null;
            FieldPlanePair currItem = (FieldPlanePair) element;
            if (currItem.isVisible()) {
                image = EnsembleToolImageStore.DOT_IMG;
            }
            return image;
        }

        public String getText(Object element) {
            return null;
        }
    }

    /**
     * Here's how we control the name of the field/plane pairs displayed in the
     * element set tree.
     */
    private class ElementSetNameColumnLabelProvider extends ColumnLabelProvider {

        public Font getFont(Object element) {

            Font f = null;
            f = SWTResourceManager.getFont("courier new", 10, SWT.NONE);
            return f;
        }

        public Image getImage(Object element) {

            return null;
        }

        public String getText(Object element) {
            FieldPlanePair currItem = (FieldPlanePair) element;
            /* Pad with a space or two so we text isn't too close to edge */
            return " " + currItem.toString();
        }
    }

    /**
     * Sorts the model family tree in alphabetcal order by model family type
     * name. See <code>ModelFamilyType</code> for more details.
     * 
     */
    private class AlphaNumericModelFamiliesTreeSorter extends ViewerSorter {

        public int compare(Viewer v, Object av1, Object av2) {

            String name1 = null;
            if (av1 instanceof ModelFamilyType) {
                name1 = ((ModelFamilyType) av1).getName();
            } else if (av1 instanceof ModelFamilySubType) {
                name1 = ((ModelFamilySubType) av1).getSubtypeName();
            }

            String name2 = null;
            if (av2 instanceof ModelFamilyType) {
                name2 = ((ModelFamilyType) av2).getName();
            } else if (av2 instanceof ModelFamilySubType) {
                name2 = ((ModelFamilySubType) av2).getSubtypeName();
            }

            int retval = 0;
            if (name1 == null || name2 == null) {
                retval = -1;
            } else {
                retval = name1.compareTo(name2);
            }

            return retval;
        }
    }

    /**
     * Sorts the element set tree in alphabetcal order by field/plane pair name.
     * 
     */
    private class AlphaNumericElementSetNodeTreeSorter extends ViewerSorter {

        public int compare(Viewer v, Object av1, Object av2) {

            FieldPlanePair item1 = (FieldPlanePair) av1;
            FieldPlanePair item2 = (FieldPlanePair) av2;

            String name1 = (String) item1.toString();
            String name2 = (String) item2.toString();

            return name1.compareTo(name2);
        }
    }

}
