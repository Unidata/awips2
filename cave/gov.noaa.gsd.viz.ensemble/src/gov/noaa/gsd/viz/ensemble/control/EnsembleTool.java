package gov.noaa.gsd.viz.ensemble.control;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.IToolLayerChanged;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.GlobalPreferencesComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.FieldPlanePair;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.ModelSources;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.VizMatrixEditor;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.VizMatrixEditor.MatrixNavigationOperation;
import gov.noaa.gsd.viz.ensemble.util.ViewerWindowState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.d2d.ui.map.SideView;
import com.raytheon.uf.viz.xy.timeseries.TimeSeriesEditor;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * The Ensemble Tool is a CAVE feature/tool that currently runs in the D/2D
 * perspective by either clicking an optional button on the CAVE perspective
 * menu, or choosing a menu item of the same name in the Tools menu. Depending
 * upon how this tool is initially configured in CAVE there may be only one of
 * those starting buttons available.
 * 
 * This class is the controlling manager class for the D/2D Ensemble Tool. When
 * the user opens the Ensemble Tool, a navigator view is opened and an
 * EnsembleToolLayer instance, given the name "Ensemble Tool", gets associated
 * with the currently active AbstractEditor, and also gets set to "editable". It
 * is this active and editable instance of tool layer which allows the user to
 * activate or deactivate the Ensemble Tool for the associated active editor.
 * 
 * When the Ensemble Tool is opened and editable ("powered on"), any resources
 * subsequently loaded will be virtually associated with the currently active
 * editor and tool layer pair. In addition, these resources will be stored in
 * the resource manager (<code>EnsembleResourceManager</code>). Yet instead of
 * showing these resource's legends in the default manner of displaying the
 * legend in the active editor, these resources are displayed in the Ensemble
 * Tool's navigator view, which is a <code>ViewPart</code> called the
 * <code>EnsembleToolViewer</code>.
 * 
 * This manager class is therefore responsible for associating and maintaining
 * all instances of EnsembleToolLayers and their associated AbstractEditor
 * instances, and adding an additional layer of control for displaying those
 * resources, and representing those resources in the EnsembleToolViewer.
 * 
 * When the tool is active, it can be in one of the tool modes: legend browser
 * mode or matrix navigation mode. Legend browser mode is used to store legends
 * in the viewer instead of in the active editor. Matrix navigator mode is used
 * to allow users to compare one-to-many model sources on a field/plane pair
 * basis.
 * 
 * @author polster
 * @author jing
 * 
 *         <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2014    5056    polster     Initial creation
 * Nov 13, 2015   13211    polster     Initiate the matrix vs legend task
 * Jan 15, 2016   12301    jing        Added accessor to viewer
 * </pre>
 * 
 * @version 1.0
 */

public class EnsembleTool extends AbstractTool implements
        IRenderableDisplayChangedListener, IToolLayerChanged, IDisposeListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleTool.class);

    public enum EnsembleToolMode {
        NONE, MATRIX, LEGENDS_PLAN_VIEW, LEGENDS_TIME_SERIES
    };

    private static boolean isToolRunning = false;

    private static EnsembleTool SINGLETON = null;

    private static Thread shutdownHook = null;

    protected static final int MAX_TOOL_LAYER_COUNT = 5;

    protected EnsembleToolViewer ensembleToolViewer = null;

    private IWorkbenchPartReference viewPartRef = null;

    private boolean foreignEditableToolLoading = false;

    private EnsembleEditorPartListener theEditorsListener = null;

    /*
     * The EnsembleTool adheres to a singleton pattern. This method is the
     * accessor to the singleton instance reference.
     */
    public static EnsembleTool getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new EnsembleTool();
        }
        return SINGLETON;
    }

    /*
     * The EnsembleTool adheres to a singleton pattern. This constructor is
     * therefore private.
     * 
     * This ctor creates the ETLMResourceDataManager singleton and also adds a
     * shutdown hook which guarantees that this class is disposed of properly.
     * 
     * The entire tool is controlled by the isRunning static flag. Certain
     * eternally living class instances (IRenderableDisplayCustomizer) should
     * not interact with this tool when it is not isRunning.
     */

    private EnsembleTool() {

        ETLMResourceDataManager.getInstance();

        shutdownHook = new Thread() {
            @Override
            public void run() {
                dispose();
            }
        };
        Runtime.getRuntime().addShutdownHook(shutdownHook);
    }

    /*
     * Are they any ensemble tool layers still in existence? Yes then the
     * ensemble tool is considered running.
     */
    public static boolean isToolRunning() {
        return isToolRunning;
    }

    /**
     * Is the ensemble tool layer not in the active editor?
     */
    public static boolean isToolNotLoaded() {
        boolean notLoaded = true;

        AbstractEditor activeEditor = (AbstractEditor) EditorUtil
                .getActiveEditor();

        IDisplayPane[] displayPanes = activeEditor.getDisplayPanes();

        for (IDisplayPane pane : displayPanes) {
            IDescriptor desc = pane.getDescriptor();
            Iterator<ResourcePair> iter = desc.getResourceList().iterator();

            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                AbstractVizResource<?, ?> rsc = pair.getResource();
                if (rsc instanceof EnsembleToolLayer) {
                    notLoaded = false;
                    break;
                }
            }
        }

        return notLoaded;
    }

    /**
     * Dispose and null out all owned references
     */
    public void dispose() {

        EnsembleResourceManager.getInstance().dispose();

        theEditorsListener = null;
        ensembleToolViewer = null;
        viewPartRef = null;

        shutdownHook = null;

        isToolRunning = false;
        /* next call to getInstance() turns the tool on again */
        SINGLETON = null;

    }

    /*
     * This method is associated with the AbstractTool parent class. It would
     * normally be associated with an action such as a button click, but the RCP
     * requires those classes to have a default constructor, and this class is a
     * singleton. So the EnsembleToolLayerManagerAction class is the pass-thru
     * AbstractTool whose execute method calls this execute method.
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        if (editor == null) {
            super.setEditor(EditorUtil.getActiveVizContainer());
            if (editor == null) {
                /*
                 * no editor availabe (e.g. all editors closed) so do nothing.
                 */
                return null;
            }
        }

        if (theEditorsListener == null) {
            theEditorsListener = new EnsembleEditorPartListener();
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().addPartListener(theEditorsListener);
        }

        if (ensembleToolViewer == null) {
            try {
                ensembleToolViewer = initView();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        /*
         * either find an existing ensemble tool layer in the active editor ...
         * or create a new one. Either way this method will make the new or
         * existing tool editable.
         */
        EnsembleToolLayer etl = getToolLayer();
        if (etl == null) {
            createToolLayer(editor);
        }
        refreshTool(false);

        isToolRunning = true;

        return null;

    }

    public int verifyCloseActiveToolLayer() {

        int userResponse = 0;

        if (PlatformUI.getWorkbench().isClosing()) {
            return ISaveablePart2.NO;
        }

        String tln = "";
        boolean lastRemainingResource = false;

        /* No need to prompt when there are no tool layers. Just close the view. */
        if (ETLMResourceDataManager.getInstance().isEmpty()) {
            userResponse = ISaveablePart2.NO;
            ensembleToolViewer = null;
        }
        /* There is still at least one tool layer existing */
        else {
            EnsembleToolLayer etl = getToolLayer();
            if (etl != null) {
                tln = etl.getName();

                if (isActiveToolLayerEmpty()) {
                    userResponse = ISaveablePart2.NO;
                    etl.unload();
                } else {
                    boolean userRequestedClose = MessageDialog.openConfirm(
                            Display.getCurrent().getActiveShell(),
                            "Confirm Ensemble Tool layer close",
                            "Close the current tool layer: " + tln + "?");

                    if (userRequestedClose) {
                        if (ETLMResourceDataManager.getInstance()
                                .getToolLayerCount() == 1) {
                            lastRemainingResource = true;
                        }
                        etl.unload();
                        if (lastRemainingResource) {
                            userResponse = ISaveablePart2.NO;
                            ensembleToolViewer = null;
                        } else {
                            userResponse = ISaveablePart2.CANCEL;
                        }
                    } else {
                        userResponse = ISaveablePart2.CANCEL;
                    }
                }
            }
        }
        return userResponse;
    }

    public EnsembleTool.EnsembleToolMode getToolMode() {
        EnsembleTool.EnsembleToolMode tm = EnsembleTool.EnsembleToolMode.NONE;
        if (getToolLayer() != null) {
            tm = getToolLayer().getToolMode();
        }
        return tm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IDisposeListener#disposed(com.raytheon.uf
     * .viz.core.rsc.AbstractVizResource)
     * 
     * Did the ensemble tool layer get disposed?
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {

        if (rsc instanceof EnsembleToolLayer) {
            EnsembleToolLayer toolLayer = (EnsembleToolLayer) rsc;
            if (toolLayer != null) {
                EnsembleResourceManager.getInstance().removeMapping(toolLayer);
                removeToolLayer(toolLayer);
            }
        }

    }

    private void removeToolLayer(EnsembleToolLayer toolLayer) {

        if (toolLayer != null) {

            toolLayer.unregisterListener((IToolLayerChanged) this);
            toolLayer.unregisterListener((IDisposeListener) this);

            IDisplayPaneContainer editorPane = ETLMResourceDataManager
                    .getInstance().findEditor(toolLayer);

            if (editorPane != null) {

                editorPane.removeRenderableDisplayChangedListener(this);

                if (theEditorsListener != null) {
                    theEditorsListener.removeEditor(editorPane);
                }

                ETLMResourceDataManager.getInstance().unload(toolLayer);

                /*
                 * Was this the very last ensemble tool layer? Then clean up and
                 * shut down this tool.
                 */
                if (ensembleToolViewer != null) {
                    if (ETLMResourceDataManager.getInstance().isEmpty()) {
                        setViewerWindowState(ViewerWindowState.CLOSE);
                        dispose();
                    }
                }
            }
        }
    }

    /*
     * Add a new ensemble tool layer to the given editor. Keep track of the
     * association in the ETLMResourceDataManager. This method requires that the
     * super.editor data member is already set.
     */
    public void createToolLayer(IDisplayPaneContainer editorPane) {

        if (editorPane != null) {
            super.setEditor(editorPane);
            IDescriptor descr = editor.getActiveDisplayPane().getDescriptor();
            EnsembleToolLayer etl = null;
            try {
                if (editor instanceof VizMapEditor) {
                    etl = ETLMResourceDataManager.getInstance()
                            .constructToolLayer(editor, descr,
                                    EnsembleToolMode.LEGENDS_PLAN_VIEW);
                } else if (editor instanceof TimeSeriesEditor) {
                    etl = ETLMResourceDataManager.getInstance()
                            .constructToolLayer(editor, descr,
                                    EnsembleToolMode.LEGENDS_TIME_SERIES);
                } else if (editor instanceof VizMatrixEditor) {
                    etl = ETLMResourceDataManager.getInstance()
                            .constructToolLayer(editor, descr,
                                    EnsembleToolMode.MATRIX);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                statusHandler.error("Unable to create a tool layer in editor: "
                        + ((AbstractEditor) editor).getTitle());
                return;
            }
            if (etl != null) {
                descr.getResourceList().add(etl);
                etl.setEditable(true);
                etl.registerListener((IToolLayerChanged) this);
                editor.addRenderableDisplayChangedListener(this);
                theEditorsListener.addEditor(editor);
                etl.registerListener((IDisposeListener) EnsembleTool
                        .getInstance());

                etl.issueRefresh();
                editor.refresh();
            }
        }
    }

    public EnsembleEditorPartListener getEditorPartListener() {
        return theEditorsListener;
    }

    /*
     * Given a viz resource, return the associated resource pair.
     */
    public ResourcePair getResourcePair(AbstractVizResource<?, ?> rsc) {

        ResourcePair rp = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            rp = etl.getResourcePair(rsc);
        }
        return rp;
    }

    /*
     * Given a viz resource, return the associated resource pair.
     */
    public ResourcePair getResourcePair(FieldPlanePair e, ModelSources modelSrc) {

        ResourcePair rp = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            rp = etl.getResourcePair(e, modelSrc);
        }
        return rp;
    }

    /*
     * Given an ensemble tool layer return its associated editor.
     */
    public IDisplayPaneContainer findEditor(EnsembleToolLayer tool) {
        IDisplayPaneContainer e = ETLMResourceDataManager.getInstance()
                .findEditor(tool);
        return e;
    }

    /*
     * Does the given editor have an assoicated ensemble tool layer?
     */
    public boolean hasToolLayer(IDisplayPaneContainer editor) {

        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            return true;
        }
        return false;
    }

    public boolean hasEditor(IDisplayPaneContainer editorPane) {
        return ETLMResourceDataManager.getInstance().hasEditor(editorPane);
    }

    /*
     * Given the group name (aka the top-level title of an ensemble product) set
     * it to be the default resource for subsequent calculations on the
     * ResourceManager.
     */
    public void setEnsembleCalculationResource(String rscGroupName) {
        if (editor != null) {
            EnsembleResourceManager.getInstance()
                    .setEnsembleCalculationResourceName(
                            (AbstractEditor) editor, rscGroupName);
        }
    }

    /*
     * Get the group name (aka the top-level title of an ensemble product) for
     * the default resource for subsequent calculations on the ResourceManager.
     */
    public String getEnsembleCalculationResourceName() {

        String s = "";
        if (editor != null) {
            s = EnsembleResourceManager
                    .getInstance()
                    .getEnsembleCalculationResourceName((AbstractEditor) editor);
        }
        return s;
    }

    /*
     * Update the legend time information in the navigator view.
     */
    public void frameChanged(FramesInfo framesInfo) {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.frameChanged(framesInfo);
        }
    }

    /*
     * Return the time basis resource name by getting the active editor and
     * requesting the name from the ResourceManager.
     */
    public String getTimeBasisResourceName() {

        String s = "";
        if (getToolLayer() != null && !getToolLayer().isEmpty()) {
            s = EnsembleResourceManager.getInstance().getTimeBasisResourceName(
                    getToolLayer());
        }
        return s;
    }

    /*
     * Return the time basis legend time by getting the active editor and
     * requesting the time from the ResourceManager.
     */
    public String getTimeBasisLegendTime() {

        String s = "";
        if (getToolLayer() != null) {
            s = EnsembleResourceManager.getInstance().getTimeBasisLegendTime(
                    getToolLayer());
        }
        return s;

    }

    public String getActiveResourceTime() {
        String rscTime = null;
        if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW
                || EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
            rscTime = getTimeBasisLegendTime();
        } else if (EnsembleTool.getInstance().getToolMode() == EnsembleToolMode.MATRIX) {
            rscTime = ensembleToolViewer.getActiveRscTime();
        }

        return rscTime;
    }

    public String getTimeSeriesPoint() {
        String s = "";
        if (getToolLayer() != null) {
            s = EnsembleResourceManager.getInstance().getTimeSeriesPoint(
                    getToolLayer());
        }
        return s;
    }

    public IDisplayPaneContainer getActiveEditor() {
        return editor;
    }

    public IDisplayPaneContainer getCurrentLegendsToolEditor() {
        return ETLMResourceDataManager.getInstance()
                .getCurrentLegendsToolEditor();
    }

    public IDisplayPaneContainer getCurrentMatrixToolEditor() {
        return ETLMResourceDataManager.getInstance()
                .getCurrentMatrixToolEditor();
    }

    public void setEditor(IDisplayPaneContainer editor) {
        super.setEditor(editor);
        EnsembleToolMode toolMode = getToolMode(editor);
        switch (toolMode) {
        case LEGENDS_PLAN_VIEW:
        case LEGENDS_TIME_SERIES:
            ETLMResourceDataManager.getInstance().setCurrentLegendsTool(editor);
            break;
        case MATRIX:
            ETLMResourceDataManager.getInstance().setCurrentMatrixTool(editor);
            break;
        case NONE:
            break;
        }

    }

    public static EnsembleToolMode getToolMode(IDisplayPaneContainer editor) {

        EnsembleToolMode toolMode = EnsembleToolMode.NONE;
        if (editor instanceof VizMapEditor) {
            toolMode = EnsembleToolMode.LEGENDS_PLAN_VIEW;
        } else if (editor instanceof TimeSeriesEditor) {
            toolMode = EnsembleToolMode.LEGENDS_TIME_SERIES;
        } else if (editor instanceof VizMatrixEditor) {
            toolMode = EnsembleToolMode.MATRIX;
        }
        return toolMode;
    }

    /*
     * This class is an IRenderableDisplayChangedListener so we must override
     * this method. This happens during a swap.
     */
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {

        /*
         * New editor becoming coming active
         */
        if (type == DisplayChangeType.ADD
                && newRenderableDisplay.getContainer() instanceof IDisplayPaneContainer) {

            IDisplayPaneContainer editorChanged = (IDisplayPaneContainer) newRenderableDisplay
                    .getContainer();
            super.setEditor(editorChanged);

            EnsembleToolLayer etl = getToolLayer();
            if (etl != null) {
                if (pane.getRenderableDisplay().getContainer() == editor) {
                    ETLMResourceDataManager.getInstance()
                            .updateToolLayerEditor(
                                    etl,
                                    (IWorkbenchPart) newRenderableDisplay
                                            .getContainer());
                }
            }
            refreshTool(false);
        }

        /*
         * Remove to side view
         */
        if ((type == DisplayChangeType.REMOVE)
                && (newRenderableDisplay.getContainer() instanceof SideView)) {
            EnsembleToolLayer etl = getToolLayer();
            if (etl != null) {
                if (pane.getRenderableDisplay().getContainer() == editor) {
                    ETLMResourceDataManager.getInstance()
                            .updateToolLayerEditor(
                                    etl,
                                    (IWorkbenchPart) newRenderableDisplay
                                            .getContainer());
                }
            }
        }
    }

    /*
     * The currently active editor (super.editor) must be set before calling
     * this method.
     */
    protected IDisplayPane[] getSelectedPanes() {

        IDisplayPane[] displayPanes = editor.getDisplayPanes();

        if (editor instanceof IMultiPaneEditor) {
            IDisplayPane selected = ((IMultiPaneEditor) editor)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            if (selected != null) {
                displayPanes = new IDisplayPane[] { selected };
            }
        }
        return displayPanes;
    }

    public static EnsembleToolLayer getToolLayer(IDisplayPaneContainer absEditor) {

        EnsembleToolLayer foundToolLayer = null;
        if (absEditor != null) {
            for (IDisplayPane pane : absEditor.getDisplayPanes()) {
                IDescriptor desc = pane.getDescriptor();
                List<EnsembleToolLayer> layers = desc.getResourceList()
                        .getResourcesByTypeAsType(EnsembleToolLayer.class);
                if (layers != null && !layers.isEmpty()) {
                    foundToolLayer = layers.get(0);
                }
            }
        }
        return foundToolLayer;
    }

    /*
     * Is there an ensemble tool layer in the currently known editor?
     */
    public EnsembleToolLayer getToolLayer() {

        EnsembleToolLayer foundToolLayer = null;
        if (editor != null) {
            foundToolLayer = EnsembleTool.getToolLayer(editor);
        }
        return foundToolLayer;
    }

    public void showEditor(AbstractEditor anotherEditor) {

        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.activate(anotherEditor);
    }

    /*
     * This method will set the visibility state of the EnsembleToolViewer to
     * "show with focus", "show without focus", "minimized", or "closed".
     */
    synchronized public void setViewerWindowState(
            final ViewerWindowState windowState) {

        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {

                IWorkbenchWindow window = null;
                IWorkbenchPage page = null;

                // Get the active window
                window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
                if (window == null) {
                    return;
                }

                // Get the active page
                page = window.getActivePage();
                if (page == null) {
                    return;
                }

                if (windowState == ViewerWindowState.CLOSE) {
                    IViewPart viewPart = page.findView(EnsembleToolViewer.ID);
                    page.hideView(viewPart);
                } else if (windowState == ViewerWindowState.SHOW_WITH_FOCUS) {
                    try {
                        if (viewPartRef != null) {
                            // IWorkbenchPartReference viewPartRef = page
                            // .findViewReference(EnsembleToolViewer.ID);
                            page.setPartState(viewPartRef,
                                    IWorkbenchPage.STATE_RESTORED);
                            page.showView(EnsembleToolViewer.ID, null,
                                    IWorkbenchPage.VIEW_ACTIVATE);
                        }
                    } catch (PartInitException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to show Ensemble viewer.");
                    }
                } else if (windowState == ViewerWindowState.SHOW_WITHOUT_FOCUS) {
                    try {
                        if (viewPartRef != null) {
                            // IWorkbenchPartReference viewPartRef = page
                            // .findViewReference(EnsembleToolViewer.ID);
                            page.setPartState(viewPartRef,
                                    IWorkbenchPage.STATE_RESTORED);
                            page.showView(EnsembleToolViewer.ID, null,
                                    IWorkbenchPage.VIEW_VISIBLE);
                        }
                    } catch (PartInitException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to show Ensemble viewer.");
                    }
                } else if (windowState == ViewerWindowState.MINIMIZED) {
                    if (viewPartRef != null) {
                        page.setPartState(viewPartRef,
                                IWorkbenchPage.STATE_MINIMIZED);
                    }
                }
            }
        });
    }

    /*
     * This method should be called when the Ensemble Tool is initially opened
     * or re-opened. It returns the newly created EnsmebleToolViewer, which is,
     * in essence, a singleton.
     */
    private EnsembleToolViewer initView() throws VizException {

        IViewPart viewPart = null;

        EnsembleToolViewer viewer = null;
        // Get the active window
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        if (window == null) {
            throw new VizException("Can't get Active Workbench Window!");
        }

        // Get the active page
        IWorkbenchPage page = window.getActivePage();
        if (page == null) {
            throw new VizException("Can't get Active Workbench Page!");
        }

        viewPart = page.findView(EnsembleToolViewer.ID);
        if (viewPart == null) {
            // EnsembleToolViewer was not already created ...
            try {
                page.showView(EnsembleToolViewer.ID, null,
                        IWorkbenchPage.VIEW_CREATE);
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to show EnsembleToolViewer.");
            }
            viewPart = page.findView(EnsembleToolViewer.ID);

            viewPartRef = page.findViewReference(EnsembleToolViewer.ID);
            page.setPartState(viewPartRef, IWorkbenchPage.STATE_RESTORED);
            if (viewPart == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to create EnsembleToolViewer on second pass.");
                throw new VizException(
                        "Unable to create EnsembleToolViewer on second pass!");
            } else {
                if (viewPart instanceof EnsembleToolViewer) {
                    viewer = (EnsembleToolViewer) viewPart;
                }
            }

            // the EnsembleToolViewer is already created ....
        } else {
            IWorkbenchPartReference viewPartRef = page
                    .findViewReference(EnsembleToolViewer.ID);
            page.setPartState(viewPartRef, IWorkbenchPage.STATE_RESTORED);
            if (viewPart instanceof EnsembleToolViewer) {
                viewer = (EnsembleToolViewer) viewPart;
            } else {
                viewer = null;
                statusHandler.handle(Priority.PROBLEM,
                        "EnsembleToolViewer gone MIA");
                throw new VizException("EnsembleToolViewer gone MIA!");
            }
        }
        return viewer;
    }

    public EnsembleToolViewer getEnsembleToolViewer() {
        return ensembleToolViewer;
    }

    /*
     * Given a top level ensemble resource name, unload its members.
     */
    public void unloadResourcesByName(String topLevelEnsembleName) {

        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.unloadAllResourcesByName(topLevelEnsembleName);
        }
    }

    /*
     * Get the map of resources associated with the current active tool layer.
     */
    public Map<String, List<AbstractResourceHolder>> getCurrentToolLayerResources() {

        Map<String, List<AbstractResourceHolder>> ensembleResources = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            ensembleResources = etl.getEnsembleResources();
        } else {
            ensembleResources = getEmptyResourceMap();
        }
        return ensembleResources;
    }

    public Map<String, List<AbstractResourceHolder>> getEmptyResourceMap() {
        return new HashMap<>();
    }

    /*
     * Exercise a given calculation on the visible resources of a given tool
     * layer.
     */
    public void calculate(Calculation algorithm) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.calculate(algorithm);
        }
    }

    /*
     * Exercise a given calculation against a given range on the visible
     * resources of a given tool layer.
     */
    public void calculate(Calculation algorithm, Range range) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.calculate(algorithm, range);
        }
    }

    /*
     * This tool manager is responsible for updating the state of the navigator
     * widget, EnsembleToolViewer. Call this method to refresh the view to
     * reflect a change in state of the active editor.
     */
    public void refreshView() {

        if (isToolEditable()) {
            ensembleToolViewer.refreshInput(getToolLayer());
        }
    }

    synchronized private void refreshToolForeignLoad() {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.setViewEditable(false);
            if (isMinimizeOnForeignToolLoadPreference()) {
                setViewerWindowState(ViewerWindowState.MINIMIZED);
            }
            foreignEditableToolLoading = false;
        }
    }

    /*
     * Make sure you have called the super.setEditor method before calling this
     * method. Return true if there is an ensemble tool layer associated with
     * the given editor argument.
     */
    synchronized public boolean refreshTool(boolean isAlreadyActivated) {

        boolean toolLayerFound = false;

        EnsembleToolLayer etl = getToolLayer();

        if (ensembleToolViewer != null) {
            if (etl == null) {
                ensembleToolViewer.disableTool();
                if (!isAlreadyActivated) {
                    setViewerWindowState(ViewerWindowState.MINIMIZED);
                }
            } else {
                toolLayerFound = true;
                ensembleToolViewer.setToolMode(etl.getToolMode());
                ensembleToolViewer.setViewEditable(etl.isEditable());
                if (!isAlreadyActivated) {
                    if (etl.isEditable()) {
                        setViewerWindowState(ViewerWindowState.SHOW_WITHOUT_FOCUS);
                    } else {
                        if (GlobalPreferencesComposite
                                .isMinimizeOnToggleUneditablePreference()) {
                            setViewerWindowState(ViewerWindowState.MINIMIZED);
                        }
                    }
                }
            }
            refreshView();
        }

        return toolLayerFound;
    }

    /*
     * (non-Javadoc)
     * 
     * The sole purpose of this method is to minimize or restore the ensemble
     * tool viewer (ViewPart) when its editability is turned off or on,
     * respectively (if it is not already done).
     */
    @Override
    public void resourceChanged(EnsembleToolLayer etl, ChangeType type,
            Object object) {

        if (type == ChangeType.CAPABILITY
                && object instanceof EditableCapability) {

            if (isForeignEditableToolLoading()) {
                refreshToolForeignLoad();
            } else {
                refreshTool(false);
            }
        }
    }

    /*
     * Turn the tool layer for the ensemble tool on or off.
     */
    public void setEditable(boolean makeEditable) {

        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.setEditable(makeEditable);
        }
    }

    public boolean isDirty() {
        return !isActiveToolLayerEmpty();
    }

    /*
     * Are there resources in the given tool layer
     */
    public boolean isActiveToolLayerEmpty() {
        boolean isEmpty = true;
        EnsembleToolLayer toolLayer = getToolLayer();
        if (toolLayer != null) {
            Map<String, List<AbstractResourceHolder>> ensembleResources = toolLayer
                    .getEnsembleResources();
            if (ensembleResources != null) {
                isEmpty = ensembleResources.isEmpty();
            }
        }
        return isEmpty;
    }

    /*
     * Is the Ensemble Tool "on"? (i.e. is it filtering and trapping incoming
     * resources). Only if the activeToolLayer is editable.
     */
    public boolean isToolEditable() {

        boolean isReady = false;
        EnsembleToolLayer etl = getToolLayer();
        if ((etl != null) && (ensembleToolViewer != null)) {
            if (etl.isEditable()) {
                isReady = true;
            }
        }
        return isReady;
    }

    /*
     * Is the user preference set to minimize the viewer when another tool type
     * is being loaded into the active display?
     */
    public boolean isMinimizeOnForeignToolLoadPreference() {
        boolean i = false;
        if (isToolEditable()) {
            i = GlobalPreferencesComposite
                    .isMinimizeOnForeignToolLoadPreference();
        }
        return i;
    }

    /*
     * Is the user preference set to set the active ensemble tool layer to
     * editable when
     */
    public boolean isMakeEditableOnRestorePreference() {
        boolean i = false;
        if (isToolEditable()) {
            i = GlobalPreferencesComposite.isEditableOnRestorePreference();
        }
        return i;
    }

    /*
     * Is the user preference set to set the active ensemble tool layer to
     * editable when it is being swapped-in?
     */
    public boolean isMakeEditableOnSwapInPreference() {
        boolean i = false;
        if (isToolEditable()) {
            i = GlobalPreferencesComposite.isEditableOnSwapInPreference();
        }
        return i;

    }

    /*
     * Is the user preference set to set the active ensemble tool layer to
     * editable when it is being swapped-in?
     */
    public boolean isCreateNewToolLayerOnNewEditor() {
        boolean i = false;
        if (ensembleToolViewer != null) {
            i = GlobalPreferencesComposite
                    .isCreateToolLayerOnNewEditorPreference();
        }
        return i;
    }

    /*
     * Did the ensemble tool renderable display customizer recognize a new
     * editor being created?
     */
    public void prepareForNewEditor() {
        ensembleToolViewer.prepareForNewToolInput();
    }

    /*
     * Is another editable tool type (i.e. not an ensemble tool) in the process
     * of loading?
     */
    public boolean isForeignEditableToolLoading() {
        return foreignEditableToolLoading;
    }

    /*
     * Did the ensemble tool renderable display customizer recognize another
     * tool type (i.e. not an ensemble tool) loading?
     */
    public void setForeignEditableToolLoading() {
        foreignEditableToolLoading = true;
    }

    /*
     * Clear any flags associated with the tool layer and view editable states.
     */
    public void clearEditableToggleFlags() {
        foreignEditableToolLoading = false;
    }

    public void setExpandedElements(List<String> expandedElems) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.setExpandedElements(expandedElems);
        }
    }

    public List<String> getExpandedElements() {
        List<String> expandedElems = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            expandedElems = etl.getExpandedElements();
        } else {
            expandedElems = new ArrayList<String>();

        }
        return expandedElems;
    }

    /*
     * The resource data manager keeps track of the associated components of a
     * given tool layer including whether the tool layer is in use, the owning
     * editor, and the editor part listener. These components are stored in
     * instances of the inner class ETLMResourceDataUseState.
     */
    protected static class ETLMResourceDataManager {

        private static int TOOL_LAYER_COUNT = 0;

        public static ETLMResourceDataManager SINGLETON = null;

        public static ETLMResourceDataManager getInstance() {
            if (SINGLETON == null) {
                SINGLETON = new ETLMResourceDataManager();
            }
            return SINGLETON;
        }

        public boolean isEmpty() {
            return toolLayerMetaData.isEmpty();
        }

        public int getToolLayerCount() {
            return toolLayerMetaData.size();
        }

        protected List<ETLMResourceData> toolLayerMetaData = new ArrayList<ETLMResourceData>();

        protected ETLMResourceDataManager() {
        }

        protected void unload(EnsembleToolLayer etl) {
            ETLMResourceData foundData = null;
            boolean wasCurrentToolLayer = false;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == etl) {
                    foundData = d;
                    wasCurrentToolLayer = true;
                    d.clear();
                    break;
                }
            }
            etl.unregisterListener((IToolLayerChanged) EnsembleTool
                    .getInstance());
            toolLayerMetaData.remove(foundData);

            /*
             * If the unloaded tool layer was current then set another to be
             * current. There can be many legend tools open, but only one matrix
             * tool.
             */
            if (wasCurrentToolLayer) {
                for (ETLMResourceData d : toolLayerMetaData) {
                    d.setCurrentLegendsToolLayer(true);
                    break;
                }
            }

        }

        protected void updateToolLayerEditor(EnsembleToolLayer etl,
                IWorkbenchPart part) {

            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == etl) {
                    d.workbenchPart = part;
                    break;
                }
            }
        }

        protected List<EnsembleToolLayer> getAllToolLayers() {

            List<EnsembleToolLayer> toolLayers = new ArrayList<EnsembleToolLayer>();
            for (ETLMResourceData d : toolLayerMetaData) {
                toolLayers.add(d.getToolLayer());
            }
            return toolLayers;
        }

        protected boolean hasEditor(IDisplayPaneContainer editor) {
            boolean hasEditor = false;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getEditor().equals(editor)) {
                    hasEditor = true;
                    break;
                }
            }
            return hasEditor;
        }

        protected boolean hasLayer(EnsembleToolLayer toolLayer) {
            boolean hasLayer = false;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (toolLayer.equals(d.getToolLayer())) {
                    hasLayer = true;
                    break;
                }
            }
            return hasLayer;

        }

        protected IDisplayPaneContainer findEditor(EnsembleToolLayer tool) {

            IDisplayPaneContainer editor = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == tool) {
                    editor = d.getEditor();
                    break;
                }
            }
            return editor;
        }

        protected EnsembleToolLayer findToolLayer(IDisplayPaneContainer editor) {

            EnsembleToolLayer etl = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getEditor() == editor) {
                    etl = d.getToolLayer();
                    break;
                }
            }
            return etl;
        }

        protected IDisplayPaneContainer getCurrentLegendsToolEditor() {
            IDisplayPaneContainer idpc = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.isCurrentLegendsToolLayer()) {
                    idpc = d.getEditor();
                }
            }
            return idpc;
        }

        protected void setCurrentLegendsTool(EnsembleToolLayer tool) {

            for (ETLMResourceData d : toolLayerMetaData) {
                d.setCurrentLegendsToolLayer(d.getToolLayer() == tool);
            }
        }

        public void setCurrentLegendsTool(IDisplayPaneContainer editor) {
            for (ETLMResourceData d : toolLayerMetaData) {
                d.setCurrentLegendsToolLayer(d.getEditor() == editor);
            }

        }

        protected IDisplayPaneContainer getCurrentMatrixToolEditor() {
            IDisplayPaneContainer idpc = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.isCurrentMatrixToolLayer()) {
                    idpc = d.getEditor();
                }
            }
            return idpc;
        }

        protected void setCurrentMatrixTool(EnsembleToolLayer tool) {

            for (ETLMResourceData d : toolLayerMetaData) {
                d.setCurrentMatrixToolLayer(d.getToolLayer() == tool);
            }

        }

        public void setCurrentMatrixTool(IDisplayPaneContainer editor) {
            for (ETLMResourceData d : toolLayerMetaData) {
                d.setCurrentMatrixToolLayer(d.getEditor() == editor);
            }

        }

        protected EnsembleToolLayer constructToolLayer(
                IDisplayPaneContainer editor, IDescriptor desc,
                EnsembleTool.EnsembleToolMode toolMode) throws VizException {

            GenericToolsResourceData<EnsembleToolLayer> ard = null;

            if (isEmpty()) {
                TOOL_LAYER_COUNT = 0;
            }
            String fullName = EnsembleToolLayer.DEFAULT_NAME + " "
                    + ((AbstractEditor) editor).getTitle().trim() + "-"
                    + TOOL_LAYER_COUNT++;
            ard = new GenericToolsResourceData<EnsembleToolLayer>(fullName,
                    EnsembleToolLayer.class);
            LoadProperties props = null;
            if (desc instanceof TimeSeriesDescriptor) {
                props = new LoadProperties();
                props.setResourceType(ResourceType.TIME_SERIES);
                props.setLoadWithoutData(true);
            } else {
                props = new LoadProperties();
            }
            EnsembleToolLayer tool = ard.construct(props, desc);
            tool.setToolMode(toolMode);
            tool.setInnerName(fullName);

            ETLMResourceData rdState = new ETLMResourceData(tool,
                    (IWorkbenchPart) editor);
            if (toolMode == EnsembleToolMode.LEGENDS_PLAN_VIEW
                    || toolMode == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                rdState.setCurrentLegendsToolLayer(true);
            } else if (toolMode == EnsembleToolMode.MATRIX) {
                rdState.setCurrentMatrixToolLayer(true);
            }
            toolLayerMetaData.add(rdState);

            return tool;

        }

        private class ETLMResourceData {

            private IWorkbenchPart workbenchPart = null;

            private EnsembleToolLayer toolLayer = null;

            private boolean currentMatrixToolLayer = false;

            private boolean currentLegendsToolLayer = false;

            public IDisplayPaneContainer getEditor() {
                IDisplayPaneContainer editorPane = null;
                if (workbenchPart instanceof SideView) {
                    SideView view = (SideView) workbenchPart;
                    editorPane = view.getActiveDisplayPane()
                            .getRenderableDisplay().getContainer();
                } else if (workbenchPart instanceof AbstractEditor) {
                    AbstractEditor editor = (AbstractEditor) workbenchPart;
                    editorPane = editor.getActiveDisplayPane()
                            .getRenderableDisplay().getContainer();
                }
                return editorPane;
            }

            public ETLMResourceData(EnsembleToolLayer t, IWorkbenchPart e) {
                workbenchPart = e;
                toolLayer = t;
                currentMatrixToolLayer = false;
                currentLegendsToolLayer = false;
            }

            public EnsembleToolLayer getToolLayer() {
                return toolLayer;
            }

            public void clear() {
                workbenchPart = null;
                toolLayer = null;
            }

            public boolean isCurrentMatrixToolLayer() {
                return currentMatrixToolLayer;
            }

            public void setCurrentMatrixToolLayer(boolean currentMatrixToolLayer) {
                this.currentMatrixToolLayer = currentMatrixToolLayer;
            }

            public boolean isCurrentLegendsToolLayer() {
                return currentLegendsToolLayer;
            }

            public void setCurrentLegendsToolLayer(
                    boolean currentLegendsToolLayer) {
                this.currentLegendsToolLayer = currentLegendsToolLayer;
            }
        }
    }

    public void matrixNavigationRequest(MatrixNavigationOperation operationmode) {
        if (this.getToolMode() == EnsembleToolMode.MATRIX) {
            if (ensembleToolViewer != null) {
                ensembleToolViewer.matrixNavigationRequest(operationmode);
            }
        }
    }

    public void clearToolLayer() {
        if (getToolLayer() != null) {
            getToolLayer().unloadAllResources();
            ensembleToolViewer.clearAll();
        }
    }

    public IDisplayPaneContainer getEditor(EnsembleToolLayer ensembleToolLayer) {
        return ETLMResourceDataManager.getInstance().findEditor(
                ensembleToolLayer);
    }

    public int verifyCloseTool() {
        int userResponse = 0;

        if (PlatformUI.getWorkbench().isClosing()) {
            return ISaveablePart2.NO;
        }

        boolean userRequestedClose = MessageDialog
                .openConfirm(Display.getCurrent().getActiveShell(),
                        "Confirm Ensemble Tool Close",
                        "Close the entire Ensemble Tool (All tool layers will be closed) ?");

        if (userRequestedClose) {
            List<EnsembleToolLayer> layers = ETLMResourceDataManager
                    .getInstance().getAllToolLayers();
            for (EnsembleToolLayer layer : layers) {
                layer.unload();
            }
            userResponse = ISaveablePart2.NO;
        } else {
            userResponse = ISaveablePart2.CANCEL;
        }

        return userResponse;
    }

}
