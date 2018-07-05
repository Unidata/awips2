package gov.noaa.gsd.viz.ensemble.control;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.uf.viz.xy.timeseries.TimeSeriesEditor;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractTool;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.EnsembleMembersHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.EnsSamplingResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayerData;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.IToolLayerChanged;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.GlobalPreferencesComposite;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.FieldPlanePair;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.ModelSourceKind;
import gov.noaa.gsd.viz.ensemble.util.ViewerWindowState;

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
 * Nov 19, 2016   19443    polster     Fix for swapping and other refactoring
 * Mar 01, 2017   19443    polster     Cleaned up clear and close behavior
 * Mar 17  2017   19325    jing        Resource group behavior added
 * Dec 01, 2017   41520    polster     Now supports matrix editor
 * Jan 10, 2018   20524    polster     isCompatibleResource method fixed
 * 
 *         </pre>
 * 
 * @version 1.0
 */

public class EnsembleTool extends AbstractTool
        implements IToolLayerChanged, IRenderableDisplayChangedListener,
        RemoveListener, IToolModeChangedProvider {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleTool.class);

    public enum EnsembleToolMode {
        NONE, MATRIX, LEGENDS_PLAN_VIEW, LEGENDS_TIME_SERIES
    }

    public enum MatrixNavigationOperation {
        NONE, UP_MODEL_SOURCE, DOWN_MODEL_SOURCE, LEFT_FRAME, RIGHT_FRAME
    }

    public enum EnsembleToolCompatibility {
        NOT_COMPATIBLE,
        COMPATIBLE_CONTAINS_ENSEMBLE_TOOL_LAYER,
        COMPATIBLE_NO_ENSEMBLE_TOOL_LAYER
    }

    public enum SwapState {
        SWAPPED_IN, SWAPPED_OUT, NOT_SWAPPING
    }

    private static boolean isToolRunning = false;

    private static EnsembleTool SINGLETON = null;

    protected EnsembleToolViewer ensembleToolViewer = null;

    private IWorkbenchPartReference viewPartRef = null;

    private boolean foreignEditableToolLoading = false;

    private EnsembleEditorPartListener theEditorsListener = null;

    private IDisplayPaneContainer lastUsedMatrixEditorPane = null;

    private IDisplayPaneContainer lastUsedLegendsEditorPane = null;

    private IDisplayPaneContainer lastUsedTimeSeriesEditorPane = null;

    private SwapState swapState = SwapState.NOT_SWAPPING;

    private final List<IToolModeChangedListener> toolModeChangedListeners = new CopyOnWriteArrayList<>();

    /**
     * The EnsembleTool adheres to a singleton pattern. This method is the
     * accessor to the singleton instance reference.
     */
    public static EnsembleTool getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new EnsembleTool();
        }
        return SINGLETON;
    }

    /**
     * The EnsembleTool adheres to a singleton pattern. This constructor is
     * therefore private.
     * 
     * The entire tool is controlled by the isToolRunning static flag. Certain
     * eternally living class instances (IRenderableDisplayCustomizer) should
     * not interact with this tool when it is not isRunning.
     */
    private EnsembleTool() {
        EnsembleResourceIngester.getInstance();
        preOpenVolumeBrowser();

    }

    /**
     * The Ensemble Tool currently (as of 17.3.1) only allows image and contour
     * data to be loaded.
     */
    public static boolean isCompatibleResource(AbstractVizResource<?, ?> rsc) {

        boolean isCompatible = false;
        EnsembleToolMode mode = EnsembleTool
                .getToolMode(EditorUtil.getActiveVizContainer());
        switch (mode) {
        case MATRIX:
            if (rsc instanceof AbstractGridResource<?>) {
                isCompatible = true;
            }
            break;
        case LEGENDS_TIME_SERIES:
            if (rsc instanceof TimeSeriesResource) {
                isCompatible = true;
            }
            break;
        case LEGENDS_PLAN_VIEW:
            if (rsc instanceof AbstractGridResource<?>) {
                if (((AbstractGridResource<?>) rsc)
                        .getDisplayType() == DisplayType.IMAGE
                        || ((AbstractGridResource<?>) rsc)
                                .getDisplayType() == DisplayType.CONTOUR) {
                    isCompatible = true;
                }
            } else if (rsc instanceof EnsSamplingResource
                    || rsc instanceof HistogramResource) {
                isCompatible = true;
            }
            break;
        default:
            if (rsc instanceof TimeSeriesResource) {
                isCompatible = true;
            }
            break;

        }
        return isCompatible;
    }

    /**
     * Create the VB dialog if it has not yet been created. Then force it to be
     * hidden. This is so the user doesn't have to wait so long when they
     * manually load the VB or load the model family browser in the Matrix
     * navigator.
     */
    private void preOpenVolumeBrowser() {

        if (!PlatformUI.getWorkbench().isClosing()) {
            if (VolumeBrowserAction.getVolumeBrowserDlg() == null) {

                /*
                 * Obtain IServiceLocator implementer, e.g. from
                 * PlatformUI.getWorkbench()
                 */
                IServiceLocator serviceLocator = PlatformUI.getWorkbench();

                ICommandService commandService = (ICommandService) serviceLocator
                        .getService(ICommandService.class);

                Command command = commandService.getCommand(
                        "com.raytheon.viz.volumebrowser.volumeBrowserRef");

                /*
                 * Optionally pass a ExecutionEvent instance, default no-param
                 * arg creates blank event
                 */

                try {

                    command.executeWithChecks(new ExecutionEvent());
                } catch (ExecutionException | NotDefinedException
                        | NotEnabledException | NotHandledException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

            }

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    VolumeBrowserAction.getVolumeBrowserDlg().hide();
                }
            });

        }
    }

    public SwapState getSwapState() {
        return swapState;
    }

    public void setSwapState(SwapState swapState) {
        this.swapState = swapState;
    }

    /**
     * Are they any ensemble tool layers still in existence? Yes then the
     * ensemble tool is considered running.
     */
    public static boolean isToolRunning() {
        return isToolRunning;
    }

    synchronized public static boolean isExtant() {
        return (SINGLETON != null);
    }

    /**
     * Is the ensemble tool layer not in the active editor?
     */
    public static boolean isToolLoaded() {
        boolean isLoaded = false;

        IEditorPart editorPart = EditorUtil.getActiveEditor();
        if (editorPart instanceof AbstractEditor) {
            AbstractEditor activeEditor = (AbstractEditor) editorPart;
            List<AbstractVizResource<?, ?>> etlList = activeEditor
                    .getActiveDisplayPane().getDescriptor().getResourceList()
                    .getResourcesByType(EnsembleToolLayer.class);
            if (!etlList.isEmpty()) {
                isLoaded = true;
            }
        }
        return isLoaded;
    }

    /**
     * Dispose and null out all owned references
     */
    @Override
    synchronized public void dispose() {

        EnsembleResourceIngester.getInstance().dispose();

        if (theEditorsListener != null) {
            if (PlatformUI.getWorkbench() != null
                    && PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow() != null
                    && PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage() != null)
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().removePartListener(theEditorsListener);
            theEditorsListener = null;
        }

        ensembleToolViewer = null;
        viewPartRef = null;

        lastUsedLegendsEditorPane = null;
        lastUsedTimeSeriesEditorPane = null;
        lastUsedMatrixEditorPane = null;

        isToolRunning = false;

        /* next call to getInstance() turns the tool on again */
        SINGLETON = null;

    }

    public void ignorePartActivatedEvent(boolean ignore) {
        if (theEditorsListener != null) {
            theEditorsListener.ignorePartActivatedEvent(ignore);
        }
    }

    /**
     * This checks to see if the this is an otherwise empty map editor. An
     * virtually empty map editor is considered to be empty when its resource
     * list has the same resources of a newly created map editor and no more.
     * This would be any resource that is not a system resource and also not a
     * map resource.
     * 
     * The caller must make sure the argument is a display container of a Map
     * editor, otherwise the method throws an unchecked illegal argument
     * exception.
     * 
     * This method will check to see if any other resources exist in the
     * resource list other than the default on-load resources.
     * 
     * TODO: This needs to be revisited as it should not be up to this class to
     * define what it means to be an "empty" map editor.
     * 
     * @param rscList
     * @return true if the map editor is empty
     */
    private boolean isVirtuallyEmpty(IDisplayPaneContainer dc) {
        boolean isEmpty = true;

        if (EnsembleTool.isMapEditor(dc)) {
            ResourceList rscList = dc.getActiveDisplayPane().getDescriptor()
                    .getResourceList();

            /*
             * if the resource isn't any of the initial (default) on-load
             * resources then the resource list is considered not empty.
             */
            for (ResourcePair rp : rscList) {
                if ((!rp.getResource().getProperties().isSystemResource())
                        && (!rp.getResource().getProperties().isMapLayer())) {
                    isEmpty = false;
                    break;
                }
            }
        } else {
            throw new IllegalArgumentException(
                    "Method must take display container that is a map editor.");
        }
        return isEmpty;
    }

    /**
     * This method is associated with the AbstractTool parent class. It would
     * normally be associated with an action such as a button click, but the RCP
     * requires those classes to have a default constructor, and this class is a
     * singleton. So the EnsembleToolAction class is the pass-thru to this
     * parent's AbstractTool whose execute method calls this execute method.
     * 
     * When the Ensemble Tool is initially opened (i.e. menu item is clicked)
     * then the super.editor data member will be null so set up the ensemble
     * tool if the main editor is the map editor.
     * 
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        IDisplayPaneContainer dc = EditorUtil.getActiveVizContainer();

        if (dc == null) {
            return null;
        }

        /*
         * Single instance of editor part listener;
         */
        if (theEditorsListener == null) {
            theEditorsListener = new EnsembleEditorPartListener();
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
                    .addPartListener(theEditorsListener);
        }

        /*
         * If the active display pane is an standard map editor (VizMapEditor
         * with a title of "Map") and the active editor already does not already
         * have a tool layer in it.
         */
        if (EnsembleTool.isMapEditor(dc) && !hasToolLayer(dc)) {

            if (!isVirtuallyEmpty(dc)) {
                MessageDialog.openWarning(Display.getCurrent().getActiveShell(),
                        "Ensemble Tool",
                        "The Ensemble Tool must be opened in a new map editor.");
                return null;

            }
            super.setEditor(dc);
            editor.addRenderableDisplayChangedListener(this);

            lastUsedLegendsEditorPane = editor;

            if (ensembleToolViewer == null) {
                try {
                    ensembleToolViewer = initView();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            try {
                EnsembleTool.getInstance().createToolLayer(editor,
                        EnsembleToolMode.LEGENDS_PLAN_VIEW);
            } catch (VizException e1) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to create tool layer for Plan View (map) editor",
                        e1);
            }

            refreshTool(false);

            setHideLegendsMode();
            isToolRunning = true;

        }
        return null;

    }

    /**
     * Utility method to hide product legends.
     */
    public void setHideLegendsMode() {
        D2DLegendResource lgdRsc = null;
        if (editor != null && editor.getActiveDisplayPane() != null
                && editor.getActiveDisplayPane().getDescriptor() != null) {
            List<D2DLegendResource> rscList = editor.getActiveDisplayPane()
                    .getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(D2DLegendResource.class);
            if (rscList != null && !rscList.isEmpty()) {
                lgdRsc = rscList.get(0);
                lgdRsc.setLegendMode(LegendMode.HIDE);
            }

        }
    }

    /**
     * Get the tool mode from the tool layer.
     * 
     * @return returns the tool mode of matrix, legends_plan_view, or
     *         legends_time_series
     */
    public EnsembleTool.EnsembleToolMode getToolMode() {
        EnsembleTool.EnsembleToolMode tm = EnsembleTool.EnsembleToolMode.NONE;
        if (getToolLayer() != null) {
            tm = getToolLayer().getToolMode();
        }
        return tm;
    }

    /**
     * This method is called when the ensemble tool layer is being disposed.
     * 
     * Remove any association of the tool layer with the resource manager and
     * initially clean up the ensemble view by clearing by the tool layer's
     * mode, disabling the viewer widget, and minimizing the viewer.
     * 
     * If there was another tool layer previously used in another editor then
     * assign the current editor to be that previously used editor.
     * 
     * If this is the last tool layer then dispose of the entire Ensemble Tool.
     * 
     * @param toolLayer
     *            The tool layer that was just closed.
     */

    synchronized public void handleToolLayerDisposed(
            EnsembleToolLayer toolLayer) {

        if (!PlatformUI.getWorkbench().isClosing()) {

            if (ensembleToolViewer != null) {
                /*
                 * The matrix needs special attention as the composite needs to
                 * be disposed of so subsequent requests to open the Matrix
                 * Navigator will result in a fresh start.
                 */
                if (toolLayer.getToolMode() == EnsembleToolMode.MATRIX) {
                    ensembleToolViewer.resetMatrixNavigator();
                    if (lastUsedMatrixEditorPane != null) {
                        setEditorPartState(lastUsedMatrixEditorPane,
                                ViewerWindowState.CLOSE);
                    }
                }
                /*
                 * The time series editor can also be removed if it had an
                 * ensemble tool layer associated with it.
                 */
                else if (toolLayer
                        .getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES) {
                    if (lastUsedMatrixEditorPane != null) {
                        setEditorPartState(lastUsedTimeSeriesEditorPane,
                                ViewerWindowState.CLOSE);
                    }
                }

            }

            editor.removeRenderableDisplayChangedListener(this);

            switch (toolLayer.getToolMode()) {
            case LEGENDS_PLAN_VIEW:
                lastUsedLegendsEditorPane = null;
                break;
            case LEGENDS_TIME_SERIES:
                lastUsedTimeSeriesEditorPane = null;
                break;
            case MATRIX:
                lastUsedMatrixEditorPane = null;
                break;
            case NONE:
                break;
            }

            /*
             * There is no longer a tool layer in the active editor so set to
             * null.
             */
            editor = null;

            /*
             * Reset the editor to any valid previously working editor.
             */
            if (lastUsedLegendsEditorPane != null) {
                editor = lastUsedLegendsEditorPane;
            }
            if (editor == null) {
                if (lastUsedMatrixEditorPane != null) {
                    editor = lastUsedMatrixEditorPane;
                }
                if (editor == null) {
                    if (lastUsedTimeSeriesEditorPane != null) {
                        editor = lastUsedTimeSeriesEditorPane;
                    }
                }
            }

        }
    }

    /**
     * Are there ensemble tool layers left to manage? If not, then clean up and
     * reset the entire Ensemble Tool to its pre-invoked state.
     */
    public void checkForLastToolLayerClosed() {
        if (ensembleToolViewer != null) {
            if (!moreToolLayersExist()) {
                setViewerWindowState(ViewerWindowState.CLOSE);
                dispose();
                return;
            } else {
                if (editor != null) {
                    refreshView();
                }
            }
        }
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (rp.getResource() instanceof EnsembleToolLayer) {
            EnsembleToolLayer etl = (EnsembleToolLayer) rp.getResource();
            etl.getDescriptor().getResourceList()
                    .removePostRemoveListener(EnsembleTool.getInstance());
            EnsembleTool.getInstance().handleToolLayerDisposed(etl);
        }
    }

    /**
     * Activate the given editor.
     * 
     * @param anotherEditor
     */
    public void showEditor(IDisplayPaneContainer anotherEditor) {

        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.activate((IWorkbenchPart) anotherEditor);
    }

    /**
     * Add a new ensemble tool layer to the given editor.
     */
    public EnsembleToolLayer createToolLayer(IDisplayPaneContainer editorPane,
            EnsembleToolMode mode) throws VizException {

        EnsembleToolLayer etl = null;
        if (editorPane != null) {
            super.setEditor(editorPane);
            editor.addRenderableDisplayChangedListener(this);
            IDescriptor descr = editor.getActiveDisplayPane().getDescriptor();
            if (mode == EnsembleToolMode.MATRIX) {
                etl = constructToolLayer(editor, descr,
                        EnsembleToolMode.MATRIX);
            } else if (mode == EnsembleToolMode.LEGENDS_PLAN_VIEW) {
                etl = constructToolLayer(editor, descr,
                        EnsembleToolMode.LEGENDS_PLAN_VIEW);
            } else if (editor instanceof TimeSeriesEditor) {
                etl = constructToolLayer(editor, descr,
                        EnsembleToolMode.LEGENDS_TIME_SERIES);
            }
            if (etl != null) {
                descr.getResourceList().add(etl);
                etl.setEditable(true);
                etl.registerListener((IToolLayerChanged) this);
                etl.issueRefresh();
                editor.refresh();
            }
        }
        return etl;
    }

    /**
     * Create an ensemble tool layer for a given tool mode.
     */
    private EnsembleToolLayer constructToolLayer(
            IDisplayPaneContainer editorPane, IDescriptor desc,
            EnsembleTool.EnsembleToolMode toolMode) throws VizException {

        String fullName = EnsembleToolLayer.DEFAULT_NAME + " "
                + ((AbstractEditor) editorPane).getTitle().trim();

        EnsembleToolLayerData etld = new EnsembleToolLayerData();
        LoadProperties props = null;
        if (desc instanceof TimeSeriesDescriptor) {
            props = new LoadProperties();
            props.setResourceType(ResourceType.TIME_SERIES);
            props.setLoadWithoutData(true);
        } else {
            props = new LoadProperties();
        }

        EnsembleToolLayer tool = etld.construct(props, desc);
        tool.setToolMode(toolMode);
        tool.setInnerName(fullName);

        return tool;

    }

    /**
     * Given a viz resource, return the associated resource pair found in the
     * active ensemble tool layer.
     * 
     * @param rsc
     * @return resource pair which contains the resource argument
     */
    public ResourcePair getResourcePair(AbstractVizResource<?, ?> rsc) {

        ResourcePair rp = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            rp = etl.getResourcePair(rsc);
        }
        return rp;
    }

    /**
     * Given a model source and a field/plane pair, return the associated
     * resource pair found in the active tool layer.
     */
    public ResourcePair getResourcePair(FieldPlanePair e,
            ModelSourceKind modelSrc) {

        ResourcePair rp = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            rp = etl.getResourcePair(e, modelSrc);
        }
        return rp;
    }

    /**
     * Update the legend time information in the navigator view.
     */
    public void frameChanged(FramesInfo framesInfo) {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.frameChanged(framesInfo);
        }
    }

    public IDisplayPaneContainer getActiveEditor() {
        return editor;
    }

    public IDisplayPaneContainer getCurrentLegendsEditor() {
        return lastUsedLegendsEditorPane;
    }

    public IDisplayPaneContainer getCurrentMatrixEditor() {
        return lastUsedMatrixEditorPane;
    }

    public IDisplayPaneContainer getCurrentTimeSeriesEditor() {
        return lastUsedTimeSeriesEditorPane;
    }

    /**
     * Refresh the tool based on the currently active editor.
     */
    public void refreshToolByActiveEditor() {

        IDisplayPaneContainer dc = EditorUtil.getActiveVizContainer();
        setEditor(dc);
        refreshTool(false);
    }

    /**
     * Refresh the tool based on the currently active editor.
     */
    public void refreshToolByEditor(IDisplayPaneContainer pane) {

        setEditor(pane);
        refreshTool(false);
    }

    /**
     * Reflect the change in editor. If the editor has a tool layer then upate
     * the tool mode. Keep track of the last used editor. Set by calling parent.
     */
    public void setEditor(IDisplayPaneContainer editorPane) {
        if (EnsembleTool.hasToolLayer(editorPane)) {
            super.setEditor(editorPane);
            EnsembleToolMode toolMode = getToolMode(editorPane);
            setToolMode(toolMode);
            switch (toolMode) {
            case LEGENDS_PLAN_VIEW:
                lastUsedLegendsEditorPane = editor;
                break;
            case LEGENDS_TIME_SERIES:
                lastUsedTimeSeriesEditorPane = editor;
                break;
            case MATRIX:
                lastUsedMatrixEditorPane = editor;
                break;
            case NONE:
                break;
            }
        } else {
            /*
             * disable the tool viewer but not for matrix mode which will clear
             * itself.
             */
            if (ensembleToolViewer != null) {
                if (lastUsedMatrixEditorPane != editor) {
                    ensembleToolViewer.clearAllByActiveMode();
                }
                ensembleToolViewer.setEditable(false);
                super.setEditor(null);
            }
        }
    }

    public void setToolMode(EnsembleTool.EnsembleToolMode toolMode) {
        notifyToolModeChanged(toolMode);
    }

    /**
     * This method will set the visibility state of the EnsembleToolViewer to
     * "show with focus", "show without focus", "minimized", or "closed".
     */
    public void setViewerWindowState(final ViewerWindowState windowState) {

        Display.getDefault().syncExec(new Runnable() {
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

    /**
     * TODO: This method is currently only in support of the requirement that we
     * need to be able to close the Matrix editor programmatically.
     * 
     * TODO: This method only defines a CLOSE capability but remains as more
     * generic method if future needs also require other states to be handled
     * (i.e. minimized, restored, etc)
     */
    public void setEditorPartState(final IDisplayPaneContainer editorPane,
            final ViewerWindowState windowState) {

        Display.getDefault().syncExec(new Runnable() {
            @Override
            public void run() {

                IWorkbenchPage page = null;
                final EditorPart editorPart = (EditorPart) editorPane;

                // Get the active page
                page = editorPart.getSite().getPage();
                if (page == null) {
                    return;
                }

                if (windowState == ViewerWindowState.CLOSE) {
                    page.closeEditor(editorPart, false);
                }
            }
        });
    }

    /**
     * This method should be called when the Ensemble Tool is initially opened
     * or when the user has closed the view for a given ensemble tool layer, and
     * then activated another editor with another ensemble tool layer in it. It
     * returns the newly created EnsmebleToolViewer, which is, in essence, a
     * singleton.
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

    /**
     * Returns the tool mode based on the given editor.
     */
    public static EnsembleToolMode getToolMode(
            IDisplayPaneContainer editorPane) {

        EnsembleToolMode toolMode = EnsembleToolMode.NONE;
        if (editorPane instanceof TimeSeriesEditor) {
            toolMode = EnsembleToolMode.LEGENDS_TIME_SERIES;
        } else if (EnsembleTool.isMapEditor(editorPane)) {
            toolMode = EnsembleToolMode.LEGENDS_PLAN_VIEW;
        } else if (EnsembleTool.isMatrixEditor(editorPane)) {
            toolMode = EnsembleToolMode.MATRIX;
        }
        return toolMode;
    }

    /**
     * Convenience method to get the ensemble tool layer in the currently active
     * editor.
     */
    public EnsembleToolLayer getToolLayer() {

        EnsembleToolLayer foundToolLayer = null;
        if (editor != null) {
            foundToolLayer = EnsembleTool.getToolLayer(editor);
        }
        return foundToolLayer;
    }

    public EnsembleToolViewer getEnsembleToolViewer() {
        return ensembleToolViewer;
    }

    /**
     * Given a top level ensemble resource name, unload its members.
     */
    public void unloadResourcesByName(String topLevelEnsembleName) {

        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.unloadAllResourcesByName(topLevelEnsembleName);
        }
    }

    /**
     * Exercise a given calculation on the visible resources of a given tool
     * layer.
     */
    public void calculate(Calculation algorithm) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.calculate(algorithm);
        }
    }

    /**
     * Exercise a given calculation against a given range on the visible
     * resources of a given tool layer.
     */
    public void calculate(Calculation algorithm, Range range) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.calculate(algorithm, range);
        }
    }

    /**
     * This tool manager is responsible for updating the state of the navigator
     * widget, EnsembleToolViewer. Call this method to refresh the view to
     * reflect a change in state of the active editor.
     */
    public void refreshView() {

        if (isToolEditable()) {
            ensembleToolViewer
                    .refreshInput(getToolLayer().getResourceHolders());
            ensembleToolViewer.setEditable(true);
        }
    }

    public void refreshEditor() {
        if (editor != null) {
            editor.refresh();
        }
    }

    /**
     * Another (non-ensemble-tool) tool layer was loaded. Minimize the ensemble
     * tool view if dictated by user preferences.
     */
    synchronized private void refreshToolForeignLoad() {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.setEditable(false);
            if (isMinimizeOnForeignToolLoadPreference()) {
                setViewerWindowState(ViewerWindowState.MINIMIZED);
            }
            foreignEditableToolLoading = false;
        }
    }

    /**
     * Refresh the tool/viewer based on the existence of a tool layer in the
     * current editor.
     * 
     * NOTE: Make sure the super.editor member is set before calling this
     * method.
     */
    synchronized public void refreshTool(boolean isAlreadyActivated) {

        EnsembleToolLayer etl = getToolLayer();

        if (ensembleToolViewer != null) {
            if (etl == null) {
                /* disable the tool viewer */
                ensembleToolViewer.setEditable(false);
                if (!isAlreadyActivated) {
                    setViewerWindowState(ViewerWindowState.MINIMIZED);
                }
            } else {
                ensembleToolViewer.setToolMode(etl.getToolMode());
                ensembleToolViewer.setEditable(etl.isEditable());
                if (!isAlreadyActivated) {
                    if (swapState == SwapState.SWAPPED_IN) {
                        if (etl.isEditable()) {
                            setViewerWindowState(
                                    ViewerWindowState.SHOW_WITHOUT_FOCUS);
                        } else {
                            setViewerWindowState(ViewerWindowState.MINIMIZED);
                        }
                        swapState = SwapState.NOT_SWAPPING;
                    } else if (swapState == SwapState.SWAPPED_OUT) {
                        setViewerWindowState(ViewerWindowState.MINIMIZED);
                        swapState = SwapState.NOT_SWAPPING;
                    }
                } else {
                    if (GlobalPreferencesComposite
                            .isMinimizeOnToggleUneditablePreference()) {
                        setViewerWindowState(ViewerWindowState.MINIMIZED);
                    }
                }
            }
            refreshView();
        }

    }

    public List<AbstractResourceHolder> getResourceList() {
        List<AbstractResourceHolder> resourceHolders = null;
        if (getToolLayer() != null) {
            resourceHolders = getToolLayer().getResourceHolders();
        } else {
            resourceHolders = getEmptyResourceList();
        }
        return resourceHolders;
    }

    public List<AbstractResourceHolder> getEmptyResourceList() {
        List<AbstractResourceHolder> resourceHolders = new ArrayList<>();
        return resourceHolders;
    }

    /**
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

    /**
     * Turn the tool layer for the ensemble tool on or off.
     */
    public void setEditable(boolean makeEditable) {

        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.setEditable(makeEditable);
        }
    }

    /**
     * Are there resources in the active tool layer?
     */
    public boolean isActiveToolLayerEmpty() {
        boolean isEmpty = true;
        EnsembleToolLayer toolLayer = getToolLayer();
        if (toolLayer != null) {
            isEmpty = toolLayer.getResourceList().isEmpty();
        }
        return isEmpty;
    }

    /**
     * Is the Ensemble Tool "on"? (i.e. is it filtering and trapping incoming
     * resources). Only if the activeToolLayer is editable.
     */
    public boolean isToolEditable() {

        boolean isToolEditable = false;
        EnsembleToolLayer etl = getToolLayer();
        if ((etl != null) && (ensembleToolViewer != null)) {
            if (etl.isEditable()) {
                isToolEditable = true;
            }
        }
        return isToolEditable;
    }

    /**
     * Is the user preference set to minimize the viewer when another tool type
     * is being loaded into the active display?
     */
    public boolean isMinimizeOnForeignToolLoadPreference() {
        boolean isMinimizeOnForeignToolLoad = false;
        if (isToolEditable()) {
            isMinimizeOnForeignToolLoad = GlobalPreferencesComposite
                    .isMinimizeOnForeignToolLoadPreference();
        }
        return isMinimizeOnForeignToolLoad;
    }

    /**
     * Is the user preference set to set the active ensemble tool layer to
     * editable when
     */
    public boolean isMakeEditableOnRestorePreference() {
        boolean isMakeEditableOnRestore = false;
        if (isToolEditable()) {
            isMakeEditableOnRestore = GlobalPreferencesComposite
                    .isEditableOnRestorePreference();
        }
        return isMakeEditableOnRestore;
    }

    /**
     * Is the user preference set to set the active ensemble tool layer to
     * editable when it is being swapped-in?
     */
    public boolean isMakeEditableOnSwapInPreference() {
        boolean isMakeEditableOnSwapIn = false;
        if (isToolEditable()) {
            isMakeEditableOnSwapIn = GlobalPreferencesComposite
                    .isEditableOnSwapInPreference();
        }
        return isMakeEditableOnSwapIn;
    }

    /**
     * Did the ensemble tool renderable display customizer recognize a new
     * editor being created?
     */
    public void prepareForNewEditor() {
        ensembleToolViewer.prepareForNewToolInput();
    }

    /**
     * Is another editable tool type (i.e. not an ensemble tool) in the process
     * of loading?
     */
    public boolean isForeignEditableToolLoading() {
        return foreignEditableToolLoading;
    }

    /**
     * Did the ensemble tool renderable display customizer recognize another
     * tool type (i.e. not an ensemble tool) loading?
     */
    public void setForeignEditableToolLoading() {
        foreignEditableToolLoading = true;
    }

    /**
     * Clear any flags associated with the tool layer and view editable states.
     */
    public void clearEditableToggleFlags() {
        foreignEditableToolLoading = false;
    }

    /**
     * Stores the most recent expansion of the legend tree resources.
     */
    public void setExpandedElements(List<EnsembleMembersHolder> expandedElems) {
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            etl.setExpandedElements(expandedElems);
        }
    }

    /**
     * Gets the most recent expansion of the legend tree resources.
     */
    public List<EnsembleMembersHolder> getExpandedElements() {
        List<EnsembleMembersHolder> expandedElems = null;
        EnsembleToolLayer etl = getToolLayer();
        if (etl != null) {
            expandedElems = etl.getExpandedElements();
        } else {
            expandedElems = new ArrayList<EnsembleMembersHolder>();

        }
        return expandedElems;
    }

    /**
     * The navigation operation is one of: UP_ARROW, DOWN_ARROW, RIGHT_ARROW, or
     * LEFT_ARROW. It is generated by the ensemble tool plugin extensions.
     * Delegation method for the ensemble tool viewer.
     * 
     * @param operationmode
     */
    public void matrixNavigationRequest(
            MatrixNavigationOperation operationmode) {
        if (this.getToolMode() == EnsembleToolMode.MATRIX) {
            if (ensembleToolViewer != null) {
                ensembleToolViewer.matrixNavigationRequest(operationmode);
            }
        }
    }

    /**
     * Unload all resources from the active tool layer and clear the ensemble
     * tool viewer.
     */
    public void clearToolLayer() {
        if (getToolLayer() != null) {
            getToolLayer().unloadAllResources();
        }
    }

    /**
     * Get the number of extant ensemble tool layers.
     * 
     * @return the number of extant ensemble tool layers.
     */
    private int getToolLayerCount() {
        int toolLayersCount = 0;

        IEditorReference[] editorRefs = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage()
                .getEditorReferences();

        for (IEditorReference er : editorRefs) {
            IEditorPart editorPart = er.getEditor(false);
            if (editorPart instanceof VizMapEditor
                    || editorPart instanceof TimeSeriesEditor) {
                IDisplayPaneContainer pane = (AbstractEditor) editorPart;
                if (hasToolLayer(pane)) {
                    toolLayersCount++;
                }
            }
        }
        return toolLayersCount;

    }

    /**
     * Check all editors in the workbench and see if any more ensemble tool
     * layers exist.
     */
    private boolean moreToolLayersExist() {

        boolean moreToolLayersExist = false;

        IEditorReference[] editorRefs = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage()
                .getEditorReferences();

        for (IEditorReference er : editorRefs) {
            IEditorPart editorPart = er.getEditor(false);
            if (editorPart instanceof VizMapEditor
                    || editorPart instanceof TimeSeriesEditor) {
                IDisplayPaneContainer pane = (AbstractEditor) editorPart;
                if (hasToolLayer(pane)) {
                    moreToolLayersExist = true;
                    break;
                }
            }
        }
        return moreToolLayersExist;
    }

    /**
     * Currently, two CAVE Editors are compatible with the Ensemble Tool: the
     * VizMapEditor (for both the Legend and Matrix mode) and the
     * TimeSeriesEditor. Given a part reference, return whether the editor is
     * compatible with the Ensemble Tool.
     */
    public static EnsembleToolCompatibility getEditorCompatibility(
            IWorkbenchPartReference partRef) {

        EnsembleToolCompatibility compatibility = EnsembleToolCompatibility.NOT_COMPATIBLE;

        IWorkbenchPart workbenchPart = partRef.getPart(false);
        if (workbenchPart instanceof VizMapEditor
                || workbenchPart instanceof TimeSeriesEditor) {
            IDisplayPaneContainer editor = (IDisplayPaneContainer) workbenchPart;
            if (EnsembleTool.hasToolLayer(editor)) {
                compatibility = EnsembleToolCompatibility.COMPATIBLE_CONTAINS_ENSEMBLE_TOOL_LAYER;
            } else {
                compatibility = EnsembleToolCompatibility.COMPATIBLE_NO_ENSEMBLE_TOOL_LAYER;
            }
        }
        return compatibility;
    }

    /**
     * Is the given editor pane a Time Series editor?
     * 
     * @param editorPane
     * @return
     */
    public static boolean isTimeSeriesEditor(IDisplayPaneContainer editorPane) {
        boolean isTimeSeriesEditor = false;
        EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(editorPane);
        if (toolLayer != null) {
            isTimeSeriesEditor = (toolLayer
                    .getToolMode() == EnsembleToolMode.LEGENDS_TIME_SERIES);
        }
        return isTimeSeriesEditor;
    }

    /**
     * Is the given editor pane a Matrix editor?
     * 
     * @param editorPane
     * @return
     */
    public static boolean isMatrixEditor(IDisplayPaneContainer editorPane) {
        boolean isMatrixEditor = false;
        EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(editorPane);
        if (toolLayer != null) {
            isMatrixEditor = (toolLayer
                    .getToolMode() == EnsembleToolMode.MATRIX);
        }
        return isMatrixEditor;
    }

    /**
     * Is the given editor pane a Map editor?
     * 
     * This boolean method must work when there is no tool layers already
     * associated with the given editor.
     * 
     * @param editorPane
     * @return
     */
    public static boolean isMapEditor(IDisplayPaneContainer editorPane) {
        boolean isMapEditor = false;
        EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(editorPane);
        if (toolLayer != null) {
            isMapEditor = (toolLayer
                    .getToolMode() == EnsembleToolMode.LEGENDS_PLAN_VIEW);
        } else {
            /*
             * NOTE: The matrix editor could also pass this RTTI test but
             * currently the Matrix editor is only created in one place and will
             * already have a matrix-flavoured tool layer associated with it so
             * this test will never get reached for a Matrix-flavoured
             * VizMapEditor.
             */
            if (editorPane instanceof VizMapEditor) {
                isMapEditor = true;
            }
        }
        return isMapEditor;
    }

    /**
     * Given a resource to load, find the EnsembleToolLayer in the same display
     * in order to be able to associate the incoming resource with the proper
     * tool layer.
     */
    public static EnsembleToolLayer getToolLayer(
            AbstractVizResource<?, ?> rsc) {
        EnsembleToolLayer toolLayer = null;

        /* find the ensemble tool layer */
        ResourceList r = rsc.getDescriptor().getResourceList();
        if (r != null) {
            List<AbstractVizResource<?, ?>> resources = r
                    .getResourcesByType(EnsembleToolLayer.class);
            if (resources == null || resources.isEmpty()) {
                return null;
            }
            AbstractVizResource<?, ?> innerResource = resources.get(0);
            if (innerResource == null) {
                return null;
            }
            toolLayer = (EnsembleToolLayer) innerResource;
        }
        return toolLayer;
    }

    /**
     * Get the ensemble tool layer from the given editor.
     */
    public static EnsembleToolLayer getToolLayer(
            IDisplayPaneContainer editorPane) {

        EnsembleToolLayer toolLayer = null;
        if (editorPane instanceof VizMapEditor
                || editorPane instanceof TimeSeriesEditor) {
            ResourceList rscList = editorPane.getActiveDisplayPane()
                    .getDescriptor().getResourceList();
            List<EnsembleToolLayer> toolLayers = rscList
                    .getResourcesByTypeAsType(EnsembleToolLayer.class);
            if (!toolLayers.isEmpty()) {
                toolLayer = toolLayers.get(0);
            }
        }

        return toolLayer;
    }

    /**
     * Does the given editor have an assoicated ensemble tool layer?
     */
    public static boolean hasToolLayer(IDisplayPaneContainer editorPane) {
        return (EnsembleTool.getToolLayer(editorPane) != null);
    }

    /**
     * Called when the user closes the ensemble tool viewer by using the close
     * 'x' button on the view tab. Check with the user that they are okay
     * closing the active tool layer.
     * 
     * This method returns an ISaveablePart2.CANCEL if the editable state of the
     * viewer is set to 'not-editable' or there is no tool layer in the current
     * editor.
     * 
     * If the user chooses to close the active tool layer then return
     * ISaveablePart2.NO. Otherwise return ISaveablePart2.CANCEL.
     */
    public int verifyCloseTool() {
        int userResponse = ISaveablePart2.NO;

        if (PlatformUI.getWorkbench().isClosing()) {
            return ISaveablePart2.NO;
        }

        if (editor != null) {

            /*
             * Don't allow the user to close the viewer if the tool layer is not
             * editable.
             */
            if (getToolLayer() == null || !getToolLayer().isEditable()) {
                return ISaveablePart2.CANCEL;
            }

            /*
             * if there are no resources in the active tool layer then default
             * to close.
             */
            boolean shouldClose = true;
            if (!getToolLayer().getResourceList().isEmpty()) {
                shouldClose = MessageDialog.openConfirm(
                        Display.getCurrent().getActiveShell(),
                        "Confirm Active Ensemble Tool Layer",
                        "Close the current tool layer?");
            }
            if (shouldClose) {
                ignorePartActivatedEvent(true);
                /*
                 * If there are more tool layers out there then no need to close
                 * the view.
                 */
                if (getToolLayerCount() > 1) {
                    userResponse = ISaveablePart2.CANCEL;
                } else {
                    userResponse = ISaveablePart2.NO;
                }
                /*
                 * When the active editor's ensemble tool viewer is requested to
                 * be closed, the policy is that tool layer be closed also.
                 */
                getToolLayer().unload();
            } else {
                userResponse = ISaveablePart2.CANCEL;
            }
        }
        return userResponse;
    }

    /**
     * Store the state of whether the tool layer was swapped in or out.
     */
    @Override
    synchronized public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {

        /*
         * New editor becoming coming active; only change the swap state if the
         * incoming pane has an ensemble tool layer in it. Swapping in will
         * override the swapped out state.
         */
        if (type == DisplayChangeType.ADD) {
            if (EnsembleTool
                    .hasToolLayer(newRenderableDisplay.getContainer())) {
                swapState = SwapState.SWAPPED_IN;
            }
        }

        /*
         * Remove to side view; only change the swap state if the outgoing pane
         * has an ensemble tool layer in it.
         */
        else if (type == DisplayChangeType.REMOVE) {
            if (EnsembleTool
                    .hasToolLayer(newRenderableDisplay.getContainer())) {
                swapState = SwapState.SWAPPED_OUT;
            }
        }

    }

    /**
     * When the user closes the ensemble tool viewer by using the close 'x'
     * button on the view tab, this method is called to null out the viewer
     * reference. Also, turns off the editor part listener ignore flag.
     */
    public void handleViewerDisposed() {
        ensembleToolViewer = null;
        ignorePartActivatedEvent(false);
    }

    public ResourceList getActiveResourceList() {
        ResourceList rl = null;
        if (getToolLayer() != null) {
            rl = getToolLayer().getResourceList();
        } else {
            rl = new ResourceList();
        }
        return rl;
    }

    public boolean activeToolLayerContains(AbstractVizResource<?, ?> rsc) {
        boolean containsRsc = false;
        if (this.getToolLayer() != null) {
            containsRsc = getToolLayer().getResourceList().containsRsc(rsc);
        }
        return containsRsc;

    }

    public void turnOffOtherHistograms(HistogramGridResourceHolder hgr) {
        if (getToolLayer() != null) {
            getToolLayer().turnOffOtherHistograms(hgr);
        }
    }

    public void updateGenerated(AbstractResourceHolder arh) {
        if (getToolLayer() != null) {
            getToolLayer().updateGenerated(arh);
        }
    }

    /**
     * This method is meant to be called when any resource in the resource list
     * has changed and the gui tree in the viewer needs to be updated (i.e. not
     * full refresh). The element argument is either a resource name or an
     * abstract resource holder.
     */
    public void updateElementInView(AbstractResourceHolder arh) {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.updateElementInTree(arh);
        }
    }

    public void clearAllByMode(EnsembleToolMode mode) {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.clearAllByMode(mode);
        }
    }

    @Override
    public void addToolModeChangedListener(IToolModeChangedListener listener) {
        toolModeChangedListeners.add(listener);
    }

    @Override
    public void removeToolModeChangedListener(
            IToolModeChangedListener listener) {
        toolModeChangedListeners.remove(listener);
    }

    @Override
    public void notifyToolModeChanged(EnsembleToolMode toolmode) {
        for (IToolModeChangedListener tmcl : toolModeChangedListeners) {
            tmcl.toolModeChanged(toolmode);
        }
    }

}
