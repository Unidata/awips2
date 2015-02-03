package gov.noaa.gsd.viz.ensemble.navigator.ui.layer;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.ViewerWindowState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.xy.timeseries.TimeSeriesEditor;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.VizMultiPaneEditor;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.tools.AbstractTool;

/*
 *   The Ensemble Tool is a CAVE feature/tool that currently runs in the D/2D 
 * perspective by either clicking an optional button on the CAVE perspective
 * menu, or choosing a menu item of the same name in the Tools menu. Depending
 * upon how this tool is initially configured in CAVE there may be only one of
 * those starting buttons available.
 * 
 *   This class is the controlling manager class for the D/2D Ensemble Tool. 
 * When the user opens the Ensemble Tool, a navigator view is opened and an
 * EnsembleToolLayer instance, given the name "Ensemble Tool", gets associated
 * with the currently active AbstractEditor, and also gets set to "editable".
 * It is this active and editable instance of tool layer which allows the user
 * to activate or deactivate the Ensemble Tool for the associated active 
 * editor. 
 * 
 *   When the Ensemble Tool is opened and editable ("powered on"), any 
 * resources subsequently loaded, from the main resource menus or the Volume  
 * Browser, will be virtually associated with the currently active editor
 * and tool layer pair. In addition, these resources will be stored in the 
 * ResourceManager class (see gov.noaa.gsd.viz.ensemble.display.control
 * package). Yet instead of showing these resource's legends in the default 
 * manner of displaying the legend in the active editor, these resources are
 * displayed in the Ensemble Tool's "navigator view", which is a PartView called 
 * the EnsembleToolViewer.
 * 
 * This manager class is therefore responsible for associating and maintaining 
 * all instances of EnsembleToolLayers and their associated AbstractEditor 
 * instances, and adding an additional layer of control for displaying those 
 * resources, and representing those resources in the EnsembleToolViewer.
 *
 * @author polster
 * @author jing
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2014    5056    polster     Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class EnsembleToolManager extends AbstractTool implements
        IRenderableDisplayChangedListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleToolManager.class);

    protected static final int MAX_TOOL_LAYER_COUNT = 5;

    private EnsembleToolLayer activeToolLayer;

    protected EnsembleToolViewer ensembleToolViewer = null;

    protected boolean viewAlreadyClosed = false;

    private IWorkbenchPartReference viewPartRef = null;

    public static EnsembleToolManager SINGLETON = null;

    /*
     * The EnsembleToolManager adheres to a singleton pattern. This method is
     * the accessor to the singleton instance reference.
     */
    public static EnsembleToolManager getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new EnsembleToolManager();
        }
        return SINGLETON;
    }

    private boolean isReady = false;

    /*
     * The EnsembleToolManager adheres to a singleton pattern. This constructor
     * is therefore private.
     * 
     * This ctor creates the ETLMResourceDataManager singleton and also adds a
     * shutdown hook which guarantees that this class is disposed of properly.
     */
    private EnsembleToolManager() {

        ETLMResourceDataManager.getInstance();

    }

    /*
     * Given a viz resource, return the associated resource pair.
     */
    public ResourcePair getResourcePair(AbstractVizResource<?, ?> rsc) {

        ResourcePair rp = null;
        if (activeToolLayer != null) {
            rp = activeToolLayer.getResourcePair(rsc);
        }
        return rp;
    }

    /*
     * Accessor for the current active tool layer. There is only one active tool
     * layer at any time.
     */
    public EnsembleToolLayer getActiveToolLayer() {
        return activeToolLayer;
    }

    /*
     * This method sets the active tool layer from the supplied argument.
     */
    public void setActiveToolLayer(EnsembleToolLayer etl) {

        etl.issueRefresh();
        activeToolLayer = etl;
        refreshView();
    }

    /*
     * This tool manager is responsible for updating the state of the navigator
     * widget, EnsembleToolViewer. Call this method to refresh the view to
     * reflect a change in state of the active editor.
     */
    public void refreshView() {

        if (ensembleToolViewer != null) {
            ensembleToolViewer.refreshInput();
        }

    }

    /*
     * Given an AbstractEditor, switch to that editor and activate that editor's
     * tool layer.
     */
    public void switchToEditor(AbstractEditor editor, boolean viewJustClosed) {

        // get the tool layer and set it.

        EnsembleToolLayer t = ETLMResourceDataManager.getInstance()
                .findToolLayer(editor);

        if (t == null) {
            statusHandler
                    .error("No tool layer associated with the given editor.");
            ETLMResourceDataManager.getInstance();
            return;
        }

        // If this is the same tool layer we don't need to reset to the same
        // tool layer, unless the view was closed manually, but then the user
        // decided against closing it, so we reopened the view and now we need
        // to reinitialize the newly opened view.

        ResourceType rt = ResourceType.PLAN_VIEW;
        if (ensembleToolViewer != null) {
            if (TimeSeriesEditor.class.isAssignableFrom(editor.getClass())) {
                rt = ResourceType.TIME_SERIES;
            } else if (VizMultiPaneEditor.class.isAssignableFrom(editor
                    .getClass())) {
                rt = ResourceType.PLAN_VIEW;
            }
            ensembleToolViewer.setEditorType(rt);
        }

        if (t != null) {
            // is this a different tool layer from the current tool layer? Then
            // activate this different tool layer.
            if (t != activeToolLayer) {
                setActiveToolLayer(t);
            }
            // otherwise this is the same tool layer so only setActiveToolLayer
            // if view was closed then reopened ...
            else {
                if (viewJustClosed) {
                    setActiveToolLayer(t);
                }
            }
        }

    }

    /*
     * Given an ensemble tool layer return its associated editor.
     */
    public AbstractEditor findEditor(EnsembleToolLayer tool) {
        AbstractEditor e = ETLMResourceDataManager.getInstance().findEditor(
                tool);
        return e;
    }

    /*
     * Given an editor return its associated ensemble tool layer.
     */
    public EnsembleToolLayer findToolLayer(AbstractEditor editor) {
        EnsembleToolLayer t = ETLMResourceDataManager.getInstance()
                .findToolLayer(editor);
        return t;
    }

    /*
     * Does the given editor have an assoicated ensemble tool layer?
     */
    public boolean hasToolLayer(AbstractEditor editor) {
        EnsembleToolLayer t = ETLMResourceDataManager.getInstance()
                .findToolLayer(editor);
        if (t != null) {
            return true;
        }
        return false;
    }

    /*
     * Add a new ensemble tool layer to the given editor. Keep track of the
     * association in the ETLMResourceDataManager.
     */
    public void addToolLayer(AbstractEditor editor) {

        IPartListener2 editorListener = new EnsembleEditorPartListener(editor);
        IDescriptor descr = editor.getActiveDisplayPane().getDescriptor();
        EnsembleToolLayer t = null;
        try {
            t = ETLMResourceDataManager.getInstance().constructToolLayer(
                    editor, descr, editorListener);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            statusHandler.error("Unable to create a tool layer in editor: "
                    + editor.getTitle());
            return;
        }
        if (t != null) {
            descr.getResourceList().add(t);
            activeToolLayer = t;
            activeToolLayer.getCapability(EditableCapability.class)
                    .setEditable(true);
            editor.addRenderableDisplayChangedListener(this);
            editor.getSite().getPage().addPartListener(editorListener);
        }
    }

    /*
     * Remove the given tool layer. If this was the only tool layer remaining
     * then close the Ensemble Tool.
     */
    public void removeToolLayer(EnsembleToolLayer ensembleToolLayer) {

        AbstractEditor editor = ETLMResourceDataManager.getInstance()
                .findEditor(ensembleToolLayer);
        editor.removeRenderableDisplayChangedListener(this);

        IPartListener2 editorListener = ETLMResourceDataManager.getInstance()
                .findPartListener(ensembleToolLayer);
        editor.getSite().getPage().removePartListener(editorListener);

        ETLMResourceDataManager.getInstance().unload(ensembleToolLayer);

        if (ensembleToolLayer == activeToolLayer) {
            activeToolLayer = null;
            if (!viewAlreadyClosed) {
                if (!isEmpty()) {
                    setViewerWindowState(ViewerWindowState.MINIMIZED);
                    EnsembleToolLayer etl = ETLMResourceDataManager
                            .getInstance().findToolLayer(editor);
                    if (etl != null) {
                        activeToolLayer = etl;
                    } else {
                        try {
                            etl = ETLMResourceDataManager.getInstance()
                                    .constructToolLayer(
                                            editor,
                                            editor.getActiveDisplayPane()
                                                    .getDescriptor(),
                                            editorListener);
                        } catch (VizException e) {
                            // TODO Auto-generated catch block. Please revise as
                            // appropriate.
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                        activeToolLayer = etl;
                    }
                    if ((activeToolLayer != null)
                            && (ensembleToolViewer != null)) {
                        ensembleToolViewer.refreshInput();
                    }
                } else {
                    setViewerWindowState(ViewerWindowState.CLOSE);
                    ensembleToolViewer = null;
                    isReady = false;
                }
            }
        }
    }

    /*
     * Given the group name (aka the top-level title of an ensemble product) set
     * it to be the default resource for subsequent calculations on the
     * ResourceManager.
     */
    public void setEnsembleCalculationResource(String rscGroupName) {
        if (activeToolLayer != null) {
            AbstractEditor editor = ETLMResourceDataManager.getInstance()
                    .findEditor(activeToolLayer);
            EnsembleResourceManager.getInstance()
                    .setEnsembleCalculationResourceName(editor, rscGroupName);
        }
    }

    /*
     * Get the group name (aka the top-level title of an ensemble product) for
     * the default resource for subsequent calculations on the ResourceManager.
     */
    public String getEnsembleCalculationResourceName() {

        String s = "";
        if (activeToolLayer != null) {
            AbstractEditor editor = ETLMResourceDataManager.getInstance()
                    .findEditor(activeToolLayer);
            s = EnsembleResourceManager.getInstance()
                    .getEnsembleCalculationResourceName(editor);
        }
        return s;
    }

    /*
     * Update the legend time information in the navigator view.
     */
    public void updateLegendTimeInfo() {
        if (ensembleToolViewer != null) {
            ensembleToolViewer.updateLegendTimeInfo();
        }
    }

    /*
     * Return the time basis resource name by getting the active editor and
     * requesting the name from the ResourceManager.
     */
    public String getTimeBasisResourceName() {

        String s = "";
        if (activeToolLayer != null) {
            AbstractEditor editor = ETLMResourceDataManager.getInstance()
                    .findEditor(activeToolLayer);
            s = EnsembleResourceManager.getInstance().getTimeBasisResourceName(
                    editor);
        }
        return s;
    }

    /*
     * Return the time basis legend time by getting the active editor and
     * requesting the time from the ResourceManager.
     */
    public String getTimeBasisLegendTime() {

        String s = "";
        if (activeToolLayer != null) {
            AbstractEditor editor = ETLMResourceDataManager.getInstance()
                    .findEditor(activeToolLayer);
            s = EnsembleResourceManager.getInstance().getTimeBasisLegendTime(
                    editor);
        }
        return s;

    }

    /*
     * Close the Ensemble Tool by looping through all controlled tool layers and
     * unloading each one.
     */
    public void closeEnsembleTool() {
        List<EnsembleToolLayer> toolLayers = ETLMResourceDataManager
                .getInstance().getAllToolLayers();
        if (toolLayers != null) {
            for (EnsembleToolLayer t : toolLayers) {
                unloadToolLayer(t);
            }
            isReady = false;
        }
    }

    /*
     * This class is an IRenderableDiasplayedChangedListener so we must override
     * this method. This happens during a swap. (Need a better description of
     * how event is generated and called in the case of a swap).
     */
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {

        EnsembleToolLayer etl = null;
        boolean found = false;
        if (type == DisplayChangeType.ADD) {
            ResourceList list = pane.getDescriptor().getResourceList();
            for (int i = 0; i < list.size(); i++) {
                ResourcePair rp = list.get(i);
                if (rp.getResource() instanceof EnsembleToolLayer) {
                    etl = (EnsembleToolLayer) rp.getResource();
                    AbstractEditor editor = ETLMResourceDataManager
                            .getInstance().findEditor(etl);
                    isReady = true;
                    setActiveToolLayer(etl);
                    EnsembleResourceManager.getInstance().updateFrameChanges(
                            editor);
                    setViewerWindowState(ViewerWindowState.SHOW_WITHOUT_FOCUS);
                    found = true;
                    break;
                }
            }
            if (!found) {
                isReady = false;
                setViewerWindowState(ViewerWindowState.MINIMIZED);

            }
        }
        if (type == DisplayChangeType.REMOVE) {
            // currently do nothing on remove
        }
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
        super.execute(arg0);

        boolean turnOnEditable = false;

        EnsembleToolLayer tmpActiveToolLayer = null;

        // either find an existing ensemble tool layer (ETL) in the active
        // editor ... or create a new ETL. Put the ETL into the activeResource
        // variable.

        // looking for this exact resource, not one that is equivalent
        for (IDisplayPane pane : getSelectedPanes()) {
            IDescriptor desc = pane.getDescriptor();
            Iterator<ResourcePair> iter = desc.getResourceList().iterator();

            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                AbstractVizResource<?, ?> rsc = pair.getResource();

                if (rsc instanceof EnsembleToolLayer) {
                    tmpActiveToolLayer = (EnsembleToolLayer) rsc;
                }
            }
        }

        // if the descriptor has the saved resource then enable editable
        if (tmpActiveToolLayer != null) {
            turnOnEditable = true;
            setActiveToolLayer(tmpActiveToolLayer);
        }
        // otherwise we need to create a new tool layer
        else {
            try {
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                IPartListener2 editorListener = new EnsembleEditorPartListener(
                        editor);

                for (IDisplayPane pane : getSelectedPanes()) {
                    IDescriptor desc = pane.getDescriptor();
                    try {
                        tmpActiveToolLayer = ETLMResourceDataManager
                                .getInstance().constructToolLayer(editor, desc,
                                        editorListener);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }

                    if (tmpActiveToolLayer != null) {
                        desc.getResourceList().add(tmpActiveToolLayer);
                        // This is for a single resource that is referenced in
                        // a four panel pane
                        desc.getTimeMatcher().redoTimeMatching(desc);
                        setActiveToolLayer(tmpActiveToolLayer);

                        editor.addRenderableDisplayChangedListener(this);
                        editor.getSite().getPage()
                                .addPartListener(editorListener);

                        // this is the situation where there are no tool layers
                        // yet associated
                        // with the EnsembleToolManager. Let's add the newly
                        // created tool layer
                        // into the resource manager, so it will be there even
                        // if no one loads
                        // any resources into this editor.
                        EnsembleResourceManager.getInstance()
                                .registerToolLayer(editor);

                        turnOnEditable = true;

                        break;
                    }
                }

                // Assume getSelectedPanes() has already tried to locate the
                // editor.
                // Only do something when editor is found.
                if (editor != null) {
                    for (IDisplayPane pane : editor.getDisplayPanes()) {
                        pane.getDescriptor().getTimeMatcher()
                                .redoTimeMatching(pane.getDescriptor());
                    }

                    editor.refresh();
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to initalized map tool", e);
            }

        }

        if (ensembleToolViewer == null) {
            try {
                ensembleToolViewer = initView();
                if (ensembleToolViewer != null)
                    isReady = true;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            setViewerWindowState(ViewerWindowState.SHOW_WITHOUT_FOCUS);
        }

        // make resource editable if needed
        if ((tmpActiveToolLayer != null) && (turnOnEditable)) {
            EditableManager.makeEditable(tmpActiveToolLayer, true);
        }

        return null;

    }

    /*
     * Return the dislpay panes associated the currently active editor.
     */
    protected IDisplayPane[] getSelectedPanes() {
        if (this.editor == null) {
            this.editor = EditorUtil.getActiveVizContainer();
        }

        if (this.editor == null) {
            // User does not have a display editor showing.
            return new IDisplayPane[0];
        }

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

    /*
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
                    viewAlreadyClosed = true;
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
                        // IWorkbenchPartReference viewPartRef = page
                        // .findViewReference(EnsembleToolViewer.ID);
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
            viewAlreadyClosed = false;
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
                viewAlreadyClosed = false;
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

    /*
     * Given a top level ensemble resource name, unload its members.
     */
    public void unloadResourcesByName(String topLevelEnsembleName) {

        if (activeToolLayer != null) {
            activeToolLayer.unloadAllResourcesByName(topLevelEnsembleName);
        }
    }

    /*
     * Get the map of resources associated with the current active tool layer.
     */
    public Map<String, List<GenericResourceHolder>> getEnsembleResources() {

        Map<String, List<GenericResourceHolder>> ensembleResources = null;
        if (activeToolLayer != null) {
            ensembleResources = activeToolLayer.getEnsembleResources();
        } else {
            ensembleResources = new HashMap<String, List<GenericResourceHolder>>();
        }
        return ensembleResources;
    }

    /*
     * Exercise a given calculation on the visible resources of a given tool
     * layer.
     */
    public void calculate(Calculation algorithm) {
        if (activeToolLayer != null) {
            activeToolLayer.calculate(algorithm);
        }

    }

    /*
     * Exercise a given calculation against a given range on the visible
     * resources of a given tool layer.
     */
    public void calculate(Calculation algorithm, Range range) {
        if (activeToolLayer != null) {
            activeToolLayer.calculate(algorithm, range);
        }

    }

    /*
     * The behavior of the Ensemble Tool is when the user closes the
     * EnsembleToolViewer (ViewPart) by pressing the close "x" button (on the
     * view's title tab).
     * 
     * However, when the user presses the close "x" button on the view, Eclipse
     * will automatically call dispose and then close the view. No option for a
     * warning dialog. Ouch.
     * 
     * So there is currently no elegant way to trap for this event, so instead,
     * after the close to occurs, prompt the user to ask if that is what the
     * user realy wanted to do. If they did not want the view closed then
     * reintialize the view by using the still active tool layer.
     */
    public void close() {

        // there is a situation where the view can already be closed when a
        // user unloads the only (last remaining) tool layer and it closes
        // the view programmatically. In this case the flag viewAlreadyClosed
        // will be set and we will not need to prompt the user ... also, if
        // there is nothing in the viewer then no need to prompt user with
        // close dialog.

        if ((!viewAlreadyClosed) && (!isEmpty())) {

            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            boolean proceed = MessageDialog
                    .openQuestion(
                            shell,
                            "Close Ensemble Tool?",
                            "This will close the Ensemble Tool and all currently loaded tool layers. \n Are you sure you want close the Ensemble Tool?");

            if (proceed) {
                viewAlreadyClosed = true;
                ensembleToolViewer = null;
                closeEnsembleTool();
            } else {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        // running async and sleeping will allow
                        // EnsembleToolViewer.dispose to happen first
                        while (EnsembleToolViewer.isDisposing()) {
                            try {
                                Thread.sleep(10);
                            } catch (InterruptedException e1) {
                                // ignore
                            }
                        }
                        try {
                            ensembleToolViewer = initView();
                            if (ensembleToolViewer != null)
                                isReady = true;
                        } catch (VizException e) {
                            // TODO Auto-generated catch block. Please revise as
                            // appropriate.
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                            return; // Whiskey Tango Foxtrot do we do here?
                        }
                        if (activeToolLayer != null) {
                            AbstractEditor e = findEditor(activeToolLayer);
                            switchToEditor(e, true);
                        }
                    }
                });
            }
        } else {
            // let's reset the flag
            viewAlreadyClosed = false;
            ensembleToolViewer = null;
        }
    }

    /*
     * Are there zero tool layers in the ETLMResourceDataManager?
     */
    private boolean isEmpty() {
        boolean isEmpty = false;
        if (ETLMResourceDataManager.getInstance().getToolLayerCount() == 0) {
            isEmpty = true;
        } else if (ETLMResourceDataManager.getInstance().getToolLayerCount() == 1) {
            if (activeToolLayer != null) {
                Map<String, List<GenericResourceHolder>> ensembleResources = null;
                if (activeToolLayer != null) {
                    ensembleResources = activeToolLayer.getEnsembleResources();
                    isEmpty = ensembleResources.isEmpty();
                }
            }
        }
        return isEmpty;
    }

    /*
     * Given a tool layer, unload and dispose of it.
     */
    public void unloadToolLayer(EnsembleToolLayer toolLayer) {

        if (toolLayer != null) {
            AbstractEditor editor = ETLMResourceDataManager.getInstance()
                    .findEditor(toolLayer);
            if ((toolLayer.getDescriptor() == null)
                    || (toolLayer.getDescriptor() instanceof TimeSeriesDescriptor)) {
                toolLayer.dispose();
            } else {
                toolLayer.unload();
                toolLayer.dispose();
            }
        }
    }

    /*
     * Unload the active tool layer.
     */
    public void unloadActiveToolLayer() {

        if (activeToolLayer != null) {
            unloadToolLayer(activeToolLayer);
        }
    }

    /*
     * Turn the EnsembleToolManager on or off.
     */
    public void setEditable(boolean makeEditable) {
        if (activeToolLayer != null) {
            activeToolLayer.setEditable(makeEditable);
        }

    }

    /*
     * Is the Ensemble Tool ready to handle state change?
     */
    public boolean isReady() {
        return isReady;
    }

    /*
     * Set the ready state to true when the EnsembleToolManager has been
     * initialized.
     */
    public void setReady(boolean b) {
        isReady = b;
    }

    /*
     * The resource data manager keeps track of the associated components of a
     * given tool layer including whether the tool layer is in use, the owning
     * editor, and the editor part listener. These components are stored in
     * instances of the inner class ETLMResourceDataUseState.
     */
    protected static class ETLMResourceDataManager {

        public static ETLMResourceDataManager SINGLETON = null;

        public static ETLMResourceDataManager getInstance() {
            if (SINGLETON == null) {
                SINGLETON = new ETLMResourceDataManager();
            }
            return SINGLETON;
        }

        public int getToolLayerCount() {
            return toolLayerMetaData.size();
        }

        protected List<ETLMResourceData> toolLayerMetaData = new ArrayList<ETLMResourceData>();

        protected ETLMResourceDataManager() {
        }

        protected void unload(EnsembleToolLayer etl) {
            ETLMResourceData foundData = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == etl) {
                    foundData = d;
                    d.clear();
                }
            }
            toolLayerMetaData.remove(foundData);
        }

        protected List<EnsembleToolLayer> getAllToolLayers() {

            List<EnsembleToolLayer> toolLayers = new ArrayList<EnsembleToolLayer>();
            for (ETLMResourceData d : toolLayerMetaData) {
                toolLayers.add(d.getToolLayer());
            }
            return toolLayers;
        }

        protected AbstractEditor findEditor(EnsembleToolLayer tool) {

            AbstractEditor editor = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == tool) {
                    editor = d.getEditor();
                    break;
                }
            }
            return editor;
        }

        protected IPartListener2 findPartListener(EnsembleToolLayer tool) {
            IPartListener2 partListener = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getToolLayer() == tool) {
                    partListener = d.getEditorListener();
                    break;
                }
            }
            return partListener;
        }

        protected EnsembleToolLayer findToolLayer(AbstractEditor editor) {

            EnsembleToolLayer etl = null;
            for (ETLMResourceData d : toolLayerMetaData) {
                if (d.getEditor() == editor) {
                    etl = d.getToolLayer();
                    break;
                }
            }
            return etl;
        }

        protected EnsembleToolLayer constructToolLayer(AbstractEditor editor,
                IDescriptor desc, IPartListener2 editorListener)
                throws VizException {

            GenericToolsResourceData<EnsembleToolLayer> ard = null;
            String fullName = EnsembleToolLayer.DEFAULT_NAME + " "
                    + editor.getTitle().trim();
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
            ETLMResourceData rdState = new ETLMResourceData(tool, editor, ard,
                    editorListener);
            toolLayerMetaData.add(rdState);

            return tool;

        }

        private class ETLMResourceData {

            private AbstractEditor editor = null;

            private EnsembleToolLayer toolLayer = null;

            private GenericToolsResourceData<EnsembleToolLayer> resourceData;

            private IPartListener2 editorListener = null;

            public AbstractEditor getEditor() {
                return editor;
            }

            @SuppressWarnings("unused")
            public GenericToolsResourceData<EnsembleToolLayer> getResourceData() {
                return resourceData;
            }

            public ETLMResourceData(EnsembleToolLayer t, AbstractEditor e,
                    GenericToolsResourceData<EnsembleToolLayer> etlrd,
                    IPartListener2 el) {
                editor = e;
                editorListener = el;
                resourceData = etlrd;
                toolLayer = t;
            }

            public IPartListener2 getEditorListener() {
                return editorListener;
            }

            public EnsembleToolLayer getToolLayer() {
                return toolLayer;
            }

            public void clear() {
                editor = null;
                editorListener = null;
                resourceData = null;
                toolLayer = null;
            }
        }

    }

    public void prepareForNewEditor() {
        ensembleToolViewer.prepareForNewToolInput();
    }
}
