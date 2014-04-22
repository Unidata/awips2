/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.ui.perspectives;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.EditorAreaHelper;
import org.eclipse.ui.internal.EditorReference;
import org.eclipse.ui.internal.EditorStack;
import org.eclipse.ui.internal.LayoutPart;
import org.eclipse.ui.internal.WorkbenchPage;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractModalTool;
import com.raytheon.viz.ui.tools.ModalToolManager;

/**
 * Manager for generic perspectives. Default implementation for general GUI
 * interface management.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------ ----------  ----------- --------------------------
 * Jul 22, 2008             randerso    Initial creation
 * Mar 26, 2013 1799        bsteffen    Fix pan/zoom when in views.
 * Jun 19, 2013 2116        bsteffen    Do not deactivate contexts for parts
 *                                      when closing an inactive perspective.
 * Jan 14, 2014 2594        bclement    added low memory notification
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public abstract class AbstractVizPerspectiveManager implements
        IBackgroundColorChangedListener {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractVizPerspectiveManager.class);

    private static class PerspectivePageListener implements IPageListener {

        @Override
        public void pageActivated(IWorkbenchPage page) {
        }

        @Override
        public void pageClosed(IWorkbenchPage page) {
            page.removePartListener(partListener);
        }

        @Override
        public void pageOpened(IWorkbenchPage page) {
            page.addPartListener(partListener);
        }

    }

    private static class PerspectivePartListener implements IPartListener {

        @Override
        public void partActivated(IWorkbenchPart part) {
            // update editor on last selected modal tool
            if (part instanceof IEditorPart
                    && part instanceof IDisplayPaneContainer) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    for (AbstractModalTool tool : mgr.getToolManager()
                            .getSelectedModalTools()) {
                        if (tool != null && tool.getCurrentEditor() != part) {
                            tool.deactivate();
                            tool.setEditor((IDisplayPaneContainer) part);
                            tool.activate();
                        }
                    }
                }

            }
        }

        @Override
        public void partBroughtToTop(IWorkbenchPart part) {
            partActivated(part);
        }

        @Override
        public void partClosed(IWorkbenchPart part) {
        }

        @Override
        public void partDeactivated(IWorkbenchPart part) {
            // update editor on last selected modal tool
            if (part instanceof IEditorPart
                    && part instanceof IDisplayPaneContainer) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                IWorkbenchPart newPart = part.getSite().getPage()
                        .getActivePart();
                if (newPart instanceof IEditorPart) {
                    if (mgr != null) {
                        for (AbstractModalTool tool : mgr.getToolManager()
                                .getSelectedModalTools()) {
                            if (tool.getCurrentEditor() == part) {
                                tool.deactivate();
                            }
                        }
                    }
                }
            }
        }

        @Override
        public void partOpened(IWorkbenchPart part) {
            // update editor on last selected modal tool
            if (part instanceof IEditorPart
                    && part instanceof IDisplayPaneContainer) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null
                        && !mgr.opened
                        && mgr.getToolManager().getSelectedModalTools()
                                .isEmpty()) {
                    try {
                        mgr.activateDefaultTool(((AbstractEditor) part)
                                .getDefaultTool());
                        if (mgr.getToolManager().getSelectedModalTools()
                                .isEmpty()) {
                            // Hack due to tool activation not sending whether
                            // it should be activated or deactivated and is just
                            // toggling instead. TODO: Make AbstractModalTool
                            // required command parameter for activate or
                            // deactivate
                            mgr.activateDefaultTool(((AbstractEditor) part)
                                    .getDefaultTool());
                        }
                    } catch (VizException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                "Error activating tool set", e);
                    }
                }
            }
        }
    }

    public static IPartListener partListener = new PerspectivePartListener();

    public static IPageListener pageListener = new PerspectivePageListener();

    /** The window the perspective is loaded to */
    protected IWorkbenchWindow perspectiveWindow;

    protected IWorkbenchPage page;

    protected Map<IEditorReference, String> layoutMap = new HashMap<IEditorReference, String>();

    /** Saved editors for the perspective */
    protected List<IEditorReference> perspectiveEditors = new ArrayList<IEditorReference>();

    private IEditorReference activeEditor;

    /** Has the perspective been opened */
    protected boolean opened = false;

    /** StatusLineManager for the window */
    protected IStatusLineManager statusLine;

    /** Perspective id the manager is managing */
    protected String perspectiveId;

    /** The tool manager for the perspective */
    protected ModalToolManager toolManager;

    /** True if the editors should be saved when switching perspectives */
    protected boolean saveEditors = false;

    /** List of perspective dialogs */
    protected List<IPerspectiveSpecificDialog> perspectiveDialogs;

    private List<IContributionItem> items = new ArrayList<IContributionItem>();

    private IMemento storedState;

    private BackgroundColor backgroundColor;

    private String title;

    public AbstractVizPerspectiveManager() {
        // new up a tool manager for the perspective
        toolManager = new ModalToolManager();
        perspectiveDialogs = new CopyOnWriteArrayList<IPerspectiveSpecificDialog>();
    }

    /**
     * Override this method to have items automatically added/removed from the
     * status line on activation/deactivation. This method will be called when
     * the perspective is opened. Items will be disposed and removed when closed
     * and just removed when deactivated. Always return new items from this
     * method as they will be automatically disposed
     * 
     * @return
     */
    protected List<ContributionItem> getStatusLineItems() {
        return new ArrayList<ContributionItem>();
    }

    /**
     * This is called when the perspective is opened from scratch
     */
    protected abstract void open();

    public void close() {
        if (opened) {
            // Cleanup hidden editors
            if (perspectiveEditors.size() > 0) {
                page.closeEditors(
                        perspectiveEditors
                                .toArray(new IEditorReference[perspectiveEditors
                                        .size()]), false);
            }

            perspectiveEditors.clear();
            layoutMap.clear();

            opened = false;

            closeDialogs();
            deactivateContexts();
            removeFromStatusLine();

            if (backgroundColor != null) {
                backgroundColor.removeListener(BGColorMode.GLOBAL, this);
            }
        }
    }

    /**
     * Activate the perspective. This will call open if the perspective has not
     * been activated yet or has been closed, otherwise restores saved editors
     */
    public void activate() {
        if (perspectiveWindow.getActivePage() == null) {
            // don't attempt to load until there is an active page
            return;
        }

        page = perspectiveWindow.getActivePage();

        if (!opened) {
            backgroundColor = BackgroundColor
                    .getInstance(page.getPerspective());
            backgroundColor.addListener(BGColorMode.GLOBAL, this);
            List<?> workbooks = ((WorkbenchPage) page).getEditorPresentation()
                    .getWorkbooks();
            if (workbooks.size() > 1) {
                for (Object o : workbooks) {
                    EditorStack stack = (EditorStack) o;
                    if (stack.getEditors() == null
                            || stack.getEditors().length == 0) {
                        EditorAreaHelper.derefPart(stack);
                    }
                }
            }
            open();
            opened = true;
        } else {
            activateInternal();
        }
        activateContexts();
        contributeToStatusLine();

        perspectiveWindow.getShell().setText(getTitle(title));
    }

    /**
     * Overridable set title method, takes original window title and
     * perspectives can display what they want. Default implementation sets
     * title to be "title - perspective"
     * 
     * @param title
     */
    protected String getTitle(String title) {
        return title + " - " + getLabel();
    }

    /**
     * Get the label for the perspective
     * 
     * @return
     */
    protected final String getLabel() {
        return page.getPerspective().getLabel();
    }

    protected void activateInternal() {
        // Restore the editorArea state
        EditorAreaHelper editorArea = ((WorkbenchPage) page)
                .getEditorPresentation();
        if (storedState != null) {
            editorArea.restoreState(storedState);
        }
        // Always remove first workbook as it was added by default when
        // restoring state because editors were hidden, not closed
        EditorAreaHelper.derefPart((LayoutPart) editorArea.getWorkbooks()
                .get(0));

        for (IEditorReference ref : perspectiveEditors) {
            editorArea.addEditor((EditorReference) ref, layoutMap.get(ref),
                    false);
        }

        if (activeEditor != null) {
            page.bringToTop(activeEditor.getPart(false));
            activeEditor = null;
        }

        // Activate any perspective dialogs
        activateDialogs();

        perspectiveEditors.clear();
    }

    /**
     * Deactivate the perspective, stores editors to be opened again
     */
    public void deactivate() {
        // Save the editor area state
        EditorAreaHelper editorArea = ((WorkbenchPage) page)
                .getEditorPresentation();
        storedState = XMLMemento.createWriteRoot("perspectiveSwitch");
        editorArea.saveState(storedState);

        VizWorkbenchManager mgr = VizWorkbenchManager.getInstance();
        for (IEditorReference ref : page.getEditorReferences()) {
            if (activeEditor == null
                    && mgr.isVisibleEditor(page.getWorkbenchWindow(),
                            ref.getEditor(false))) {
                activeEditor = ref;
            }
            layoutMap.put(ref, ((EditorReference) ref).getPane().getStack()
                    .getID());

            perspectiveEditors.add(ref);
            page.hideEditor(ref);
        }

        deactivateDialogs();
        deactivateContexts();
        removeFromStatusLine();
    }

    /**
     * Can be overridden to allow perspectives to override the editor's default
     * tool to have a default perspective tool
     * 
     * @param tool
     * @throws VizException
     */
    protected void activateDefaultTool(String tool) throws VizException {
        toolManager.activateToolSet(tool);
    }

    /**
     * WorkbenchWindow setter, should be called immediately after construction.
     * not passed in through constructor bc class will be instantiated through
     * eclipse extension point
     * 
     * @param window
     */
    public void setPerspectiveWindow(IWorkbenchWindow window) {
        this.perspectiveWindow = window;
        this.title = window.getShell().getText();
    }

    /**
     * Set the status line manager so the perspective can add to the status line
     * 
     * @param statusLine
     */
    public void setStatusLineManager(IStatusLineManager statusLine) {
        this.statusLine = statusLine;
    }

    /**
     * Set the perspective id registered with the manager in the plugin.xml
     * 
     * @param perspectiveId
     */
    void setPerspectiveId(String perspectiveId) {
        this.perspectiveId = perspectiveId;
    }

    public ModalToolManager getToolManager() {
        return toolManager;
    }

    /**
     * Add a perspective dialog
     * 
     * @param dialog
     */
    public void addPerspectiveDialog(IPerspectiveSpecificDialog dialog) {
        if (perspectiveDialogs.contains(dialog) == false) {
            perspectiveDialogs.add(dialog);
        }
    }

    /**
     * Remove a perspective dialog
     * 
     * @param dialog
     */
    public void removePespectiveDialog(IPerspectiveSpecificDialog dialog) {
        perspectiveDialogs.remove(dialog);
    }

    private void activateDialogs() {
        for (IPerspectiveSpecificDialog dialog : perspectiveDialogs) {
            dialog.restore();
        }
    }

    private void deactivateDialogs() {
        for (IPerspectiveSpecificDialog dialog : perspectiveDialogs) {
            dialog.hide();
        }
    }

    private void closeDialogs() {
        List<IPerspectiveSpecificDialog> dialogsToClose = new ArrayList<IPerspectiveSpecificDialog>();
        dialogsToClose.addAll(perspectiveDialogs);
        perspectiveDialogs.clear();
        for (IPerspectiveSpecificDialog dialog : dialogsToClose) {
            dialog.close();
        }
    }

    protected static void loadDefaultBundle(String filePath) {
        File defaultBundle = PathManagerFactory.getPathManager().getStaticFile(
                filePath);
        try {
            Procedure proc = null;
            proc = (Procedure) LoadSerializedXml.deserialize(defaultBundle);
            LoadSerializedXml.loadProcedureToScreen(proc, true);
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error activating perspective", e);
            return;
        }
    }

    public final void activateContexts() {
        activateContexts(ContextManager.getInstance(perspectiveWindow));
    }

    protected void activateContexts(ContextManager manager) {
        manager.activateContexts(this);
        if (page != null) {
            manager.activateContexts(page.getActivePart());
        }
    }

    public final void deactivateContexts() {
        deactivateContexts(ContextManager.getInstance(perspectiveWindow));
    }

    protected void deactivateContexts(ContextManager manager) {
        manager.deactivateContexts(this);
        if (page != null && page.getActivePart() != null
                && perspectiveId.equals(page.getPerspective().getId())) {
            manager.deactivateContexts(page.getActivePart());
        }
    }

    /**
     * Get an array of the editors in the perspective. Note: These editors may
     * not be visible and this perspective may not be the active perspective.
     * 
     * @return Array of availble editors for the perspective
     */
    public AbstractEditor[] getPerspectiveEditors() {
        List<AbstractEditor> editors = new ArrayList<AbstractEditor>();
        if (page != null) {
            for (IEditorReference ref : page.getEditorReferences()) {
                IEditorPart part = ref.getEditor(false);
                if (part instanceof AbstractEditor) {
                    editors.add((AbstractEditor) part);
                }
            }
        }
        return editors.toArray(new AbstractEditor[editors.size()]);
    }

    /**
     * Get the perspective id the manager manages
     * 
     * @return
     */
    public String getPerspectiveId() {
        return perspectiveId;
    }

    /**
     * Have the perspecitve manager open a new editor for the perspective
     * 
     * @return the new editor or null if no editor was opened
     */
    public AbstractEditor openNewEditor() {
        // default does nothing
        return null;
    }

    private void contributeToStatusLine() {
        items.addAll(getStatusLineItems());
        for (int i = items.size() - 1; i >= 0; --i) {
            IContributionItem item = items.get(i);
            item.setVisible(true);
            statusLine.appendToGroup(StatusLineManager.MIDDLE_GROUP, item);
        }
        statusLine.update(true);
        // relayout the shell since we added the widget
        perspectiveWindow.getShell().layout(true, true);
    }

    private void removeFromStatusLine() {
        for (IContributionItem item : items) {
            statusLine.remove(item);
            item.dispose();
        }
        statusLine.update(true);
        // relayout the shell since we added the widget
        perspectiveWindow.getShell().layout(true, true);
        items.clear();
    }

    /**
     * Adds perspective specific context menu items to the specified
     * IMenuManager on the IDisplayPaneContainer for the IDisplayPane
     * 
     * @param menuManager
     * @param container
     * @param pane
     */
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        // do nothing default implementation
    }

    @Override
    public void setColor(BGColorMode mode, RGB newColor) {
        for (IEditorReference editorRef : perspectiveEditors) {
            IEditorPart part = editorRef.getEditor(false);
            if (part instanceof AbstractEditor) {
                ((AbstractEditor) part).setColor(mode, newColor);
            }
        }
    }
    
    /**
     * Notify perspective manager when heap space is running low. Default action
     * is to pop up a warning to the user. Perspectives can override the default
     * behavior to take more extreme actions to reduce memory usage.
     * 
     * @param freeMemory
     *            free memory available in bytes
     * @return true if notification was displayed
     */
    public boolean notifyLowMemory(long availMemory) {
        final String msg = getLowMemoryMessage(availMemory);
        final boolean[] status = new boolean[1];
        VizApp.runSync(new Runnable() {
            public void run() {
                Display display = Display.getDefault();
                status[0] = MessageDialog.open(MessageDialog.WARNING,
                        display.getActiveShell(), "Low Memory", msg, SWT.NONE);
            }
        });
        return status[0];
    }

    /**
     * Create the default low memory message to be displayed to the user
     * 
     * @param availMemory
     *            free memory available in bytes
     * @return
     */
    protected String getLowMemoryMessage(long availMemory) {
        return "This CAVE is nearing its maximum memory limit. "
                + "Performance may degrade significantly. "
                + SizeUtil.prettyByteSize(availMemory) + " available";
    }

}
