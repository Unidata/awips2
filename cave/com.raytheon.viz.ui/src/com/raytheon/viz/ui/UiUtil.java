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
package com.raytheon.viz.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.Validate;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.EditorReference;
import org.eclipse.ui.views.IViewDescriptor;
import org.eclipse.ui.views.IViewRegistry;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.UiUtil.ContainerPart.Container;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.statusline.VizActionBarAdvisor;

/**
 * UiUtil - contains UI utility methods
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2007            chammack    Initial Creation.
 * 12/02/2008   1450       randerso    Added getEditors method
 * 12/05/2008              ebabin      Changed findView to not always assume
 *                                      view has a secondaryid.
 *                                     Added hideView method for quickly hiding views.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class UiUtil {

    public static final String SECONDARY_ID_SEPARATOR = ":";

    public static class ContainerPart {

        public static class Container {
            public String layoutId;

            public IRenderableDisplay[] displays;
        }

        public String id;

        public List<Container> containers;

        private ContainerPart(String id, List<Container> containers) {
            this.id = id;
            this.containers = containers;
        }
    }

    /**
     * Get a map of all active CAVE panes, keyed by the editor or view
     * 
     * @return the pane map
     */
    public static List<ContainerPart> getActiveDisplayMap() {
        List<ContainerPart> parts = new ArrayList<ContainerPart>();
        Map<String, ContainerPart> partMap = new HashMap<String, ContainerPart>();

        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();

        if (window != null) {
            IWorkbenchPage pages[] = window.getPages();
            for (IWorkbenchPage page : pages) {
                IEditorReference[] refs = page.getEditorReferences();

                // Pull out editors
                for (IEditorReference ref : refs) {
                    IEditorPart part = ref.getEditor(false);
                    if (part == null) {
                        continue;
                    }

                    if (part instanceof IDisplayPaneContainer) {
                        IDisplayPaneContainer container = (IDisplayPaneContainer) part;
                        IRenderableDisplay[] editorDisplays = getDisplaysFromContainer(container);
                        if (editorDisplays != null && editorDisplays.length > 0) {
                            ContainerPart cp = partMap.get(ref.getId());
                            if (cp == null) {
                                List<Container> list = new ArrayList<Container>();
                                cp = new ContainerPart(ref.getId(), list);
                                partMap.put(ref.getId(), cp);
                            }
                            Container c = new Container();
                            c.displays = editorDisplays;
                            c.layoutId = ((EditorReference) ref).getPane()
                                    .getStack().getID();
                            cp.containers.add(c);
                        }
                    }
                }

                // Pull out views
                IViewReference[] viewReferences = page.getViewReferences();
                for (IViewReference ref : viewReferences) {
                    IViewPart view = ref.getView(false);
                    if (view == null) {
                        continue;
                    }

                    if (view instanceof IDisplayPaneContainer) {
                        IDisplayPaneContainer container = (IDisplayPaneContainer) view;
                        IRenderableDisplay[] displays = getDisplaysFromContainer(container);

                        if (displays != null && displays.length > 0) {
                            String id = ref.getId() + SECONDARY_ID_SEPARATOR
                                    + ref.getSecondaryId();
                            ContainerPart cp = partMap.get(id);
                            if (cp == null) {
                                List<Container> list = new ArrayList<Container>();
                                cp = new ContainerPart(id, list);
                                partMap.put(id, cp);
                            }
                            Container c = new Container();
                            c.displays = displays;
                            cp.containers.add(c);
                        }
                    }
                }
            }
        }
        parts.addAll(partMap.values());
        return parts;
    }

    /**
     * Return the list of displays from a display container
     * 
     * @param container
     *            the container to retrieve from
     * @return the list of displays
     */
    public static IRenderableDisplay[] getDisplaysFromContainer(
            IDisplayPaneContainer container) {
        List<IRenderableDisplay> displays = new ArrayList<IRenderableDisplay>();

        IDisplayPane[] panes = container.getDisplayPanes();
        for (IDisplayPane pane : panes) {
            if (pane != null) {
                IRenderableDisplay display = pane.getRenderableDisplay();
                if (display != null) {
                    displays.add(display);
                }
            }
        }

        return displays.toArray(new IRenderableDisplay[displays.size()]);

    }

    /**
     * Get a reference to a view given the id.
     * 
     * @param view
     *            the id of the view to find
     * @param createIfNotFound
     *            if not found, should the view be opened
     * @return
     */
    public static IViewPart findView(String view, boolean createIfNotFound) {
        Validate.notNull(view);

        IWorkbenchWindow[] windows = PlatformUI.getWorkbench()
                .getWorkbenchWindows();
        String id = null;
        String secondaryId = null;

        if (view.contains(SECONDARY_ID_SEPARATOR)) {
            id = view.split(SECONDARY_ID_SEPARATOR)[0];
            secondaryId = view.split(SECONDARY_ID_SEPARATOR)[1];
        } else {
            id = view;
        }

        // Search all windows for view
        for (IWorkbenchWindow window : windows) {
            IViewPart viewPart = findView(window, view, false);
            if (viewPart != null) {
                return viewPart;
            }
        }

        if (createIfNotFound) {
            try {
                return PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage()
                        .showView(id, secondaryId, IWorkbenchPage.VIEW_VISIBLE);
            } catch (PartInitException e) {
                return null;
            }
        }

        return null;
    }

    /**
     * Given the window, find the view and create if not found and desired
     * 
     * @param windowToSearch
     * @param view
     * @param createIfNotFound
     * @return The found or created view part or null if not found and should
     *         not create
     */
    public static IViewPart findView(IWorkbenchWindow windowToSearch,
            String view, boolean createIfNotFound) {
        Validate.notNull(view);
        Validate.notNull(windowToSearch);

        String id = null;
        String secondaryId = null;

        if (view.contains(SECONDARY_ID_SEPARATOR)) {
            id = view.split(SECONDARY_ID_SEPARATOR)[0];
            secondaryId = view.split(SECONDARY_ID_SEPARATOR)[1];
        } else {
            id = view;
        }

        IWorkbenchPage pages[] = windowToSearch.getPages();
        for (IWorkbenchPage page : pages) {
            IViewReference[] refs = page.getViewReferences();

            for (IViewReference r : refs) {
                if (id.equals(r.getId())
                        && ((secondaryId != null && r.getSecondaryId() != null && secondaryId
                                .equals(r.getSecondaryId())) || (secondaryId == null || r
                                .getSecondaryId() == null))) {
                    return (IViewPart) r.getPart(true);
                }
            }
        }

        if (createIfNotFound) {
            try {
                return windowToSearch.getActivePage().showView(id);
            } catch (PartInitException e) {
                return null;
            }
        }

        return null;
    }

    /**
     * Given the id, determine if the id corresponds to a view
     * 
     * @param id
     *            the id to check
     * @return true if id corresponds to view, false otherwise
     */
    public static boolean isView(String id) {
        IViewRegistry registry = PlatformUI.getWorkbench().getViewRegistry();

        String parsedId = id;

        if (id.contains(SECONDARY_ID_SEPARATOR)) {
            parsedId = id.split(SECONDARY_ID_SEPARATOR)[0];
        }

        IViewDescriptor d = registry.find(parsedId);

        if (d == null) {
            return false;
        }

        return true;

    }

    /**
     * Given the id, determine if the id corresponds to an editor
     * 
     * @param id
     *            the id to check
     * @return true if id corresponds to an editor, false otherwise
     */
    public static boolean isEditor(String id) {
        IEditorRegistry registry = PlatformUI.getWorkbench()
                .getEditorRegistry();

        IEditorDescriptor d = registry.findEditor(id);

        if (d == null) {
            return false;
        }

        return true;

    }

    /**
     * Gets the currently active window
     * 
     * @return
     */
    public static IWorkbenchWindow getCurrentWindow() {
        return VizWorkbenchManager.getInstance().getCurrentWindow();
    }

    /**
     * Given the editor id and the renderable displays, create or open an editor
     * with the given displays on the active window
     * 
     * @param editor
     * @param displays
     * @return the created or opened editor
     */
    public static AbstractEditor createOrOpenEditor(String editor,
            IRenderableDisplay... displays) {
        return createOrOpenEditor(getCurrentWindow(), editor, displays);
    }

    /**
     * Given the editor id and the renderable displays, create or open an editor
     * with the given displays on the specified window
     * 
     * @param windowToLoadTo
     * @param editor
     * @param displays
     * @return the created or opened editor
     */
    public static AbstractEditor createOrOpenEditor(
            IWorkbenchWindow windowToLoadTo, String editor,
            IRenderableDisplay... displays) {
        String editorName = (editor == null ? "com.raytheon.viz.ui.glmap.GLMapEditor"
                : editor);
        if (windowToLoadTo == null) {
            windowToLoadTo = getCurrentWindow();
        }
        // Check the current editor first
        IEditorPart ep = EditorUtil.getActiveEditor(windowToLoadTo);
        if (ep instanceof AbstractEditor) {
            AbstractEditor currentEditor = (AbstractEditor) ep;
            if (currentEditor != null
                    && currentEditor.getEditorSite().getId().equals(editorName)) {
                currentEditor = makeCompatible(currentEditor, displays);
                if (currentEditor != null) {
                    return currentEditor;
                }
            }
        }

        IWorkbenchPage activePage = windowToLoadTo.getActivePage();
        IEditorReference[] references = new IEditorReference[0];
        if (activePage != null) {
            references = activePage.getEditorReferences();
        }

        for (IEditorReference ref : references) {
            if (editorName.equals(ref.getId())) {
                IEditorPart editorPart = ref.getEditor(false);
                if (editorPart instanceof AbstractEditor) {
                    AbstractEditor aEditor = (AbstractEditor) editorPart;
                    aEditor = makeCompatible(aEditor, displays);
                    if (aEditor != null) {
                        activePage.bringToTop(aEditor);
                        return aEditor;
                    }
                }
            }
        }

        // If we get here, the editor isn't there, or has a different number of
        // panes... construct it
        return createEditor(windowToLoadTo, editorName, displays);
    }

    private static AbstractEditor makeCompatible(AbstractEditor currentEditor,
            IRenderableDisplay... displays) {
        for (int i = 0; i < displays.length; i++) {
            IDescriptor currentDesc = currentEditor.getDisplayPanes()[0]
                    .getDescriptor();
            if (i < currentEditor.getDisplayPanes().length) {
                currentDesc = currentEditor.getDisplayPanes()[i]
                        .getDescriptor();
            }
            IDescriptor newDesc = displays[i].getDescriptor();
            if (!currentDesc.isCompatible(newDesc)) {
                return null;
            }
        }
        if (currentEditor instanceof IMultiPaneEditor) {
            IMultiPaneEditor mpe = (IMultiPaneEditor) currentEditor;
            if (currentEditor.getDisplayPanes().length < displays.length) {
                currentEditor.getDisplayPanes()[0].clear();
                IRenderableDisplay display = currentEditor.getDisplayPanes()[0]
                        .getRenderableDisplay();
                for (int i = 1; i < displays.length; ++i) {
                    mpe.addPane(display.createNewDisplay());
                }
            }
            return currentEditor;
        } else if (currentEditor.getDisplayPanes().length == displays.length) {
            return currentEditor;
        }
        return null;
    }

    /**
     * Opens a new editor with the specified displays on the currently active
     * window
     * 
     * @param editor
     * @param displays
     * @return
     */
    public static AbstractEditor createEditor(String editor,
            IRenderableDisplay... displays) {
        return createEditor(getCurrentWindow(), editor, displays);
    }

    /**
     * Opens a new editor with the specified displays on the specified window
     * 
     * @param windowToLoadTo
     * @param editor
     * @param displays
     * @return
     */
    public static AbstractEditor createEditor(IWorkbenchWindow windowToLoadTo,
            String editor, IRenderableDisplay... displays) {
        String editorName = (editor == null ? "com.raytheon.viz.ui.glmap.GLMapEditor"
                : editor);
        if (windowToLoadTo == null) {
            windowToLoadTo = getCurrentWindow();
        }
        AbstractEditor aEditor = null;
        EditorInput cont = new EditorInput(displays);
        try {
            IWorkbenchPage activePage = windowToLoadTo.getActivePage();
            if (activePage != null) {
                aEditor = (AbstractEditor) activePage.openEditor(cont,
                        editorName);
            }
        } catch (PartInitException e) {
            UiPlugin.getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                            "Error constituting editor", e));
        }
        return aEditor;
    }

    /**
     * Find all editors for a perspective in a window
     * 
     * @param window
     * @return array of AbstractEditors in the perspective
     */
    public static AbstractEditor[] getEditors(IWorkbenchWindow window,
            String perspectiveId) {
        VizPerspectiveListener listener = VizPerspectiveListener
                .getInstance(window);
        if (listener != null) {
            AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                    .getInstance(window).getPerspectiveManager(perspectiveId);

            if (mgr != null) {
                return mgr.getPerspectiveEditors();
            }
        }
        return new AbstractEditor[0];
    }

    public static void updateWindowCoolBar(IWorkbenchWindow window) {
        try {
            VizActionBarAdvisor advisor = VizActionBarAdvisor
                    .getInstance(window);
            if (advisor != null) {
                ICoolBarManager cbm = advisor.getCoolBar();
                IContributionItem[] items = cbm.getItems();
                for (IContributionItem item : items) {
                    item.update(ICoolBarManager.SIZE);
                }
            }
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

}
