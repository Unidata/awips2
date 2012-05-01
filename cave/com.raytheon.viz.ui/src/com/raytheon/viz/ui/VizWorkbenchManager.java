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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Class that manages the current editor/window using listeners. Use this to
 * retrieve current editor/window over PlatformUI as this class does not require
 * being ran on the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VizWorkbenchManager implements IPartListener, IPartListener2,
        IWindowListener, IPageListener {

    private IWorkbenchWindow currentWindow = null;

    private Map<IWorkbenchWindow, IEditorPart> activeEditorMap = new HashMap<IWorkbenchWindow, IEditorPart>();

    private Map<IWorkbenchWindow, Set<IEditorPart>> visibleParts = new HashMap<IWorkbenchWindow, Set<IEditorPart>>();

    private Set<IVizEditorChangedListener> changeListeners = new HashSet<IVizEditorChangedListener>();

    private static VizWorkbenchManager instance = new VizWorkbenchManager();

    public static VizWorkbenchManager getInstance() {
        return instance;
    }

    private VizWorkbenchManager() {

    }

    /**
     * Get the active editor for the active window
     * 
     * @return
     */
    public synchronized IEditorPart getActiveEditor() {
        return activeEditorMap.get(currentWindow);
    }

    /**
     * Get the active editor for the window
     * 
     * @param window
     * @return
     */
    public synchronized IEditorPart getActiveEditor(IWorkbenchWindow window) {
        return activeEditorMap.get(window);
    }

    /**
     * Is this object a visible editor in the currently active window
     * 
     * @param object
     * @return
     */
    public synchronized boolean isVisibleEditor(Object object) {
        return isVisibleEditor(currentWindow, object);
    }

    /**
     * Is this object a visible editor in this window
     * 
     * @param currentWindow
     * @param object
     * @return
     */
    public synchronized boolean isVisibleEditor(IWorkbenchWindow currentWindow,
            Object object) {
        Set<IEditorPart> parts = visibleParts.get(currentWindow);
        if (parts != null) {
            return parts.contains(object);
        }
        return false;
    }

    /**
     * Get the currently active workbench window
     * 
     * @return
     */
    public synchronized IWorkbenchWindow getCurrentWindow() {
        return currentWindow;
    }

    public void addListener(IVizEditorChangedListener listener) {
        changeListeners.add(listener);
    }

    public void removeListener(IVizEditorChangedListener listener) {
        changeListeners.remove(listener);
    }

    /**
     * Notify the listeners the current viz editor changed
     */
    private void notifyListeners() {
        IDisplayPaneContainer container = (IDisplayPaneContainer) activeEditorMap
                .get(currentWindow);
        for (IVizEditorChangedListener listener : changeListeners) {
            listener.editorChanged(container);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public synchronized void partActivated(IWorkbenchPart part) {
        IWorkbenchWindow partWindow = part.getSite().getWorkbenchWindow();
        IEditorPart currentEditor = activeEditorMap.get(partWindow);
        if (part instanceof IEditorPart && currentEditor != part) {
            activeEditorMap.put(partWindow, (IEditorPart) part);
            if (part instanceof IDisplayPaneContainer) {
                notifyListeners();
            }
            updateUI(partWindow);
        }
        ContextManager.getInstance(partWindow).activateContexts(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart
     * )
     */
    @Override
    public synchronized void partBroughtToTop(IWorkbenchPart part) {
        IWorkbenchWindow partWindow = part.getSite().getWorkbenchWindow();
        IEditorPart currentEditor = activeEditorMap.get(partWindow);
        if (part instanceof IEditorPart && currentEditor != part) {
            activeEditorMap.put(partWindow, (IEditorPart) part);
            if (part instanceof IDisplayPaneContainer) {
                notifyListeners();
            }
            updateUI(part.getSite().getWorkbenchWindow());
        }
        ContextManager.getInstance(partWindow).activateContexts(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public synchronized void partClosed(IWorkbenchPart part) {
        IWorkbenchWindow partWindow = part.getSite().getWorkbenchWindow();
        IEditorPart currentEditor = activeEditorMap.get(partWindow);
        if (part == currentEditor) {
            activeEditorMap.put(partWindow, (IEditorPart) null);
            notifyListeners();
        }
        ContextManager.getInstance(partWindow).deactivateContexts(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart
     * )
     */
    @Override
    public synchronized void partDeactivated(IWorkbenchPart part) {
        IWorkbenchWindow window = getCurrentWindow();
        if (part != null) {
            window = part.getSite().getWorkbenchWindow();
        }
        ContextManager.getInstance(window).deactivateContexts(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public synchronized void partOpened(IWorkbenchPart part) {
        IWorkbenchWindow partWindow = part.getSite().getWorkbenchWindow();
        IEditorPart currentEditor = activeEditorMap.get(partWindow);
        if (part instanceof IEditorPart && currentEditor != part) {
            activeEditorMap.put(partWindow, (IEditorPart) part);
            if (part instanceof IDisplayPaneContainer) {
                notifyListeners();
            }
            updateUI(part.getSite().getWorkbenchWindow());
        }
        ContextManager.getInstance(partWindow).activateContexts(part);
    }

    @Override
    public synchronized void windowActivated(IWorkbenchWindow window) {
        this.currentWindow = window;

        VizPerspectiveListener listener = VizPerspectiveListener
                .getInstance(window);
        if (listener != null) {
            AbstractVizPerspectiveManager mgr = listener
                    .getActivePerspectiveManager();
            if (mgr != null) {
                mgr.activateContexts();
            }
        }

        updateUI(window);
    }

    @Override
    public synchronized void windowClosed(IWorkbenchWindow window) {
        window.removePageListener(this);
        for (IWorkbenchPage page : window.getPages()) {
            page.removePartListener((IPartListener) this);
            page.removePartListener((IPartListener2) this);
        }
        if (window == currentWindow) {
            currentWindow = null;
        }
        visibleParts.remove(window);
    }

    @Override
    public synchronized void windowDeactivated(IWorkbenchWindow window) {
        VizPerspectiveListener listener = VizPerspectiveListener
                .getInstance(window);
        if (listener != null) {
            AbstractVizPerspectiveManager mgr = listener
                    .getActivePerspectiveManager();
            if (mgr != null) {
                mgr.deactivateContexts();
            }
        }
    }

    @Override
    public synchronized void windowOpened(IWorkbenchWindow window) {
        window.addPageListener(this);
        for (IWorkbenchPage page : window.getPages()) {
            page.addPartListener((IPartListener) this);
            page.addPartListener((IPartListener2) this);
        }
        this.currentWindow = window;
        visibleParts.put(window, new HashSet<IEditorPart>());
    }

    @Override
    public void pageActivated(IWorkbenchPage page) {

    }

    @Override
    public void pageClosed(IWorkbenchPage page) {
        IWorkbenchWindow partWindow = page.getWorkbenchWindow();
        IEditorPart currentEditor = activeEditorMap.get(partWindow);
        for (IEditorReference ref : page.getEditorReferences()) {
            if (ref.getEditor(false) == currentEditor) {
                if (currentEditor instanceof IDisplayPaneContainer) {
                    notifyListeners();
                }
                activeEditorMap.put(partWindow, null);
                break;
            }
        }
        page.removePartListener((IPartListener) this);
        page.removePartListener((IPartListener2) this);
    }

    @Override
    public void pageOpened(IWorkbenchPage page) {
        page.addPartListener((IPartListener) this);
        page.addPartListener((IPartListener2) this);
    }

    private void updateUI(IWorkbenchWindow window) {
        IEditorPart currentEditor = activeEditorMap.get(window);
        VizGlobalsManager
                .getInstance(window)
                .updateUI(
                        currentEditor instanceof IDisplayPaneContainer ? (IDisplayPaneContainer) currentEditor
                                : null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partHidden(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        if (part instanceof IEditorPart) {
            Set<IEditorPart> parts = visibleParts.get(part.getSite()
                    .getWorkbenchWindow());
            if (parts != null) {
                parts.remove(part);
            }
            IEditorPart active = activeEditorMap.get(part.getSite()
                    .getWorkbenchWindow());
            if (active == part) {
                activeEditorMap.put(part.getSite().getWorkbenchWindow(), null);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        if (part instanceof IEditorPart) {
            Set<IEditorPart> parts = visibleParts.get(part.getSite()
                    .getWorkbenchWindow());
            if (parts != null) {
                parts.add((IEditorPart) part);
            }

            IEditorPart active = activeEditorMap.get(part.getSite()
                    .getWorkbenchWindow());
            if (active == null) {
                // Active will be null if our active editor became hidden, try
                // and get an active editor from the page
                active = part.getSite().getPage().getActiveEditor();
                if (active == null) {
                    // Active will be null here if the page doesn't have an
                    // active editor so we should use the part that became
                    // visible
                    active = (IEditorPart) part;
                }
                activeEditorMap
                        .put(part.getSite().getWorkbenchWindow(), active);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partActivated(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partActivated(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partBroughtToTop(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partClosed(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partDeactivated(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partOpened(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partOpened(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partInputChanged(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {

    }
}
