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
package com.raytheon.viz.ui.views;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.ILayoutContainer;
import org.eclipse.ui.internal.LayoutPart;
import org.eclipse.ui.internal.PartPane;
import org.eclipse.ui.internal.PartStack;
import org.eclipse.ui.internal.ViewPane;
import org.eclipse.ui.internal.ViewSite;
import org.eclipse.ui.internal.WorkbenchPage;

/**
 * The Eclipse WorkbenchPage has lots of nifty functions for dealing with views,
 * but unfortunately it fails to take into account CaveFloatingViews that are in
 * CaveDetachedWindows, the goal of this class is to provide an API of equal
 * niftosity that works for CaveFloatingViews.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CaveWorkbenchPageManager {

    private static Map<IWorkbenchPage, CaveWorkbenchPageManager> instanceMap = new HashMap<IWorkbenchPage, CaveWorkbenchPageManager>();

    public static CaveWorkbenchPageManager getInstance(IWorkbenchPage page) {
        synchronized (instanceMap) {
            CaveWorkbenchPageManager instance = instanceMap.get(page);
            if (instance == null) {
                instance = new CaveWorkbenchPageManager(page);
                instanceMap.put(page, instance);
            }
            return instance;
        }
    }

    public static CaveWorkbenchPageManager getActiveInstance() {
        synchronized (instanceMap) {
            return getInstance(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage());
        }
    }

    private final IWorkbenchPage workbenchPage;

    private List<CaveDetachedWindow> detachedWindows = new ArrayList<CaveDetachedWindow>();

    private CaveWorkbenchPageManager(IWorkbenchPage workbenchPage) {
        this.workbenchPage = workbenchPage;
        workbenchPage.getWorkbenchWindow().addPageListener(
                new PageListener(this));
    }

    public IWorkbenchPage getWorkbenchPage() {
        return workbenchPage;
    }

    public IViewReference[] getViewReferences() {
        if (detachedWindows.isEmpty()) {
            return workbenchPage.getViewReferences();
        }
        List<IViewReference> result = Arrays.asList(workbenchPage
                .getViewReferences());
        result = new ArrayList<IViewReference>(result);
        for (CaveDetachedWindow window : detachedWindows) {
            for (LayoutPart part : window.getChildren()) {
                if (part instanceof ViewPane) {
                    result.add(((ViewPane) part).getViewReference());
                }
            }
        }
        return result.toArray(new IViewReference[0]);
    }

    public IViewPart showView(String viewID) throws PartInitException {
        return showView(viewID, null, IWorkbenchPage.VIEW_ACTIVATE);
    }

    public IViewPart showView(String viewId, String secondaryId, int mode)
            throws PartInitException {
        IViewReference ref = findDetachedViewReference(viewId, secondaryId);
        if (ref != null) {
            IViewPart view = ref.getView(true);
            if (mode == IWorkbenchPage.VIEW_ACTIVATE) {
                activate(view);
            } else if (mode == IWorkbenchPage.VIEW_VISIBLE) {
                workbenchPage.bringToTop(view);
            }
            return view;
        }
        return workbenchPage.showView(viewId, secondaryId, mode);
    }

    public void activate(IViewPart view) {
        for (CaveDetachedWindow window : detachedWindows) {
            for (LayoutPart part : window.getChildren()) {
                if (((ViewPane) part).getPartReference().getPart(false) == view) {
                    window.getShell().forceActive();
                    ILayoutContainer container = part.getContainer();
                    if (container != null && container instanceof PartStack) {
                        PartStack folder = (PartStack) container;
                        if (folder.getSelection() != part) {
                            folder.setSelection(part);
                        }
                    }
                    return;
                }
            }
        }
        workbenchPage.activate(view);
    }

    public void bringToTop(IViewPart view) {
        for (CaveDetachedWindow window : detachedWindows) {
            for (LayoutPart part : window.getChildren()) {
                if (((ViewPane) part).getPartReference().getPart(false) == view) {
                    window.getShell().moveAbove(null);
                    ILayoutContainer container = part.getContainer();
                    if (container != null && container instanceof PartStack) {
                        PartStack folder = (PartStack) container;
                        if (folder.getSelection() != part) {
                            folder.setSelection(part);
                        }
                    }
                    return;
                }
            }
        }
        workbenchPage.bringToTop(view);
    }

    public IViewReference findViewReference(String viewId) {
        return findViewReference(viewId, null);
    }

    public IViewReference findViewReference(String viewId, String secondaryId) {
        IViewReference ref = findDetachedViewReference(viewId, secondaryId);
        if (ref == null) {
            ref = workbenchPage.findViewReference(viewId, secondaryId);
        }
        return ref;
    }

    private IViewReference findDetachedViewReference(String viewId,
            String secondaryId) {
        for (CaveDetachedWindow window : detachedWindows) {
            for (LayoutPart part : window.getChildren()) {
                if (part instanceof ViewPane) {
                    IViewReference ref = ((ViewPane) part).getViewReference();
                    if (ref.getId().equals(viewId)
                            && ((secondaryId == null && ref.getSecondaryId() == null) || secondaryId
                                    .equals(ref.getSecondaryId()))) {
                        return ref;
                    }
                }
            }
        }
        return null;
    }

    public void hideView(IViewReference view) {
        for (CaveDetachedWindow window : detachedWindows) {
            for (LayoutPart part : window.getChildren()) {
                if (part instanceof ViewPane) {
                    if (((ViewPane) part).getPartReference() == view) {
                        WorkbenchPage page = (WorkbenchPage) workbenchPage;
                        if (page.getActivePartReference() == view) {
                            // You cannot hide the active part, normally the
                            // page changes the active part but since we bypass
                            // the page we need to choose a new active part
                            for (IWorkbenchPartReference wbRef : page
                                    .getSortedParts()) {
                                if (wbRef != view) {
                                    page.activate(wbRef.getPart(false));
                                    break;
                                }
                            }
                        }
                        page.getActivePerspective().hideView(view);
                        return;
                    }
                }
            }
        }
        workbenchPage.hideView(view);
    }

    public void floatView(IViewPart part) {
        ViewSite site = ((ViewSite) part.getViewSite());
        WorkbenchPage page = (WorkbenchPage) site.getPage();
        PartPane pane = site.getPane();
        Control control = pane.getStack().getControl();
        Rectangle bounds = control.getBounds();
        Point corner = control.getParent().toDisplay(bounds.x, bounds.y);
        bounds.x = corner.x;
        bounds.y = corner.y;
        CaveDetachedWindow window = new CaveDetachedWindow(page);
        window.setBounds(bounds);
        window.create();
        window.drop(pane);
        window.getShell().setBounds(bounds);
        window.getShell().addShellListener(new ShellListener(this, window));
        window.open();
        window.getShell().forceActive();
        detachedWindows.add(window);
    }

    public void dockView(IViewPart part) {
        WorkbenchPage page = (WorkbenchPage) part.getSite().getPage();
        IViewReference ref = (IViewReference) page.getReference(part);
        page.attachView(ref);
    }

    private static class ShellListener extends ShellAdapter {

        private final CaveWorkbenchPageManager manager;

        private final CaveDetachedWindow window;

        public ShellListener(CaveWorkbenchPageManager manager,
                CaveDetachedWindow window) {
            this.manager = manager;
            this.window = window;
        }

        @Override
        public void shellClosed(ShellEvent e) {
            manager.detachedWindows.remove(window);
        }

    }

    private static class PageListener implements IPageListener {

        private final CaveWorkbenchPageManager manager;

        public PageListener(CaveWorkbenchPageManager manager) {
            this.manager = manager;
        }

        @Override
        public void pageActivated(IWorkbenchPage page) {

        }

        @Override
        public void pageClosed(IWorkbenchPage page) {
            if (page == manager.getWorkbenchPage()) {
                page.getWorkbenchWindow().removePageListener(this);
            }

        }

        @Override
        public void pageOpened(IWorkbenchPage page) {

        }

    }
}
