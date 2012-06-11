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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

/**
 * An Object that watches a view and if that view becomes detached it will
 * ensure that the VizWorkbenchManager keeps the same contexts active as if the
 * view was attached.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DetachedViewListener implements Listener, IWindowListener,
        IPartListener {

    private IWorkbenchPart part;

    private Shell activeShell;

    public DetachedViewListener(IWorkbenchPart part, IWorkbenchSite site) {
        super();
        this.part = part;
        this.activeShell = site.getShell();
        site.getPage().addPartListener(this);
        PlatformUI.getWorkbench().addWindowListener(this);
        checkDetached(site);
    }

    private void checkDetached(IWorkbenchSite site) {
        Shell partShell = site.getShell();
        Shell windowShell = site.getWorkbenchWindow().getShell();
        if (partShell != activeShell) {
            // We switched shells, add and or remove shell listeners.
            // Don't add or remove listeners from the windowShell
            if (activeShell != windowShell) {
                if (!activeShell.isDisposed()) {
                    activeShell.removeListener(SWT.Activate, this);
                    activeShell.removeListener(SWT.Deactivate, this);
                }
            }
            activeShell = partShell;
            if (partShell != windowShell) {
                activeShell.addListener(SWT.Activate, this);
                activeShell.addListener(SWT.Deactivate, this);
            }
        }
    }

    @Override
    public void handleEvent(Event event) {
        VizWorkbenchManager manager = VizWorkbenchManager.getInstance();
        IWorkbenchWindow window = part.getSite().getWorkbenchWindow();
        switch (event.type) {
        case SWT.Activate:
            manager.windowActivated(window);
            break;
        case SWT.Deactivate:
            manager.windowDeactivated(window);
            break;
        }
    }

    @Override
    public void windowActivated(IWorkbenchWindow window) {
        checkDetached(part.getSite());
    }

    @Override
    public void windowDeactivated(IWorkbenchWindow window) {
        checkDetached(part.getSite());
    }

    @Override
    public void windowClosed(IWorkbenchWindow window) {

    }

    @Override
    public void windowOpened(IWorkbenchWindow window) {

    }

    @Override
    public void partActivated(IWorkbenchPart part) {

    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {

    }

    @Override
    public void partClosed(IWorkbenchPart part) {
        if (part == this.part) {
            PlatformUI.getWorkbench().removeWindowListener(this);
            part.getSite().getPage().removePartListener(this);
        }

    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {

    }

    @Override
    public void partOpened(IWorkbenchPart part) {

    }

}