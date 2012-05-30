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

import java.io.File;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.ILayoutContainer;
import org.eclipse.ui.internal.PartPane;
import org.eclipse.ui.internal.PartPlaceholder;
import org.eclipse.ui.internal.ViewPane;
import org.eclipse.ui.internal.ViewSite;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.dnd.DragUtil;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.viz.ui.UiPlugin;

/**
 * Creates the ability to have a dialog that can be minimized and maximized and
 * is separate from the original CAVE window from a view
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public abstract class CaveFloatingView extends ViewPart {

    private boolean detached;

    private CaveDetachedWindow window;

    private ILayoutContainer container;

    private Rectangle bounds;

    /**
     * Constructs the view
     */
    public CaveFloatingView() {
        super();
    }

    @Override
    public void createPartControl(final Composite parent) {
        createToolbarButton();
    }

    public IViewPart getPart() {
        return this;
    }

    /**
     * Creates the "Float" toolbar button
     */
    protected void createToolbarButton() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        Action floatAction = new Action("Float", SWT.TOGGLE) {
            @Override
            public void run() {
                // should only run when the user is currently attached to cave
                // (or detached, but not a floating dialog)
                detached = true;
                WorkbenchPage page = (WorkbenchPage) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                bounds = ((ViewSite) ((ViewPart) getPart()).getViewSite())
                        .getPane().getBounds();
                container = ((ViewSite) ((ViewPart) getPart()).getViewSite())
                        .getPane().getStack();
                container.add(new PartPlaceholder("temp "));
                window = new CaveDetachedWindow(page);
                window.create();
                window.open();

                ViewPane pane = (ViewPane) ((ViewSite) getPart().getViewSite())
                        .getPane();
                container = pane.getContainer();
                window.drop(pane);
                window.getShell().setBounds(bounds);
            };

            @Override
            public void runWithEvent(Event event) {
                if (!detached) {
                    run();
                    return;
                }
                detached = false;
                WorkbenchPage page = (WorkbenchPage) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                PartPane layoutPart = ((ViewSite) getPart().getViewSite())
                        .getPane();
                Point point = new Point(bounds.x + bounds.width / 2, bounds.y
                        + bounds.height / 2);
                window.getShell().setVisible(false);
                DragUtil.dragTo(Display.getCurrent(), layoutPart, point, bounds);
            }
        };
        floatAction.setImageDescriptor(UiPlugin.getImageDescriptor("icons"
                + File.separator + "float.gif"));
        mgr.add(floatAction);
    }
}
