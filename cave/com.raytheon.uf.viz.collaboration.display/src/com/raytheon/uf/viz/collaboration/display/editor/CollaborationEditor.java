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
package com.raytheon.uf.viz.collaboration.display.editor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ScrollBar;

import com.raytheon.uf.viz.collaboration.display.editor.input.CollaborationInputHandler;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * A collaboration editor that displays the display of an editor shared by the
 * Data Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationEditor extends AbstractEditor {

    public static final String EDITOR_ID = "com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor";

    private String sessionId;

    private Composite canvasComp;

    private ScrolledComposite scrollable;

    private CollaborationInputHandler inputHandler = new CollaborationInputHandler();

    @Override
    protected PaneManager getNewPaneManager() {
        return new PaneManager() {
            @Override
            public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
                // // scrollable composite
                scrollable = new ScrolledComposite(composite, SWT.H_SCROLL
                        | SWT.V_SCROLL);
                scrollable.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                        true));
                // Sets background color of scrollable composite to white
                scrollable.setBackground(scrollable.getDisplay()
                        .getSystemColor(SWT.COLOR_WHITE));

                // Composite for canvas (fixed size)
                canvasComp = new Composite(scrollable, SWT.NONE);
                GridLayout gl = new GridLayout(1, false);
                gl.marginHeight = 0;
                gl.marginWidth = 0;
                canvasComp.setLayout(gl);
                canvasComp.setSize(1000, 1000);

                // Set canvasComp as content on scrollable
                scrollable.setContent(canvasComp);
                scrollable.addListener(SWT.Resize, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        setCanvasSize(canvasComp.getBounds());
                    }
                });

                IDisplayPane pane = addPane(renderableDisplay, canvasComp);
                canvasComp.layout();
                return pane;
            }
        };
    }

    @Override
    protected void addCustomHandlers(InputManager manager) {
        super.registerMouseHandler(inputHandler, InputPriority.SYSTEM_RESOURCE);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        inputHandler.registerInputHandler(handler);
    }

    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        inputHandler.unregisterInputHandler(handler);
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    public void setCanvasSize(Rectangle bounds) {
        canvasComp.setSize(bounds.width, bounds.height);

        // This code centers the GLCanvas in the scrollable composite
        ScrollBar vertical = scrollable.getVerticalBar();
        ScrollBar horizon = scrollable.getHorizontalBar();
        Rectangle scrollableBounds = scrollable.getBounds();
        if (vertical.isVisible()) {
            scrollableBounds.width -= vertical.getSize().x;
        }
        if (horizon.isVisible()) {
            scrollableBounds.height -= horizon.getSize().y;
        }

        canvasComp.setLocation(
                Math.max(0, (scrollableBounds.width - bounds.width) / 2),
                Math.max(0, (scrollableBounds.height - bounds.height) / 2));
        canvasComp.layout();
    }

}
