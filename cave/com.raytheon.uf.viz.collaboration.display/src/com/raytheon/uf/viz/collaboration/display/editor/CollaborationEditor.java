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

    private Composite wrapperComp;

    private Composite canvasComp;

    private ScrolledComposite scrollable;

    private Rectangle scrollableBounds;

    private Rectangle canvasBounds;

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

                // Composite for canvas comp
                wrapperComp = new Composite(scrollable, SWT.NONE);
                GridLayout gl = new GridLayout(1, false);
                gl.marginHeight = 0;
                gl.marginWidth = 0;
                // Sets background color of wrapper composite to white
                wrapperComp.setBackground(wrapperComp.getDisplay()
                        .getSystemColor(SWT.COLOR_WHITE));
                wrapperComp.setSize(1000, 1000);

                canvasComp = new Composite(wrapperComp, SWT.NONE);
                canvasComp.setLayout(gl);
                canvasComp.setSize(1000, 1000);

                // Set canvasComp as content on scrollable
                scrollable.setContent(wrapperComp);
                scrollable.addListener(SWT.Resize, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        scrollableBounds = ((Composite) event.widget)
                                .getBounds();
                        setCanvasSize(canvasBounds);
                    }
                });

                IDisplayPane pane = addPane(renderableDisplay, canvasComp);
                canvasComp.layout();
                scrollableBounds = scrollable.getBounds();
                canvasBounds = canvasComp.getBounds();
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
        canvasBounds = bounds;
        canvasComp.setSize(bounds.width, bounds.height);

        Rectangle scrollableBounds = new Rectangle(this.scrollableBounds.x,
                this.scrollableBounds.y, this.scrollableBounds.width,
                this.scrollableBounds.height);

        // Subtract size of scroll bars if visible
        ScrollBar vertical = scrollable.getVerticalBar();
        ScrollBar horizon = scrollable.getHorizontalBar();
        if (scrollableBounds.width <= canvasBounds.width) {
            scrollableBounds.height -= horizon.getSize().y;
        }
        if (scrollableBounds.height <= canvasBounds.height) {
            scrollableBounds.width -= vertical.getSize().x;
        }

        wrapperComp.setSize(
                Math.max(canvasBounds.width, scrollableBounds.width),
                Math.max(canvasBounds.height, scrollableBounds.height));
        canvasComp.setLocation(
                Math.max(0, (scrollableBounds.width - bounds.width) / 2),
                Math.max(0, (scrollableBounds.height - bounds.height) / 2));
        wrapperComp.layout();
        canvasComp.layout();
    }
}
