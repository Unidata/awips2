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
package com.raytheon.uf.viz.collaboration.ui.editor;

import java.util.IdentityHashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ScrollBar;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * PaneManager for the CollaborationEditor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationPaneManager extends PaneManager {

    private static class DisplayData {

        private IDisplayPane pane;

        private Composite wrapperComp;

        private Composite canvasComp;

        private ScrolledComposite scrollable;

        private Rectangle scrollableBounds;

        private Rectangle canvasBounds;

    }

    private Map<IRenderableDisplay, DisplayData> displayMap = new IdentityHashMap<IRenderableDisplay, DisplayData>();

    private DisplayData activeData;

    private LoopProperties loopProperties = new LoopProperties();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.panes.PaneManager#initializeComponents(com.raytheon
     * .uf.viz.core.IDisplayPaneContainer, org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void initializeComponents(IDisplayPaneContainer container,
            Composite comp) {
        super.initializeComponents(container, comp);
        adjustPaneLayout(1);
    }

    public IDisplayPane activateDisplay(int displayId,
            final IRenderableDisplay renderableDisplay) {
        DisplayData data = displayMap.get(renderableDisplay);
        if (data == null) {
            data = new DisplayData();
            final DisplayData finalData = data;
            // // scrollable composite
            data.scrollable = new ScrolledComposite(composite, SWT.H_SCROLL
                    | SWT.V_SCROLL);
            data.scrollable.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
                    true, true));

            // Composite for canvas comp
            data.wrapperComp = new Composite(data.scrollable, SWT.NONE);
            GridLayout gl = new GridLayout(1, false);
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            // Sets background color of wrapper composite to white
            data.wrapperComp.setBackground(data.wrapperComp.getDisplay()
                    .getSystemColor(SWT.COLOR_WHITE));
            data.wrapperComp.setSize(1, 1);

            data.canvasComp = new Composite(data.wrapperComp, SWT.NONE);
            data.canvasComp.setLayout(gl);
            data.canvasComp.setSize(1, 1);

            // Set canvasComp as content on scrollable
            data.scrollable.setContent(data.wrapperComp);
            data.scrollable.addListener(SWT.Resize, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    finalData.scrollableBounds = ((Composite) event.widget)
                            .getBounds();
                    setCanvasSize(renderableDisplay, finalData.canvasBounds);
                }
            });

            try {
                data.pane = createNewPane(renderableDisplay, data.canvasComp);
                registerHandlers(data.pane);
                data.canvasComp.layout();
                data.scrollableBounds = data.scrollable.getBounds();
                data.canvasBounds = data.canvasComp.getBounds();
                displayMap.put(renderableDisplay, data);
            } catch (Exception e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
                return null;
            }
        }

        if (activeData != null) {
            setExclude(activeData, true);
        }
        setExclude(data, false);
        activeData = data;
        composite.layout();
        activatedPane = activeData.pane;
        displayPanes.clear();
        displayPanes.add((VizDisplayPane) activatedPane);
        return activeData.pane;
    }

    /**
     * @param display
     */
    public void dispose(IRenderableDisplay display) {
        DisplayData data = displayMap.remove(display);
        if (data != null) {
            data.pane.dispose();
            data.scrollable.dispose();
            if (activeData == data) {
                activeData = null;
                activatedPane = null;
                displayPanes.clear();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.panes.PaneManager#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.panes.PaneManager#setLoopProperties(com.raytheon.
     * uf.viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        this.loopProperties = loopProperties;
    }

    public Rectangle getCanvasSize(IRenderableDisplay display) {
        Rectangle bounds = null;
        DisplayData data = displayMap.get(display);
        if (data != null) {
            bounds = data.canvasBounds;
        }
        return bounds;
    }

    public void setCanvasSize(IRenderableDisplay display, Rectangle bounds) {
        DisplayData data = displayMap.get(display);
        if (data == null) {
            return;
        }
        data.canvasBounds = bounds;
        data.canvasComp.setSize(bounds.width, bounds.height);

        Rectangle scrollableBounds = new Rectangle(data.scrollableBounds.x,
                data.scrollableBounds.y, data.scrollableBounds.width,
                data.scrollableBounds.height);

        // Subtract size of scroll bars if visible
        ScrollBar vertical = data.scrollable.getVerticalBar();
        ScrollBar horizon = data.scrollable.getHorizontalBar();
        if (scrollableBounds.width <= data.canvasBounds.width) {
            scrollableBounds.height -= horizon.getSize().y;
        }
        if (scrollableBounds.height <= data.canvasBounds.height) {
            scrollableBounds.width -= vertical.getSize().x;
        }

        data.wrapperComp.setSize(
                Math.max(data.canvasBounds.width, scrollableBounds.width),
                Math.max(data.canvasBounds.height, scrollableBounds.height));
        data.canvasComp.setLocation(
                Math.max(0, (scrollableBounds.width - bounds.width) / 2),
                Math.max(0, (scrollableBounds.height - bounds.height) / 2));
        data.wrapperComp.layout();
        data.canvasComp.layout();
    }

    private void setExclude(DisplayData data, boolean exclude) {
        GridData gd = (GridData) data.scrollable.getLayoutData();
        data.scrollable.setVisible(!exclude);
        gd.exclude = exclude;
    }

}
