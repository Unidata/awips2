/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy;

import java.util.List;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.InputManager;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.util.AbstractGraphPanHandler;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;

/**
 * Represents a single X/Y graph pane that supports custom zoom/pan handling.
 * X/Y panes typically have the main graph canvas, along with an inset map
 * canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 08, 2022 8792       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public abstract class AbstractCustomHandlingXyPane extends XyPane {

    private final boolean registerCustomHandlers;

    private AbstractGraphZoomHandler zoomHandler;

    private AbstractGraphPanHandler panHandler;

    /**
     * {@inheritDoc}
     */
    public AbstractCustomHandlingXyPane(IDisplayPaneContainer paneContainer,
            Composite composite, IRenderableDisplay renderableDisplay,
            List<IPane> panes) throws VizException {
        super(paneContainer, composite, renderableDisplay, panes);

        /*
         * Only one zoom and one pan handler is needed for a particular pane
         * type, so determine if we need to register new ones or if a different
         * pane's handlers already cover this pane.
         */
        registerCustomHandlers = panes.stream()
                .noneMatch(pane -> pane
                        .getMainCanvas() instanceof AbstractCustomHandlingXyPane
                        && pane.getMainCanvas().getDescriptor().isCompatible(
                                renderableDisplay.getDescriptor()));
    }

    /**
     * Create a custom zoom handler for the given display.
     *
     * @param display
     * @return the custom zoom handler
     */
    protected abstract AbstractGraphZoomHandler createZoomHandler(
            IRenderableDisplay display);

    /**
     * Create a custom pan handler for the given display.
     *
     * @param display
     * @return the custom pan handler
     */
    protected abstract AbstractGraphPanHandler createPanHandler(
            IRenderableDisplay display);

    @Override
    public final void registerHandlers(InputManager inputManager) {
        super.registerHandlers(inputManager);

        if (registerCustomHandlers) {
            IRenderableDisplay display = getMainCanvas().getRenderableDisplay();
            zoomHandler = createZoomHandler(display);
            inputManager.registerMouseHandler(zoomHandler);
            panHandler = createPanHandler(display);
            inputManager.registerMouseHandler(panHandler);
        }
    }

    @Override
    protected void onCompositeDispose() {
        if (zoomHandler != null) {
            /*
             * Hand handlers off to a remaining compatible pane, if one exists.
             * Otherwise, unregister them from the editor.
             */
            boolean removeHandlers = true;
            for (IPane pane : paneContainer.getPanes()) {
                if (pane instanceof AbstractCustomHandlingXyPane
                        && pane.getMainCanvas().getDescriptor().isCompatible(
                                getMainCanvas().getDescriptor())) {
                    IRenderableDisplay display = pane.getMainCanvas()
                            .getRenderableDisplay();
                    zoomHandler.setRenderableDisplay(display);
                    ((AbstractCustomHandlingXyPane) pane).zoomHandler = zoomHandler;
                    panHandler.setRenderableDisplay(display);
                    ((AbstractCustomHandlingXyPane) pane).panHandler = panHandler;
                    removeHandlers = false;
                    break;
                }
            }

            if (removeHandlers) {
                paneContainer.unregisterMouseHandler(zoomHandler);
                paneContainer.unregisterMouseHandler(panHandler);
            }
        }

        super.onCompositeDispose();
    }

    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        if (DisplayChangeType.ADD.equals(type)) {
            if (zoomHandler != null && newRenderableDisplay.getDescriptor()
                    .isCompatible(zoomHandler.getRenderableDisplay()
                            .getDescriptor())) {
                zoomHandler.setRenderableDisplay(newRenderableDisplay);
                panHandler.setRenderableDisplay(newRenderableDisplay);
            }
        }
        super.renderableDisplayChanged(pane, newRenderableDisplay, type);
    }
}
