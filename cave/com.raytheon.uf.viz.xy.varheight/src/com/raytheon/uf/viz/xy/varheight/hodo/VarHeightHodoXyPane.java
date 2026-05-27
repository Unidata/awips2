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
package com.raytheon.uf.viz.xy.varheight.hodo;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.hodo.HodographDescriptor;
import com.raytheon.uf.viz.xy.hodo.HodographRenderableDisplay;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightXyPane;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * Represents a single var height graph pane that has an inset hodograph canvas
 * along with the typical inset map canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2022 8791       mapeters    Initial creation
 * Sep 30, 2022 8792       mapeters    Refactored from com.raytheon.uf.viz.xy.hodo.HodoXyPane,
 *                                     extend VarHeightXyPane
 *
 * </pre>
 *
 * @author mapeters
 */
public class VarHeightHodoXyPane extends VarHeightXyPane {

    /**
     * Constructor.
     *
     * @param paneContainer
     *            the editor/container that this pane is going in
     * @param composite
     *            SWT composite for this pane's area
     * @param renderableDisplay
     *            main display to initially render on this pane
     * @param panes
     *            the other panes that are already in the pane container
     * @throws VizException
     */
    public VarHeightHodoXyPane(IDisplayPaneContainer paneContainer,
            Composite composite, IRenderableDisplay renderableDisplay,
            List<IPane> panes) throws VizException {
        super(paneContainer, composite, renderableDisplay, panes);
    }

    @Override
    protected void initInsetCanvases(IDisplayPaneContainer paneContainer,
            Composite composite, IRenderableDisplay renderableDisplay,
            List<IPane> panes) throws VizException {
        super.initInsetCanvases(paneContainer, composite, renderableDisplay,
                panes);

        Composite hodoComp = new Composite(composite, SWT.NONE);
        hodoComp.setLayout(new FormLayout());
        hodoComp.setLayoutData(getHodographFormData());

        IRenderableDisplay hodoDisplay = new HodographRenderableDisplay();
        hodoDisplay.setDescriptor(
                new HodographDescriptor(new PixelExtent(0, 1000, 0, 1000)));
        hodoDisplay.setExtent(new PixelExtent(0, 1000, 0, 1000));
        ((HodographRenderableDisplay) hodoDisplay)
                .setParentDisplay(renderableDisplay);
        hodoDisplay.getDescriptor().getResourceList()
                .instantiateResources(hodoDisplay.getDescriptor(), true);

        for (IPane pane : panes) {
            if (renderableDisplay.getDescriptor()
                    .isCompatible(pane.getMainCanvas().getDescriptor())) {
                IDisplayPane hodoCanvas = pane
                        .getCanvas(CanvasType.SECONDARY_INSET);
                if (hodoCanvas != null) {
                    hodoDisplay.setDescriptor(hodoCanvas.getDescriptor());
                    break;
                }
            }
        }

        IDisplayPane hodoCanvas = new VizDisplayPane(paneContainer, hodoComp,
                CanvasType.SECONDARY_INSET, hodoDisplay, true);
        ((VizDisplayPane) hodoCanvas).getCanvas()
                .setLayoutData(getFullFormData());
        setInsetVisibility(hodoCanvas);
        addCanvas(hodoCanvas);
    }

    private static FormData getHodographFormData() {
        FormData fd = new FormData();
        fd.left = new FormAttachment(0, 0);
        fd.right = new FormAttachment(35, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(35, 0);
        return fd;
    }
}
