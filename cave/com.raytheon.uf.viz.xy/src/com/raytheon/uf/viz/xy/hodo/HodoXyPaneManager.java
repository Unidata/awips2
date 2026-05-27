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
package com.raytheon.uf.viz.xy.hodo;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.IPane.CanvasType;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.XyPaneManager;
import com.raytheon.viz.ui.panes.LegacyPane;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * Pane manager implementation for an editor containing X/Y panes that each have
 * a hodograph inset canvas and a map inset canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            bsteffen    Initial creation
 * Sep 13, 2022 8792       mapeters    Updated for tracking of LegacyPanes
 * Oct 12, 2022 8946       mapeters    Added getCanvases(CanvasType)
 *
 * </pre>
 *
 * @author bsteffen
 */
public class HodoXyPaneManager extends XyPaneManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HodoXyPaneManager.class);

    protected Map<IDisplayPane, IDisplayPane> hodoPaneMap;

    protected Map<IDisplayPane, IDisplayPane> graphHodoPaneMap;

    public HodoXyPaneManager() {
        super();
        hodoPaneMap = new HashMap<>();
        graphHodoPaneMap = new HashMap<>();
    }

    @Override
    public IDisplayPane[] getDisplayPanes() {
        // Checks active pane to see if active pane is inset pane or graph
        if (hodoPaneMap.containsKey(getActiveDisplayPane())) {
            return getHodoPanes();
        } else {
            return super.getDisplayPanes();
        }
    }

    public IDisplayPane[] getHodoPanes() {
        return hodoPaneMap.keySet()
                .toArray(new IDisplayPane[hodoPaneMap.size()]);
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay,
            final Composite graphComposite) {
        try {
            IDisplayPane hodoPane = null, graphPane = null;
            final Composite hodoComp = new Composite(graphComposite, SWT.NONE);
            hodoComp.setLayout(new FormLayout());
            hodoComp.setLayoutData(getHodographLocation());
            HodographRenderableDisplay hodoDisplay = new HodographRenderableDisplay();
            hodoDisplay.setDescriptor(
                    new HodographDescriptor(new PixelExtent(0, 1000, 0, 1000)));
            hodoDisplay.setExtent(new PixelExtent(0, 1000, 0, 1000));
            hodoDisplay.setParentDisplay(renderableDisplay);
            hodoDisplay.getDescriptor().getResourceList()
                    .instantiateResources(hodoDisplay.getDescriptor(), true);
            for (IDisplayPane pane : hodoPaneMap.keySet()) {
                hodoDisplay.setDescriptor(pane.getDescriptor());
                break;
            }

            final VizDisplayPane hodographPane = new VizDisplayPane(this,
                    hodoComp, CanvasType.SECONDARY_INSET, hodoDisplay, true);
            hodographPane.getCanvas().setLayoutData(getFullFormData());

            hodographPane.getCanvas()
                    .addMouseTrackListener(new MouseTrackAdapter() {
                        @Override
                        public void mouseEnter(MouseEvent e) {
                            activatedPane = hodographPane;
                            currentMouseHoverPane = hodographPane;
                        }

                        @Override
                        public void mouseExit(MouseEvent e) {
                            activatedPane = insetMapPaneMap.get(hodographPane);
                        }
                    });
            hodographPane.addListener(SWT.Resize, event -> {
                setVisibility(hodographPane);
                graphComposite.layout();
            });

            hodoPane = hodographPane;

            if (currentDensity < displayInsetDensity) {
                hodoComp.setVisible(false);
            }
            super.addPane(renderableDisplay, graphComposite);
            graphPane = mainCanvasToPaneMap.lastKey();
            if (hodoPane != null) {
                registerHandlers(hodoPane);
                hodoPaneMap.put(hodoPane, graphPane);
                graphHodoPaneMap.put(graphPane, hodoPane);
                LegacyPane pane = mainCanvasToPaneMap.get(graphPane);
                pane.addCanvas(hodoPane);
            }
            return graphPane;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding pane", e);
        }
        return null;
    }

    @Override
    public void removePane(IDisplayPane pane) {
        super.removePane(pane);
        IDisplayPane hodo = graphHodoPaneMap.remove(pane);
        if (hodo != null) {
            hodo.dispose();
            hodoPaneMap.remove(hodo);
        }
    }

    @Override
    public void refresh() {
        super.refresh();
        for (IDisplayPane pane : hodoPaneMap.keySet()) {
            pane.refresh();
        }
    }

    public FormData getHodographLocation() {
        FormData fd = new FormData();
        fd.right = new FormAttachment(35, 0);
        fd.left = new FormAttachment(0, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(35, 0);
        return fd;
    }

    @Override
    public IPane getActivePane() {
        IDisplayPane activeCanvas = getActiveDisplayPane();
        IDisplayPane mainCanvas = hodoPaneMap.get(activeCanvas);
        if (mainCanvas != null) {
            // Hodo inset is active
            return mainCanvasToPaneMap.get(mainCanvas);
        }
        return super.getActivePane();
    }

    @Override
    public IDisplayPane[] getCanvases(CanvasType type) {
        if (type == CanvasType.SECONDARY_INSET) {
            return getHodoPanes();
        }
        return super.getCanvases(type);
    }
}
