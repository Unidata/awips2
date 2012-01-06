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
package com.raytheon.viz.ui.tools.nav;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.tools.nav.rsc.ZoomToolResourceData;

/**
 * Input handler for the zoom tool
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ZoomHandler extends InputAdapter {

    private static final ZoomHandler instance = new ZoomHandler();

    private int firstX, firstY;

    private Rectangle zoomRect;

    private IDisplayPane pane;

    private ResourcePair resource;

    private ZoomToolResourceData ztrd;

    private IDisplayPaneContainer container = null;

    private ZoomHandler() {
        ztrd = new ZoomToolResourceData(this);
        resource = ResourcePair.constructSystemResourcePair(ztrd);
    }

    public static ZoomHandler getInstance(IDisplayPaneContainer container) {
        instance.container = container;
        return instance;
    }

    public Rectangle getZoomRect() {
        return zoomRect;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int aX, int aY, int button) {
        if (button != 1) {
            return false;
        }
        pane = container.getActiveDisplayPane();
        if (pane == null) {
            return false;
        }
        // Add zoom tool resource to active pane
        pane.getDescriptor().getResourceList().add(resource);
        pane.getDescriptor().getResourceList()
                .instantiateResources(pane.getDescriptor(), true);
        container.refresh();
        firstX = aX;
        firstY = aY;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    public boolean handleMouseDownMove(int aX, int aY, int button) {
        if (button != 1 || pane == null) {
            return false;
        }
        setZoombox(firstX, firstY, aX, aY);
        container.refresh();
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int aX, int aY, int button) {
        if (button != 1 || pane == null) {
            return false;
        }

        // Zoom to zoom box on each pane, moved from GLDisplayPane
        for (IDisplayPane pane : container.getDisplayPanes()) {
            zoomToZoombox(pane);
        }

        // Remove zoom tool resource from active pane
        pane.getDescriptor().getResourceList().remove(resource);
        pane = null;
        zoomRect = null;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#setZoombox(int, int, int, int)
     */
    public void setZoombox(int firstX, int firstY, int lastX, int lastY) {
        IExtent extent = pane.getRenderableDisplay().getExtent();
        Rectangle bounds = pane.getBounds();

        int correctedX = (int) ((firstX * (extent.getMaxX() - extent.getMinX()) / bounds.width) + extent
                .getMinX());
        int correctedX2 = (int) ((lastX * (extent.getMaxX() - extent.getMinX()) / bounds.width) + extent
                .getMinX());
        int correctedY = (int) ((firstY * (extent.getMaxY() - extent.getMinY()) / bounds.height) + extent
                .getMinY());
        int correctedY2 = (int) ((lastY * (extent.getMaxY() - extent.getMinY()) / bounds.height) + extent
                .getMinY());

        this.zoomRect = new Rectangle(correctedX, correctedY, correctedX2
                - correctedX, correctedY2 - correctedY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#zoomToZoombox()
     */
    public void zoomToZoombox(IDisplayPane pane) {

        if (zoomRect == null) {
            return;
        }

        Rectangle curDisplay = pane.getBounds();

        double ratioX = (double) this.zoomRect.width
                / (double) curDisplay.width;
        double ratioY = (double) this.zoomRect.height
                / (double) curDisplay.height;

        double newRatio = 0.0;
        if (ratioX > ratioY) {
            newRatio = ratioX;
        } else {
            newRatio = ratioY;
        }

        double wd = (curDisplay.width * newRatio);
        double ht = (int) (curDisplay.height * newRatio);

        int centerX = this.zoomRect.x + this.zoomRect.width / 2;
        int centerY = this.zoomRect.y + this.zoomRect.height / 2;
        IExtent extent = null;

        try {
            extent = GraphicsFactory.getGraphicsAdapter().constructExtent(
                    centerX - wd / 2, centerX + wd / 2, centerY - ht / 2,
                    centerY + ht / 2);
        } catch (VizException e) {
            /*
             * Failed to construct extent with the factory. Default to
             * PixelExtent type.
             */
            extent = new PixelExtent(centerX - wd / 2, centerX + wd / 2,
                    centerY - ht / 2, centerY + ht / 2);
        }

        pane.getRenderableDisplay().setExtent(extent);
        pane.setZoomLevel(pane.getRenderableDisplay().recalcZoomLevel(
                pane.getRenderableDisplay().getDimensions()));
        pane.refresh();

        this.zoomRect = null;
    }
}
