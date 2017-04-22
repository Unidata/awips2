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
package com.raytheon.uf.viz.kml.export.graphics;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.AbstractView;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;

/**
 * 
 * Minimalistic implementation of a view for KML. Can not be shifted or
 * modified, so KML does not supprt Pan/Zoom.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlView extends AbstractView {

    private final Rectangle canvasBounds;

    public KmlView(IExtent extent, Rectangle canvasBounds) {
        super(extent);
        this.canvasBounds = canvasBounds;
    }

    @Override
    public void setupView(IGraphicsTarget target) {
        if (target instanceof KmlGraphicsTarget) {
            ((KmlGraphicsTarget) target).setView(this);
            return;
        }
        throw new IllegalArgumentException(
                "KmlView was expecting a KmlGraphicsTarget but recieved "
                        + target.getClass().getSimpleName());
    }

    @Override
    public double recalcZoomLevel(int[] dimensions) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public void zoom(double zoomLevel) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public void setExtent(IExtent pe) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public void scaleToClientArea(Rectangle clientArea, int[] dimensions) {
        throw new UnsupportedOperationException("KmlView is read only!");
    }

    @Override
    public KmlView clone() {
        return new KmlView(extent.clone(), canvasBounds);
    }

    @Override
    public Rectangle getCanvasBounds(IGraphicsTarget target) {
        return canvasBounds;
    }

}
