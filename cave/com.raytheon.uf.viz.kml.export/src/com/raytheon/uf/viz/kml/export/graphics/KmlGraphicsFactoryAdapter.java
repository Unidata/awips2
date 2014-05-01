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
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Not very interesting, just constructs KML things
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
public class KmlGraphicsFactoryAdapter extends AbstractGraphicsFactoryAdapter {

    // The view needs to know the extent and canvas bounds from the source pane.
    private final IExtent extent;

    private final Rectangle canvasBounds;

    public KmlGraphicsFactoryAdapter(IExtent extent, Rectangle canvasBounds) {
        this.extent = extent;
        this.canvasBounds = canvasBounds;
    }

    @Override
    public KmlView constructView() {
        return new KmlView(extent, canvasBounds);
    }

    @Override
    public KmlGraphicsTarget constructTarget(Canvas canvas, float width,
            float height) throws VizException {
        return new KmlGraphicsTarget();
    }

    @Override
    public IExtent constructExtent(Coordinate[] coords) throws VizException {
        return new PixelExtent(coords);
    }

    @Override
    public IExtent constructExtent(double aMinX, double aMaxX, double aMinY,
            double aMaxY) throws VizException {
        return new PixelExtent(aMinX, aMaxX, aMinY, aMaxY);
    }

    @Override
    public IExtent constructExtent(Rectangle rect) throws VizException {
        return new PixelExtent(rect);
    }

    @Override
    public IExtent constructExtent(GridEnvelope range) throws VizException {
        return new PixelExtent(range);
    }

    @Override
    public Canvas constrcutCanvas(Composite canvasComp) throws VizException {
        // Its possible we should just return null and not worry so much
        throw new UnsupportedOperationException(
                "KmlGraphicsFactoryAdapter does not support creating a canvas");
    }

}
