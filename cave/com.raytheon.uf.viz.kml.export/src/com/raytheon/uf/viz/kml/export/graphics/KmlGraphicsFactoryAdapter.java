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

import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Not very interesting, just constructs KML things
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 01, 2012           bsteffen  Initial creation
 * Nov 14, 2016  5976     bsteffen  Remove deprecated methods
 * 
 * </pre>
 * 
 * @author bsteffen
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
    public Canvas constrcutCanvas(Composite canvasComp) throws VizException {
        // Its possible we should just return null and not worry so much
        throw new UnsupportedOperationException(
                "KmlGraphicsFactoryAdapter does not support creating a canvas");
    }

}
