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
package com.raytheon.uf.viz.xy.map.rsc;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO: This class was made OBE by IGraphicsTarget.drawPoint(...) Simple class
 * for drawing a star-like point at a given lat/lon
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PointRenderable implements IRenderable {

    private Coordinate location;

    private RGB color;

    private MapDescriptor descriptor;

    private static final int LENGTH = 5;

    private static final int LONG_LENGTH = 7;

    public PointRenderable(Coordinate location, RGB color,
            MapDescriptor descriptor) {
        this.location = location;
        this.color = color;
        this.descriptor = descriptor;
    }

    public void setColor(RGB color) {
        this.color = color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        double[] pixels = descriptor.worldToPixel(new double[] { location.x,
                location.y });
        double ratio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double val = LENGTH / ratio;
        target.drawLine(pixels[0] + val, pixels[1] + val, 0.0, pixels[0] - val,
                pixels[1] - val, 0.0, color, 2.0f);
        target.drawLine(pixels[0] - val, pixels[1] + val, 0.0, pixels[0] + val,
                pixels[1] - val, 0.0, color, 2.0f);
        val = (LONG_LENGTH / ratio);
        target.drawLine(pixels[0], pixels[1] - val, 0.0, pixels[0], pixels[1]
                + val, 0.0, color, 2.0f);
    }
}
