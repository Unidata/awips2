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
package com.raytheon.viz.gfe.edittool;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEPreference;
import org.locationtech.jts.geom.Coordinate;

/**
 * Basic implementation of a simple free form drawing object
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 10, 2008           chammack  Initial Creation.
 * Apr 14, 2009  2058     rjpeter   Ensured nulls couldn't be added to
 *                                  polyLineVis.
 * Jul 23, 2014  3429     mapeters  Updated deprecated drawLine() call.
 * Jan 15, 2016  5193     bsteffen  Lazy load prefs.
 * Jan 23, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */
public class FreeformRenderable implements IRenderable {

    protected List<Coordinate> polyLineVis;

    /**
     * Constructor
     */
    public FreeformRenderable() {
        polyLineVis = new ArrayList<>();
    }

    /**
     * Add a coordinate to the polyline
     *
     * @param coord
     */
    public void add(Coordinate coord) {
        if (coord != null) {
            polyLineVis.add(coord);
        }
    }

    /**
     * Clear this renderable's coordinates
     */
    public void clear() {
        polyLineVis.clear();
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        IDescriptor descriptor = ((EditToolPaintProperties) paintProps)
                .getDescriptor();

        if (descriptor != null) {
            double[] last = null;
            double[] nextCoord = new double[2];
            List<DrawableLine> lines = new ArrayList<>();
            for (Coordinate coord : polyLineVis) {
                nextCoord[0] = coord.x;
                nextCoord[1] = coord.y;
                double[] out = descriptor.worldToPixel(nextCoord);
                if ((last != null) && (out != null)) {
                    DrawableLine line = new DrawableLine();
                    line.setCoordinates(last[0], last[1]);
                    line.addPoint(out[0], out[1]);
                    line.basics.color = getDrawingColor();
                    lines.add(line);
                }
                last = out;
            }
            target.drawLine(lines.toArray(new DrawableLine[0]));
        }
    }

    private RGB getDrawingColor() {
        String color = GFEPreference.getString("Drawing_color", "white");
        return RGBColors.getRGBColor(color);
    }

}
