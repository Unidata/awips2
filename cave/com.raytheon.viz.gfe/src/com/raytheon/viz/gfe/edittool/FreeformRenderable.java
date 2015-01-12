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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Basic implementation of a simple free form drawing object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/10/2008              chammack    Initial Creation.
 * 04/14/2009   #2058      rjpeter     Ensured nulls couldn't be added to polyLineVis.
 * 07/23/2014   #3429      mapeters    Updated deprecated drawLine() call.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class FreeformRenderable implements IRenderable {

    protected List<Coordinate> polyLineVis;
    
    private static RGB drawingColor = new RGB(255, 255, 255);
    
    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
                if (prefs.contains("Drawing_color")) {
                    String color = prefs.getString("Drawing_color");        
                    drawingColor = RGBColors.getRGBColor(color);
                }
            }
        }.run();
    }

    public FreeformRenderable() {
        polyLineVis = new ArrayList<Coordinate>();
    }

    public void add(Coordinate coord) {
        if (coord != null) {
            polyLineVis.add(coord);
        }
    }

    public void addAll(Coordinate[] coords) {
        if (coords != null) {
            for (Coordinate coord : coords) {
                add(coord);
            }
        }
    }

    public void clear() {
        polyLineVis.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        IDescriptor descriptor = ((EditToolPaintProperties) paintProps)
                .getDescriptor();

        if (descriptor != null) {
            double[] last = null;
            double[] nextCoord = new double[2];
            List<DrawableLine> lines = new ArrayList<DrawableLine>();
            for (Coordinate coord : polyLineVis) {
                nextCoord[0] = coord.x;
                nextCoord[1] = coord.y;
                double[] out = descriptor.worldToPixel(nextCoord);
                if ((last != null) && (out != null)) {
                    DrawableLine line = new DrawableLine();
                    line.setCoordinates(last[0], last[1]);
                    line.addPoint(out[0], out[1]);
                    line.basics.color = drawingColor;
                    lines.add(line);
                }
                last = out;
            }
            target.drawLine(lines.toArray(new DrawableLine[0]));
        }
    }

}
