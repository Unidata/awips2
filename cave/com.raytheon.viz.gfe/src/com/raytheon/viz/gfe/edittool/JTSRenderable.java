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

import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Implements an on-screen renderable that is backed by JTS.
 * 
 * This class is a bridge between renderables and the JTS Compiler.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 5, 2008				chammack	Initial creation
 * May 15, 2014  #3069      randerso    Made labelSpacing settable.
 *                                      Fixed label coordinates after ReferencedGeometry
 *                                      was made non-destructive.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class JTSRenderable implements IRenderable {

    protected IWireframeShape wireframeShape;

    protected ConcurrentLinkedQueue<Geometry> pendingGeometies;

    private IFont labelFont;

    private RGB color = ColorUtil.WHITE;

    private int labelSpacing = 50;

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /**
     * @return the lineWidth
     */
    public float getLineWidth() {
        return lineWidth;
    }

    /**
     * @param lineWidth
     *            the lineWidth to set
     */
    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    public void setLabelSpacing(int labelSpacing) {
        this.labelSpacing = labelSpacing;
    }

    private float lineWidth = 1.0f;

    public JTSRenderable() {
        this.pendingGeometies = new ConcurrentLinkedQueue<Geometry>();

    }

    public void addGeometry(Geometry g) {
        this.pendingGeometies.add(g);
    }

    public void clear() {
        this.pendingGeometies.clear();
        if (wireframeShape != null) {
            wireframeShape.dispose();
            this.wireframeShape = null;
        }
    }

    public void dispose() {
        if (this.wireframeShape != null) {
            this.wireframeShape.dispose();
            this.wireframeShape = null;
        }

        if (this.labelFont != null) {
            this.labelFont.dispose();
            this.labelFont = null;
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        IDescriptor descriptor = ((EditToolPaintProperties) paintProps)
                .getDescriptor();

        if (this.wireframeShape == null) {

            this.wireframeShape = target.createWireframeShape(true, descriptor);
        }

        if (!this.pendingGeometies.isEmpty()) {
            JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape,
                    descriptor);
            while (!this.pendingGeometies.isEmpty()) {
                Geometry geom = this.pendingGeometies.remove();
                jtsCompiler.handle(geom);

                if (geom.getUserData() instanceof String) {
                    String label = (String) geom.getUserData();
                    double[] coord = new double[2];
                    int numCoords = geom.getNumPoints();
                    for (int j = 0; j < numCoords; j += this.labelSpacing) {
                        Coordinate c = geom.getCoordinates()[j];
                        coord[0] = c.x;
                        coord[1] = c.y;
                        coord = descriptor.worldToPixel(coord);
                        this.wireframeShape.addLabel(label, coord);
                    }
                }
            }
        }

        if (labelFont == null) {
            labelFont = GFEFonts.makeGFEIFont(target, "Cline_font", 2);
        }

        if (this.wireframeShape != null) {
            target.drawWireframeShape(this.wireframeShape, color, lineWidth,
                    LineStyle.SOLID, labelFont);
        }
    }
}
