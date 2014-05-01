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
package com.raytheon.viz.core.gl.internal;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import javax.media.opengl.GL;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.Activator;
import com.raytheon.viz.core.gl.GLGeometryObject2D;
import com.raytheon.viz.core.gl.GLGeometryObject2D.GLGeometryObjectData;
import com.raytheon.viz.core.gl.IGLTarget;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Wireframe Shape object adapted from GLWireframeShape that is much simpler and
 * uses VBOs, 2D object, ignores Z so it only supports 2D.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLWireframeShape2D implements IWireframeShape {

    private static final RGB DEFAULT_COLOR = new RGB(255, 255, 255);

    /** list of labels to draw */
    private List<DrawableString> labels;

    private MathTransform worldToTargetGrid;

    private boolean compiled = false;

    private GLGeometryObject2D geometry;

    private GLGeometryObjectData geomData;

    public GLWireframeShape2D(GeneralGridGeometry gridGeometry, boolean mutable) {
        geomData = new GLGeometryObjectData(GL.GL_LINE_STRIP,
                GL.GL_VERTEX_ARRAY);
        geomData.mutable = mutable;
        geomData.worldExtent = new PixelExtent(gridGeometry.getGridRange());

        try {
            worldToTargetGrid = TransformFactory.worldToGrid(gridGeometry,
                    PixelInCell.CELL_CENTER);
        } catch (FactoryException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error getting transform from base crs to target grid", e);
        }

        initialize();
    }

    private void initialize() {
        compiled = false;
        labels = new ArrayList<DrawableString>();
        geometry = new GLGeometryObject2D(geomData);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#compile()
     */
    @Override
    public void compile() {
        geomData.mutable = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#isMutable()
     */
    @Override
    public boolean isMutable() {
        return geometry.isMutable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#isDrawable()
     */
    @Override
    public boolean isDrawable() {
        return geometry.isDrawable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#dispose()
     */
    @Override
    public synchronized void dispose() {
        geometry.dispose();
        clearLabels();
        labels = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#reset()
     */
    @Override
    public synchronized void reset() {
        dispose();
        initialize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(com
     * .vividsolutions.jts.geom.Coordinate[])
     */
    @Override
    public void addLineSegment(Coordinate[] worldCoords) {
        double screenCoords[][] = new double[worldCoords.length][];
        for (int i = 0; i < worldCoords.length; ++i) {
            Coordinate c = worldCoords[i];
            if (worldToTargetGrid != null) {
                try {
                    double[] out = new double[2];
                    worldToTargetGrid.transform(new double[] { c.x, c.y }, 0,
                            out, 0, 1);
                    screenCoords[i] = out;
                } catch (TransformException e) {
                    // Ignore...
                }
            } else {
                // Assume no conversion needed
                screenCoords[i] = new double[] { c.x, c.y };
            }

        }
        addLineSegment(screenCoords);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(double
     * [][])
     */
    @Override
    public synchronized void addLineSegment(double[][] screenCoordinates) {
        geometry.addSegment(screenCoordinates);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IWireframeShape#addLabel(java.lang
     * .String, double[])
     */
    @Override
    public synchronized void addLabel(String label, double[] screenCoordinate) {
        if (labels != null) {
            DrawableString string = new DrawableString(label, DEFAULT_COLOR);
            string.horizontalAlignment = HorizontalAlignment.CENTER;
            string.verticallAlignment = VerticalAlignment.MIDDLE;
            string.setCoordinates(screenCoordinate[0], screenCoordinate[1]);
            labels.add(string);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#clearLabels()
     */
    @Override
    public synchronized void clearLabels() {
        if (labels != null) {
            labels.clear();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#allocate(int)
     */
    @Override
    public void allocate(int points) {
        geometry.allocate(points);
    }

    public synchronized void paint(IGLTarget target, IExtent viewExtent,
            Rectangle canvasSize, RGB color, float lineWidth,
            LineStyle lineStyle, IFont font, float alpha) throws VizException {
        if (isDrawable() == false) {
            return;
        }

        if (!geomData.mutable && !compiled) {
            compiled = true;
            geometry.compile(target.getGl());
        }

        GL gl = target.getGl();

        List<DrawableString> toDraw = new ArrayList<DrawableString>(
                labels.size());

        for (DrawableString str : labels) {
            if (viewExtent
                    .contains(new double[] { str.basics.x, str.basics.y })) {
                str.font = font;
                str.setText(str.getText(), color);
                str.basics.alpha = alpha;
                toDraw.add(str);
            }
        }

        boolean usedStencilBuffer = false;

        if (toDraw.size() > 0) {
            int[] colorBuffer = new int[1];
            gl.glGetIntegerv(GL.GL_DRAW_BUFFER, colorBuffer, 0);
            gl.glDrawBuffer(GL.GL_NONE);
            gl.glEnable(GL.GL_STENCIL_TEST);
            gl.glClear(GL.GL_STENCIL_BUFFER_BIT);

            gl.glStencilFunc(GL.GL_ALWAYS, 0x1, 0x1);
            gl.glStencilOp(GL.GL_REPLACE, GL.GL_REPLACE, GL.GL_REPLACE);
            usedStencilBuffer = true;
            double scale = viewExtent.getWidth() / canvasSize.width;

            double adjustedHalfWidth = 0;
            double adjustedHalfHeight = 0;
            double yPosition1 = 0;
            double yPosition2 = 0;
            double xPosition1 = 0;
            double xPosition2 = 0;
            Rectangle2D bounds = null;

            for (DrawableString label : toDraw) {
                bounds = target.getStringsBounds(label);
                double[] pos = new double[] { label.basics.x, label.basics.y };
                gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
                gl.glEnable(GL.GL_BLEND);
                gl.glColor4d(0.0, 0.0, 0.0, alpha);
                gl.glBegin(GL.GL_QUADS);

                adjustedHalfWidth = (bounds.getWidth() * scale) / 2.0;
                adjustedHalfHeight = (bounds.getHeight() * scale) / 2.0;
                xPosition1 = pos[0] - adjustedHalfWidth;
                xPosition2 = pos[0] + adjustedHalfWidth;
                yPosition1 = pos[1] - adjustedHalfHeight;
                yPosition2 = pos[1] + adjustedHalfHeight;
                gl.glVertex2d(xPosition1, yPosition1);
                gl.glVertex2d(xPosition2, yPosition1);
                gl.glVertex2d(xPosition2, yPosition2);
                gl.glVertex2d(xPosition1, yPosition2);
                gl.glEnd();
            }

            gl.glDrawBuffer(colorBuffer[0]);
        }

        gl.glEnable(GL.GL_BLEND);
        gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

        if (lineWidth > 0) {
            gl.glLineWidth(lineWidth);
        } else {
            gl.glLineWidth(2.0f);
        }

        ((GLTarget) target).handleLineStyle(lineStyle);
        gl.glColor4f(color.red / 255.0f, color.green / 255.0f,
                color.blue / 255.0f, alpha);

        if (usedStencilBuffer) {
            gl.glStencilFunc(GL.GL_NOTEQUAL, 0x1, 0x1);
            gl.glStencilOp(GL.GL_KEEP, GL.GL_KEEP, GL.GL_KEEP);
        }

        geometry.paint(target.getGl());

        gl.glDisable(GL.GL_BLEND);

        // Draw labels
        if (toDraw.size() > 0) {
            gl.glStencilFunc(GL.GL_ALWAYS, 0x0, 0x1);
            gl.glStencilOp(GL.GL_KEEP, GL.GL_KEEP, GL.GL_KEEP);
            gl.glClear(GL.GL_STENCIL_BUFFER_BIT);
            gl.glDisable(GL.GL_STENCIL_TEST);
            target.drawStrings(toDraw);
        }
    }

}
