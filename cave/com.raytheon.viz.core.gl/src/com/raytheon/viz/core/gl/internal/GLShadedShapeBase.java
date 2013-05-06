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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.List;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;
import javax.media.opengl.glu.GLUtessellator;
import javax.media.opengl.glu.GLUtessellatorCallback;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IShape;
import com.raytheon.viz.core.gl.Activator;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * Provides base implementation of shaded shapes in openGL that can be easily
 * extended to implement IShadedShape or IColormapShadedShape.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2011            bsteffen     Initial creation
 * Apr 25, 2013 1954       bsteffen    Speed up creation of      
 *                                     GLColormapShadedShapes.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLShadedShapeBase implements IShape {

    public boolean tessellate;

    private MathTransform worldToPixel = null;

    protected byte[] fillPattern;

    protected List<FloatBuffer[]> polygons = new ArrayList<FloatBuffer[]>();

    private int numVertices = 0;

    private int numContours = 0;

    protected FloatBuffer vertexBuffer;

    protected IntBuffer contourStartBuffer;

    protected IntBuffer contourLengthBuffer;

    protected IntBuffer polygonLengthBuffer;

    protected ByteBuffer colorBuffer;

    public GLShadedShapeBase(GeneralGridGeometry targetGeometry,
            boolean tessellate) {
        this.tessellate = tessellate;
        try {
            this.worldToPixel = TransformFactory.worldToGrid(targetGeometry,
                    PixelInCell.CELL_CENTER);
        } catch (FactoryException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    protected GLShadedShapeBase(GLShadedShapeBase that) {
        this.tessellate = that.tessellate;
        this.worldToPixel = that.worldToPixel;
        this.fillPattern = that.fillPattern;
        this.polygons = that.polygons;
        this.vertexBuffer = that.vertexBuffer;
        this.polygonLengthBuffer = that.polygonLengthBuffer;
        this.contourLengthBuffer = that.contourLengthBuffer;
        this.contourStartBuffer = that.contourStartBuffer;
        this.colorBuffer = that.colorBuffer;
    }

    public synchronized void addPolygon(LineString[] lineString) {
        FloatBuffer[] buffers = new FloatBuffer[lineString.length];
        for (int i = 0; i < lineString.length; i++) {
            buffers[i] = FloatBuffer.allocate(lineString[i].getNumPoints() * 2);
            for (Coordinate c : lineString[i].getCoordinates()) {
                buffers[i].put((float) c.x);
                buffers[i].put((float) c.y);
                numVertices += 1;
            }
            try {
                float[] array = buffers[i].array();
                worldToPixel.transform(array, 0, array, 0,
                        lineString[i].getNumPoints());
            } catch (TransformException e) {
                // Ignore...
            }
            numContours += 1;
        }
        polygons.add(buffers);
    }

    public synchronized void addPolygonPixelSpace(LineString[] contours) {
        FloatBuffer[] buffers = new FloatBuffer[contours.length];
        for (int i = 0; i < contours.length; i++) {
            buffers[i] = FloatBuffer.allocate(contours[i].getNumPoints() * 2);
            for (Coordinate c : contours[i].getCoordinates()) {
                buffers[i].put((float) c.x);
                buffers[i].put((float) c.y);
                numVertices += 1;
            }
            numContours += 1;
        }
        polygons.add(buffers);
    }

    public synchronized void compile() {
        if (polygons.isEmpty()) {
            return;
        }
        allocateBuffers();
        if (tessellate) {

            GLU glu = new GLU();
            Tessellator callback = new Tessellator();
            GLUtessellator tessellator = glu.gluNewTess();

            glu.gluTessCallback(tessellator, GLU.GLU_TESS_VERTEX, callback);
            glu.gluTessCallback(tessellator, GLU.GLU_TESS_BEGIN, callback);
            glu.gluTessCallback(tessellator, GLU.GLU_TESS_END, callback);
            glu.gluTessCallback(tessellator, GLU.GLU_TESS_ERROR, callback);
            glu.gluTessCallback(tessellator, GLU.GLU_TESS_COMBINE, callback);
            glu.gluTessCallback(tessellator, GLU.GLU_TESS_EDGE_FLAG, callback);
            glu.gluTessNormal(tessellator, 0.0, 0.0, -1.0);

            for (FloatBuffer[] contours : polygons) {
                glu.gluTessProperty(tessellator, GLU.GLU_TESS_WINDING_RULE,
                        GLU.GLU_TESS_WINDING_ODD);
                glu.gluTessBeginPolygon(tessellator, (double[]) null);
                int polygonStart = vertexBuffer.position() / 2;
                for (FloatBuffer contour : contours) {
                    contour.rewind();
                    glu.gluTessBeginContour(tessellator);
                    while (contour.hasRemaining()) {
                        double[] coord = new double[3];
                        coord[0] = contour.get();
                        coord[1] = contour.get();
                        glu.gluTessVertex(tessellator, coord, 0, coord);
                    }
                    glu.gluTessEndContour(tessellator);
                }
                glu.gluTessEndPolygon(tessellator);
                int polygonEnd = vertexBuffer.position() / 2;
                polygonLengthBuffer.put(polygonEnd - polygonStart);

            }

            glu.gluDeleteTess(tessellator);

            // Copy the data into a compact direct buffer.
            int length = vertexBuffer.position();
            ByteBuffer vertexByteBuffer = ByteBuffer.allocateDirect(4 * length);
            vertexByteBuffer.order(ByteOrder.nativeOrder());
            FloatBuffer vertexBuffer = vertexByteBuffer.asFloatBuffer();
            this.vertexBuffer.rewind();
            this.vertexBuffer.limit(length);
            vertexBuffer.put(this.vertexBuffer);
            this.vertexBuffer = vertexBuffer;
        } else {
            for (FloatBuffer[] contours : polygons) {
                int polygonStart = vertexBuffer.position() / 2;
                for (FloatBuffer contour : contours) {
                    contour.rewind();
                    int contourStart = vertexBuffer.position() / 2;
                    vertexBuffer.put(contour);
                    int contourEnd = vertexBuffer.position() / 2;
                    contourStartBuffer.put(contourStart);
                    contourLengthBuffer.put(contourEnd - contourStart);
                }
                int polygonEnd = vertexBuffer.position() / 2;
                polygonLengthBuffer.put(polygonEnd - polygonStart);
            }
        }
        polygons = new ArrayList<FloatBuffer[]>();
    }

    private void allocateBuffers() {
        if (polygonLengthBuffer == null) {
            polygonLengthBuffer = IntBuffer.allocate(polygons.size());
        } else {
            IntBuffer polygonLengthBuffer = IntBuffer.allocate(polygons.size()
                    + this.polygonLengthBuffer.capacity());
            this.polygonLengthBuffer.rewind();
            polygonLengthBuffer.put(this.polygonLengthBuffer);
            this.polygonLengthBuffer = polygonLengthBuffer;
        }
        if (tessellate) {
            // This over allocates to avoid future resizing
            if (vertexBuffer == null) {
                vertexBuffer = FloatBuffer.allocate(numVertices * 2 * 3);
            } else {
                FloatBuffer vertexBuffer = FloatBuffer.allocate(numVertices * 2
                        * 3 + this.vertexBuffer.capacity());
                this.vertexBuffer.rewind();
                vertexBuffer.put(this.vertexBuffer);
                this.vertexBuffer = vertexBuffer;
            }
        } else {
            if (vertexBuffer == null) {
                ByteBuffer vertexByteBuffer = ByteBuffer
                        .allocateDirect(4 * 2 * numVertices);
                vertexByteBuffer.order(ByteOrder.nativeOrder());
                vertexBuffer = vertexByteBuffer.asFloatBuffer();
            } else {
                ByteBuffer vertexByteBuffer = ByteBuffer
                        .allocateDirect(4 * (numVertices + vertexBuffer
                                .capacity()));
                vertexByteBuffer.order(ByteOrder.nativeOrder());
                FloatBuffer vertexBuffer = vertexByteBuffer.asFloatBuffer();
                this.vertexBuffer.rewind();
                vertexBuffer.put(this.vertexBuffer);
                this.vertexBuffer = vertexBuffer;
            }
            if (contourLengthBuffer == null) {
                ByteBuffer contourLengthByteBuffer = ByteBuffer
                        .allocateDirect(4 * numContours);
                contourLengthByteBuffer.order(ByteOrder.nativeOrder());
                contourLengthBuffer = contourLengthByteBuffer.asIntBuffer();
            } else {
                ByteBuffer contourLengthByteBuffer = ByteBuffer
                        .allocateDirect(4 * (numContours + contourLengthBuffer
                                .capacity()));
                contourLengthByteBuffer.order(ByteOrder.nativeOrder());
                IntBuffer contourLengthBuffer = contourLengthByteBuffer
                        .asIntBuffer();
                this.contourLengthBuffer.rewind();
                contourLengthBuffer.put(this.contourLengthBuffer);
                this.contourLengthBuffer = contourLengthBuffer;
            }
            if (contourStartBuffer == null) {
                ByteBuffer contourStartByteBuffer = ByteBuffer
                        .allocateDirect(4 * numContours);
                contourStartByteBuffer.order(ByteOrder.nativeOrder());
                contourStartBuffer = contourStartByteBuffer.asIntBuffer();
            } else {
                ByteBuffer contourStartByteBuffer = ByteBuffer
                        .allocateDirect(4 * (numContours + contourStartBuffer
                                .capacity()));
                contourStartByteBuffer.order(ByteOrder.nativeOrder());
                IntBuffer contourStartBuffer = contourStartByteBuffer
                        .asIntBuffer();
                this.contourStartBuffer.rewind();
                contourStartBuffer.put(this.contourStartBuffer);
                this.contourStartBuffer = contourStartBuffer;
            }
        }
    }

    public synchronized void color(List<RGB> colors) {
        if (colors == null || colors.isEmpty() || vertexBuffer == null) {
            return;
        }
        colorBuffer = ByteBuffer
                .allocateDirect(3 * vertexBuffer.capacity() / 2);
        colorBuffer.order(ByteOrder.nativeOrder());

        polygonLengthBuffer.rewind();
        for (RGB color : colors) {
            int length = polygonLengthBuffer.get();
            byte red = (byte) color.red;
            byte green = (byte) color.green;
            byte blue = (byte) color.blue;
            for (int i = 0; i < length; i++) {
                colorBuffer.put(red);
                colorBuffer.put(green);
                colorBuffer.put(blue);
            }
        }
        polygonLengthBuffer = null;
    }

    protected synchronized void paint(GL gl,
            boolean cardSupportsHighEndFeatures, float brightness) {
        if (!polygons.isEmpty()) {
            compile();
        }
        if (this.vertexBuffer == null) {
            return;
        }
        if (this.vertexBuffer.capacity() == 0) {
            return;
        }
        ByteBuffer colorBuffer = this.colorBuffer;
        colorBuffer.rewind();
        if (brightness < 1.0f) {
            colorBuffer = ByteBuffer
                    .allocateDirect(this.colorBuffer.capacity());
            while (this.colorBuffer.hasRemaining()) {
                colorBuffer
                        .put((byte) ((this.colorBuffer.get() & 0xFF) * brightness));
            }
            colorBuffer.rewind();
        }

        vertexBuffer.rewind();

        if (fillPattern != null) {
            gl.glEnable(GL.GL_POLYGON_STIPPLE);

            gl.glPolygonStipple(fillPattern, 0);
        }

        gl.glVertexPointer(2, GL.GL_FLOAT, 0, vertexBuffer);
        gl.glColorPointer(3, GL.GL_UNSIGNED_BYTE, 0, colorBuffer);

        if (tessellate) {
            gl.glDrawArrays(GL.GL_TRIANGLES, 0, vertexBuffer.capacity() / 2);
        } else {
            contourLengthBuffer.rewind();
            contourStartBuffer.rewind();
            if (cardSupportsHighEndFeatures) {
                gl.glMultiDrawArrays(GL.GL_POLYGON, contourStartBuffer,
                        contourLengthBuffer, contourLengthBuffer.capacity());
            } else {
                while (contourLengthBuffer.hasRemaining()) {
                    gl.glDrawArrays(GL.GL_POLYGON, contourStartBuffer.get(),
                            contourLengthBuffer.get());
                }
            }
        }

        if (fillPattern != null) {
            gl.glDisable(GL.GL_POLYGON_STIPPLE);
        }
    }

    public synchronized void dispose() {
        polygons = new ArrayList<FloatBuffer[]>();
        vertexBuffer = null;
        polygonLengthBuffer = null;
        contourLengthBuffer = null;
        contourStartBuffer = null;
        colorBuffer = null;
    }

    public void setFillPattern(byte[] fillPattern) {
        this.fillPattern = fillPattern;
    }

    private class Tessellator implements GLUtessellatorCallback {

        public void begin(int arg0) {

        }

        public void beginData(int arg0, Object arg1) {
            // Not necessary for type of tesselation
        }

        public void combine(double[] coordinates, Object[] v, float[] arg2,
                Object[] out) {
            double[] vertex = new double[3];
            vertex[0] = coordinates[0];
            vertex[1] = coordinates[1];
            vertex[2] = coordinates[2];
            out[0] = vertex;
        }

        public void combineData(double[] arg0, Object[] arg1, float[] arg2,
                Object[] arg3, Object arg4) {
            // Not necessary for type of tesselation

        }

        public void edgeFlag(boolean arg0) {
            // No operation, but needed to force GL_TRIANGLES
        }

        public void edgeFlagData(boolean arg0, Object arg1) {

        }

        public void end() {

        }

        public void endData(Object arg0) {
            // Not necessary for type of tesselation
        }

        public void error(int arg0) {
            System.err.println("Tess Error: " + arg0);
        }

        public void errorData(int arg0, Object arg1) {
            // Not necessary for type of tesselation

        }

        public void vertex(Object data) {
            if (data instanceof double[]) {
                if (vertexBuffer.remaining() < 2) {
                    FloatBuffer oldVertexBuffer = vertexBuffer;
                    vertexBuffer = FloatBuffer.allocate(oldVertexBuffer
                            .capacity() * 2);
                    int position = oldVertexBuffer.position();
                    oldVertexBuffer.rewind();
                    vertexBuffer.put(oldVertexBuffer);
                    vertexBuffer.position(position);
                }
                double[] coord = (double[]) data;
                vertexBuffer.put((float) coord[0]);
                vertexBuffer.put((float) coord[1]);
            }
        }

        public void vertexData(Object arg0, Object arg1) {
            // Not necessary for type of tesselation
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#isMutable()
     */
    @Override
    public boolean isMutable() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#isDrawable()
     */
    @Override
    public boolean isDrawable() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#reset()
     */
    @Override
    public void reset() {
        dispose();
    }
}
