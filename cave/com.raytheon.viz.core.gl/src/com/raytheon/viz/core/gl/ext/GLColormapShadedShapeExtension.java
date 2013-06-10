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
package com.raytheon.viz.core.gl.ext;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.media.opengl.GL;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.internal.GLShadedShape;
import com.raytheon.viz.core.gl.internal.GLShadedShapeBase;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLColormapShadedShapeExtension extends
        GraphicsExtension<IGLTarget> implements IColormapShadedShapeExtension {

    @Override
    public IColormapShadedShape createColormapShadedShape(
            GeneralGridGeometry targetGeometry, boolean tesselate) {
        return new GLColormapShadedShape(targetGeometry, tesselate);
    }

    @Override
    public IShadedShape createShadedShape(IColormapShadedShape baseShape,
            Map<Object, RGB> colors) {
        if (baseShape instanceof GLColormapShadedShape) {
            GLColormapShadedShape glBaseShape = (GLColormapShadedShape) baseShape;
            glBaseShape.compile();
            GLShadedShape newShape = new GLShadedShape(glBaseShape);
            List<RGB> colorList = new ArrayList<RGB>(glBaseShape.getColorKeys()
                    .size());
            for (Object colorKey : glBaseShape.getColorKeys()) {
                colorList.add(colors.get(colorKey));
            }
            newShape.color(colorList);
            return newShape;
        }
        String clazz = "null";
        if (baseShape != null) {
            clazz = baseShape.getClass().getSimpleName();
        }

        throw new IllegalArgumentException(
                "IColormapShadedShape must be of type GLColormapShadedShape but instead was "
                        + clazz);
    }

    @Override
    public void drawColormapShadedShape(IColormapShadedShape shape,
            Map<Object, RGB> colors, float alpha, float brightness)
            throws VizException {
        if (shape instanceof GLColormapShadedShape) {
            GLColormapShadedShape glBaseShape = (GLColormapShadedShape) shape;
            GL gl = target.getGl();
            gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
            gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
            gl.glEnable(GL.GL_BLEND);
            gl.glBlendColor(1.0f, 1.0f, 1.0f, alpha);
            gl.glBlendFunc(GL.GL_CONSTANT_ALPHA, GL.GL_ONE_MINUS_CONSTANT_ALPHA);

            glBaseShape.paint(gl, colors, brightness, alpha);

            gl.glDisableClientState(GL.GL_VERTEX_ARRAY);
            gl.glDisable(GL.GL_BLEND);
            gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
            return;
        }
        String clazz = "null";
        if (shape != null) {
            clazz = shape.getClass().getSimpleName();
        }

        throw new IllegalArgumentException(
                "IColormapShadedShape must be of type GLColormapShadedShape but instead was "
                        + clazz);

    }

    private static class GLColormapShadedShape extends GLShadedShapeBase
            implements IColormapShadedShape {

        private List<Object> colorKeys = new ArrayList<Object>();

        public GLColormapShadedShape(GeneralGridGeometry targetGeometry,
                boolean tessellate) {
            super(targetGeometry, tessellate);
        }

        @Override
        public void addPolygon(LineString[] lineString, Object colorKey) {
            colorKeys.add(colorKey);
            addPolygon(lineString);
        }

        @Override
        public void addPolygonPixelSpace(LineString[] contours, Object colorKey) {
            colorKeys.add(colorKey);
            addPolygonPixelSpace(contours);
        }

        @Override
        public void addGeometry(Geometry geometry, Object colorKey) {
            if (geometry instanceof GeometryCollection) {
                GeometryCollection geomCollection = (GeometryCollection) geometry;
                for (int i = 0; i < geomCollection.getNumGeometries(); i++) {
                    addGeometry(geomCollection.getGeometryN(i), colorKey);
                }
            } else if (geometry instanceof LineString) {
                LineString[] lineStrings = { (LineString) geometry };
                addPolygon(lineStrings, colorKey);
            } else if (geometry instanceof Polygon) {
                LineString[] lineStrings = { ((Polygon) geometry)
                        .getExteriorRing() };
                addPolygon(lineStrings, colorKey);
            }
        }

        protected synchronized void paint(GL gl, Map<Object, RGB> colors,
                float brightness, float alpha) {
            if (!polygons.isEmpty()) {
                compile();
            }
            if (this.vertexBuffer == null) {
                return;
            }
            if (this.vertexBuffer.capacity() == 0) {
                return;
            }

            vertexBuffer.rewind();

            if (fillPattern != null) {
                gl.glEnable(GL.GL_POLYGON_STIPPLE);

                gl.glPolygonStipple(fillPattern, 0);
            }

            gl.glVertexPointer(2, GL.GL_FLOAT, 0, vertexBuffer);
            if (tessellate) {
                int colorKeysIndex = 0;
                int start = 0;
                polygonLengthBuffer.rewind();
                while (polygonLengthBuffer.hasRemaining()) {
                    int length = polygonLengthBuffer.get();
                    Object colorKey = colorKeys.get(colorKeysIndex++);
                    setColorToPaint(gl, colors.get(colorKey), brightness);
                    gl.glDrawArrays(GL.GL_TRIANGLES, start, length);
                    start += length;
                }
            } else {
                int colorKeysIndex = 0;
                contourLengthBuffer.rewind();
                contourStartBuffer.rewind();
                while (contourLengthBuffer.hasRemaining()) {
                    Object colorKey = colorKeys.get(colorKeysIndex++);
                    setColorToPaint(gl, colors.get(colorKey), brightness);
                    gl.glDrawArrays(GL.GL_POLYGON, contourStartBuffer.get(),
                            contourLengthBuffer.get());
                }
            }

            if (fillPattern != null) {
                gl.glDisable(GL.GL_POLYGON_STIPPLE);
            }

        }

        private void setColorToPaint(GL gl, RGB color, float brightness) {
            float red = 0.0f;
            float green = 0.0f;
            float blue = 0.0f;
            if (color != null) {
                red = color.red / 255.0f;
                green = color.green / 255.0f;
                blue = color.blue / 255.0f;
            }
            if (brightness < 1.0f) {
                red *= brightness;
                green *= brightness;
                blue *= brightness;
            }
            gl.glColor3f(red, green, blue);
        }

        @Override
        public void dispose() {
            super.dispose();
        }

        public List<Object> getColorKeys() {
            return colorKeys;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IGraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
