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
package com.raytheon.uf.viz.gl.triangulated;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLBufferColorMapData;
import com.raytheon.viz.core.gl.dataformat.GLColorMapDataFormatFactory;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.raytheon.viz.core.gl.glsl.GLSLFactory;
import com.raytheon.viz.core.gl.glsl.GLSLStructFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.objects.GLTextureObject;
import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Triangle;

/**
 * Implementation of {@link ITriangulatedImage}. Renders the triangles using
 * vertex and attribute arrays to pass the coordinates and values to custom
 * shader which performs the colormapping.
 * 
 * IMPLEMENTATON NOTES:
 * 
 * The current implementation is not particularly fast at rendering or sampling,
 * and it is considered a compromise between the two.
 * 
 * Faster rendering could be accomplished by copying the data to graphics memory
 * using a Vertex Buffer Object(VBO). This would have the downside that sampling
 * the data would require copying the data off the graphics card, since sampling
 * is a brute force search, all data would need to be copied.
 * 
 * Faster sampling could be achieved by imposing more structure on the data. The
 * simplest approach would be dividing the triangles into distinct areas and
 * limiting the sampling to only areas that intersect the point of interest.
 * This could be implemented by creating separate arrays for each area but that
 * would increase memory usage and slow rendering.
 * 
 * If real world usage demonstrates reuse of location data then caching needs to
 * be introduced so that there is no need to request the data multiple times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GLTriangulatedImage implements ITriangulatedImage {

    protected final ITriangleLocationCallback locations;

    protected final IColorMapDataRetrievalCallback dataCallback;

    protected ColorMapParameters colorMapParameters;

    private GLDataMapping dataMapping;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private transient FloatBuffer vertexBuffer;

    private transient IntBuffer indexBuffer;

    private transient GLBufferColorMapData attribBuffer;

    public GLTriangulatedImage(ColorMapParameters colorMapParameters,
            ITriangleLocationCallback locations,
            IColorMapDataRetrievalCallback dataCallback) {
        this.colorMapParameters = colorMapParameters;
        this.locations = locations;
        this.dataCallback = dataCallback;
    }

    @Override
    public void setBrightness(float brightness) {
        this.brightness = brightness;
    }

    @Override
    public void setContrast(float contrast) {
        this.contrast = contrast;
    }

    @Override
    public double getDataValue(double x, double y) {
        if (indexBuffer == null) {
            return Double.NaN;
        }
        IntBuffer indexBuffer = this.indexBuffer.duplicate();
        indexBuffer.rewind();
        while (indexBuffer.hasRemaining()) {
            int i0 = indexBuffer.get();
            float tx = vertexBuffer.get(i0 * 2);
            float ty = vertexBuffer.get(i0 * 2 + 1);
            Coordinate p0 = new Coordinate(tx, ty);
            int i1 = indexBuffer.get();
            tx = vertexBuffer.get(i1 * 2);
            ty = vertexBuffer.get(i1 * 2 + 1);
            Coordinate p1 = new Coordinate(tx, ty);
            int i2 = indexBuffer.get();
            tx = vertexBuffer.get(i2 * 2);
            ty = vertexBuffer.get(i2 * 2 + 1);
            Coordinate p2 = new Coordinate(tx, ty);
            Envelope triEnv = new Envelope(p0);
            triEnv.expandToInclude(p1);
            triEnv.expandToInclude(p2);
            if (triEnv.contains(x, y)) {
                Coordinate p = new Coordinate(x, y);
                if (isInTriangle(p, p0, p1, p2)) {
                    p0.z = attribBuffer.getValue(i0, 0).doubleValue();
                    p1.z = attribBuffer.getValue(i1, 0).doubleValue();
                    p2.z = attribBuffer.getValue(i2, 0).doubleValue();
                    return Triangle.interpolateZ(p, p0, p1, p2);
                }
            }
        }

        return Double.NaN;
    }

    protected boolean isInTriangle(Coordinate p, Coordinate v0, Coordinate v1,
            Coordinate v2) {
        int orientation0 = CGAlgorithms.orientationIndex(v0, v1, p);
        if (orientation0 == 0) {
            return true;
        }
        int orientation1 = CGAlgorithms.orientationIndex(v1, v2, p);
        if (orientation0 != orientation1) {
            if (orientation1 == 0) {
                return true;
            } else {
                return false;
            }
        }
        int orientation2 = CGAlgorithms.orientationIndex(v2, v0, p);
        if (orientation0 != orientation2) {
            if (orientation1 == 0) {
                return true;
            } else {
                return false;
            }
        }
        return true;
    }

    @Override
    public void dispose() {
        dataMapping.dispose();

    }

    @Override
    public void stage() throws VizException {
        double[][] coords = locations.getCoordinates();

        vertexBuffer = ByteBuffer.allocateDirect(coords.length * 8)
                .order(ByteOrder.nativeOrder()).asFloatBuffer();
        for (double[] coord : coords) {
            vertexBuffer.put((float) coord[0]);
            vertexBuffer.put((float) coord[1]);
        }
        int[] indices = locations.getTriangleIndices();
        indexBuffer = ByteBuffer.allocateDirect(indices.length * 4)
                .order(ByteOrder.nativeOrder()).asIntBuffer().put(indices);
        ColorMapData data = dataCallback.getColorMapData();
        AbstractGLColorMapDataFormat glFormat = GLColorMapDataFormatFactory
                .getGLColorMapDataFormat(data);
        attribBuffer = new GLBufferColorMapData(data, glFormat);

    }

    public void paint(IGLTarget target, PaintProperties paintProps)
            throws VizException {
        if (this.indexBuffer == null) {
            stage();
        }
        GL gl = target.getGl();

        target.pushGLState();
        try {
            gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
            gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);

            GLTextureObject cmapTexture = target
                    .getColorMapTexture(colorMapParameters);

            gl.glActiveTexture(GL.GL_TEXTURE1);
            cmapTexture.bind(gl, GL.GL_TEXTURE_1D);

            if (colorMapParameters.isInterpolate()) {
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_LINEAR);
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_LINEAR);
            } else {
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_NEAREST);
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_NEAREST);
            }
            GLShaderProgram program = GLSLFactory.getInstance()
                    .getShaderProgram(gl, "copyValueVertexShader",
                            "colormapVarying");
            program.startShader();

            int numMappingValues = 0;
            GLSLStructFactory.createDataMapping(program, "dataMapping", 3, 4,
                    numMappingValues);

            GLSLStructFactory.createColorMapping(program, "colorMapping", 1, 2,
                    colorMapParameters);

            GLSLStructFactory.createColorModifiers(program, "modifiers",
                    paintProps.getAlpha(), brightness, contrast);

            gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
            vertexBuffer.rewind();
            gl.glVertexPointer(2, GL.GL_FLOAT, 0, vertexBuffer);

            program.setVertexAttributeData("attrib_value",
                    attribBuffer.getTextureType(), attribBuffer.getData());

            indexBuffer.rewind();
            gl.glDrawElements(GL.GL_TRIANGLES, indexBuffer.capacity(),
                    GL.GL_UNSIGNED_INT, indexBuffer);

            program.endShader();
            gl.glDisableClientState(GL.GL_VERTEX_ARRAY);

            gl.glActiveTexture(GL.GL_TEXTURE1);
            gl.glBindTexture(GL.GL_TEXTURE_1D, 0);
        } finally {
            target.popGLState();
        }

    }

}
