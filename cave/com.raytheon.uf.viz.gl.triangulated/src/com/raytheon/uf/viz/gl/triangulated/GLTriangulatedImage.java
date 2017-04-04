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

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;

import javax.measure.unit.Unit;
import com.jogamp.opengl.GL2;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;
import com.raytheon.uf.viz.drawables.triangulated.TriangleMath;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLBufferColorMapData;
import com.raytheon.viz.core.gl.dataformat.GLColorMapDataFormatFactory;
import com.raytheon.viz.core.gl.ext.imaging.AbstractGLImagingExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.raytheon.viz.core.gl.glsl.GLSLFactory;
import com.raytheon.viz.core.gl.glsl.GLSLStructFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.GLCMTextureData;
import com.raytheon.viz.core.gl.objects.GLTextureObject;
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
 * ------------- -------- --------- ---------------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * Dec 04, 2015  5146     bsteffen  Rewind attrib buffer before use.
 * May 19, 2016  5146     bsteffen  Add debug code for drawing triangles.
 * Sep 13, 2016           mjames@ucar jogamp refactor for osx
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GLTriangulatedImage implements ITriangulatedImage {

    private static final boolean SHOW_MESH_LINES = AbstractGLImagingExtension.SHOW_MESH_LINES;

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
                if (TriangleMath.isInTriangle(p, p0, p1, p2)) {
                    p0.z = attribBuffer.getValue(i0, 0).doubleValue();
                    p1.z = attribBuffer.getValue(i1, 0).doubleValue();
                    p2.z = attribBuffer.getValue(i2, 0).doubleValue();
                    return Triangle.interpolateZ(p, p0, p1, p2);
                }
            }
        }

        return Double.NaN;
    }

    @Override
    public void dispose() {
        if (dataMapping != null) {
            dataMapping.dispose();
            dataMapping = null;
        }
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
        GL2 gl = target.getGl();

        target.pushGLState();
        try {
            gl.glPolygonMode(GL2.GL_BACK, GL2.GL_FILL);
            gl.glPolygonMode(GL2.GL_FRONT, GL2.GL_FILL);

            gl.glEnable(GL2.GL_BLEND);
            gl.glBlendFunc(GL2.GL_SRC_ALPHA, GL2.GL_ONE_MINUS_SRC_ALPHA);

            GLTextureObject cmapTexture = target
                    .getColorMapTexture(colorMapParameters);

            gl.glActiveTexture(GL2.GL_TEXTURE1);
            cmapTexture.bind(gl, GL2.GL_TEXTURE_1D);

            if (colorMapParameters.isInterpolate()) {
                gl.glTexParameteri(GL2.GL_TEXTURE_1D, GL2.GL_TEXTURE_MIN_FILTER,
                        GL2.GL_LINEAR);
                gl.glTexParameteri(GL2.GL_TEXTURE_1D, GL2.GL_TEXTURE_MAG_FILTER,
                        GL2.GL_LINEAR);
            } else {
                gl.glTexParameteri(GL2.GL_TEXTURE_1D, GL2.GL_TEXTURE_MIN_FILTER,
                        GL2.GL_NEAREST);
                gl.glTexParameteri(GL2.GL_TEXTURE_1D, GL2.GL_TEXTURE_MAG_FILTER,
                        GL2.GL_NEAREST);
            }

            setupDataMapping(gl, GL2.GL_TEXTURE3, GL2.GL_TEXTURE4);

            GLShaderProgram program = GLSLFactory.getInstance()
                    .getShaderProgram(gl, "copyValueVertexShader",
                            "colormapVarying");
            program.startShader();

            int numMappingValues = 0;
            if (dataMapping != null && dataMapping.isValid()) {
                numMappingValues = dataMapping.getNumMappingValues();
            }
            GLSLStructFactory.createDataMapping(program, "dataMapping", 3, 4,
                    numMappingValues);

            GLSLStructFactory.createColorMapping(program, "colorMapping", 1, 2,
                    colorMapParameters);

            GLSLStructFactory.createColorModifiers(program, "modifiers",
                    paintProps.getAlpha(), brightness, contrast);

            gl.glEnableClientState(GL2.GL_VERTEX_ARRAY);
            vertexBuffer.rewind();
            gl.glVertexPointer(2, GL2.GL_FLOAT, 0, vertexBuffer);

            Buffer attribBufferData = attribBuffer.getData();
            attribBufferData.rewind();
            program.setVertexAttributeData("attrib_value",
                    attribBuffer.getTextureType(), attribBufferData);

            indexBuffer.rewind();
            gl.glDrawElements(GL2.GL_TRIANGLES, indexBuffer.capacity(),
                    GL2.GL_UNSIGNED_INT, indexBuffer);

            program.endShader();
            if (SHOW_MESH_LINES) {
                /*
                 * This block can be enabled to see the triangles rendered as
                 * lines on top of the image for debugging.
                 */
                indexBuffer.rewind();
                gl.glDisable(GL2.GL_BLEND);
                gl.glPolygonMode(GL2.GL_FRONT_AND_BACK, GL2.GL_LINE);
                gl.glDrawElements(GL2.GL_TRIANGLES, indexBuffer.capacity(),
                        GL2.GL_UNSIGNED_INT, indexBuffer);
                gl.glPolygonMode(GL2.GL_FRONT_AND_BACK, GL2.GL_FILL);
                gl.glEnable(GL2.GL_BLEND);
            }
            gl.glDisableClientState(GL2.GL_VERTEX_ARRAY);

            gl.glActiveTexture(GL2.GL_TEXTURE1);
            gl.glBindTexture(GL2.GL_TEXTURE_1D, 0);

            gl.glDisable(GL2.GL_BLEND);
        } finally {
            target.popGLState();
        }

    }

    protected void setupDataMapping(GL2 gl,
            int dataMappedTexBinding, int colorMappedTexBinding)
            throws VizException {
        if (dataMapping == null && colorMapParameters.getColorMap() != null) {
            Unit<?> colorMapUnit = colorMapParameters.getColorMapUnit();
            Unit<?> dataUnit = attribBuffer.getDataUnit();
            int colorMapSize = colorMapParameters.getColorMap().getSize();
            float colorMapMin = colorMapParameters.getColorMapMin();
            float colorMapMax = colorMapParameters.getColorMapMax();
            dataMapping = GLDataMappingFactory.constructGLDataMapping(gl,
                    dataUnit, colorMapUnit, colorMapMin, colorMapMax,
                    colorMapSize);
        }

        if (dataMapping != null && dataMapping.isValid()) {
            GLCMTextureData glDataMapping = dataMapping.getDataMapping();
            gl.glActiveTexture(dataMappedTexBinding);
            if (glDataMapping.isLoaded() == false) {
                glDataMapping.loadTexture(gl);
            }
            if (glDataMapping.isLoaded()) {
                gl.glBindTexture(glDataMapping.getTextureStorageType(),
                        glDataMapping.getTexId());
            }

            GLCMTextureData glColorMapping = dataMapping.getColorMapping();
            gl.glActiveTexture(colorMappedTexBinding);
            if (glColorMapping.isLoaded() == false) {
                glColorMapping.loadTexture(gl);
            }
            if (glColorMapping.isLoaded()) {
                gl.glBindTexture(glColorMapping.getTextureStorageType(),
                        glColorMapping.getTexId());
            }
        }
    }

}
