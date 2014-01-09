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
package com.raytheon.viz.core.gl.ext.imaging;

import java.util.HashSet;
import java.util.Set;

import javax.media.opengl.GL;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.AbstractGLMesh;
import com.raytheon.viz.core.gl.GLCapabilities;
import com.raytheon.viz.core.gl.GLGeometryObject2D;
import com.raytheon.viz.core.gl.GLGeometryObject2D.GLGeometryObjectData;
import com.raytheon.viz.core.gl.GLGeometryPainter;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.glsl.GLSLFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.sun.opengl.util.texture.TextureCoords;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract GL Imaging extension class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2011            mschenke    Initial creation
 * Jan  9, 2014 2680       mschenke    Switched simple PixelCoverage mesh 
 *                                     rendering to use VBOs instead of 
 *                                     deprecated immediate mode rendering
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractGLImagingExtension extends
        GraphicsExtension<IGLTarget> implements IImagingExtension {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGLImagingExtension.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.IImagingExtension#drawRasters(
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.core.DrawableImage[])
     */
    @Override
    public final boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        GL gl = target.getGl();
        boolean rval = true;
        gl.glGetError();

        target.pushGLState();
        try {
            gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
            rval = drawRastersInternal(paintProps, images);
            disableBlending(gl);
        } finally {
            target.popGLState();
        }

        target.handleError(gl.glGetError());
        return rval;
    }

    protected boolean drawRastersInternal(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        GL gl = target.getGl();
        GLCapabilities capabilities = GLCapabilities.getInstance(gl);
        // Get shader program extension uses
        String shaderProgram = getShaderProgramName();

        int repaints = 0;
        Set<String> errorMsgs = new HashSet<String>();

        GLShaderProgram program = null;
        boolean attemptedToLoadShader = false;
        int lastTextureType = -1;

        for (DrawableImage di : images) {
            AbstractGLImage glImage = (AbstractGLImage) di.getImage();
            PixelCoverage extent = di.getCoverage();

            synchronized (glImage) {
                if (glImage.getStatus() == Status.STAGED) {
                    glImage.target(target);
                }

                if (glImage.getStatus() != Status.LOADED) {
                    ++repaints;
                    continue;
                }

                int textureType = glImage.getTextureStorageType();

                if (lastTextureType != textureType) {
                    if (lastTextureType != -1) {
                        gl.glDisable(lastTextureType);
                    }
                    gl.glEnable(textureType);
                    lastTextureType = textureType;
                }

                Object dataObj = null;

                if (glImage.bind(gl)) {
                    // Notify extension image is about to be rendered
                    dataObj = preImageRender(paintProps, glImage, extent);

                    if (dataObj == null) {
                        // Skip image if preImageRender returned null
                        continue;
                    }

                    if (shaderProgram != null
                            && capabilities.cardSupportsShaders) {
                        if (program == null && !attemptedToLoadShader) {
                            attemptedToLoadShader = true;
                            program = GLSLFactory.getInstance()
                                    .getShaderProgram(gl, null, shaderProgram);
                            if (program != null) {
                                program.startShader();
                            }

                            enableBlending(gl);
                            gl.glColor4f(0.0f, 0.0f, 0.0f,
                                    paintProps.getAlpha());
                        }

                        if (program != null) {
                            loadShaderData(program, glImage, paintProps);
                        }
                    } else {
                        gl.glEnable(GL.GL_BLEND);
                        gl.glBlendFunc(GL.GL_SRC_ALPHA,
                                GL.GL_ONE_MINUS_SRC_ALPHA);
                        gl.glColor4f(1.0f, 1.0f, 1.0f, paintProps.getAlpha());
                    }

                    if (drawCoverage(paintProps, extent,
                            glImage.getTextureCoords(), 0) == PaintStatus.REPAINT) {
                        // Coverage not ready, needs repaint
                        ++repaints;
                    }

                    gl.glActiveTexture(GL.GL_TEXTURE0);
                    gl.glBindTexture(textureType, 0);

                    // Notify extension image has been rendered
                    postImageRender(paintProps, glImage, dataObj);

                    // Enable if you want to see mesh drawn
                    if (false) {
                        if (program != null) {
                            program.endShader();
                        }
                        gl.glDisable(GL.GL_BLEND);
                        gl.glColor3f(0.0f, 1.0f, 0.0f);
                        gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_LINE);
                        drawCoverage(paintProps, extent,
                                glImage.getTextureCoords(), 0);
                        gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
                        gl.glEnable(GL.GL_BLEND);
                        if (program != null) {
                            program.startShader();
                        }
                    }
                } else {
                    errorMsgs.add("Texture did not bind");
                    continue;
                }
            }
            // Needed to fix ATI driver bug, after each image render, flush the
            // GL pipeline to avoid random images being drawn improperly. May be
            // fixed with driver update or cleaning up how image coverages are
            // rendered (getting rid of glEnableClientState as it is deprecated)
            gl.glFlush();
        }

        if (lastTextureType != -1) {
            gl.glDisable(lastTextureType);
        }

        if (program != null) {
            program.endShader();
        }

        if (errorMsgs.size() > 0) {
            throw new VizException("Error rendering " + errorMsgs.size()
                    + " images: " + errorMsgs);
        }

        boolean needsRepaint = repaints > 0;
        if (needsRepaint) {
            target.setNeedsRefresh(true);
        }

        return needsRepaint == false;
    }

    /**
     * Draw an image coverage object
     * 
     * @param paintProps
     * @param pc
     * @param coords
     * @param corrFactor
     * @throws VizException
     */
    protected PaintStatus drawCoverage(PaintProperties paintProps,
            PixelCoverage pc, TextureCoords coords, float corrFactor)
            throws VizException {
        GL gl = target.getGl();
        if (pc == null) {
            return PaintStatus.ERROR;
        }
        // gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
        // gl.glColor3d(1.0, 0.0, 0.0);
        // }

        // boolean useNormals = false;
        IMesh mesh = pc.getMesh();

        // if mesh exists, use it
        if (mesh != null) {
            if (mesh instanceof AbstractGLMesh) {
                return ((AbstractGLMesh) mesh).paint(target, paintProps);
            }
        } else if (coords != null) {

            Coordinate ul = pc.getUl();
            Coordinate ur = pc.getUr();
            Coordinate lr = pc.getLr();
            Coordinate ll = pc.getLl();

            int geometryType = GL.GL_TRIANGLE_STRIP;
            GLGeometryObject2D vertexData = new GLGeometryObject2D(
                    new GLGeometryObjectData(geometryType, GL.GL_VERTEX_ARRAY));
            vertexData.allocate(4);
            vertexData.addSegment(new double[][] { { ll.x, ll.y },
                    { lr.x, lr.y }, { ul.x, ul.y }, { ur.x, ur.y } });
            vertexData.compile(gl);

            GLGeometryObject2D textureData = new GLGeometryObject2D(
                    new GLGeometryObjectData(geometryType,
                            GL.GL_TEXTURE_COORD_ARRAY));
            textureData.allocate(4);
            textureData.addSegment(new double[][] {
                    { coords.left(), coords.bottom() },
                    { coords.right(), coords.bottom() },
                    { coords.left(), coords.top() },
                    { coords.right(), coords.top() } });
            textureData.compile(gl);

            GLGeometryPainter.paintGeometries(gl, vertexData, textureData);

            vertexData.dispose();
            textureData.dispose();

            return PaintStatus.PAINTED;
        }
        return PaintStatus.ERROR;
    }

    /**
     * Function to enable blending for images
     * 
     * @param gl
     */
    protected void enableBlending(GL gl) {
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_ADD);
        gl.glEnable(GL.GL_BLEND);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_BLEND);
        gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
    }

    /**
     * Function to disable blending for images
     * 
     * @param gl
     */
    protected void disableBlending(GL gl) {
        gl.glDisable(GL.GL_BLEND);
    }

    /**
     * Setup anything that is required pre image rendering. Object returned is a
     * state object and will be passed in to postImageRender. If the image
     * should not be drawn, return null
     * 
     * @param paintProps
     * @param image
     * @return
     * @throws VizException
     */
    public Object preImageRender(PaintProperties paintProps,
            AbstractGLImage image, PixelCoverage imageCoverage)
            throws VizException {
        return this;
    }

    /**
     * Post image rendering method, can be used to change any state required.
     * Return object from preImageRender is passed in as data argument
     * 
     * @param paintProps
     * @param image
     * @param data
     * @throws VizException
     */
    public void postImageRender(PaintProperties paintProps,
            AbstractGLImage image, Object data) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    /**
     * The shader program name to execute for this extension
     * 
     * @return
     */
    public abstract String getShaderProgramName();

    /**
     * Populate the shader program with data required for execution
     * 
     * @param program
     * @param image
     * @param paintProps
     * @throws VizException
     */
    public abstract void loadShaderData(GLShaderProgram program, IImage image,
            PaintProperties paintProps) throws VizException;
}
