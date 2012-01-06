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
package com.raytheon.viz.core.gl.images;

import javax.media.opengl.GL;
import javax.media.opengl.GLContext;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.internal.GLTarget;

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
 * Aug 2, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractGLImage implements IImage {

    /** The GL graphics target */
    protected IGLTarget theTarget;

    /** The brightness of the image */
    private float brightness = 1.0f;

    /** The contrast of the image */
    private float contrast = 1.0f;

    /** The status of the image */
    private Status theStatus = Status.UNLOADED;;

    /** Should interpolation be used */
    protected boolean isInterpolated;

    /** The exception that caused failure */
    protected Throwable throwable;

    // Used for offscreen rendering
    private int fbo = -1;

    // Used for offscreen rendering
    private int rbuf = -1;

    protected AbstractGLImage(IGLTarget target) {
        this.theTarget = target;
    }

    /**
     * @return the brightness
     */
    public float getBrightness() {
        return brightness;
    }

    /**
     * @param brightness
     *            the brightness to set
     */
    public void setBrightness(float brightness) {
        this.brightness = brightness;
    }

    /**
     * @return the contrast
     */
    public float getContrast() {
        return contrast;
    }

    /**
     * @param contrast
     *            the contrast to set
     */
    public void setContrast(float contrast) {
        this.contrast = contrast;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.drawables.IImage#target(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    public void target(IGraphicsTarget target) throws VizException {
        theTarget = (GLTarget) target;
        GLContext ctx = theTarget.getContext();
        // TextureLoaderJob.getInstance().requestLoadIntoTexture(this, ctx);
        this.loadTexture(ctx);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IImage#getStatus()
     */
    public Status getStatus() {
        return theStatus;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IImage#setStatus(com.raytheon.viz.core
     * .drawables.Image.Status)
     */
    public void setStatus(Status status) {
        theStatus = status;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IImage#isInterpolated()
     */
    public boolean isInterpolated() {
        return isInterpolated;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IImage#setInterpolated(boolean)
     */
    public void setInterpolated(boolean isInterpolated) {
        this.isInterpolated = isInterpolated;
    }

    public void dispose() {
        if (fbo > 0 || rbuf > 0) {
            final int[] bufs = new int[] { -1, -1 };
            if (fbo > 0) {
                bufs[0] = fbo;
                fbo = -1;
            }
            if (rbuf > 0) {
                bufs[1] = rbuf;
                rbuf = -1;
            }

            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    theTarget.makeContextCurrent();
                    if (bufs[0] > 0) {
                        theTarget.getGl().glDeleteFramebuffersEXT(1,
                                new int[] { bufs[0] }, 0);
                    }
                    if (bufs[1] > 0) {
                        theTarget.getGl().glDeleteRenderbuffersEXT(1,
                                new int[] { bufs[1] }, 0);
                    }
                }
            });
        }
    }

    public void usaAsFrameBuffer() throws VizException {
        GL gl = theTarget.getGl();
        if (fbo != -1 && rbuf != -1) {
            gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, fbo);
            gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
            gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
            return;
        }
        gl.glBindTexture(getTextureStorageType(), 0);

        int[] ids = new int[1];
        gl.glGenFramebuffersEXT(1, ids, 0);
        fbo = ids[0];
        gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, fbo);

        // Generate and bind a render buffer for the depth component
        gl.glGenRenderbuffersEXT(1, ids, 0);
        rbuf = ids[0];
        gl.glBindRenderbufferEXT(GL.GL_RENDERBUFFER_EXT, rbuf);
        gl.glRenderbufferStorageEXT(GL.GL_RENDERBUFFER_EXT,
                GL.GL_DEPTH_COMPONENT, getWidth(), getHeight());
        gl.glBindRenderbufferEXT(GL.GL_RENDERBUFFER_EXT, 0);

        // Attach render buffer to depth of fbo
        gl.glFramebufferRenderbufferEXT(GL.GL_FRAMEBUFFER_EXT,
                GL.GL_DEPTH_ATTACHMENT_EXT, GL.GL_RENDERBUFFER_EXT, rbuf);

        // Attach texture to color attachement on fbo
        gl.glFramebufferTexture2DEXT(GL.GL_FRAMEBUFFER_EXT,
                GL.GL_COLOR_ATTACHMENT0_EXT, getTextureStorageType(),
                getTextureid(), 0);
        String errorMessage = null;

        switch (gl.glCheckFramebufferStatusEXT(GL.GL_FRAMEBUFFER_EXT)) {
        case GL.GL_FRAMEBUFFER_COMPLETE_EXT: {
            // Everything is ok.
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: {
            errorMessage = "Error: Framebuffer incomplete, fbo attachement is NOT complete";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: {
            errorMessage = "Error: Framebuffer incomplete, no image is attached to FBO";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: {
            errorMessage = "Error: Framebuffer incomplete, attached images have different dimensions";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: {
            errorMessage = "Error: Framebuffer incomplete, color attached images have different internal formats";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: {
            errorMessage = "Error: Framebuffer incomplete, draw buffer";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: {
            errorMessage = "Error: Framebuffer incomplete, read buffer";
            break;
        }
        case GL.GL_FRAMEBUFFER_UNSUPPORTED_EXT: {
            errorMessage = "Error: Framebuffer not supported by hardware/drivers";
            break;
        }
        }

        // use the window buffer
        if (errorMessage != null) {
            gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, 0);
            if (fbo != -1) {
                gl.glDeleteFramebuffersEXT(1, new int[] { fbo }, 0);
                fbo = -1;
            }
            if (rbuf != -1) {
                gl.glDeleteRenderbuffersEXT(1, new int[] { rbuf }, 0);
                rbuf = -1;
            }
            throw new VizException(errorMessage);
        }
    }

    public abstract int getTextureid();

    public abstract int getTextureStorageType();

    public abstract void stageTexture() throws VizException;

    public abstract void loadTexture(GLContext ctx) throws VizException;

}
