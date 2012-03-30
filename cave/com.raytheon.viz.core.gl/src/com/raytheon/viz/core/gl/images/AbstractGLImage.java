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
import javax.media.opengl.glu.GLU;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.GLContextBridge;
import com.raytheon.viz.core.gl.objects.GLFrameBufferObject;
import com.raytheon.viz.core.gl.objects.GLRenderBuffer;
import com.sun.opengl.util.texture.TextureCoords;

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
    private GLFrameBufferObject fbo;

    // Used for offscreen rendering
    private GLRenderBuffer rbuf;

    private Class<? extends IImagingExtension> extensionClass;

    protected AbstractGLImage(Class<? extends IImagingExtension> extensionClass) {
        this.extensionClass = extensionClass;
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
        // TextureLoaderJob.getInstance().requestLoadIntoTexture(this, ctx);
        GLContextBridge.makeMasterContextCurrent();
        this.loadTexture(GLU.getCurrentGL());
        GLContextBridge.releaseMasterContext();
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
        if (fbo != null) {
            fbo.dispose();
            fbo = null;
        }
        if (rbuf != null) {
            rbuf.dispose();
            rbuf = null;
        }
    }

    public void usaAsFrameBuffer() throws VizException {
        GL gl = GLU.getCurrentGL();
        if (fbo != null && fbo.isValid()) {
            fbo.bind(gl);
            gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
            if (rbuf != null && rbuf.isValid()) {
                gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
            } else {
                gl.glClear(GL.GL_COLOR_BUFFER_BIT);
            }
            return;
        }
        gl = GLU.getCurrentGL();

        gl.glBindTexture(getTextureStorageType(), 0);

        fbo = new GLFrameBufferObject(this);
        fbo.bind(gl);

        if (gl.glIsEnabled(GL.GL_DEPTH_TEST)) {
            // Generate and bind a render buffer for the depth component
            rbuf = new GLRenderBuffer(this);
            rbuf.bind(gl);
            rbuf.createStorage(gl, GL.GL_DEPTH_COMPONENT, getWidth(),
                    getHeight());
            gl.glBindRenderbufferEXT(GL.GL_RENDERBUFFER_EXT, 0);

            // Attach render buffer to depth of fbo
            gl.glFramebufferRenderbufferEXT(GL.GL_FRAMEBUFFER_EXT,
                    GL.GL_DEPTH_ATTACHMENT_EXT, GL.GL_RENDERBUFFER_EXT,
                    rbuf.getId());
        }
        // Attach texture to color attachement on fbo
        gl.glFramebufferTexture2DEXT(GL.GL_FRAMEBUFFER_EXT,
                GL.GL_COLOR_ATTACHMENT0_EXT, getTextureStorageType(),
                getTextureid(), 0);
        String errorMessage = fbo.checkStatus(gl);

        // use the window buffer
        if (errorMessage != null) {
            gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, 0);
            if (fbo != null) {
                fbo.dispose();
                fbo = null;
            }
            if (rbuf != null) {
                rbuf.dispose();
                rbuf = null;
            }
            throw new VizException(errorMessage);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getExtensionClass()
     */
    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return extensionClass;
    }

    /**
     * Binds the texture. Default implementation takes the textureId and binds
     * to GL_TEXTURE0. Use bind(int) for more control
     * 
     * @return
     */
    public boolean bind(GL gl) {
        return bind(gl, GL.GL_TEXTURE0);
    }

    public boolean bind(GL gl, int texture) {
        int texId = getTextureid();
        if (texId > 0) {
            int textureType = getTextureStorageType();
            gl.glActiveTexture(texture);
            gl.glBindTexture(textureType, texId);

            // Apply interpolation
            if (isInterpolated()) {
                gl.glTexParameteri(textureType, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_LINEAR);
                gl.glTexParameteri(textureType, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_LINEAR);
            } else {
                gl.glTexParameteri(textureType, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_NEAREST);
                gl.glTexParameteri(textureType, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_NEAREST);
            }
            return true;
        }
        return false;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#stage()
     */
    @Override
    public final void stage() throws VizException {
        Status status = getStatus();
        if (status != Status.LOADED && status != Status.STAGED) {
            setStatus(Status.LOADING);
            if (stageTexture()) {
                setStatus(Status.STAGED);
            } else {
                setStatus(Status.FAILED);
            }
        }
    }

    public abstract TextureCoords getTextureCoords();

    public abstract int getTextureid();

    public abstract int getTextureStorageType();

    public abstract boolean stageTexture() throws VizException;

    public abstract void loadTexture(GL gl) throws VizException;

}
