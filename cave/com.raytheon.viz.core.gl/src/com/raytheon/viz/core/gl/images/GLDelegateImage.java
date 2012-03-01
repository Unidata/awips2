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

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.sun.opengl.util.texture.TextureCoords;

/**
 * GL image that wraps another gl image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLDelegateImage<T extends AbstractGLImage> extends AbstractGLImage {

    protected T image;

    public GLDelegateImage(T image,
            Class<? extends IImagingExtension> extensionClass) {
        super(extensionClass);
        this.image = image;
    }

    public T getWrappedImage() {
        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getWidth()
     */
    @Override
    public int getWidth() {
        return image.getWidth();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getHeight()
     */
    @Override
    public int getHeight() {
        return image.getHeight();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureCoords()
     */
    @Override
    public TextureCoords getTextureCoords() {
        return image.getTextureCoords();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureid()
     */
    @Override
    public int getTextureid() {
        return image.getTextureid();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureStorageType ()
     */
    @Override
    public int getTextureStorageType() {
        return image.getTextureStorageType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#stageTexture()
     */
    @Override
    public boolean stageTexture() throws VizException {
        return image.stageTexture();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#loadTexture(javax
     * .media.opengl.GLContext)
     */
    @Override
    public void loadTexture(GL gl) throws VizException {
        image.loadTexture(gl);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.AbstractGLImage#target(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    public void target(IGraphicsTarget target) throws VizException {
        image.target(target);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getStatus()
     */
    @Override
    public Status getStatus() {
        return image.getStatus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.AbstractGLImage#setStatus(com.raytheon
     * .uf.viz.core.drawables.IImage.Status)
     */
    @Override
    public void setStatus(Status status) {
        image.setStatus(status);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#usaAsFrameBuffer()
     */
    @Override
    public void usaAsFrameBuffer() throws VizException {
        image.usaAsFrameBuffer();
    }

}
