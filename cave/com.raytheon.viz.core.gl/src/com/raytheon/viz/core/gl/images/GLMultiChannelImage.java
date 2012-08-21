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

import java.util.Map;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.Channel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.IImageChannel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.IMultiChannelImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.internal.ext.GLMultiChannelImageExtension;
import com.sun.opengl.util.texture.TextureCoords;

/**
 * GL implementation of a multi channel image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLMultiChannelImage extends AbstractGLImage implements
        IMultiChannelImage {

    private Map<Channel, IImageChannel> channelMap;

    /**
     * @param target
     * @param extensionClass
     */
    public GLMultiChannelImage(IGLTarget target,
            Map<Channel, IImageChannel> channelMap) {
        super(GLMultiChannelImageExtension.class);
        this.channelMap = channelMap;
    }

    /**
     * Get the first IColormappedImage in the map
     * 
     * @return first image in the map or null if none
     */
    private IColormappedImage getFirstImage() {
        if (channelMap != null) {
            for (IColormappedImage image : channelMap.values()) {
                if (image != null) {
                    return image;
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.AbstractGLImage#setInterpolated(boolean)
     */
    @Override
    public void setInterpolated(boolean isInterpolated) {
        super.setInterpolated(isInterpolated);
        for (IColormappedImage image : channelMap.values()) {
            ((AbstractGLImage) image).setInterpolated(isInterpolated);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getWidth()
     */
    @Override
    public int getWidth() {
        IColormappedImage image = getFirstImage();
        if (image != null) {
            return image.getWidth();
        }
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getHeight()
     */
    @Override
    public int getHeight() {
        IColormappedImage image = getFirstImage();
        if (image != null) {
            return image.getHeight();
        }
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension
     * .IMultiChannelImage#getImageMapping()
     */
    @Override
    public Map<Channel, IImageChannel> getImageMapping() {
        return channelMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension
     * .IMultiChannelImage#setImageMapping(java.util.Map)
     */
    @Override
    public void setImageMapping(Map<Channel, IImageChannel> mapping) {
        this.channelMap = mapping;
        // Set ourselves as unloaded since we are updating the mapping
        setStatus(Status.UNLOADED);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureCoords()
     */
    @Override
    public TextureCoords getTextureCoords() {
        return new TextureCoords(0, 1, 1, 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureid()
     */
    @Override
    public int getTextureid() {
        // We do not have a single textureid specifically
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureStorageType()
     */
    @Override
    public int getTextureStorageType() {
        AbstractGLImage image = (AbstractGLImage) getFirstImage();
        return image.getTextureStorageType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#stage()
     */
    @Override
    public boolean stageTexture() throws VizException {
        boolean rval = false;
        if (channelMap.size() > 0) {
            rval = true;
            for (IColormappedImage image : channelMap.values()) {
                AbstractGLImage glImage = (AbstractGLImage) image;
                glImage.stage();
                if (glImage.getStatus() == Status.FAILED) {
                    rval = false;
                }
            }
        }
        return rval;
    }

    @Override
    public void loadTexture(GL gl) throws VizException {
        if (channelMap.size() > 0) {
            Status status = Status.LOADED;
            for (IColormappedImage image : channelMap.values()) {
                AbstractGLImage glImage = (AbstractGLImage) image;
                glImage.loadTexture(gl);
                if (glImage.getStatus() == Status.FAILED) {
                    status = Status.FAILED;
                }
            }
            setStatus(status);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#dispose()
     */
    @Override
    public void dispose() {
        for (IColormappedImage image : channelMap.values()) {
            ((AbstractGLImage) image).dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#bind()
     */
    @Override
    public boolean bind(GL gl) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#bind(int)
     */
    @Override
    public boolean bind(GL gl, int texture) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#usaAsFrameBuffer()
     */
    @Override
    public void usaAsFrameBuffer() throws VizException {
        throw new VizException(
                "Using a GLMultiChannelImage as a frame buffer is not supported");
    }
}
