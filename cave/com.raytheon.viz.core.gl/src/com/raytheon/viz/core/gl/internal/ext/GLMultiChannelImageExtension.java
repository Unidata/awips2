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
package com.raytheon.viz.core.gl.internal.ext;

import java.util.Map;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.GLImageChannel;
import com.raytheon.viz.core.gl.images.GLMultiChannelImage;

/**
 * GL implementation of {@link IMultiChannelImageExtension}
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

public class GLMultiChannelImageExtension extends AbstractGLSLImagingExtension
        implements IMultiChannelImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension
     * #constructImage(java.util.Map)
     */
    @Override
    public IMultiChannelImage constructImage(
            Map<Channel, IImageChannel> imageMapping) throws VizException {
        return new GLMultiChannelImage(target, imageMapping);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension
     * #
     * constructImage(com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback
     * , com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IImageChannel constructImage(
            IColorMapDataRetrievalCallback callback, ChannelData channelData)
            throws VizException {
        return new GLImageChannel(target.getExtension(
                GLColormappedImageExtension.class).initializeRaster(callback,
                channelData.parameters), channelData);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#getShaderProgramName
     * ()
     */
    @Override
    public String getShaderProgramName() {
        return "multichannel";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#loadShaderData
     * (com.raytheon.viz.core.gl.glsl.GLShaderProgram,
     * com.raytheon.uf.viz.core.drawables.IImage,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void loadShaderData(GLShaderProgram program, IImage image,
            PaintProperties paintProps) throws VizException {
        GLMultiChannelImage glImage = null;
        if (image instanceof GLMultiChannelImage == false) {
            throw new VizException(
                    "Cannot apply glsl multichannel shader to non gl multichannel image");
        }
        glImage = (GLMultiChannelImage) image;
        Map<Channel, IImageChannel> channelMap = glImage.getImageMapping();

        program.setUniform("brightness", glImage.getBrightness());
        program.setUniform("contrast", glImage.getContrast());
        program.setUniform("alpha", paintProps.getAlpha());

        for (Channel c : Channel.values()) {
            String suffix = "";
            int texture = -1;
            int texNum = -1;
            switch (c) {
            case RED:
                suffix = "R";
                texture = GL.GL_TEXTURE0;
                texNum = 0;
                break;
            case GREEN:
                suffix = "G";
                texture = GL.GL_TEXTURE1;
                texNum = 1;
                break;
            case BLUE:
                suffix = "B";
                texture = GL.GL_TEXTURE2;
                texNum = 2;
                break;
            }

            boolean applied = false;
            IColormappedImage ci = channelMap.get(c);
            if (ci != null) {
                if (ci instanceof GLImageChannel == false) {
                    throw new VizException(
                            "Cannot apply glsl multichannel shader to non gl colormapped image");
                }
                GLImageChannel glCMImage = (GLImageChannel) ci;
                if (glCMImage.bind(target.getGl(), texture)) {
                    int textureType = glCMImage.getWrappedImage()
                            .getTextureType();

                    ColorMapParameters colorMapParameters = ci
                            .getColorMapParameters();

                    program.setUniform("rawTex" + suffix, texNum);
                    program.setUniform("naturalMin" + suffix,
                            colorMapParameters.getDataMin());
                    program.setUniform("naturalMax" + suffix,
                            colorMapParameters.getDataMax());
                    program.setUniform("cmapMin" + suffix,
                            colorMapParameters.getColorMapMin());
                    program.setUniform("cmapMax" + suffix,
                            colorMapParameters.getColorMapMax());
                    program.setUniform("isFloat" + suffix,
                            textureType == GL.GL_FLOAT
                                    || textureType == GL.GL_HALF_FLOAT_ARB ? 1
                                    : 0);
                    program.setUniform("invert" + suffix,
                            glCMImage.isInverted());
                    applied = true;
                }
            }
            program.setUniform("apply" + suffix, applied ? 1 : 0);
        }
    }

}
