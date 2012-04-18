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
package com.raytheon.viz.core.gl.glsl;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.SingleColorImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLColormappedImage;

/**
 * Shader loader for rasters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 25, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLSLRasterProgramLoader extends AbstractShaderLoader {

    /*
     * (non-Javadoc
     * 
     * @see
     * com.raytheon.viz.core.gl.glsl.IShaderLoader#loadData(com.raytheon.viz
     * .core.gl.IGLTarget, com.raytheon.viz.core.gl.glsl.GLShaderProgram,
     * com.raytheon.uf.viz.core.drawables.IImage,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void loadData(IGLTarget target, GLShaderProgram program,
            IImage iimage, PaintProperties paintProps) throws VizException {
        // Check for single color image
        if (iimage instanceof SingleColorImage) {
            program.setUniform("doSingleColor", 1);
            program.setUniform("singleColor",
                    ((SingleColorImage) iimage).getColor());
            iimage = ((SingleColorImage) iimage).getWrappedImage();
        } else {
            program.setUniform("doSingleColor", 0);
        }

        // Get image as AbstractGLImage
        AbstractGLImage image = null;
        if (iimage instanceof AbstractGLImage == false) {
            throw new VizException(
                    "Cannot apply glsl raster shader to non gl image");
        }
        image = (AbstractGLImage) iimage;

        program.setUniform("brightness", image.getBrightness());
        program.setUniform("contrast", image.getContrast());

        int texId = image instanceof GLColormappedImage ? ((GLColormappedImage) image)
                .getTextureid() : -1;
        if (texId > -1) {
            program.setUniform("doColorMap", 1);
        } else {
            program.setUniform("doColorMap", 0);
        }

        float naturalMin = 0;
        float naturalMax = 0;

        float cmapMin = 0;
        float cmapMax = 0;

        if (texId > 0) {
            GLColormappedImage colormappedImg = (GLColormappedImage) image;
            ColorMapParameters colorMapParameters = colormappedImg
                    .getColorMapParameters();

            naturalMin = colorMapParameters.getDataMin();
            naturalMax = colorMapParameters.getDataMax();
            cmapMin = colorMapParameters.getColorMapMin();
            cmapMax = colorMapParameters.getColorMapMax();

            program.setUniform("colorMapSz", colorMapParameters.getColorMap()
                    .getSize());
            int textureType = ((GLColormappedImage) image).getTextureType();
            program.setUniform("isFloat", textureType == GL.GL_FLOAT
                    || textureType == GL.GL_HALF_FLOAT_ARB ? 1 : 0);
            program.setUniform("logarithmic",
                    colorMapParameters.isLogarithmic() ? 1 : 0);
            program.setUniform("logFactor", colorMapParameters.getLogFactor());
            program.setUniform("mirror", colorMapParameters.isMirror() ? 1 : 0);

            program.setUniform("applyMask", colorMapParameters.isUseMask() ? 1
                    : 0);
        }

        program.setUniform("naturalMin", naturalMin);
        program.setUniform("naturalMax", naturalMax);
        program.setUniform("cmapMin", cmapMin);
        program.setUniform("cmapMax", cmapMax);

        program.setUniform("alphaMask", 2);
        program.setUniform("colorMap", 1);
        program.setUniform("rawText", 0);

        program.setUniform("alphaVal", paintProps.getAlpha());
    }
}
