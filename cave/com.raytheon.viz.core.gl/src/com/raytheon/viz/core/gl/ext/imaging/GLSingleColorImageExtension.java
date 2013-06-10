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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.GLSingleColorImage;

/**
 * GL implementation of ISingleColorImageExtension, uses shader to assign color
 * value
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLSingleColorImageExtension extends AbstractGLSLImagingExtension
        implements ISingleColorImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#getShaderProgramName
     * ()
     */
    @Override
    public String getShaderProgramName() {
        return "singleColor";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension#
     * constructImage(com.raytheon.uf.viz.core.drawables.IImage,
     * org.eclipse.swt.graphics.RGB)
     */
    @Override
    public ISingleColorImage constructImage(IRenderedImageCallback callback,
            RGB color) {
        return new GLSingleColorImage(callback, color);
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
    public void loadShaderData(GLShaderProgram program, IImage iimage,
            PaintProperties paintProps) throws VizException {
        // Get image as AbstractGLImage
        GLSingleColorImage image = null;
        if (iimage instanceof GLSingleColorImage == false) {
            throw new VizException(
                    "Cannot apply single color raster shader to non single color image");
        }

        image = (GLSingleColorImage) iimage;

        program.setUniform("brightness", image.getBrightness());
        program.setUniform("contrast", image.getContrast());
        program.setUniform("alpha", paintProps.getAlpha());
        program.setUniform("color", image.getColor());
        program.setUniform("rawTex", 0);
    }

}
