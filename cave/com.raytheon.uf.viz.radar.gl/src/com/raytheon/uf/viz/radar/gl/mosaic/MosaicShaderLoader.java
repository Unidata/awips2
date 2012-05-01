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
package com.raytheon.uf.viz.radar.gl.mosaic;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.glsl.AbstractShaderLoader;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLImage;

/**
 * Loads variables for mosiac fragment shader
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MosaicShaderLoader extends AbstractShaderLoader {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.glsl.IShaderLoader#loadData(com.raytheon.viz
     * .core.gl.IGLTarget, com.raytheon.viz.core.gl.glsl.GLShaderProgram,
     * com.raytheon.uf.viz.core.drawables.IImage,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void loadData(IGLTarget target, GLShaderProgram program,
            IImage image, PaintProperties paintProps) throws VizException {
        // load radar data to GL_TEXTURE0 (bound in drawRaster)
        program.setUniform("radarData", 0);

        GL gl = target.getGl();

        // grab currently writting to texture
        AbstractGLImage writeTo = (AbstractGLImage) RadarMosaicRenderer
                .getCurrentMosaicImage();

        // activate on texture2 as 0 is radar image and 1 is colormap
        gl.glActiveTexture(GL.GL_TEXTURE2);
        gl.glBindTexture(writeTo.getTextureStorageType(),
                writeTo.getTextureid());

        program.setUniform("mosaicTexture", 2);

        // pass in width and height
        program.setUniform("height", (paintProps.getCanvasBounds().height));
        program.setUniform("width", (paintProps.getCanvasBounds().width));
    }

}
