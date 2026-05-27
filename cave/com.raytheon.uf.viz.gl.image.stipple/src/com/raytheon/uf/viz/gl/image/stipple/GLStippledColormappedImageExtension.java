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
package com.raytheon.uf.viz.gl.image.stipple;

import java.util.Arrays;
import java.util.List;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.image.stipple.IStippledColormappedImageExtension;
import com.raytheon.viz.core.gl.GLCapabilities;
import com.raytheon.viz.core.gl.IGLTarget;

/**
 * GL based implementation of {@link IStippledColormappedImageExtension}.
 * 
 * This implementation simply uses the default
 * {@link IColormappedImageExtension} to render the image multiple times, once
 * for each pattern. For each pattern the GL polygon stipple is set and the
 * alpha mask in the {@link ColorMapParameters} is adjusted so that only a
 * specific range of values is rendered using the provided stipple.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 01, 2016  5957     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class GLStippledColormappedImageExtension
        extends GraphicsExtension<IGLTarget>
        implements IStippledColormappedImageExtension {

    @Override
    public GLStippledColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        return new GLStippledColormappedImage(dataCallback, colorMapParameters,
                GLStippledColormappedImageExtension.class);
    }

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {

        IColormappedImageExtension cmapExt = target
                .getExtension(IColormappedImageExtension.class);
        boolean result = true;
        GL gl = target.getGl();
        for (DrawableImage image : images) {
            GLStippledColormappedImage stippled = (GLStippledColormappedImage) image
                    .getImage();
            List<byte[]> patterns = stippled.getFillPatterns();
            if (patterns == null || patterns.isEmpty()) {
                result &= cmapExt.drawRasters(paintProps, images);
            } else {
                /*
                 * All of the complexity here is to handle if the existing alpha
                 * mask is a different size then the number of patterns.
                 */
                ColorMapParameters params = stippled.getColorMapParameters();
                /* Must be saved so it can be reset after rendering */
                byte[] originalAlpha = params.getAlphaMask();
                boolean originalUseMask = params.isUseMask();
                /*
                 * Representation of originalAlpha that is guaranteed to be the
                 * same size as alphaMask(so it may actually contained rescaled
                 * version of originalAlpha).
                 */
                byte[] alphaSource = originalAlpha;
                int alphaSize = patterns.size();
                if (originalAlpha == null || originalUseMask == false) {
                    /*
                     * source is visible everywhere(0), so create an array full
                     * of that.
                     */
                    alphaSource = new byte[alphaSize];
                } else if (alphaSize != originalAlpha.length) {

                    int maxTextureSize = GLCapabilities
                            .getInstance(gl).maxTextureSize;

                    /*
                     * For perfect representation we need the LCM of the
                     * original alpha size and the number of patterns. If the
                     * LCM is greater than max texture size then it isn't
                     * possible to achieve perfection so just try for good
                     * enough.
                     */
                    int a = alphaSize;
                    int b = originalAlpha.length;
                    while (a != b && a < maxTextureSize) {
                        if (a < b) {
                            a += alphaSize;
                        } else {
                            b += originalAlpha.length;
                        }
                    }
                    alphaSize = Math.min(a, maxTextureSize);

                    /* Expand original Alpha into alpha source. */
                    alphaSource = new byte[alphaSize];
                    for (int i = 0; i < alphaSize; i += 1) {
                        alphaSource[i] = originalAlpha[i * originalAlpha.length
                                / alphaSize];
                    }
                }
                byte[] alphaMask = new byte[alphaSize];
                Arrays.fill(alphaMask, (byte) 1);
                for (int i = 0; i < patterns.size(); i += 1) {
                    /*
                     * Copy the alpha bits that are using this pattern so alpha
                     * matches the source.
                     */
                    int alphaStart = i * alphaSize / patterns.size();
                    int alphaEnd = (i + 1) * alphaSize / patterns.size();
                    for (int a = alphaStart; a < alphaEnd; a += 1) {
                        alphaMask[a] = alphaSource[a];
                    }
                    byte[] pattern = patterns.get(i);
                    if (pattern != null) {
                        gl.glEnable(GL.GL_POLYGON_STIPPLE);
                        gl.glPolygonStipple(pattern, 0);
                    }
                    params.setAlphaMask(alphaMask);
                    params.setUseMask(true);
                    result &= cmapExt.drawRasters(paintProps, images);
                    if (pattern != null) {
                        gl.glDisable(GL.GL_POLYGON_STIPPLE);
                    }
                    /* Reset the alpha for the next loop. */
                    for (int a = alphaStart; a < alphaEnd; a += 1) {
                        alphaMask[a] = 1;
                    }
                }
                params.setAlphaMask(originalAlpha);
                params.setUseMask(originalUseMask);
            }
        }

        return result;
    }

    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
