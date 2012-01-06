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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.GLCapabilities;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.images.GLColormappedImage;
import com.raytheon.viz.core.gl.internal.GLTarget;

/**
 * GL {@link IColormappedImageExtension} implementation, creates
 * {@link GLColormappedImage} objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLColormappedImageExtension extends GraphicsExtension<IGLTarget>
        implements IColormappedImageExtension {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GLColormappedImageExtension.class);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IColormappedImageExtension#
     * initializeRaster
     * (com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        GLColormappedImage image = new GLColormappedImage(dataCallback,
                colorMapParameters, target);
        try {
            image.stageTexture();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error staging texture", e);
        }
        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        if (GLCapabilities.getInstance(target.getGl()).cardSupportsShaders
                && GLTarget.FORCE_NO_SHADER == false) {
            return Compatibilty.ENHANCED_TARGET_COMPATIBLE.value;
        } else {
            return Compatibilty.INCOMPATIBLE.value;
        }
    }

}
