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
package com.raytheon.uf.viz.gl.triangulated;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImageExtension;
import com.raytheon.viz.core.gl.IGLTarget;

/**
 * {@link ITriangulatedImageExtension} implementation for use by
 * {@link IGLTarget}s. For implementation details see
 * {@link GLTriangulatedImage}.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GLTriangulatedImageExtension extends GraphicsExtension<IGLTarget>
        implements ITriangulatedImageExtension {

    @Override
    public ITriangulatedImage initializeImage(
            ColorMapParameters colorMapParameters,
            ITriangleLocationCallback locations,
            IColorMapDataRetrievalCallback dataCallback) throws VizException {
        return new GLTriangulatedImage(colorMapParameters, locations,
                dataCallback);
    }

    @Override
    public void drawImage(PaintProperties paintProps, ITriangulatedImage image)
            throws VizException {
        if(image instanceof GLTriangulatedImage){
            ((GLTriangulatedImage) image).paint(target, paintProps);
        }else{
            throw new VizException("Must pass GL image: "
                    + image.getClass().getSimpleName());
        }
    }

    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;

    }

}
