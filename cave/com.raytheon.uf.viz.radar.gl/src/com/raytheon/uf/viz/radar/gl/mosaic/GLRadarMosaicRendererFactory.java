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

import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.radar.rsc.mosaic.MergeRasterRadarMosaicRenderer;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicRendererFactoryExtension;

/**
 * Radar mosaic renderer factory for IGLTarget
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLRadarMosaicRendererFactory extends GraphicsExtension<IGLTarget>
        implements IRadarMosaicRendererFactoryExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicRendererFactoryExtension
     * #createNewRenderer()
     */
    @Override
    public IRadarMosaicRenderer createNewRenderer(MosaicType mosaicType)
            throws VizException {
        switch (mosaicType) {
        case MergeRaster:
            return new MergeRasterRadarMosaicRenderer();
        case MaxValue:
            return new RadarMosaicRenderer();
        }
        throw new VizException("Could not find mosaic renderer for type = "
                + mosaicType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE.value;
    }

}
