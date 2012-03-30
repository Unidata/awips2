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
package com.raytheon.viz.radar.rsc.mosaic;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Radar mosaic renderer factory, creates mosaic renderers
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

public class RadarMosaicRendererFactory {

    public static enum MosaicType {
        MergeRaster, MaxValue;
    }

    public static interface IRadarMosaicRenderer {

        public void mosaic(IGraphicsTarget target, PaintProperties paintProps,
                RadarMosaicResource mosaicToRender) throws VizException;

        public void dispose();

    }

    public static IRadarMosaicRenderer createNewRenderer(MosaicType mosaicType)
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

}
