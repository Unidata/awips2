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
package com.raytheon.uf.viz.remote.graphics.extensions;

import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingColormappedShadedShapeExtension extends
        GraphicsExtension<DispatchGraphicsTarget> implements
        IColormapShadedShapeExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension
     * #
     * createColormapShadedShape(org.geotools.coverage.grid.GeneralGridGeometry,
     * boolean)
     */
    @Override
    public IColormapShadedShape createColormapShadedShape(
            GeneralGridGeometry targetGeometry, boolean tesselate) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension
     * #createShadedShape(com.raytheon.uf.viz.core.drawables.ext.colormap.
     * IColormapShadedShapeExtension.IColormapShadedShape, java.util.Map)
     */
    @Override
    public IShadedShape createShadedShape(IColormapShadedShape baseShape,
            Map<Object, RGB> colors) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension
     * #drawColormapShadedShape(com.raytheon.uf.viz.core.drawables.ext.colormap.
     * IColormapShadedShapeExtension.IColormapShadedShape, java.util.Map, float,
     * float)
     */
    @Override
    public void drawColormapShadedShape(IColormapShadedShape shape,
            Map<Object, RGB> colors, float alpha, float brightness)
            throws VizException {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(DispatchGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
