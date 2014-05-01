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
package com.raytheon.viz.core.gl.images;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.viz.core.gl.dataformat.GLColorMapData;

/**
 * Colormappable image that is writeable to in GL/GLSL but is not back by a
 * Buffer and therefore not preinitialized or inspectable
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2013       2333 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLOffscreenColormappedImage extends AbstractGLColormappedImage {

    /**
     * @param data
     * @param params
     * @param extensionClass
     */
    public GLOffscreenColormappedImage(GLColorMapData data,
            ColorMapParameters params,
            Class<? extends IImagingExtension> extensionClass) {
        super(new GLCMTextureData(data), params, extensionClass);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColormappedImage#getValue(int,
     * int)
     */
    @Override
    public double getValue(int x, int y) {
        // TODO: Read value off of graphics card?
        return Double.NaN;
    }

    @Override
    public Unit<?> getDataUnit() {
        return getColorMapParameters().getColorMapUnit();
    }

}
