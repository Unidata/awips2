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

import com.raytheon.viz.core.gl.GLCapabilities;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.ext.imaging.AbstractGLImagingExtension;
import com.raytheon.viz.core.gl.internal.GLTarget;

/**
 * Abstract GL Extension that requires shader to work properly
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

public abstract class AbstractGLSLImagingExtension extends
        AbstractGLImagingExtension {

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
            return Compatibilty.ENHANCED_TARGET_COMPATIBLE;
        } else {
            return Compatibilty.INCOMPATIBLE;
        }
    }

}
