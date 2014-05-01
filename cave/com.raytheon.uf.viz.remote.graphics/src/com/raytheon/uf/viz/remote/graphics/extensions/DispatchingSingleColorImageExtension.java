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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage.DispatchingRenderedImageCallback;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingSingleColorImage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingSingleColorImageExtension extends
        AbstractDispatchingImageExtension implements ISingleColorImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension#
     * constructImage(java.awt.image.RenderedImage,
     * org.eclipse.swt.graphics.RGB)
     */
    @Override
    public ISingleColorImage constructImage(IRenderedImageCallback callback,
            RGB color) {
        try {
            DispatchingRenderedImageCallback wrapper = new DispatchingRenderedImageCallback(
                    callback);
            return new DispatchingSingleColorImage(target.getWrappedObject()
                    .getExtension(ISingleColorImageExtension.class)
                    .constructImage(wrapper, color),
                    DispatchingSingleColorImageExtension.class,
                    target.getDispatcher(), color, wrapper);
        } catch (VizException e) {
            throw new RuntimeException(
                    "Error constring dispatching single color image", e);
        }
    }

}
