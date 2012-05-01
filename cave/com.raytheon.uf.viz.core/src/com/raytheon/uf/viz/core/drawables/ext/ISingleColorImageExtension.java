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
package com.raytheon.uf.viz.core.drawables.ext;

import java.awt.image.RenderedImage;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Extension for creating images that should be mapped to a single color
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

public interface ISingleColorImageExtension extends IImagingExtension {

    public static interface ISingleColorImage extends IImage {

        public void setColor(RGB color);

    }

    /**
     * Construct an ISingleColorImage image that maps the image passed into to
     * the single color value
     * 
     * @param image
     * @param color
     * @return
     * @throws VizException
     */
    public ISingleColorImage constructImage(RenderedImage image, RGB color);

}
