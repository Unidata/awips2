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
package com.raytheon.uf.viz.remote.graphics.events.imagery;

import java.awt.image.RenderedImage;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class CreateSingleColorImage extends UpdateSingleColorImage {

    @DynamicSerializeElement
    private RenderedImageWrapper wrapper = new RenderedImageWrapper();

    /**
     * @return the wrapper
     */
    public RenderedImageWrapper getWrapper() {
        return wrapper;
    }

    /**
     * @param wrapper
     *            the wrapper to set
     */
    public void setWrapper(RenderedImageWrapper wrapper) {
        this.wrapper = wrapper;
    }

    /**
     * @return the renderedImage
     */
    public RenderedImage getRenderedImage() {
        return wrapper.getWrappedImage();
    }

    /**
     * @param renderedImage
     *            the renderedImage to set
     */
    public void setRenderedImage(RenderedImage renderedImage) {
        wrapper.setWrappedImage(renderedImage);
    }
}
