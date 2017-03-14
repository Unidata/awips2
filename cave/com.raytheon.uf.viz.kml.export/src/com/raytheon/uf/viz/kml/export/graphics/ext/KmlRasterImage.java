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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Basically a wrapper around a RenderedImageCallback but also has logic for
 * filling in transparent parts of plots.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlRasterImage extends KmlImage {

    protected final IRenderedImageCallback callback;

    /**
     * @param imageCallback
     */
    public KmlRasterImage(IRenderedImageCallback imageCallback) {
        this.callback = imageCallback;
    }

    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return KmlRasterImageExtension.class;
    }

    /**
     * get the image for a ground overlay
     * 
     * @return
     * @throws VizException
     */
    public RenderedImage getImage() throws VizException {
        return callback.getImage();
    }

    /**
     * get the image for a plot
     * 
     * @param target
     * @return
     * @throws VizException
     */
    public RenderedImage getImage(RGB backcolor) throws VizException {
        RenderedImage ri = getImage();

        BufferedImage bi = cloneRenderedImage(ri);
        applyFill(bi, backcolor);
        return bi;
    }

    protected BufferedImage cloneRenderedImage(RenderedImage ri)
            throws VizException {
        BufferedImage bi = new BufferedImage(ri.getWidth(), ri.getHeight(),
                BufferedImage.TYPE_INT_ARGB);
        if (ri instanceof Image) {
            bi.getGraphics().drawImage((Image) ri, 0, 0, null);
        } else {
            throw new VizException("Cannot handle image of type "
                    + ri.getClass().getSimpleName());
        }
        return bi;
    }

    protected void applyFill(BufferedImage bi, RGB backcolor) {
        int backint = (0xFF << 24) | (backcolor.red << 16)
                | (backcolor.green << 8) | backcolor.blue;
        for (int i = 0; i < bi.getWidth(); i++) {
            for (int j = 0; j < bi.getHeight(); j++) {
                int argb = bi.getRGB(i, j);
                int alphaint = ((argb >> 24) & 0xFF);
                if (alphaint == 0) {
                    argb = backint;
                } else if (alphaint != 255) {
                    // blend the colors.
                    double alpha = alphaint / 255.0;
                    int red = (argb >> 16) & 0xFF;
                    int green = (argb >> 8) & 0xFF;
                    int blue = (argb >> 0) & 0xFF;
                    red = (int) (red * alpha + backcolor.red * (1.0 - alpha));
                    green = (int) (green * alpha + backcolor.green
                            * (1.0 - alpha));
                    blue = (int) (blue * alpha + backcolor.blue * (1.0 - alpha));
                    argb = (0xFF << 24) | (red << 16) | (green << 8) | blue;
                }
                bi.setRGB(i, j, argb);
            }
        }
    }

}
