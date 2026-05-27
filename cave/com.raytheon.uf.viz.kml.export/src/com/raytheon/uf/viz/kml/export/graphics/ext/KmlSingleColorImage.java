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

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension.ISingleColorImage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Adds logic to change colors to KmlRasterIamge.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlSingleColorImage extends KmlRasterImage implements
        ISingleColorImage {

    private RGB color;

    public KmlSingleColorImage(IRenderedImageCallback callback, RGB color) {
        super(callback);
        this.color = color;
    }

    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return KmlSingleColorImageExtension.class;
    }

    @Override
    public void setColor(RGB color) {
        this.color = color;
    }

    @Override
    public RenderedImage getImage() throws VizException {
        RenderedImage ri = callback.getImage();
        BufferedImage bi = cloneRenderedImage(ri);
        makeSingleColor(bi);
        return bi;
    }

    protected void makeSingleColor(BufferedImage bi) {
        int colorint = (0xFF << 24) | (color.red << 16) | (color.green << 8)
                | color.blue;
        for (int i = 0; i < bi.getWidth(); i++) {
            for (int j = 0; j < bi.getHeight(); j++) {
                int argb = bi.getRGB(i, j);
                // preserve alpha but color becomes color.
                argb = (0xFF000000 & argb) | (0x00FFFFFF & colorint);
                bi.setRGB(i, j, argb);
            }
        }
    }

}