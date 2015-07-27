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
package com.raytheon.viz.core.graphing.xy;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.display.PointIconFactory;

/**
 * XYImageData for wind barbs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2007            njensen     Initial creation
 * Aug 11, 2014 #3504      mapeters    Replaced deprecated IODataPreparer
 *                                     instances with IRenderedImageCallback.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
public class XYIconImageData extends XYImageData {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(XYIconImageData.class);

    public static final int IMAGE_SIZE = 80;

    private int iconValue;

    public XYIconImageData(Object ax, Object ay, int iconValue) {
        super(ax, ay);
        this.iconValue = iconValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.xy.XYImageData#generateImage()
     */
    @Override
    protected void generateImage() {
        try {
            PointIconFactory iconFactory = new PointIconFactory(color,
                    XYWindImageData.IMAGE_SIZE);
            final BufferedImage iconImage = iconFactory.getIcon(iconValue);
            image = target.initializeRaster(new IRenderedImageCallback() {
                @Override
                public RenderedImage getImage() throws VizException {
                    return iconImage;
                }
            });
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error generating icon: " + e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.xy.XYImageData#getDefaultSize()
     */
    @Override
    public int[] getDefaultSize() {
        return new int[] { IMAGE_SIZE, IMAGE_SIZE };
    }

}
