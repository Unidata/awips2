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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.core.graphing.Activator;
import com.raytheon.viz.pointdata.PointIconFactory;

/**
 * XYImageData for wind barbs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2007            njensen     Initial creation
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
            BufferedImage iconImage = iconFactory.getIcon(iconValue);
            image = target.initializeRaster(new IODataPreparer(iconImage,
                    "icon", 0), null);
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
