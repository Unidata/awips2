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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;

/**
 * Utilization graph label image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2013   2430         mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class UtilizationLabelImage extends AbstractCanvasImage {

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param imageMgr
     *            BandwidthImageMgr
     */
    public UtilizationLabelImage(Composite parentComp, CanvasSettings cs,
            BandwidthImageMgr imageMgr) {
        super(parentComp, cs, null, null);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
        this.imageMgr = imageMgr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.bandwidth.ui.AbstractCanvasImage#
     * disposeResources()
     */
    @Override
    public void disposeResources() {
        // No-op

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.bandwidth.ui.AbstractCanvasImage#drawImage
     * ()
     */
    @Override
    public void drawImage() {
        GC gc = new GC(image);
        gc.setAntialias(SWT.ON);

        // Fill the background of the image
        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, cs.getImageWidth(), cs.getImageHeight());

        // Get the scale labels/ These are double so the division is floating
        // point division
        int[] threshValues = imageMgr.getBandwidthThreholdValues();
        double lowerValue = threshValues[0];
        double upperValue = threshValues[1];

        // Draw scale labels
        String label1 = lowerValue + "%";
        String label2 = upperValue + "%";

        Point extent1 = gc.stringExtent(label1);
        Point extent2 = gc.stringExtent(label2);

        // Draw the label(s)
        // y is the lower threshold value, y2 is the upper
        double y = cs.getCanvasHeight()
                - ((lowerValue / 100) * cs.getCanvasHeight()) - (extent1.y / 2);
        int x = cs.getCanvasWidth() - extent1.x - 5;

        double y2 = cs.getCanvasHeight()
                - ((upperValue / 100) * cs.getCanvasHeight()) - (extent2.y / 2);
        int x2 = cs.getCanvasWidth() - extent2.x - 5;

        // Align in relation to each other
        if (y2 + extent2.y >= y) {
            y2 = y - extent2.y + 1;
        }

        // If off the top of the canvas then move down
        if (y2 <= 0) {
            y2 = 0;
            if (y <= y2 + extent2.y) {
                // If overlap then move the other
                y = y2 + extent2.y;
            }
        }

        // If off the bottom of the canvas then move up
        if (y + extent1.y > cs.getCanvasHeight()) {
            y = cs.getCanvasHeight() - extent1.y;
            if (y < y2 + extent2.y) {
                // if overlap then move the other
                y2 = y - extent1.y;
            }
        }
        if (y + extent2.y >= 60) {
            y = 60 - extent1.y - 1;
        }

        gc.drawText(label1, x, (int) y, true);
        gc.drawText(label2, x2, (int) y2, true);

        gc.dispose();
    }

    /**
     * @return the lowerValue
     */
    public int getLowerValue() {
        return imageMgr.getBandwidthThreholdValues()[0];
    }

    /**
     * @param lowerValue
     *            the lowerValue to set
     */
    public void setLowerValue(int lowerValue) {
        this.imageMgr.getBandwidthThreholdValues()[0] = lowerValue;
    }

    /**
     * @return the upperValue
     */
    public int getUpperValue() {
        return imageMgr.getBandwidthThreholdValues()[1];
    }

    /**
     * @param upperValue
     *            the upperValue to set
     */
    public void setUpperValue(int upperValue) {
        this.imageMgr.getBandwidthThreholdValues()[1] = upperValue;
    }
}
