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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;

/**
 * Y axis label image.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012   1269     lvenable    Initial creation.
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class YLabelImage extends AbstractCanvasImage {
    /** Map Rectangle -> Subscription name */
    private final Map<Rectangle, String> subNameRectMap = new HashMap<Rectangle, String>();

    /** Constant */
    private final String dots = "...";

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param graphData
     *            Graph Data
     */
    public YLabelImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData, BandwidthImageMgr imageMgr) {
        super(parentComp, cs, graphData, imageMgr);

        init();
    }

    /**
     * Initialize
     */
    private void init() {
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disposeResources() {
        // Dispose resources as needed

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getToolTipText(Point mouseCoord) {
        for (Rectangle rect : subNameRectMap.keySet()) {
            if (rect.contains(mouseCoord)) {
                return subNameRectMap.get(rect);
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void drawImage() {
        Map<Rectangle, String> checkBoxMap = new HashMap<Rectangle, String>();

        int boxWidth = 8;
        int allowedWidth = cs.getCanvasWidth() - 20;

        subNameRectMap.clear();
        GC gc = new GC(image);
        gc.setAntialias(SWT.ON);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, cs.getImageWidth(), cs.getImageHeight());

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        List<String> subscriptionList = getSortedData();

        gc.setLineWidth(1);
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLUE));
        int yCoord = cs.getImageHeight() - TEXT_OFFSET;
        for (String name : subscriptionList) {
            Point ext = gc.textExtent(name);

            subNameRectMap.put(new Rectangle(18, yCoord, ext.x, ext.y), name);

            Rectangle checkbox = new Rectangle(4, yCoord + 4, boxWidth,
                    boxWidth);
            checkBoxMap.put(checkbox, name);

            gc.drawRectangle(checkbox);
            if (this.imageMgr.isChecked(name)) {
                gc.fillRectangle(6, yCoord + 6, 5, 5);
            }

            StringBuilder sb = new StringBuilder(name);
            sb.append(dots);
            if (gc.textExtent(sb.toString()).x > allowedWidth) {
                do {
                    sb = new StringBuilder(
                            sb.substring(0, sb.indexOf(dots) - 1)).append(dots);
                } while (gc.textExtent(sb.toString()).x > allowedWidth);
                gc.drawString(sb.toString(), 18, yCoord, true);
            } else {
                gc.drawString(name, 18, yCoord, true);
            }

            yCoord -= TEXT_OFFSET;
        }

        imageMgr.setCheckBoxMap(checkBoxMap);
    }

    /**
     * Get the collection of Subscription names
     *
     * @return subscription names
     */
    public Collection<String> getSubscriptionNames() {
        return subNameRectMap.values();
    }
}