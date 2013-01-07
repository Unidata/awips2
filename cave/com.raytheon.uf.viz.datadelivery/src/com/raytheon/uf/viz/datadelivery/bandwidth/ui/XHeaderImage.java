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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.SubscriptionPriority;

/**
 * Header image for X axis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012   1269     lvenable    Initial creation.
 * Dec 13, 2012   1269     lvenable    Fixes and updates.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class XHeaderImage extends AbstractCanvasImage {
    /** Graph title */
    private final String xHeaderStr = "Download Time Windows";

    /** Priority constant */
    private final String priorityStr = "Priority: ";

    /** Colon constant */
    private final String colon = ": ";

    /** Live update constant */
    private final String liveUpdate = "Live Update: ";

    /** On constant */
    private final String on = "On";

    /** Off constant */
    private final String off = "Off";

    /** Sort by constant */
    private final String sortBy = "Sort by: ";

    /** Map of rectangles and subscription priorities. */
    private Map<Rectangle, SubscriptionPriority> rectPriMap;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param graphData
     *            The graph data
     * @param imageMgr
     *            The image manager
     */
    public XHeaderImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData, BandwidthImageMgr imageMgr) {
        super(parentComp, cs, graphData, imageMgr);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
        rectPriMap = new HashMap<Rectangle, SubscriptionPriority>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disposeResources() {
        // dispose objects here

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void drawImage() {
        GC gc = new GC(image);
        gc.setAntialias(SWT.ON);

        // Fill the background of the image
        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, cs.getImageWidth(), cs.getImageHeight());

        Point extent = gc.stringExtent(xHeaderStr);
        int yCoord = 5;
        int fontHeight = extent.y;

        int xCoord = cs.getImageWidth() / 2 - extent.x;
        gc.drawText(xHeaderStr, xCoord, yCoord, true);

        int legendSpace = 5;
        Point priorityPt = gc.stringExtent(priorityStr);
        xCoord = cs.getXSpaceBuffer() + 5;
        yCoord = cs.getCanvasHeight() - fontHeight - 3;
        gc.drawText(priorityStr, xCoord, yCoord, true);
        xCoord += priorityPt.x + legendSpace;
        Color c;
        Rectangle r;
        for (SubscriptionPriority priority : SubscriptionPriority.values()) {
            String priorityName = priority.getPriorityName() + colon;
            Point p = gc.stringExtent(priorityName);
            gc.drawText(priorityName, xCoord, yCoord, true);
            c = new Color(display, imageMgr.getPriorityColor(priority));
            gc.setBackground(c);
            xCoord += p.x + 3;
            r = new Rectangle(xCoord, yCoord + 4, 10, 10);
            gc.fillRectangle(r);
            gc.drawRectangle(r);
            xCoord += 10 + legendSpace * 3;
            c.dispose();

            rectPriMap.put(r, priority);
        }

        gc.setBackground(bgColor);
        xCoord += 50;

        if (imageMgr.isLiveUpdate()) {
            gc.drawString(liveUpdate + on, xCoord, yCoord);
        } else {
            gc.drawString(liveUpdate + off, xCoord, yCoord);
        }

        xCoord += 150;

        gc.drawString(sortBy + imageMgr.getSortBy().getSortByString(), xCoord,
                yCoord);

        // Dispose of the graphics context
        gc.dispose();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void performAction(Point mousePt) {
        for (Rectangle rec : rectPriMap.keySet()) {
            if (rec.contains(mousePt)) {
                ColorDialog colorDlg = new ColorDialog(display.getActiveShell());

                // Set the selected color in the dialog from
                // user's selected color
                colorDlg.setRGB(imageMgr.getPriorityColor(rectPriMap.get(rec)));

                // Change the title bar text
                colorDlg.setText("Select a Color");

                // Open the dialog and retrieve the selected color
                RGB rgb = colorDlg.open();

                if (rgb != null) {
                    imageMgr.setPriorityColor(rectPriMap.get(rec), rgb);
                }
                break;
            }
        }
    }
}
