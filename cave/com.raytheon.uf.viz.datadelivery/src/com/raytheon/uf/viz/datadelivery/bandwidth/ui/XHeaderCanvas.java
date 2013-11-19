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

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;

/**
 * Bandwidth utilization graph X header canvas.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2013   1531     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class XHeaderCanvas extends Canvas {
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

    /** Sort by constant */
    private final String sortBy = "Sort by: ";

    /** Map of rectangles and subscription priorities. */
    private final Map<Rectangle, SubscriptionPriority> rectPriMap;

    /** The background color */
    private final Color bgColor;

    /** Priority color map */
    private final EnumMap<SubscriptionPriority, RGB> priorityColorMap;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite
     * @param style
     *            style bits
     */
    public XHeaderCanvas(Composite parent, int style) {
        super(parent, style);

        bgColor = getDisplay().getSystemColor(SWT.COLOR_WHITE);
        rectPriMap = new HashMap<Rectangle, SubscriptionPriority>();

        priorityColorMap = new EnumMap<SubscriptionPriority, RGB>(
                SubscriptionPriority.class);
        priorityColorMap.put(SubscriptionPriority.LOW, new RGB(6, 122, 255));
        priorityColorMap.put(SubscriptionPriority.NORMAL, new RGB(0, 255, 0));
        priorityColorMap.put(SubscriptionPriority.HIGH, new RGB(255, 0, 0));
    }

    /**
     * Draw the canvas.
     * 
     * @param gc
     *            GC object
     */
    public void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        Point priorityPt = gc.stringExtent(priorityStr);

        Point extent = gc.stringExtent(xHeaderStr);
        int yCoord = 5;
        int fontHeight = extent.y;
        int xCoord = this.getBounds().width / 2 - extent.x / 2 - 15;
        gc.drawText(xHeaderStr, xCoord, yCoord, true);

        int legendSpace = 5;
        yCoord += fontHeight + 5;
        xCoord -= 150;
        gc.drawText(priorityStr, xCoord, yCoord, true);
        xCoord += priorityPt.x + legendSpace;
        Color c;
        Rectangle r;
        for (SubscriptionPriority priority : SubscriptionPriority.values()) {
            String priorityName = priority.getPriorityName() + colon;
            Point p = gc.stringExtent(priorityName);
            gc.drawText(priorityName, xCoord, yCoord, true);
            c = new Color(getDisplay(), this.priorityColorMap.get(priority));
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

        gc.drawString(liveUpdate + on, xCoord, yCoord);

        xCoord += 150;

        gc.drawString(sortBy, xCoord, yCoord);

        // Dispose of the graphics context
        gc.dispose();
    }
}