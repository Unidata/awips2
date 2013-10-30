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

import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthImageMgr.GraphSection;

/**
 * Utilization header image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2013   2430     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class UtilizationHeaderImage extends AbstractCanvasImage {
    /** Title text */
    private final String TITLE = "Percent of Bandwidth Used";

    /** Legend text */
    private final String LEGEND = "Legend: ";

    /** Percent sign */
    private final String PERCENT = "%";

    /** Map of Rectangles -> GraphSection */
    private final Map<Rectangle, GraphSection> rectPercentMap = new HashMap<Rectangle, GraphSection>();

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param imageMgr
     *            The image manager
     */
    public UtilizationHeaderImage(Composite parentComp, CanvasSettings cs,
            BandwidthImageMgr imageMgr) {
        super(parentComp, cs, null, imageMgr);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
    }

    @Override
    public void disposeResources() {
        // No-op

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

        String titleStr = TITLE + " (" + imageMgr.getNetwork().name() + ")";

        Point extent = gc.stringExtent(titleStr);
        int yCoord = 5;
        int fontHeight = extent.y;

        int xCoord = cs.getImageWidth() / 2 - extent.x;
        gc.drawText(titleStr, xCoord, yCoord, true);

        int legendSpace = 7;
        Point legendPt = gc.stringExtent(LEGEND);
        xCoord = cs.getXSpaceBuffer() + legendSpace;
        yCoord = cs.getCanvasHeight() - fontHeight - 3;
        gc.drawText(LEGEND, xCoord, yCoord, true);
        xCoord += legendPt.x + legendSpace;

        Color c;
        Rectangle r;
        int[] thresholdValues = imageMgr.getBandwidthThreholdValues();
        for (GraphSection section : imageMgr.getPercentageColorMap().keySet()) {
            StringBuilder percentString = new StringBuilder("> ");
            if (section == GraphSection.MIDDLE) {
                percentString.append(thresholdValues[0]).append(PERCENT);
            } else if (section == GraphSection.UPPER) {
                percentString.append(thresholdValues[1]).append(PERCENT);
            } else {
                percentString.append("0").append(PERCENT);
            }
            Point p = gc.stringExtent(percentString.toString());
            gc.drawText(percentString.toString(), xCoord, yCoord, true);
            c = new Color(display, imageMgr.getPercentageColorMap()
                    .get(section));
            gc.setBackground(c);
            xCoord += p.x + 3;
            r = new Rectangle(xCoord, yCoord + 4, 10, 10);
            gc.fillRectangle(r);
            gc.drawRectangle(r);
            xCoord += 10 + legendSpace * 3;
            c.dispose();
            rectPercentMap.put(r, section);
        }

        gc.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.bandwidth.ui.AbstractCanvasImage#
     * performAction(org.eclipse.swt.graphics.Point)
     */
    @Override
    public void performAction(Point mousePt) {
        for (Rectangle rec : this.rectPercentMap.keySet()) {
            if (rec.contains(mousePt)) {
                ColorDialog colorDlg = new ColorDialog(display.getActiveShell());

                // Set the selected color in the dialog from
                // user's selected color
                colorDlg.setRGB(imageMgr.getPercentColor(rectPercentMap
                        .get(rec)));

                // Change the title bar text
                colorDlg.setText("Select a Color");

                // Open the dialog and retrieve the selected color
                RGB rgb = colorDlg.open();

                if (rgb != null) {
                    imageMgr.setPercentColor(rectPercentMap.get(rec), rgb);
                }
                break;
            }
        }
    }
}
