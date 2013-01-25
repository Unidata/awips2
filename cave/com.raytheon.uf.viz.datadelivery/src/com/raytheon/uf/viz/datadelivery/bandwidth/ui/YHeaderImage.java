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
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012            lvenable    Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class YHeaderImage extends AbstractCanvasImage {
    private final String yHeaderStr = "Subscriptions";

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite
     * @param cs
     *            Canvas settings
     * @param graphData
     *            Graph data
     */
    public YHeaderImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData) {
        super(parentComp, cs, graphData, null);
        bgColor = display.getSystemColor(SWT.COLOR_WHITE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disposeResources() {
        // TODO dispose resources as needed

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void drawImage() {
        GC gc = new GC(image);
        gc.setAntialias(SWT.ON);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, cs.getImageWidth(), cs.getImageHeight());

        // Rotate the Y header text.
        int labelInPixels = gc.getFontMetrics().getAverageCharWidth()
                * yHeaderStr.length();

        int yCoord = (cs.getCanvasHeight() / 2) + (labelInPixels / 2);

        Transform t = new Transform(gc.getDevice());
        t.translate(10, yCoord); // new origin
        t.rotate(-90f);
        gc.setTransform(t);
        gc.drawString(yHeaderStr, 0, 0, true);
        t.dispose();

        // Dispose of the graphics context
        gc.dispose();
    }
}
