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
package com.raytheon.uf.viz.core.image;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Images for use in Viz Code. These are small circular images.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2013    1655    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatusImages {

    /**
     * Status Image enum.
     */
    public enum StatusImage {
        RED(SWT.COLOR_RED), YELLOW(SWT.COLOR_YELLOW), GREEN(SWT.COLOR_GREEN), UNKNOWN(
                SWT.COLOR_GRAY);

        private final int colorId;

        StatusImage(int id) {
            colorId = id;
        }

        public int getColorId() {
            return colorId;
        }
    }

    /** Array of images */
    private final Map<StatusImage, Image> statusImages = new HashMap<StatusImage, Image>();

    private final Shell shell;

    /**
     * Constructor.
     * 
     * @param shell
     *            The shell these images are associated with.
     */
    public StatusImages(Shell shell) {
        this.shell = shell;
        this.shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (StatusImage si : statusImages.keySet()) {
                    statusImages.get(si).dispose();
                }
            }
        });
    }

    /**
     * Get a status image.
     * 
     * @param image
     *            The image to get
     * @param width
     *            image width
     * @param height
     *            image height
     * @return The image
     */
    private Image getStatusImage(StatusImage image, int width, int height) {
        Image img = statusImages.get(image);
        if (img == null) {
            img = new Image(Display.getCurrent(), width, height);
            GC gc = new GC(img);
            drawCircleImage(gc, width, height, image.getColorId());
            gc.dispose();
            statusImages.put(image, img);
        }

        return img;
    }

    /**
     * Get a status image 12 x 12 in size.
     * 
     * @param image
     *            The image to get
     * @return The image
     */
    public Image getStatusImage(StatusImage image) {
        return getStatusImage(image, 12, 12);
    }

    /**
     * Draw the circle image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param colorId
     *            Color to paint the image.
     */
    private void drawCircleImage(GC gc, int imgWidth, int imgHeight, int colorId) {
        gc.setAntialias(SWT.ON);

        Display disp = Display.getCurrent();

        gc.setBackground(disp.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(disp.getSystemColor(colorId));
        gc.fillOval(0, 0, imgWidth - 1, imgHeight - 1);

        gc.setBackground(disp.getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(0, 0, imgWidth - 1, imgHeight - 1);
    }
}
