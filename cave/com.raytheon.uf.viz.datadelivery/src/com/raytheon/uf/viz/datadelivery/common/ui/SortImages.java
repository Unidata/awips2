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
package com.raytheon.uf.viz.datadelivery.common.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * Images used when sorting tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class SortImages {

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Parent display.
     */
    private Display parentDisplay;

    /**
     * Array of sort images.
     */
    private Image[] sortImageArray;

    /**
     * Sort direction enumeration that identifies which image to use.
     */
    public enum SortDirection {
        ASCENDING, DESCENDING
    };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public SortImages(Composite parent) {
        this.parent = parent;
        parentDisplay = parent.getDisplay();

        addDisposeToParent();

        createSortImages();
    }

    /**
     * Add a dispose listener to the parent composite. When the parent composite
     * is disposed, the images will be disposed of.
     */
    private void addDisposeToParent() {
        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (Image img : sortImageArray) {
                    img.dispose();
                }
            }
        });
    }

    /**
     * Create the sort images.
     */
    private void createSortImages() {
        int imgWidthSm = 12;
        int imgHeight = 12;
        Image tmpImg;

        GC gc;

        sortImageArray = new Image[SortDirection.values().length];

        for (int i = 0; i < SortDirection.values().length; i++) {

            sortImageArray[i] = new Image(parentDisplay, imgWidthSm, imgHeight);
        }

        /*
         * Sort ascending image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidthSm, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidthSm, imgHeight);
        gc.dispose();

        ImageData idata = tmpImg.getImageData();

        int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        Image transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawAscendingImage(gc, imgWidthSm, imgHeight);
        gc.dispose();

        sortImageArray[SortDirection.ASCENDING.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Sort descending image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidthSm, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidthSm, imgHeight);
        gc.dispose();

        idata = tmpImg.getImageData();

        whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawDescendingImage(gc, imgWidthSm, imgHeight);
        gc.dispose();

        sortImageArray[SortDirection.DESCENDING.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();
    }

    /**
     * Draw the descending sort image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawDescendingImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        int[] pointArray = new int[] { 1, 10, 6, 1, 11, 10, 1, 10 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the ascending sort image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawAscendingImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        int[] pointArray = new int[] { 1, 1, 6, 10, 11, 1, 1, 1 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Get the sort image
     * 
     * @param sd
     *            Sort direction.
     * @return The image associated with the specified sort direction.
     */
    public Image getImage(SortDirection sd) {
        return this.sortImageArray[sd.ordinal()];
    }
}
