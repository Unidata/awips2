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
package com.raytheon.uf.viz.datadelivery.filter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * 
 * This class holds the images for the expand bar controls and the expand bar
 * items.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FilterImages {

    /**
     * An array of expand state images.
     */
    private Image[] expandItemStateImgArray;

    /**
     * An array of expand bar controls images.
     */
    private Image[] expandBarControlImgArray;

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Parent display.
     */
    private Display parentDisplay;

    /**
     * Enumeration for the Expand item states.
     */
    public enum ExpandItemState {
        Entries(SWT.COLOR_GREEN), NoEntries(SWT.COLOR_WHITE), Disabled(SWT.COLOR_GRAY);

        private int colorId;

        ExpandItemState(int id) {
            colorId = id;
        }

        public int getColorId() {
            return colorId;
        }
    };

    /**
     * Enumeration for the expand bar control images.
     */
    public enum ExpandBarControlImage {
        Expand, Collapse, ExpandSelected, Disable, Preview, ClearAll
    };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public FilterImages(Composite parent) {
        this.parent = parent;
        parentDisplay = parent.getDisplay();

        addDisposeToParent();
        createControlImages();
        createExpandStateImages();
    }

    /**
     * Add a dispose listener to a parent composite so when the composite gets
     * disposed the images can be disposed of as well.
     */
    private void addDisposeToParent() {
        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (Image img : expandItemStateImgArray) {
                    img.dispose();
                }

                for (Image img : expandBarControlImgArray) {
                    img.dispose();
                }
            }
        });
    }

    /**
     * Create the expand bar control images.
     */
    private void createControlImages() {

        int imgWidth = 12;
        int imgWidthLrg = 24;
        int imgHeight = 12;

        expandBarControlImgArray = new Image[ExpandBarControlImage.values().length];

        // Collapse image
        expandBarControlImgArray[ExpandBarControlImage.Collapse.ordinal()] = new Image(parentDisplay, imgWidth,
                imgHeight);
        GC gc = new GC(expandBarControlImgArray[ExpandBarControlImage.Collapse.ordinal()]);
        drawExpandCollapseImage(gc, imgWidth, imgHeight, ExpandBarControlImage.Collapse);
        gc.dispose();

        // Expand image
        expandBarControlImgArray[ExpandBarControlImage.Expand.ordinal()] = new Image(parentDisplay, imgWidth, imgHeight);
        gc = new GC(expandBarControlImgArray[ExpandBarControlImage.Expand.ordinal()]);
        drawExpandCollapseImage(gc, imgWidth, imgHeight, ExpandBarControlImage.Expand);
        gc.dispose();

        // Disable image
        expandBarControlImgArray[ExpandBarControlImage.Disable.ordinal()] = new Image(parentDisplay, imgWidth,
                imgHeight);
        gc = new GC(expandBarControlImgArray[ExpandBarControlImage.Disable.ordinal()]);
        int color = ExpandItemState.Disabled.getColorId();
        drawCircleImage(gc, imgWidth, imgHeight, color);
        gc.dispose();

        // Clear all image
        expandBarControlImgArray[ExpandBarControlImage.ClearAll.ordinal()] = new Image(parentDisplay, imgWidth,
                imgHeight);
        gc = new GC(expandBarControlImgArray[ExpandBarControlImage.ClearAll.ordinal()]);
        color = ExpandItemState.NoEntries.getColorId();
        drawCircleImage(gc, imgWidth, imgHeight, color);
        gc.dispose();

        // Expand selected image
        expandBarControlImgArray[ExpandBarControlImage.ExpandSelected.ordinal()] = new Image(parentDisplay,
                imgWidthLrg, imgHeight);
        gc = new GC(expandBarControlImgArray[ExpandBarControlImage.ExpandSelected.ordinal()]);
        drawExpandSelectedImage(gc, imgWidthLrg, imgHeight);
        gc.dispose();

        // Preview image
        expandBarControlImgArray[ExpandBarControlImage.Preview.ordinal()] = new Image(parentDisplay, imgWidth,
                imgHeight);
        gc = new GC(expandBarControlImgArray[ExpandBarControlImage.Preview.ordinal()]);
        drawPreviewImage(gc, imgWidth, imgHeight);
        gc.dispose();
    }

    /**
     * Draw the collapse or expand image based on the flag passed in.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param expCollapse
     *            Expand/Collapse flag.
     */
    private void drawExpandCollapseImage(GC gc, int imgWidth, int imgHeight, ExpandBarControlImage expCollapse) {
        gc.setAntialias(SWT.OFF);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        gc.drawRectangle(0, 0, imgWidth - 1, imgHeight - 1);

        // Draw the "minus" sign
        gc.drawLine(2, 5, 10, 5);
        gc.drawLine(2, 6, 10, 6);

        // If the expand flag is true then draw vertical lines to
        // make a "plus" sign.
        if (expCollapse == ExpandBarControlImage.Expand) {
            gc.drawLine(5, 1, 5, 9);
            gc.drawLine(6, 1, 6, 9);
        }
    }

    /**
     * Draw the expand selected image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawExpandSelectedImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.OFF);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the "plus" sign
        gc.drawLine(2, 5, 10, 5);
        gc.drawLine(2, 6, 10, 6);
        gc.drawLine(5, 1, 5, 9);
        gc.drawLine(6, 1, 6, 9);

        /*
         * Selected circle.
         */
        gc.setAntialias(SWT.ON);

        // "Erase" the canvas by filling it in with a white rectangle.

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_GREEN));
        gc.fillOval(imgWidth / 2, 0, imgWidth / 2 - 1, imgHeight - 1);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(imgWidth / 2, 0, imgWidth / 2 - 1, imgHeight - 1);
    }

    /**
     * Draw the preview image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawPreviewImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.OFF);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        gc.drawRectangle(0, 0, imgWidth - 1, imgHeight - 1);

        // Draw the "lines" that represent text.
        gc.drawLine(2, 2, 5, 2);
        gc.drawLine(2, 4, 9, 4);
        gc.drawLine(2, 6, 6, 6);
        gc.drawLine(2, 8, 8, 8);
    }

    /**
     * Create the expand item state images.
     */
    private void createExpandStateImages() {

        int imgWidth = 12;
        int imgHeight = 12;

        expandItemStateImgArray = new Image[ExpandItemState.values().length];

        GC gc;

        for (int i = 0; i < ExpandItemState.values().length; i++) {
            expandItemStateImgArray[i] = new Image(parentDisplay, imgWidth, imgHeight);
            gc = new GC(expandItemStateImgArray[i]);
            int color = ExpandItemState.values()[i].getColorId();
            drawExpandStateImage(gc, imgWidth, imgHeight, color);
            gc.dispose();
        }
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

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(parentDisplay.getSystemColor(colorId));
        gc.fillOval(0, 0, imgWidth - 1, imgHeight - 1);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(0, 0, imgWidth - 1, imgHeight - 1);
    }

    /**
     * Draw the expand state image.
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
    private void drawExpandStateImage(GC gc, int imgWidth, int imgHeight, int colorId) {
        gc.setAntialias(SWT.ON);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(parentDisplay.getSystemColor(colorId));
        gc.fillOval(0, 0, imgWidth - 1, imgHeight - 1);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(0, 0, imgWidth - 1, imgHeight - 1);
    }

    /**
     * Get the expand control image.
     * 
     * @param expCollapse
     *            The specified image to retrieve.
     * @return The expand control image.
     */
    public Image getExpandControlImage(ExpandBarControlImage expCollapse) {
        return expandBarControlImgArray[expCollapse.ordinal()];
    }

    /**
     * Get the expand item state image.
     * 
     * @param expandImage
     *            The specific state image to retrieve.
     * @return The expand item state image.
     */
    public Image getExpandItemImage(ExpandItemState expandImage) {
        return expandItemStateImgArray[expandImage.ordinal()];
    }
}
