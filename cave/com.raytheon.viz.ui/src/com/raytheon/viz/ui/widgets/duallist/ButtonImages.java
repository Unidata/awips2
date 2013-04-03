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
package com.raytheon.viz.ui.widgets.duallist;

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
 * Images for the Add All, Add, Remove, Remove All, Move Up, and Move Down
 * buttons.  Reused from Data Delivery
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class ButtonImages {

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Parent display.
     */
    private Display parentDisplay;

    /**
     * Array of button images.
     */
    private Image[] buttonArray;

    /**
     * Button image enumeration that identifies which image to use.
     */
    public enum ButtonImage {
        AddAll, Add, Remove, RemoveAll, Up, Down
    };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public ButtonImages(Composite parent) {
        this.parent = parent;
        parentDisplay = parent.getDisplay();

        addDisposeToParent();

        createButtonImages();
    }

    /**
     * Add a dispose listener to the parent composite. When the parent composite
     * is disposed, the images will be disposed of.
     */
    private void addDisposeToParent() {
        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (Image img : buttonArray) {
                    img.dispose();
                }
            }
        });
    }

    /**
     * Create the button images.
     */
    private void createButtonImages() {
        int imgWidthSm = 12;
        int imgWidth = 24;
        int imgHeight = 12;
        Image tmpImg;

        GC gc;

        buttonArray = new Image[ButtonImage.values().length];

        for (int i = 0; i < ButtonImage.values().length; i++) {
            if (ButtonImage.values()[i] == ButtonImage.Up || ButtonImage.values()[i] == ButtonImage.Down) {
                buttonArray[i] = new Image(parentDisplay, imgWidthSm, imgHeight);
            }
            else {
                buttonArray[i] = new Image(parentDisplay, imgWidth, imgHeight);
            }
        }

        /*
         * Add all image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidth, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.dispose();

        ImageData idata = tmpImg.getImageData();

        int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        Image transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawAddAllImage(gc, imgWidth, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.AddAll.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Add image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidth, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.dispose();

        idata = tmpImg.getImageData();

        whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawAddImage(gc, imgWidth, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.Add.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Remove image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidth, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.dispose();

        idata = tmpImg.getImageData();

        whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawRemoveImage(gc, imgWidth, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.Remove.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Remove all image
         */
        tmpImg = new Image(parent.getDisplay(), imgWidth, imgHeight);
        gc = new GC(tmpImg);
        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.dispose();

        idata = tmpImg.getImageData();

        whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
        idata.transparentPixel = whitePixel;
        transparentIdeaImage = new Image(parent.getDisplay(), idata);

        gc = new GC(transparentIdeaImage);
        drawRemoveAllImage(gc, imgWidth, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.RemoveAll.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Move up image
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
        drawMoveUpImage(gc, imgWidthSm, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.Up.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();

        /*
         * Move down image
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
        drawMoveDownImage(gc, imgWidthSm, imgHeight);
        gc.dispose();

        buttonArray[ButtonImage.Down.ordinal()] = transparentIdeaImage;
        tmpImg.dispose();
    }

    /**
     * Draw the "add all" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawAddAllImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the left arrow
        int[] pointArray = new int[] { 10, 6, 1, 1, 1, 10, 10, 6 };
        gc.fillPolygon(pointArray);

        // Draw the right arrow
        pointArray = new int[] { 22, 6, 13, 1, 13, 10, 22, 6 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the "add" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawAddImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the add arrow
        int[] pointArray = new int[] { 17, 6, 8, 1, 8, 10, 17, 6 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the "remove" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawRemoveImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the add arrow
        int[] pointArray = new int[] { 8, 6, 17, 1, 17, 10, 8, 6 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the "remove all" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawRemoveAllImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the left arrow
        int[] pointArray = new int[] { 1, 6, 10, 1, 10, 10, 1, 6 };
        gc.fillPolygon(pointArray);

        // Draw the right arrow
        pointArray = new int[] { 13, 6, 22, 1, 22, 10, 13, 6 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the "move up" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawMoveUpImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the add arrow
        int[] pointArray = new int[] { 1, 10, 6, 1, 11, 10, 1, 10 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Draw the "move down" image.
     * 
     * @param gc
     *            Graphics context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawMoveDownImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        gc.setBackground(parentDisplay.getSystemColor(SWT.COLOR_BLACK));

        // Draw the add arrow
        int[] pointArray = new int[] { 1, 1, 6, 10, 11, 1, 1, 1 };
        gc.fillPolygon(pointArray);
    }

    /**
     * Get the image associated with the specified button image.
     * 
     * @param bi
     *            Button image identifier.
     * @return The associated button image.
     */
    public Image getImage(ButtonImage bi) {
        return this.buttonArray[bi.ordinal()];
    }
}
