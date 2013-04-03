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
package com.raytheon.uf.viz.datadelivery.notification;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;

/**
 * 
 * Class that generates all of the priority images for the notification table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012   687      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class PriorityImages {

    /**
     * Array of color images.
     */
    private Image[] colorImgs;

    /**
     * Array of color and number images.
     */
    private Image[] colorNumImgs;

    /**
     * Array of color, number, and name images.
     */
    private Image[] colorNumNameImgs;

    /**
     * Array of number images.
     */
    private Image[] numImgs;

    /**
     * Array of number and name images.
     */
    private Image[] numNameImgs;

    /**
     * Parent shell.
     */
    private Shell parentShell;

    /**
     * Font used for the priority.
     */
    private Font priorityFont;

    /**
     * Font width.
     */
    private int fontWidth = 0;

    /**
     * Dash.
     */
    private String dash = "-";

    /**
     * "Off-white" color.
     */
    private Color offWhite;

    /**
     * Priority enumeration.
     */
    public enum Priority {
        Priority1(1, "Crit"), Priority2(2, "Warn"), Priority3(3, "Stat"), Priority4(4, "Info"), Priority5(5, "Gen");

        private int priNum;

        private String priName;

        Priority(int num, String name) {
            priNum = num;
            priName = name;
        }

        public int getPriorityNum() {
            return priNum;
        }

        public String getPriorityName() {
            return priName;
        }
    }

    /**
     * Priority display enumeration.
     */
    public enum PriorityDisplay {
        Color, ColorNum, ColorNumName, Num, NumName
    };

    /**
     * The selected display.
     */
    private PriorityDisplay selectedDisplay = PriorityDisplay.Color;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public PriorityImages(Shell parentShell) {
        this.parentShell = parentShell;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        priorityFont = new Font(parentShell.getDisplay(), "Monospace", 10, SWT.BOLD);
        offWhite = new Color(parentShell.getDisplay(), 255, 255, 254);
        calculateFontWidth();

        parentShell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (Image i : colorImgs) {
                    i.dispose();
                }

                for (Image i : colorNumImgs) {
                    i.dispose();
                }

                for (Image i : colorNumNameImgs) {
                    i.dispose();
                }

                priorityFont.dispose();
            }
        });

        generatePriorityColorImages();
        generatePriorityNumberImages();
        generatePriorityNumNameImages();
        generatePriorityNoColorNumberImages();
        generatePriorityNoColorNumNameImages();
    }

    /**
     * Generate the priority color images.
     */
    private void generatePriorityColorImages() {
        colorImgs = new Image[Priority.values().length];
        int imgWidth = 50;
        int imgHeight = 21;

        for (Priority p : Priority.values()) {

            Image img = new Image(parentShell.getDisplay(), imgWidth, imgHeight);
            GC gc = new GC(img);
            gc.setFont(priorityFont);

            drawPriColorImage(gc, imgWidth, imgHeight, p);

            ImageData idata = img.getImageData();

            int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
            idata.transparentPixel = whitePixel;
            Image transparentIdeaImage = new Image(parentShell.getDisplay(), idata);
            colorImgs[p.ordinal()] = transparentIdeaImage;

            gc.dispose();
            img.dispose();
        }
    }

    /**
     * Draw the priority color image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param pri
     *            Priority.
     */
    private void drawPriColorImage(GC gc, int imgWidth, int imgHeight, Priority pri) {
        gc.setBackground(getColrByPriority(pri));
        gc.setForeground(getColrByPriority(pri));
        gc.fillOval(imgWidth / 2 - imgHeight / 2, 1, imgHeight - 2, imgHeight - 2);

        gc.setLineWidth(2);
        gc.setBackground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(imgWidth / 2 - imgHeight / 2, 1, imgHeight - 2, imgHeight - 2);
    }

    /**
     * Generate the priority number images.
     */
    private void generatePriorityNumberImages() {
        colorNumImgs = new Image[Priority.values().length];
        int imgWidth = 50;
        int imgHeight = 21;

        for (Priority p : Priority.values()) {

            Image img = new Image(parentShell.getDisplay(), imgWidth, imgHeight);
            GC gc = new GC(img);
            gc.setFont(priorityFont);

            drawPriColorNumImage(gc, imgWidth, imgHeight, p);

            ImageData idata = img.getImageData();

            int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
            idata.transparentPixel = whitePixel;
            Image transparentIdeaImage = new Image(parentShell.getDisplay(), idata);
            colorNumImgs[p.ordinal()] = transparentIdeaImage;

            img.dispose();
        }
    }

    /**
     * Draw the priority color and number image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param pri
     *            Priority.
     */
    private void drawPriColorNumImage(GC gc, int imgWidth, int imgHeight, Priority pri) {
        gc.setLineWidth(3);

        gc.setBackground(offWhite);
        gc.fillOval(imgWidth / 2 - imgHeight / 2, 1, imgHeight - 3, imgHeight - 3);

        gc.setBackground(getColrByPriority(pri));
        gc.setForeground(getColrByPriority(pri));
        gc.drawOval(imgWidth / 2 - imgHeight / 2, 1, imgHeight - 3, imgHeight - 3);

        gc.setBackground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawString(String.valueOf(pri.getPriorityNum()), imgWidth / 2 - fontWidth / 2, 2, true);
    }

    /**
     * Generate the priority number and name images.
     */
    private void generatePriorityNumNameImages() {
        colorNumNameImgs = new Image[Priority.values().length];

        int stringSize = largestPriorityStringWidth();
        int stringPixels = fontWidth * stringSize;

        int imgWidth = stringPixels + 20;
        int imgHeight = 21;

        for (Priority p : Priority.values()) {

            Image img = new Image(parentShell.getDisplay(), imgWidth, imgHeight);
            GC gc = new GC(img);
            gc.setFont(priorityFont);

            drawPriColorNumNameImage(gc, imgWidth, imgHeight, p);

            ImageData idata = img.getImageData();

            int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
            idata.transparentPixel = whitePixel;
            Image transparentIdeaImage = new Image(parentShell.getDisplay(), idata);
            colorNumNameImgs[p.ordinal()] = transparentIdeaImage;

            img.dispose();
        }
    }

    /**
     * Draw the priority color, number, and name image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param pri
     *            Priority.
     */
    private void drawPriColorNumNameImage(GC gc, int imgWidth, int imgHeight, Priority pri) {
        gc.setLineWidth(3);

        gc.setBackground(offWhite);
        gc.fillRoundRectangle(3, 1, imgWidth - 6, imgHeight - 3, 15, 15);

        gc.setBackground(getColrByPriority(pri));
        gc.setForeground(getColrByPriority(pri));
        gc.drawRoundRectangle(3, 1, imgWidth - 6, imgHeight - 3, 15, 15);

        gc.setBackground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        String s = pri.getPriorityNum() + dash + pri.priName;
        int strPixels = s.length() * fontWidth / 2;

        gc.drawString(s, imgWidth / 2 - strPixels, 2, true);
    }

    /**
     * Generate the priority with no color only number images.
     */
    private void generatePriorityNoColorNumberImages() {
        numImgs = new Image[Priority.values().length];
        int imgWidth = 50;
        int imgHeight = 21;

        for (Priority p : Priority.values()) {

            Image img = new Image(parentShell.getDisplay(), imgWidth, imgHeight);
            GC gc = new GC(img);
            gc.setFont(priorityFont);

            drawPriNumImage(gc, imgWidth, imgHeight, p);

            ImageData idata = img.getImageData();

            int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
            idata.transparentPixel = whitePixel;
            Image transparentIdeaImage = new Image(parentShell.getDisplay(), idata);
            numImgs[p.ordinal()] = transparentIdeaImage;

            img.dispose();
        }
    }

    /**
     * Draw the priority with no color only number image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param pri
     *            Priority.
     */
    private void drawPriNumImage(GC gc, int imgWidth, int imgHeight, Priority pri) {

        gc.setBackground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawString(String.valueOf(pri.getPriorityNum()), imgWidth / 2 - fontWidth / 2, 2, true);
    }

    /**
     * Generate the priority image with no color only number and name.
     */
    private void generatePriorityNoColorNumNameImages() {
        numNameImgs = new Image[Priority.values().length];

        int stringSize = largestPriorityStringWidth();
        int stringPixels = fontWidth * stringSize;

        int imgWidth = stringPixels + 20;
        int imgHeight = 21;

        for (Priority p : Priority.values()) {
            Image img = new Image(parentShell.getDisplay(), imgWidth, imgHeight);
            GC gc = new GC(img);
            gc.setFont(priorityFont);

            drawPriNumNameImage(gc, imgWidth, imgHeight, p);

            ImageData idata = img.getImageData();

            int whitePixel = idata.palette.getPixel(new RGB(255, 255, 255));
            idata.transparentPixel = whitePixel;
            Image transparentIdeaImage = new Image(parentShell.getDisplay(), idata);
            numNameImgs[p.ordinal()] = transparentIdeaImage;

            img.dispose();
        }
    }

    /**
     * Draw the priority image with no color only number and name.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     * @param pri
     *            Priority.
     */
    private void drawPriNumNameImage(GC gc, int imgWidth, int imgHeight, Priority pri) {
        gc.setBackground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(parentShell.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        String s = pri.getPriorityNum() + dash + pri.priName;
        int strPixels = s.length() * fontWidth / 2;

        gc.drawString(s, imgWidth / 2 - strPixels, 2, true);
    }

    /**
     * Calculate the font width.
     */
    private void calculateFontWidth() {
        Image image = new Image(parentShell.getDisplay(), 10, 10);
        GC gc = new GC(image);
        gc.setFont(priorityFont);

        fontWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.dispose();
        image.dispose();
    }

    /**
     * Find the largest priority string width (number of characters.
     * 
     * @return The
     */
    private int largestPriorityStringWidth() {
        int prefixCharLen = dash.length() + 1;
        int largestWidth = 0;

        for (Priority p : Priority.values()) {
            int length = p.priName.length() + prefixCharLen;

            largestWidth = Math.max(largestWidth, length);
        }

        return largestWidth;
    }

    /**
     * Set the priority display.
     * 
     * @param pd
     *            Priority display.
     */
    public void setPriorityDisplay(PriorityDisplay pd) {
        this.selectedDisplay = pd;
    }

    /**
     * Get the image based on the priority passed in.
     * 
     * @param pri
     *            Priority.
     * @return The desired image.
     */
    public Image getImage(Priority pri) {
        if (selectedDisplay == PriorityDisplay.Color) {
            return colorImgs[pri.ordinal()];
        }
        else if (selectedDisplay == PriorityDisplay.ColorNum) {
            return colorNumImgs[pri.ordinal()];
        }
        else if (selectedDisplay == PriorityDisplay.ColorNumName) {
            return colorNumNameImgs[pri.ordinal()];
        }
        else if (selectedDisplay == PriorityDisplay.Num) {
            return numImgs[pri.ordinal()];
        }
        else if (selectedDisplay == PriorityDisplay.NumName) {
            return numNameImgs[pri.ordinal()];
        }
        return colorImgs[pri.ordinal()];
    }

    /**
     * Get the color by priority.
     * 
     * @param pri
     *            Priority
     * @return Color associated with the priority.
     */
    private Color getColrByPriority(Priority pri) {
        if (pri == Priority.Priority1) {
            return parentShell.getDisplay().getSystemColor(SWT.COLOR_RED);
        }
        else if (pri == Priority.Priority2) {
            return parentShell.getDisplay().getSystemColor(SWT.COLOR_YELLOW);
        }
        else if (pri == Priority.Priority3) {
            return parentShell.getDisplay().getSystemColor(SWT.COLOR_GREEN);
        }
        else if (pri == Priority.Priority4) {
            return parentShell.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }
        else if (pri == Priority.Priority5) {
            return parentShell.getDisplay().getSystemColor(SWT.COLOR_BLUE);
        }

        return parentShell.getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY);
    }
}
