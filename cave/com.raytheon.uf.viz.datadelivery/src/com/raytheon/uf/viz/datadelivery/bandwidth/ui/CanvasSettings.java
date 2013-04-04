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

/**
 * Class containing the settings for a canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class CanvasSettings {
    /** Canvas width. */
    private int canvasWidth = 740;

    /** Canvas height. */
    private int canvasHeight = 440;

    /** Image width */
    private int imageWidth = canvasWidth;

    /** Image height */
    private int imageHeight = canvasHeight;

    /** Space buffer for the x coordinate. */
    private int xSpaceBuffer = 20;

    /** Space buffer for the y coordinate. */
    private int ySpaceBuffer = 20;

    /** Draw width */
    private int drawWidth = 0;

    /** draw height */
    private int drawHeight = 0;

    /**
     * Constructor.
     *
     * @param canvasWidth
     *            Canvas width.
     * @param canvasHeight
     *            Canvas height.
     * @param xSpaceBuffer
     *            Space buffer for the x coordinate.
     * @param ySpaceBuffer
     *            Space buffer for the y coordinate.
     */
    public CanvasSettings(int canvasWidth, int canvasHeight, int imageWidth,
            int imageHeight, int xSpaceBuffer, int ySpaceBuffer) {
        this.canvasWidth = canvasWidth;
        this.canvasHeight = canvasHeight;

        if (imageWidth < canvasWidth) {
            this.imageWidth = canvasWidth;
        } else {
            this.imageWidth = imageWidth;
        }

        if (imageHeight < canvasHeight) {
            this.imageHeight = canvasHeight;
        } else {
            this.imageHeight = imageHeight;
        }

        this.xSpaceBuffer = xSpaceBuffer;
        this.ySpaceBuffer = ySpaceBuffer;
        this.drawWidth = canvasWidth - xSpaceBuffer * 2;
        this.drawHeight = canvasHeight - ySpaceBuffer * 2;
    }

    /**
     * Get the canvas width.
     *
     * @return The canvas width.
     */
    public int getCanvasWidth() {
        return canvasWidth;
    }

    /**
     * Get the canvas height.
     *
     * @return The canvas height.
     */
    public int getCanvasHeight() {
        return canvasHeight;
    }

    /**
     * Get the space buffer for the x coordinate.
     *
     * @return The space buffer for the x coordinate.
     */
    public int getXSpaceBuffer() {
        return xSpaceBuffer;
    }

    /**
     * Get the space buffer for the y coordinate.
     *
     * @return The space buffer for the y coordinate.
     */
    public int getYSpaceBuffer() {
        return ySpaceBuffer;
    }

    /**
     * @return the imageWidth
     */
    public int getImageWidth() {
        return imageWidth;
    }

    /**
     * @param imageWidth
     *            the imageWidth to set
     */
    public void setImageWidth(int imageWidth) {
        this.imageWidth = imageWidth;
    }

    /**
     * @return the imageHeight
     */
    public int getImageHeight() {
        return imageHeight;
    }

    /**
     * @param imageHeight
     *            the imageHeight to set
     */
    public void setImageHeight(int imageHeight) {
        this.imageHeight = imageHeight;
    }

    /**
     * @return the drawWidth
     */
    public int getDrawWidth() {
        return drawWidth;
    }

    /**
     * @param drawWidth
     *            the drawWidth to set
     */
    public void setDrawWidth(int drawWidth) {
        this.drawWidth = drawWidth;
    }

    /**
     * @return the drawHeight
     */
    public int getDrawHeight() {
        return drawHeight;
    }

    /**
     * @param drawHeight
     *            the drawHeight to set
     */
    public void setDrawHeight(int drawHeight) {
        this.drawHeight = drawHeight;
    }
}
