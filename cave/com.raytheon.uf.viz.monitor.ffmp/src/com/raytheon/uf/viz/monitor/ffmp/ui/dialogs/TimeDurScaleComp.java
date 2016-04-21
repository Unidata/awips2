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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.ArrayList;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * Time Duration Scale Composite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2013      #2488 randerso    Changed to use DejaVu font
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class TimeDurScaleComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Time slider canvas that displays the thumb control, time range, and the
     * current selected hour value.
     */
    private Canvas timeSliderCanvas;

    /**
     * Font used for the time labels.
     */
    private Font canvasFont;

    /**
     * Time duration hour.
     */
    private double timeDurHour = 0.0;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 51;

    /**
     * Canvas width.
     */
    private int CANVAS_WIDTH = 450;

    /**
     * Section height of the canvas. There are 3 sections of the canvas
     * (selected hour, thumb slider, and the time range). The height is the
     * canvas height divided by 3.
     */
    private final int CANVAS_SECTION_HEIGHT = CANVAS_HEIGHT / 3;

    /**
     * Offset used for the left side of the canvas to move what is drawn over a
     * specified number of pixels.
     */
    private double xCoordOffset = 5.0;

    /**
     * Lower hour range value (always zero).
     */
    private final double lowerHourRangeVal = 0.00;

    /**
     * Upper hour range value.
     */
    private double upperHourRangeVal = 24.00;

    /**
     * Range value that is from current time (0.00) to some number of hours in
     * the future.
     */
    private double rangeVal = (Math.abs(upperHourRangeVal))
            - (Math.abs(lowerHourRangeVal));

    /**
     * Action callback.
     */
    private ITimeDurationAction actionCallback;

    /**
     * Split flag.
     */
    public boolean split = false;

    /**
     * Display numbers for the range.
     */
    private double[] displayNumbers;

    /**
     * Thumb pixel index array.
     */
    private int[] thumbPixelIdx;

    /**
     * Previous hour value.
     */
    private double prevHourVal = 0.0;

    /**
     * Rectangle that defines the bound of the thumb slider.
     */
    private Rectangle timeThumbSliderRec;

    /**
     * Rectangle that defines the bound of the slider bar.
     */
    private Rectangle timeBarRec;

    /**
     * Color of the time slider bar.
     */
    private Color timeBarColor;

    /**
     * Grey color.
     */
    private Color greyColor;

    /**
     * Yellow color.
     */
    private Color yellowColor;

    /**
     * Thumb slider middle color.
     */
    private Color thumbMiddleColor;

    /**
     * Thumb slider background color.
     */
    private Color thumbBgColor;

    /**
     * Flag indicating the mouse button is down.
     */
    private boolean mouseDown = false;

    /**
     * Flag indicating if the mouse click was in the thumb slider control.
     */
    private boolean inThumbSlider = false;

    /**
     * Mouse offset for the thumb slider so when dragging the slider the mouse
     * stays at the same X coord where it was clicked.
     */
    private int thumbSliderMouseOffset = 0;

    /**
     * Mouse thumb offset.
     */
    private int mouseThumbOffset = 0;

    /**
     * Number of pixels per hour value.
     */
    private double pixPerThumbInc = 0;

    /**
     * Index used to determine if the slider is moved back to the same position
     * we don't need to update.
     */
    private int thumbPixIdx = -1;

    /**
     * Arc value for the slider bar.
     */
    private int arcNumberBar = 5;

    /**
     * Arc value for the thumb slider control.
     */
    private int arcNumberThumb = 10;

    /**
     * Pixel offset for the color of the middle of the thumb control.
     */
    private int thumbDividerOffset = 12;

    /**
     * X coordinate of the thumb control.
     */
    private int thumbXCoord = 0;

    private double pixPerInc = 0;

    /**
     * Map of thumb pixel indexes and the hour value associated with that index.
     */
    private TreeMap<Integer, Double> indexSelHrsMap = new TreeMap<Integer, Double>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param actionCallback
     *            Action callback.
     */
    public TimeDurScaleComp(Composite parent, ITimeDurationAction actionCallback) {
        super(parent, 0);

        this.parentComp = parent;

        this.actionCallback = actionCallback;

        upperHourRangeVal = 24.00;

        calcDisplayNumbers();

        // pixPerInc = (CANVAS_WIDTH - 35 - xCoordOffset) / rangeVal;
        // pixPerInc = (CANVAS_WIDTH - 35 - xCoordOffset) / rangeVal;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        // Create the font

        FontData fd = parentComp.getDisplay().getSystemFont().getFontData()[0];
        // TODO not have hard coded font name
        fd.setName("DejaVu Sans Mono");
        canvasFont = new Font(parentComp.getDisplay(), fd);

        // Create the time bar colors
        thumbMiddleColor = new Color(parentComp.getDisplay(), new RGB(220, 220,
                240));
        thumbBgColor = new Color(parentComp.getDisplay(),
                new RGB(100, 100, 250));
        greyColor = new Color(parentComp.getDisplay(), new RGB(250, 250, 250));
        yellowColor = new Color(parentComp.getDisplay(), new RGB(255, 255, 0));
        timeBarColor = greyColor;

        // Calculate the rectangles for the time bar and the slider thumb
        // control.
        timeBarRec = new Rectangle(3, CANVAS_SECTION_HEIGHT + 6,
                CANVAS_WIDTH - 6, CANVAS_SECTION_HEIGHT - 12);
        timeThumbSliderRec = new Rectangle(100, CANVAS_SECTION_HEIGHT + 3, 45,
                CANVAS_SECTION_HEIGHT - 5);

        // Calculate the number of pixels per time slider thumb increment.
        double movableWidth = ((timeBarRec.width - timeThumbSliderRec.width));
        double numIncrements = (upperHourRangeVal - lowerHourRangeVal) * 4.0;
        pixPerThumbInc = movableWidth / numIncrements;

        // Calculate the pixel per increment value
        // pixPerInc = (CANVAS_WIDTH - 35 - xCoordOffset) / rangeVal;
        pixPerInc = (timeBarRec.width - timeThumbSliderRec.width) / rangeVal;

        // Populate the thumb index array
        populateThumbPixArray();

        // Get the split flag.
        split = FFMPConfig.getInstance().isSplit();

        // Set up the composite layout and data
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        this.setLayout(gl);
        this.setLayoutData(gd);

        // Create the hours array and the controls for the time slider/labels.
        populateHoursArray();
        createTimeSliderCanvas();

        this.pack();

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                greyColor.dispose();
                yellowColor.dispose();
                thumbBgColor.dispose();
                thumbMiddleColor.dispose();
                canvasFont.dispose();
            }
        });

        // TODO : remove line
        updateTimeDuration();
    }

    /**
     * Create the time slider canvas (slider control and the labels).
     */
    private void createTimeSliderCanvas() {
        timeSliderCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        timeSliderCanvas.setLayoutData(new GridData(CANVAS_WIDTH + 10,
                CANVAS_HEIGHT));
        timeSliderCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawTimeSliderCanvas(e.gc);
            }
        });

        double timeFrame = 1.50;

        for (int key : indexSelHrsMap.keySet()) {
            if (timeFrame == indexSelHrsMap.get(key)) {
                thumbXCoord = thumbPixelIdx[key];
                timeThumbSliderRec.x = thumbPixelIdx[thumbXCoord];
            }
        }

        /*
         * Add the mouse listeners
         */
        timeSliderCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                // If the mouse click is not the left mouse button then return.
                if (e.button != 1) {
                    return;
                }

                mouseDown = true;

                if (timeBarRec.contains(e.x, e.y)
                        && timeThumbSliderRec.contains(e.x, e.y) == false) {
                    if (e.x < timeThumbSliderRec.x) {
                        // Decrement the thumb slider control.
                        ArrayList<Integer> array = new ArrayList<Integer>(
                                indexSelHrsMap.keySet());

                        for (int i = 0; i < array.size(); i++) {
                            if (timeDurHour == indexSelHrsMap.get(array.get(i))) {
                                if (i != 0) {
                                    internalSetTimeDuration(indexSelHrsMap
                                            .get(array.get(i - 1)));
                                    break;
                                }
                            }
                        }
                    } else {
                        // Increment the thumb slider control.
                        ArrayList<Integer> array = new ArrayList<Integer>(
                                indexSelHrsMap.keySet());

                        for (int i = 0; i < array.size(); i++) {
                            if (timeDurHour == indexSelHrsMap.get(array.get(i))) {
                                if (i != (array.size() - 1)) {
                                    internalSetTimeDuration(indexSelHrsMap
                                            .get(array.get(i + 1)));
                                    break;
                                }
                            }
                        }
                    }
                } else if (timeThumbSliderRec.contains(e.x, e.y)) {
                    inThumbSlider = true;
                    thumbSliderMouseOffset = e.x - timeThumbSliderRec.x;
                }
            }

            @Override
            public void mouseUp(MouseEvent e) {
                mouseDown = false;
                inThumbSlider = false;

                if (thumbPixIdx != thumbPixelIdx[thumbXCoord]) {
                    updateTimeDurHours();
                    determineSplitValue();

                    actionCallback.timeDurationUpdated(timeDurHour, split);

                    thumbPixIdx = thumbPixelIdx[thumbXCoord];
                }
            }
        });

        timeSliderCanvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                if (mouseDown == true) {
                    if (inThumbSlider) {
                        // Move the thumb control.
                        mouseThumbOffset = e.x - thumbSliderMouseOffset;

                        if (mouseThumbOffset < timeBarRec.x) {
                            thumbXCoord = 0;
                        } else if (mouseThumbOffset + timeThumbSliderRec.width >= timeBarRec.width
                                + timeBarRec.x) {
                            thumbXCoord = thumbPixelIdx.length - 1;
                        } else {
                            thumbXCoord = mouseThumbOffset - timeBarRec.x;
                        }

                        timeThumbSliderRec.x = thumbPixelIdx[thumbXCoord];

                        updateTimeDuration();
                    }

                }
            }
        });

        updateTimeDurHours();
    }

    /**
     * Populate the thumbPixelIdx array. Each entry in the array represents one
     * pixel on the display. The size of the array is the number of pixels the
     * thumb slider is allowed to move. Each entry in the array is a "snap-to"
     * pixel on the slider.
     */
    private void populateThumbPixArray() {
        thumbPixelIdx = new int[timeBarRec.width - timeThumbSliderRec.width];

        double incMultiplier = 1.0;

        for (int i = 0; i < thumbPixelIdx.length; i++) {
            if (i == 0) {
                thumbPixelIdx[i] = 0;
            } else if (i == thumbPixelIdx.length - 1) {
                thumbPixelIdx[i] = timeBarRec.width - timeThumbSliderRec.width;
            } else {
                if (i > (pixPerThumbInc * incMultiplier)) {
                    incMultiplier += 1.0;
                }

                thumbPixelIdx[i] = (int) Math.round(pixPerThumbInc
                        * incMultiplier);
            }
        }

        for (int x : thumbPixelIdx) {
            indexSelHrsMap.put(x, 0.0);
        }
    }

    /**
     * Populate the hours array which contains all of the possible hours in 0.25
     * hour increments.
     */
    private void populateHoursArray() {
        double newVal = 0.0;

        for (int key : indexSelHrsMap.keySet()) {
            indexSelHrsMap.put(key, newVal);
            newVal += .25;
        }
    }

    /**
     * Draw the slider time canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawTimeSliderCanvas(GC gc) {
        gc.setFont(canvasFont);

        /*
         * Clear and refill with base color
         */
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
        gc.setAntialias(SWT.ON);

        /*
         * Draw the selected time value
         */
        drawScaleValueCanvas(gc);

        /*
         * Draw the time bar and the slider thumb
         */

        // Draw the "slider" bar
        gc.setBackground(timeBarColor);
        gc.fillRoundRectangle(timeBarRec.x, timeBarRec.y, timeBarRec.width,
                timeBarRec.height, arcNumberBar, arcNumberBar);

        gc.setBackground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));
        gc.drawRoundRectangle(timeBarRec.x, timeBarRec.y, timeBarRec.width,
                timeBarRec.height, arcNumberBar, arcNumberBar);

        // Draw "thumb" control
        gc.setBackground(thumbBgColor);

        gc.fillRoundRectangle(timeThumbSliderRec.x + timeBarRec.x,
                timeThumbSliderRec.y, timeThumbSliderRec.width,
                timeThumbSliderRec.height, arcNumberThumb, arcNumberThumb);

        gc.setBackground(thumbMiddleColor);

        gc.fillRectangle(timeThumbSliderRec.x + 3 + thumbDividerOffset,
                timeThumbSliderRec.y, (timeThumbSliderRec.x
                        + timeThumbSliderRec.width - thumbDividerOffset + 3)
                        - (timeThumbSliderRec.x + 3 + thumbDividerOffset),
                timeThumbSliderRec.height);

        gc.drawLine(timeThumbSliderRec.x + 3 + thumbDividerOffset,
                timeThumbSliderRec.y, timeThumbSliderRec.x + 3
                        + thumbDividerOffset, timeThumbSliderRec.y
                        + timeThumbSliderRec.height);

        gc.drawLine(timeThumbSliderRec.x + timeThumbSliderRec.width
                - thumbDividerOffset + 3, timeThumbSliderRec.y,
                timeThumbSliderRec.x + timeThumbSliderRec.width
                        - thumbDividerOffset + 3, timeThumbSliderRec.y
                        + timeThumbSliderRec.height);

        gc.drawRoundRectangle(timeThumbSliderRec.x + timeBarRec.x,
                timeThumbSliderRec.y, timeThumbSliderRec.width,
                timeThumbSliderRec.height, arcNumberThumb, arcNumberThumb);

        /*
         * Draw the time range
         */
        drawScaleRangeCanvas(gc);
    }

    /**
     * Draw the time label that moves with the "thumb" control. It indicates
     * what time the thumb control is surrently at.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawScaleValueCanvas(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        int newXCoord = (int) Math.round(((this.timeDurHour + Math
                .abs(lowerHourRangeVal)) * pixPerInc + xCoordOffset));

        // if (Math.abs(this.timeDurHour) >= 10)
        // {
        // // newXCoord -= fontAveWidth / 2 - 2;
        // newXCoord -= (int) (Math.round(fontAveWidth / 2.0));
        // }

        if (this.timeDurHour < 0.0) {
            newXCoord -= 5;
        }

        gc.drawString(String.format("%5.2f", this.timeDurHour), newXCoord, 1,
                true);
    }

    /**
     * Draw the time labels below the thumb slider on the canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawScaleRangeCanvas(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        for (double dVal : displayNumbers) {
            int newXCoord = (int) Math.round(((dVal + Math
                    .abs(lowerHourRangeVal)) * pixPerInc + xCoordOffset));

            // if (Math.abs(dVal) >= 10)
            // {
            // // newXCoord -= (int) (Math.round(fontAveWidth / 2.0 - 2.0));
            // newXCoord -= (int) (Math.round(fontAveWidth / 2.0));
            // }

            if (dVal < 0.0) {
                newXCoord -= 5;
            }

            gc.drawString(String.format("%5.2f", dVal), newXCoord,
                    CANVAS_SECTION_HEIGHT * 2 + 1, true);
        }
    }

    /**
     * Set the time duration hours variable based on the position of the thumb
     * slider.
     */
    private void updateTimeDurHours() {
        timeDurHour = indexSelHrsMap.get(thumbPixelIdx[thumbXCoord]);
    }

    /**
     * Set the time duration scale to the hour provided.
     * 
     * @param hourVal
     *            Hour on the scale to move the slider to.
     */
    private void setTimeDurationScale(double hourVal) {
        for (int key : indexSelHrsMap.keySet()) {

            if (hourVal == indexSelHrsMap.get(key)) {
                // Find an 'X' coordinate for the thumb slider.

                for (int i = 0; i < thumbPixelIdx.length; i++) {
                    if (thumbPixelIdx[i] == key) {
                        thumbXCoord = i;
                    }
                }

                timeThumbSliderRec.x = thumbPixelIdx[thumbXCoord];
                break;
            }
        }

        timeSliderCanvas.redraw();
    }

    /**
     * Update the time duration.
     */
    private void updateTimeDuration() {
        determineSliderColor();
        updateTimeDurHours();

        timeSliderCanvas.redraw();
    }

    /**
     * Determine if we have to adjust for the split value.
     */
    private void determineSplitValue() {
        if (split == true) {
            if (timeDurHour < 1.00) {
                internalSetTimeDuration(1.00);
            }
        }
    }

    /**
     * Determine the slider color.
     */
    private void determineSliderColor() {
        if (thumbXCoord == 0 || split == true) {
            timeBarColor = yellowColor;
        } else {
            timeBarColor = greyColor;
        }
    }

    private void calcDisplayNumbers() {
        double hours = validateHours(upperHourRangeVal);
        int divisor = getDivisor(hours);
        int hourLbls = (int) (hours / divisor);

        // Account for the 0.00 hour label
        ++hourLbls;

        displayNumbers = new double[hourLbls];

        if (hours % divisor != 0) {
            for (int i = 0; i < displayNumbers.length - 1; i++) {
                displayNumbers[i] = (i * divisor);
            }

            displayNumbers[displayNumbers.length - 1] = hours;
        } else {
            for (int i = 0; i < displayNumbers.length; i++) {
                displayNumbers[i] = (i * divisor);
            }
        }
    }

    private double validateHours(double hours) {
        if (hours < 12.00) {
            return 12.00;
        }

        if (hours > 36.00) {
            return 36.00;
        }

        double validHour = ((int) hours);

        return validHour;
    }

    private int getDivisor(double hours) {
        if (hours < 17) {
            return 2;
        } else if (hours < 25) {
            return 3;
        } else if (hours < 30) {
            return 4;
        } else if (hours < 36) {
            return 5;
        }
        return 6;
    }

    /**
     * Set the time duration to the specified hour argument.
     * 
     * @param hourVal
     *            Hour value.
     */
    public void setTimeDuration(double hourVal) {
        thumbPixIdx = -1;
        internalSetTimeDuration(hourVal);
    }

    private void internalSetTimeDuration(double hourVal) {
        setTimeDurationScale(hourVal);
        updateTimeDuration();
    }

    /**
     * Set the time duration and update by calling the callback action.
     * 
     * @param val
     */
    public void setTimeDurationAndUpdate(double hourVal) {
        if (hourVal != prevHourVal) {
            setTimeDurationScale(hourVal);
            updateTimeDurHours();
            determineSliderColor();
            timeSliderCanvas.redraw();
            actionCallback.timeDurationUpdated(timeDurHour, split);

            prevHourVal = hourVal;
        }
    }

    /**
     * Get the selected hours value.
     * 
     * @return The selected hours value.
     */
    public double getSelectedHoursValue() {
        return timeDurHour;
    }

    /**
     * Set the split value and update the thumb slider control.
     * 
     * @param split
     *            Split flag.
     */
    public void setSplit(boolean split) {
        this.split = split;
        determineSplitValue();
        updateTimeDuration();
    }
}
