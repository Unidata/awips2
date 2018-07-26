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

import java.text.DecimalFormat;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

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
 * Feb 15, 2016      #5378 randerso    Fix label spacing in TimeDurScale
 *                                     Major code restructure
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class TimeDurScale extends Canvas implements PaintListener {
    private class MouseListener extends MouseAdapter implements
            MouseMoveListener {
        /**
         * Flag to indicate we are dragging the thumb
         */
        private boolean dragging = false;

        /**
         * Offset from mouse click to center of thumb. Used to keep cursor over
         * the spot on thumb where you clicked
         */
        private int dragOffset;

        @Override
        public void mouseDown(MouseEvent e) {
            if (e.button == 1) {

                // if in thumb
                if (thumbRect.contains(e.x, e.y)) {
                    dragging = true;
                    dragOffset = e.x - (thumbRect.x + thumbRect.width / 2);

                } else {
                    if (e.x < thumbRect.x) {
                        // Decrement the thumb slider control.
                        setTimeDuration(timeDurHour - 0.25);
                    } else {
                        // Increment the thumb slider control.
                        setTimeDuration(timeDurHour + 0.25);
                    }
                }
            }
        }

        @Override
        public void mouseUp(MouseEvent e) {
            dragging = false;
            actionCallback.timeDurationUpdated(timeDurHour, split);
        }

        @Override
        public void mouseMove(MouseEvent e) {
            if (dragging) {
                // Move the thumb control
                int x = e.x - dragOffset;

                double hourVal = computeHour(x);
                if (hourVal < lowerHourRangeVal) {
                    hourVal = lowerHourRangeVal;
                } else if (hourVal > upperHourRangeVal) {
                    hourVal = upperHourRangeVal;
                }
                setTimeDuration(hourVal);
            }
        }
    }

    private static final double SLIDER_HEIGHT = 0.2; // inches

    /**
     * Font used for the time labels.
     */
    private Font canvasFont;

    /**
     * Time duration hour.
     */
    private double timeDurHour;

    private DecimalFormat labelFormat = new DecimalFormat("0.00");

    /**
     * Height of the slider bar.
     */
    private int sliderHeight;

    /**
     * Height of the label text.
     */
    private int labelHeight;

    /**
     * Width of a single label text.
     */
    private int labelWidth;

    /**
     * Spacing between labels
     */
    private int labelSpacing;

    /**
     * Used to center labels on the decimal point
     */
    private int labelOffset;

    /**
     * Lower hour range value (always zero).
     */
    private final int lowerHourRangeVal = 0;

    /**
     * Upper hour range value.
     */
    private int upperHourRangeVal = 24;

    /**
     * Action callback.
     */
    private ITimeDurationAction actionCallback;

    /**
     * Split flag.
     */
    private boolean split = false;

    /**
     * Values for scale labels
     */
    private double[] labelValues;

    /**
     * Rectangle that defines the bound of the thumb slider.
     */
    private Rectangle thumbRect;

    /**
     * Rectangle that defines the bound of the slider bar.
     */
    private Rectangle timeBarRect;

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

    private double pixPerHour;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param actionCallback
     *            Action callback.
     */
    public TimeDurScale(Composite parent, ITimeDurationAction actionCallback) {
        super(parent, SWT.DOUBLE_BUFFERED);
        this.actionCallback = actionCallback;
        this.upperHourRangeVal = validateHours(24);

        /*
         * Get the split flag.
         */
        split = FFMPConfig.getInstance().isSplit();

        /*
         * Create the font
         */
        FontData fd = getFont().getFontData()[0];
        fd.setName("Monospace");
        canvasFont = new Font(getDisplay(), fd);

        Display display = getDisplay();
        GC gc = new GC(display);
        gc.setFont(canvasFont);
        Point labelExtent = gc
                .textExtent(labelFormat.format(upperHourRangeVal));
        labelWidth = labelExtent.x;
        labelHeight = labelExtent.y;
        labelSpacing = gc.textExtent(" ").x * 3 / 2;
        labelOffset = (int) Math.ceil(gc.textExtent("00.00").x / 2.0);
        gc.dispose();

        sliderHeight = (int) (SLIDER_HEIGHT * display.getDPI().y);

        /*
         * Create the time bar colors
         */
        thumbMiddleColor = new Color(getDisplay(), new RGB(220, 220, 240));
        thumbBgColor = new Color(getDisplay(), new RGB(100, 100, 250));
        greyColor = new Color(getDisplay(), new RGB(250, 250, 250));
        yellowColor = new Color(getDisplay(), new RGB(255, 255, 0));
        timeBarColor = greyColor;

        /*
         * Create the hours array and the controls for the time slider/labels.
         */
        addPaintListener(this);
        addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                resize();
            }
        });

        /*
         * Add the mouse listener
         */
        MouseListener mouseListener = new MouseListener();
        addMouseListener(mouseListener);
        addMouseMoveListener(mouseListener);

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

        setTimeDuration(1.0);
    }

    @Override
    public Point computeSize(int wHint, int hHint, boolean changed) {
        int width = labelWidth * 2 + labelSpacing;
        int height = labelHeight * 2 + sliderHeight;
        return new Point(width, height);
    }

    private void resize() {

        int thumbHeight = sliderHeight * 2 / 3;
        int thumbWidth = thumbHeight * 45 / 12;

        thumbRect = new Rectangle(0, labelHeight + (sliderHeight - thumbHeight)
                / 2, thumbWidth, thumbHeight);

        timeBarRect = new Rectangle(0, labelHeight
                + (sliderHeight - thumbHeight / 2) / 2, getSize().x - 1,
                thumbHeight / 2);

        pixPerHour = (timeBarRect.width - thumbRect.width)
                / (double) (upperHourRangeVal - lowerHourRangeVal);

        calcDisplayNumbers(timeBarRect.width);
    }

    @Override
    public void paintControl(PaintEvent event) {
        GC gc = event.gc;
        gc.setFont(canvasFont);

        // Clear and refill with base color
        gc.setBackground(getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(getBounds());

        drawSelectedValueLabel(gc);
        drawTimeSlider(gc);
        drawTimeScaleLabels(gc);
    }

    private int computeXcoord(double hour) {
        return (int) Math.round(hour * pixPerHour) + thumbRect.width / 2;
    }

    private double computeHour(int xCoord) {
        return (xCoord - thumbRect.width / 2) / pixPerHour;
    }

    /**
     * Draw the time label that moves with the "thumb" control. It indicates
     * what time the thumb control is currently at.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawSelectedValueLabel(GC gc) {
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        String label = labelFormat.format(this.timeDurHour);
        int width = gc.textExtent(label).x;
        int x = computeXcoord(this.timeDurHour) - width + labelOffset;

        gc.drawString(label, x, 0, true);
    }

    private void drawTimeSlider(GC gc) {
        // draw tick marks
        int i = 0;
        for (double hour = lowerHourRangeVal; hour <= upperHourRangeVal; hour += 1.0) {
            int x = computeXcoord(hour);
            if (hour == labelValues[i]) {
                i++;
                gc.drawLine(x, labelHeight, x, labelHeight + sliderHeight);
            } else {
                gc.drawLine(x, thumbRect.y, x, thumbRect.y + thumbRect.height);
            }
        }

        // Draw the "slider" bar
        gc.setBackground(timeBarColor);
        gc.fillRoundRectangle(timeBarRect.x, timeBarRect.y, timeBarRect.width,
                timeBarRect.height, timeBarRect.height, timeBarRect.height);
        gc.drawRoundRectangle(timeBarRect.x, timeBarRect.y, timeBarRect.width,
                timeBarRect.height, timeBarRect.height, timeBarRect.height);

        // Draw "thumb" control
        gc.setBackground(thumbBgColor);

        thumbRect.x = computeXcoord(this.timeDurHour) - thumbRect.width / 2;

        gc.fillRoundRectangle(thumbRect.x, thumbRect.y, thumbRect.width,
                thumbRect.height, thumbRect.height, thumbRect.height);
        gc.drawRoundRectangle(thumbRect.x, thumbRect.y, thumbRect.width,
                thumbRect.height, thumbRect.height, thumbRect.height);

        gc.setBackground(thumbMiddleColor);
        int dx = thumbRect.width / 4;
        gc.fillRectangle(thumbRect.x + dx, thumbRect.y, thumbRect.width - dx
                * 2, thumbRect.height);
        gc.drawRectangle(thumbRect.x + dx, thumbRect.y, thumbRect.width - dx
                * 2, thumbRect.height);
    }

    /**
     * Draw the time labels below the thumb slider on the canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawTimeScaleLabels(GC gc) {
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        for (double dVal : labelValues) {
            String label = labelFormat.format(dVal);
            int width = gc.textExtent(label).x;

            int x = computeXcoord(dVal) - width + labelOffset;

            gc.drawString(label, x, labelHeight + sliderHeight, true);
        }
    }

    /**
     * Determine if we have to adjust for the split value.
     */
    private void determineSplitValue() {
        if (split) {
            if (timeDurHour < 1.00) {
                setTimeDuration(1.00);
            }
        }
    }

    /**
     * Determine the slider color.
     */
    private void determineSliderColor() {
        if (timeDurHour == 0.0 || split) {
            timeBarColor = yellowColor;
        } else {
            timeBarColor = greyColor;
        }
    }

    private void calcDisplayNumbers(int barWidth) {
        int totalDur = upperHourRangeVal - lowerHourRangeVal;
        int labelInc = 1;
        int numLabels = totalDur + 1;
        while (numLabels * (labelWidth + labelSpacing) > barWidth) {
            labelInc++;
            if (totalDur % labelInc != 0) {
                continue;
            }

            numLabels = totalDur / labelInc + 1;

            if (numLabels == 2) {
                break;
            }
        }

        double[] labelValues = new double[numLabels];
        for (int i = 0; i < numLabels - 1; i++) {
            labelValues[i] = lowerHourRangeVal + labelInc * i;
        }
        labelValues[numLabels - 1] = upperHourRangeVal;

        if (!Arrays.equals(this.labelValues, labelValues)) {
            this.labelValues = labelValues;
        }
    }

    private int validateHours(int hours) {
        if (hours < 12) {
            return 12;
        }

        if (hours > 36) {
            return 36;
        }

        return hours;
    }

    private double roundToQuarterHour(double hourVal) {
        return Math.round(hourVal * 4.0) / 4.0;
    }

    /**
     * Set the time duration to the specified hour argument.
     * 
     * @param hourVal
     *            Hour value.
     */
    public void setTimeDuration(double hourVal) {
        double roundedHourVal = roundToQuarterHour(hourVal);
        if (roundedHourVal != this.timeDurHour) {
            this.timeDurHour = roundedHourVal;
            determineSliderColor();
            redraw();
        }
    }

    /**
     * Set the time duration and update by calling the callback action.
     * 
     * @param hourVal
     */
    public void setTimeDurationAndUpdate(double hourVal) {
        if (hourVal != this.timeDurHour) {
            setTimeDuration(hourVal);
            actionCallback.timeDurationUpdated(timeDurHour, split);
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
    }

    /**
     * @return true if is split
     */
    public boolean isSplit() {
        return this.split;
    }
}
