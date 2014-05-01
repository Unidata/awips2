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
package com.raytheon.viz.ui.widgets;

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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * A two value slider widget.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2013   2430     mpduff      Initial version.
 * 
 * </pre>
 */

public class TwoValueSliderCanvas {
    /** Parent composite */
    private final Composite parentComp;

    /** The display */
    private final Display display;

    /** The lower range color */
    private Color lowRangeColor;

    /** The middle range color */
    private Color midRangeColor;

    /** The upper range color */
    private Color upperRangeColor;

    /*
     * Canvas information
     */
    private Canvas canvas;

    private final int CANVAS_WIDTH = 255;

    private final int CANVAS_HEIGHT = 70;

    /*
     * Bar information.
     */
    private final int barBottomYCoord = 45;

    private final int barWidth = 200;

    private final int barHeight = 10;

    private final int barXCoord = 25;

    private final int barYCoord = 35;

    /*
     * Upper arrow, label and values
     */
    private Region upperRegion;

    private int[] upperPtArray = new int[] { 0, 0 };

    private int upperArrowXCoord = 150;

    private Rectangle upperLblRect;

    private String upperStr;

    private final int upperLblYCoord = 1;

    private boolean moveUpper = false;

    private int upperDisplayVal = 0;

    /*
     * Lower arrow, label and values
     */
    private Region lowerRegion;

    private int[] lowerPtArray = new int[] { 0, 0 };

    private int lowerArrowXCoord = 50;

    private Rectangle lowerLblRect;

    private String lowerStr;

    private final int lowerLblYCoord = 14;

    private boolean moveLower = false;

    private int lowerDisplayVal = 0;

    /*
     * Mouse information.
     */
    private Point mousePt;

    private boolean mouseDown = false;

    /*
     * Font/text information
     */
    private Font labelFont;

    private int textWidth = 0;

    private int textHeight = 0;

    /*
     * Min/Range/Increment information
     */
    private int minValue = Integer.MIN_VALUE;

    private int rangeValue = Integer.MIN_VALUE;

    private int incValue = Integer.MIN_VALUE;

    private double incPerPixel = Double.NaN;

    private String formatStr;

    private int minValLblPixWidth = 0;

    private int maxValLblPixWidth = 0;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The parent composite
     * @param min
     *            The minimum value
     * @param range
     *            The range
     * @param inc
     *            The increment
     * @param startingLowerVal
     *            The starting lower value
     * @param startingUpperVal
     *            The starting upper value
     */
    public TwoValueSliderCanvas(Composite parentComp, int min, int range,
            int inc, int startingLowerVal, int startingUpperVal) {
        this(parentComp, min, range, inc, startingLowerVal, startingUpperVal,
                null, null, null);
    }

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The parent composite
     * @param min
     *            The minimum value
     * @param range
     *            The range
     * @param inc
     *            The increment
     * @param startingLowerVal
     *            The starting lower value
     * @param startingUpperVal
     *            The starting upper value
     * @param lowerRGB
     *            Low range RGB color
     * @param midRGB
     *            Mid range RGB color
     * @param upperRGB
     *            High range RGB color
     */
    public TwoValueSliderCanvas(Composite parentComp, int min, int range,
            int inc, int startingLowerVal, int startingUpperVal, RGB lowerRGB,
            RGB midRGB, RGB upperRGB) {
        this.parentComp = parentComp;

        display = this.parentComp.getDisplay();

        setValues(min, range, inc, startingLowerVal, startingUpperVal);

        createColors(lowerRGB, midRGB, upperRGB);
        calcMinMaxLabelXCoords();
        init();
        createCanvas();
    }

    /**
     * Create the colors.
     * 
     * @param lowerRGB
     * @param midRGB
     * @param upperRGB
     */
    private void createColors(RGB lowerRGB, RGB midRGB, RGB upperRGB) {
        if (lowerRGB == null) {
            lowRangeColor = new Color(display, display.getSystemColor(
                    SWT.COLOR_GREEN).getRGB());
        } else {
            lowRangeColor = new Color(display, lowerRGB);
        }

        if (midRGB == null) {
            midRangeColor = new Color(display, display.getSystemColor(
                    SWT.COLOR_YELLOW).getRGB());
        } else {
            midRangeColor = new Color(display, midRGB);
        }

        if (upperRGB == null) {
            upperRangeColor = new Color(display, display.getSystemColor(
                    SWT.COLOR_RED).getRGB());
        } else {
            upperRangeColor = new Color(display, upperRGB);
        }
    }

    /**
     * Initialize
     */
    private void init() {
        upperLblRect = new Rectangle(0, 0, 0, 0);
        lowerLblRect = new Rectangle(0, 0, 0, 0);

        labelFont = new Font(display, "Monospaced", 10, SWT.BOLD);
        mousePt = new Point(0, 0);
        upperRegion = new Region(display);
        lowerRegion = new Region(display);

        makeCalculations();
    }

    /**
     * Create the canvas.
     */
    private void createCanvas() {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        canvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (upperRegion.contains(e.x, e.y) == true
                        || upperLblRect.contains(e.x, e.y) == true) {
                    mousePt.x = e.x;
                    mousePt.y = e.y;
                    mouseDown = true;
                    moveUpper = true;
                } else if (lowerRegion.contains(e.x, e.y) == true
                        || lowerLblRect.contains(e.x, e.y) == true) {
                    mousePt.x = e.x;
                    mousePt.y = e.y;
                    mouseDown = true;
                    moveLower = true;
                }
            }

            @Override
            public void mouseUp(MouseEvent e) {
                mouseDown = false;
                moveLower = false;
                moveUpper = false;
            }
        });

        canvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                handleMouseMove(e);
            }
        });

        canvas.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                upperRegion.dispose();
                lowerRegion.dispose();
                labelFont.dispose();
                lowRangeColor.dispose();
                midRangeColor.dispose();
                upperRangeColor.dispose();
            }
        });
    }

    /**
     * Draw the canvas.
     * 
     * @param gc
     */
    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        gc.setFont(labelFont);
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setBackground(lowRangeColor);
        gc.fillRectangle(barXCoord, barYCoord, barWidth, barHeight);

        gc.setBackground(midRangeColor);
        gc.fillRectangle(lowerArrowXCoord, barYCoord, barWidth + 25
                - lowerArrowXCoord, barHeight);

        gc.setBackground(upperRangeColor);
        gc.fillRectangle(upperArrowXCoord, barYCoord, barWidth + 25
                - upperArrowXCoord, barHeight);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(barXCoord, barYCoord, barWidth, barHeight);

        gc.drawString(String.valueOf(minValue) + "%", barXCoord
                - (minValLblPixWidth / 2), barYCoord + 10, true);
        gc.drawString(String.valueOf(minValue + rangeValue) + "%", barXCoord
                + barWidth - (maxValLblPixWidth / 2), barYCoord + 10, true);

        updateLowerArrow(gc);
        updateLowerLabel(gc);

        updateUpperArrow(gc);
        updateUpperLabel(gc);
    }

    /**
     * Update the upper arrow.
     * 
     * @param gc
     */
    private void updateUpperArrow(GC gc) {
        upperRegion.subtract(upperPtArray);
        upperPtArray = new int[] { upperArrowXCoord,
                barBottomYCoord - barHeight - 3, upperArrowXCoord + 4,
                barBottomYCoord, upperArrowXCoord - 4, barBottomYCoord };
        upperRegion.add(upperPtArray);

        gc.setBackground(upperRangeColor);
        gc.fillPolygon(upperPtArray);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawPolygon(upperPtArray);
    }

    /**
     * Update the lower label.
     * 
     * @param gc
     */
    private void updateUpperLabel(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        upperStr = createDisplayString(upperDisplayVal);
        int lblXCoord = (int) (upperArrowXCoord - textWidth
                * (double) upperStr.length() / 2.0);
        gc.drawString(upperStr + "%", lblXCoord, upperLblYCoord, true);
        upperLblRect.x = lblXCoord;
        upperLblRect.y = upperLblYCoord;
        upperLblRect.width = textWidth * upperStr.length() + textWidth * 2;
        upperLblRect.height = textHeight;
    }

    /**
     * Update lower arrow.
     * 
     * @param gc
     */
    private void updateLowerArrow(GC gc) {
        lowerRegion.subtract(lowerPtArray);
        lowerPtArray = new int[] { lowerArrowXCoord,
                barBottomYCoord - barHeight - 3, lowerArrowXCoord + 4,
                barBottomYCoord, lowerArrowXCoord - 4, barBottomYCoord };
        lowerRegion.add(lowerPtArray);

        gc.setBackground(midRangeColor);
        gc.fillPolygon(lowerPtArray);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawPolygon(lowerPtArray);
    }

    /**
     * Update lower label
     * 
     * @param gc
     */
    private void updateLowerLabel(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        lowerStr = createDisplayString(lowerDisplayVal);
        int lblXCoord = (int) (lowerArrowXCoord - Math.round((double) textWidth
                * (double) lowerStr.length() / 2.0));
        gc.drawString(lowerStr + "%", lblXCoord, lowerLblYCoord, true);
        lowerLblRect.x = lblXCoord;
        lowerLblRect.y = lowerLblYCoord;
        lowerLblRect.width = textWidth * lowerStr.length() + textWidth * 2;
        lowerLblRect.height = textHeight;
    }

    /**
     * Mouse mover handler.
     * 
     * @param e
     */
    private void handleMouseMove(MouseEvent e) {
        if (mouseDown == false) {
            return;
        }

        if (moveUpper == true) {
            int xDiff = e.x - mousePt.x;
            upperArrowXCoord += xDiff;

            if (upperArrowXCoord > 225) {
                upperArrowXCoord = 225;
            } else if (upperArrowXCoord < 25) {
                upperArrowXCoord = 25;
            } else if (upperArrowXCoord < lowerArrowXCoord) {
                upperArrowXCoord = lowerArrowXCoord;
            }

            upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        } else if (moveLower == true) {
            int xDiff = e.x - mousePt.x;
            lowerArrowXCoord += xDiff;

            if (lowerArrowXCoord > 225) {
                lowerArrowXCoord = 225;
            } else if (lowerArrowXCoord < 25) {
                lowerArrowXCoord = 25;
            } else if (lowerArrowXCoord > upperArrowXCoord) {
                lowerArrowXCoord = upperArrowXCoord;
            }
            lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
        }

        mousePt.x = e.x;
        mousePt.y = e.y;
        canvas.redraw();
    }

    /**
     * Determine text height and width.
     */
    private void makeCalculations() {
        GC gc = new GC(parentComp);
        gc.setFont(labelFont);

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();

        gc.dispose();
    }

    /**
     * Calculate the display value for the provided x coordinate.
     * 
     * @param xCoord
     * @return
     */
    private int calcDisplayValue(int xCoord) {
        double xCoordAsValue = (xCoord - barXCoord) * incPerPixel + minValue;
        if (incValue == .25) {
            return (int) ((Math.round(xCoordAsValue * 4.00)) / 4.00);
        } else if (incValue == .10) {
            return (int) ((Math.round(xCoordAsValue * 10.00)) / 10.00);
        } else {
            return (int) Math.round(xCoordAsValue);
        }
    }

    /**
     * Calculate the min/max pixel values.
     */
    private void calcMinMaxLabelXCoords() {
        GC gc = new GC(parentComp);
        gc.setFont(labelFont);

        Point ext = gc.stringExtent(String.valueOf(minValue));
        minValLblPixWidth = ext.x;

        ext = gc.stringExtent(String.valueOf(minValue + rangeValue));
        maxValLblPixWidth = ext.x;

        gc.dispose();
    }

    /**
     * Create the display string.
     * 
     * @param displVal
     * @return
     */
    private String createDisplayString(int displVal) {
        return String.format(formatStr, displVal);
    }

    /**
     * Calculate the bar value to x coordinate value.
     * 
     * @param val
     * @return
     */
    private int calcValueToBarXCoord(double val) {
        int result = (int) Math.round((val - minValue) / incPerPixel
                + barXCoord);

        return result;
    }

    /**
     * Set the values.
     * 
     * @param min
     * @param range
     * @param inc
     * @param startingLowerVal
     * @param startingUpperVal
     */
    private void setValues(int min, int range, int inc, int startingLowerVal,
            int startingUpperVal) {
        this.minValue = min;
        this.rangeValue = range;
        this.incValue = inc;

        if (startingUpperVal < startingLowerVal) {
            startingUpperVal = startingLowerVal;
        }

        if (startingLowerVal < min || startingLowerVal > (min + range)) {
            startingLowerVal = min;
        }

        if (startingUpperVal < min || startingUpperVal > (min + range)) {
            startingUpperVal = min + range;
        }

        incPerPixel = this.rangeValue / (double) barWidth;

        formatStr = "%d";

        upperArrowXCoord = calcValueToBarXCoord(startingUpperVal);
        lowerArrowXCoord = calcValueToBarXCoord(startingLowerVal);

        upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
    }

    /**
     * Get the upper value.
     * 
     * @return the upper value
     */
    public int getUpperValue() {
        return upperDisplayVal;
    }

    /**
     * Get the lower value
     * 
     * @return The lower value
     */
    public int getLowerValue() {
        return lowerDisplayVal;
    }
}
