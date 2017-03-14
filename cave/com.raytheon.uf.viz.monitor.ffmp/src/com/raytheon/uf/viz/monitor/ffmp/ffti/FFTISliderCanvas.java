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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * FFTI Slider Canvas
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2015 #5070      randerso    Changed to use system font name (not AWT)
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class FFTISliderCanvas {
    private Composite parentComp;

    private Display display;

    private Color blackColor;

    private Color canvasBgColor;

    /*
     * Canvas information
     */
    private Canvas canvas;

    private final int CANVAS_WIDTH = 460;

    private final int CANVAS_HEIGHT = 50;

    /*
     * Bar information.
     */
    private int barBottomYCoord = 45;

    private int barWidth = 400;

    private int barHeight = 10;

    private int barXCoord = 25;

    private int barYCoord = 35;

    private int barEndPixVal = barXCoord + barWidth;

    /*
     * Bar colors. These are not to be disposed because they are system colors.
     */
    private Color redColor;

    private Color yellowColor;

    private Color greenColor;

    /*
     * Upper arrow, label and values
     */
    private Region upperRegion;

    private int[] upperPtArray = new int[] { 0, 0 };

    private int upperArrowXCoord = 150;

    private Rectangle upperLblRect;

    private String upperStr;

    private int upperLblYCoord = 1;

    private boolean moveUpper = false;

    private Double upperDisplayVal = 0.0;

    /*
     * Lower arrow, label and values
     */
    private Region lowerRegion;

    private int[] lowerPtArray = new int[] { 0, 0 };

    private int lowerArrowXCoord = 50;

    private Rectangle lowerLblRect;

    private String lowerStr;

    private int lowerLblYCoord = 14;

    private boolean moveLower = false;

    private Double lowerDisplayVal = 0.0;

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
    private double minValue = Double.NaN;

    private double maxValue = Double.NaN;

    private double rangeValue = Double.NaN;

    private double incValue = Double.NaN;

    private double incPerPixel = Double.NaN;

    private boolean displayAtInt = false;

    private String formatStr;

    private FFTIAttribute attribVal;

    public FFTISliderCanvas(Composite parentComp, FFTIAttribute accumAttrib) {
        this.parentComp = parentComp;

        display = this.parentComp.getDisplay();

        this.attribVal = accumAttrib;
        setValuesNoRedraw(attribVal.getMin(), attribVal.getMax(),
                attribVal.getInc(), attribVal.getRedThreshold(),
                attribVal.getYellowThreshold());

        init();
        createCanvas();
    }

    public void updateSliderVals(FFTIAttribute fftiAttrib) {
        attribVal = fftiAttrib;
    }

    private void init() {
        redColor = display.getSystemColor(SWT.COLOR_RED);
        yellowColor = display.getSystemColor(SWT.COLOR_YELLOW);
        greenColor = display.getSystemColor(SWT.COLOR_GREEN);

        canvasBgColor = new Color(display, 120, 120, 110);
        blackColor = display.getSystemColor(SWT.COLOR_BLACK);

        upperLblRect = new Rectangle(0, 0, 0, 0);
        lowerLblRect = new Rectangle(0, 0, 0, 0);

        labelFont = new Font(display, "Monospace", 10, SWT.BOLD);
        mousePt = new Point(0, 0);
        upperRegion = new Region(display);
        lowerRegion = new Region(display);

        makeCalculations();
    }

    private void createCanvas() {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        // canvas = new Canvas(shell, SWT.NO_BACKGROUND);
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
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
                canvasBgColor.dispose();
            }
        });
    }

    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        gc.setFont(labelFont);
        gc.setBackground(canvasBgColor);

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setBackground(greenColor);
        gc.fillRectangle(barXCoord, barYCoord, barWidth, barHeight);

        gc.setBackground(yellowColor);
        gc.fillRectangle(lowerArrowXCoord, barYCoord, barWidth + 25
                - lowerArrowXCoord, barHeight);

        gc.setBackground(redColor);
        gc.fillRectangle(upperArrowXCoord, barYCoord, barWidth + 25
                - upperArrowXCoord, barHeight);

        gc.setForeground(blackColor);
        gc.drawRectangle(barXCoord, barYCoord, barWidth, barHeight);

        updateLowerArrow(gc);
        updateLowerLabel(gc);

        updateUpperArrow(gc);
        updateUpperLabel(gc);
    }

    private void updateUpperArrow(GC gc) {
        upperRegion.subtract(upperPtArray);
        upperPtArray = new int[] { upperArrowXCoord,
                barBottomYCoord - barHeight - 3, upperArrowXCoord + 4,
                barBottomYCoord, upperArrowXCoord - 4, barBottomYCoord };
        upperRegion.add(upperPtArray);

        gc.setBackground(redColor);
        gc.fillPolygon(upperPtArray);
        gc.setForeground(blackColor);
        gc.drawPolygon(upperPtArray);
    }

    private void updateUpperLabel(GC gc) {
        gc.setForeground(redColor);
        upperStr = calcDisplayString(upperDisplayVal);
        int lblXCoord = upperArrowXCoord - (textWidth * upperStr.length() / 2);
        gc.drawString(upperStr, lblXCoord, upperLblYCoord, true);
        upperLblRect.x = lblXCoord;
        upperLblRect.y = upperLblYCoord;
        upperLblRect.width = textWidth * upperStr.length();
        upperLblRect.height = textHeight;

        this.attribVal.setRedThreshold(upperDisplayVal);
    }

    private void updateLowerArrow(GC gc) {
        lowerRegion.subtract(lowerPtArray);
        lowerPtArray = new int[] { lowerArrowXCoord,
                barBottomYCoord - barHeight - 3, lowerArrowXCoord + 4,
                barBottomYCoord, lowerArrowXCoord - 4, barBottomYCoord };
        lowerRegion.add(lowerPtArray);

        gc.setBackground(yellowColor);
        gc.fillPolygon(lowerPtArray);
        gc.setForeground(blackColor);
        gc.drawPolygon(lowerPtArray);
    }

    private void updateLowerLabel(GC gc) {
        gc.setForeground(yellowColor);
        lowerStr = calcDisplayString(lowerDisplayVal);
        int lblXCoord = (int) (lowerArrowXCoord - ((double) textWidth
                * (double) lowerStr.length() / 2));
        gc.drawString(lowerStr, lblXCoord, lowerLblYCoord, true);
        lowerLblRect.x = lblXCoord;
        lowerLblRect.y = lowerLblYCoord;
        lowerLblRect.width = textWidth * lowerStr.length();
        lowerLblRect.height = textHeight;

        this.attribVal.setYellowThreshold(lowerDisplayVal);
    }

    private void handleMouseMove(MouseEvent e) {
        if (mouseDown == false) {
            return;
        }

        if (moveUpper == true) {
            int xDiff = e.x - mousePt.x;
            upperArrowXCoord += xDiff;

            if (upperArrowXCoord > barEndPixVal) {
                upperArrowXCoord = barEndPixVal;
            } else if (upperArrowXCoord < barXCoord) {
                upperArrowXCoord = barXCoord;
            } else if (upperArrowXCoord < lowerArrowXCoord) {
                upperArrowXCoord = lowerArrowXCoord;
            }

            upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        } else if (moveLower == true) {
            int xDiff = e.x - mousePt.x;
            lowerArrowXCoord += xDiff;

            if (lowerArrowXCoord > barEndPixVal) {
                lowerArrowXCoord = barEndPixVal;
            } else if (lowerArrowXCoord < barXCoord) {
                lowerArrowXCoord = barXCoord;
            } else if (lowerArrowXCoord > upperArrowXCoord) {
                lowerArrowXCoord = upperArrowXCoord;
            }
            lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
        }

        mousePt.x = e.x;
        mousePt.y = e.y;
        canvas.redraw();
    }

    private void makeCalculations() {
        Image image = new Image(display, 100, 100);
        GC gc = new GC(image);
        gc.setFont(labelFont);

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();

        gc.dispose();
        image.dispose();
    }

    private double calcDisplayValue(int xCoord) {

        // calculate the display value to .10 or .05 increments

        double xCoordAsValue = (xCoord - barXCoord) * incPerPixel + minValue;

        if (xCoordAsValue >= this.maxValue) {
            return this.maxValue;
        }

        if (incValue == .25) {
            return (Math.round(xCoordAsValue * 4.00)) / 4.00;
        } else if (incValue == .05) {
            return Math.round(xCoordAsValue * 20.0) / 20.0;
        } else if (incValue == .10) {
            return (Math.round(xCoordAsValue * 10.00)) / 10.00;
        } else {
            return Math.round(xCoordAsValue);
        }
    }

    private String calcDisplayString(Double displVal) {
        if (displayAtInt == true) {
            return String.format(formatStr, Math.round(displVal));
        } else {
            return String.format(formatStr, displVal);
        }
    }

    private int calcValueToBarXCoord(double val) {
        int result = (int) Math.round((val - minValue) / incPerPixel
                + barXCoord);

        return result;
    }

    private void setValuesNoRedraw(double min, double max, double inc,
            double startingUpperVal, double startingLowerVal) {
        if (max < min) {
            return;
        }

        this.maxValue = max;
        this.minValue = min;
        this.rangeValue = max - min;
        this.incValue = inc;

        incPerPixel = this.rangeValue / barWidth;

        if (inc < 1.00) {
            displayAtInt = false;
            formatStr = "%1.2f";
        } else {
            displayAtInt = true;
            formatStr = "%d";
        }

        upperArrowXCoord = calcValueToBarXCoord(startingUpperVal);
        lowerArrowXCoord = calcValueToBarXCoord(startingLowerVal);

        upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
    }

    public void setValues(FFTIAttribute attribVal) {
        this.attribVal = attribVal;
        setValues(attribVal.getMin(), attribVal.getMax(), attribVal.getInc(),
                attribVal.getRedThreshold(), attribVal.getYellowThreshold());
    }

    public void setValues(double min, double max, double inc,
            double startingUpperVal, double startingLowerVal) {
        setValuesNoRedraw(min, max, inc, startingUpperVal, startingLowerVal);
        canvas.redraw();
    }

    public void setMaxAndIncValues(double newMaxValue, double newIncValue) {
        this.maxValue = newMaxValue;
        this.incValue = newIncValue;

        double startingUpperVal = Double.NaN;
        double startingLowerVal = Double.NaN;

        if (lowerDisplayVal > maxValue) {
            startingUpperVal = maxValue;
            startingLowerVal = maxValue;

            setValues(this.minValue, this.maxValue, this.incValue,
                    startingUpperVal, startingLowerVal);
        } else if (upperDisplayVal > maxValue) {
            startingUpperVal = maxValue;
            setValues(this.minValue, this.maxValue, this.incValue,
                    startingUpperVal, getLowerValue());
        } else {
            setValues(this.minValue, this.maxValue, this.incValue,
                    getUpperValue(), getLowerValue());
        }
    }

    public double getUpperValue() {
        return upperDisplayVal;
    }

    public double getLowerValue() {
        return lowerDisplayVal;
    }
}
