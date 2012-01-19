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
package com.raytheon.viz.aviation.climatology;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;

/**
 * TrendWindSpdCanvasComp class draws the slider information on the wind speed
 * canvas and handles the user inputs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TrendWindSpdCanvasComp extends TrendSliderComp implements
        MouseMoveListener, MouseListener {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Mouse is down flag.
     */
    private boolean mouseDown = false;

    /**
     * Move arrow flag.
     */
    private boolean moveArrow = false;

    /**
     * Move the left side of the range flag.
     */
    private boolean moveBarLeftSide = false;

    /**
     * Move the right side of the range flag.
     */
    private boolean moveBarRightSide = false;

    /**
     * Move the arrow and range flag.
     */
    private boolean moveAll = false;

    /**
     * X coordinate of the 0 value.
     */
    private int xCoord0 = hLineStartXCoord;

    /**
     * X coordinate of the 4 value.
     */
    private int xCoord4 = xCoord0 + 50;

    /**
     * X coordinate of the 10 value.
     */
    private int xCoord10 = xCoord4 + 70;

    /**
     * X coordinate of the 30 value.
     */
    private int xCoord30 = xCoord10 + 130;

    /**
     * X coordinate of the 31+ value.
     */
    private int xCoord31Plus = hLineEndXCoord;

    /**
     * X coordinate difference between 31+ and 30.
     */
    private int xCoord30_31Diff = ((xCoord31Plus - xCoord30) / 2) + xCoord30;

    /**
     * Value to pixel factor from 0 to 4.
     */
    private double factor0to4 = 4.0 / (xCoord4 - xCoord0);

    /**
     * Value to pixel factor from 4 to 10.
     */
    private double factor4to10 = 6.0 / (xCoord10 - xCoord4);

    /**
     * Value to pixel factor from 10 to 30.
     */
    private double factor10to30 = 20.0 / (xCoord30 - xCoord10);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public TrendWindSpdCanvasComp(Composite parent) {
        super(parent, "Wind Speed");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        arrowCenterXCoord = 150;

        barWidth = 100;

        barRect = new Rectangle(xCoord4, hLineYcoord + 3, barWidth, 15);

        barRightXCoord = barWidth + barRect.x;

        moveAll(150);

        drawingCanvas.addMouseListener(this);
        drawingCanvas.addMouseMoveListener(this);

        validateValueListeners();
        validateRangeListeners();
    }

    /**
     * Draw the dial information on the wind speed canvas.
     * 
     * @param gc
     *            Graphical context.
     */
    @Override
    public void drawCanvas(GC gc) {
        int fontHeight = gc.getFontMetrics().getHeight();
        int fontWidth = gc.getFontMetrics().getAverageCharWidth();

        // ---------------------------------------
        // Draw the background color.
        // ---------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, canvasWidth + 5, canvasHeight + 5);

        // -------------------------------------------
        // Draw the horizontal line and hash marks
        // -------------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // Draw horizontal line.
        gc.drawLine(hLineStartXCoord, hLineYcoord, hLineEndXCoord, hLineYcoord);

        // Draw hash marks.
        gc.drawLine(xCoord0, hLineYcoord, xCoord0, hLineYcoord - dashLine);
        gc.drawLine(xCoord4, hLineYcoord, xCoord4, hLineYcoord - dashLine);
        gc.drawLine(xCoord10, hLineYcoord, xCoord10, hLineYcoord - dashLine);
        gc.drawLine(xCoord30, hLineYcoord, xCoord30, hLineYcoord - dashLine);
        gc.drawLine(xCoord31Plus, hLineYcoord, xCoord31Plus, hLineYcoord
                - dashLine);

        // ---------------------------------------------
        // Draw the labels
        // ---------------------------------------------
        gc.drawString("0", xCoord0 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("4", xCoord4 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("10", xCoord10 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("30", xCoord30 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("31+", xCoord31Plus - (fontWidth * 3) / 2 - 2,
                hLineYcoord - dashLine - 3 - fontHeight, true);

        // -------------------------------------------------
        // Draw the range rectangle and blue arrow marker
        // -------------------------------------------------

        // Draw blue arrow.
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        calcArrow();
        gc.fillPolygon(arrowPoints);

        // Draw red rectangle range.
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.setAlpha(80);
        gc.fillRectangle(barRect);
        gc.setAlpha(255);
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.setLineWidth(2);
        gc.drawRectangle(barRect);
    }

    /**
     * Calculate the arrow position.
     */
    private void calcArrow() {
        if (region.isEmpty() == false) {
            region.subtract(arrowPoints);
        }

        arrowPoints = new int[] { arrowCenterXCoord, arrowBottomYCoord,
                arrowCenterXCoord + 5, arrowTopYCoord, arrowCenterXCoord - 5,
                arrowTopYCoord, arrowCenterXCoord, arrowBottomYCoord };

        region.add(arrowPoints);
    }

    /**
     * Not Implemented...
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
    }

    /**
     * Called when a mouse button is pressed.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        if (region.contains(e.x, e.y)) {
            if (e.button == 2) {
                moveAll = true;
                mouseDown = true;
            } else if (e.button == 1) {
                mouseDown = true;
                moveArrow = true;
            }
            return;
        }

        if (barRect.contains(e.x, e.y)) {
            if (e.x <= barRect.x + 2) {
                moveBarLeftSide = true;
                mouseDown = true;
            } else if (e.x >= barRightXCoord - 2) {
                moveBarRightSide = true;
                mouseDown = true;
            }
        }
    }

    /**
     * Called when a mouse button is released.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseUp(MouseEvent e) {
        mouseDown = false;
        moveArrow = false;
        moveBarLeftSide = false;
        moveBarRightSide = false;
        moveAll = false;
    }

    /**
     * Called when the mouse is moved over the control.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (mouseDown == false) {
            return;
        }

        if (moveArrow == true) {
            moveArrow(e.x);
        } else if (moveBarLeftSide == true) {
            moveLeftSideOfRange(e.x);
        } else if (moveBarRightSide == true) {
            moveRightSideOfRange(e.x);
        } else if (moveAll == true) {
            if (e.x <= hLineStartXCoord || e.x >= hLineEndXCoord) {
                return;
            }

            moveAll(e.x);
        }

        drawingCanvas.redraw();
    }

    /**
     * Move the arrow.
     * 
     * @param xCoord
     *            X coordinate to move to.
     */
    private void moveArrow(int xCoord) {
        arrowCenterXCoord = calcNewXCoord(xCoord);

        Double val = calcXCoordToValue(arrowCenterXCoord);

        setValueText(val);
    }

    /**
     * Move the starting range (left side of bar).
     * 
     * @param xCoord
     *            X coordinate to move to.
     */
    private void moveLeftSideOfRange(int xCoord) {
        int newXCoord = calcNewXCoord(xCoord);

        if (newXCoord >= barRightXCoord) {
            return;
        }

        barRect.width = barRightXCoord - newXCoord;
        barRect.x = newXCoord;

        Double startVal = calcXCoordToValue(barRect.x);
        Double endVal = calcXCoordToValue(barRightXCoord);

        setRangeText(startVal, endVal);
    }

    /**
     * Move the ending range (right side of bar).
     * 
     * @param xCoord
     *            X coordinate to move to.
     */
    private void moveRightSideOfRange(int xCoord) {
        int newXCoord = calcNewXCoord(xCoord);

        if (newXCoord <= barRect.x) {
            return;
        }

        barRightXCoord = newXCoord;
        barRect.width = barRightXCoord - barRect.x;

        Double startVal = calcXCoordToValue(barRect.x);
        Double endVal = calcXCoordToValue(barRightXCoord);

        setRangeText(startVal, endVal);
    }

    /**
     * Move the arrow and the range.
     * 
     * @param xCoord
     */
    private void moveAll(int xCoord) {
        if (xCoord > xCoord30) {
            return;
        }

        int currArrowX = arrowCenterXCoord;
        int currBarLeftX = barRect.x;
        int currBarRightX = barRightXCoord;
        int offset = xCoord - arrowCenterXCoord;

        if (currArrowX + offset < hLineStartXCoord
                || currBarLeftX + offset < hLineStartXCoord) {
            return;
        }

        if (currArrowX + offset >= hLineEndXCoord) {
            return;
        }

        if (currBarRightX != xCoord31Plus) {
            if (currBarRightX + offset > xCoord30) {
                return;
            }
        }

        moveArrow(xCoord);
        moveLeftSideOfRange(currBarLeftX + offset);

        if (barRightXCoord != xCoord31Plus) {
            moveRightSideOfRange(currBarRightX + offset);
        }
    }

    /**
     * Calculate the new X coordinate based on the X coordinate passed in.
     * 
     * @param xCoord
     *            New X coordinate.
     * @return Adjusted X coordinate.
     */
    private int calcNewXCoord(int xCoord) {
        int newXCoord = xCoord;

        if (xCoord < xCoord0) {
            newXCoord = xCoord0;
        } else if (xCoord > xCoord31Plus) {
            newXCoord = xCoord31Plus;
        } else if (xCoord >= xCoord30 && xCoord < xCoord30_31Diff) {
            newXCoord = xCoord30;
        } else if (xCoord >= xCoord30_31Diff && xCoord <= xCoord31Plus) {
            newXCoord = xCoord31Plus;
        } else {
            newXCoord = xCoord;
        }

        return newXCoord;
    }

    /**
     * Calculate the X coordinate to a display value.
     * 
     * @param xCoord
     *            X coordinate.
     * @return Wind speed value.
     */
    private Double calcXCoordToValue(int xCoord) {
        double rv = 0.0;

        if (xCoord < xCoord0) {
            return rv;
        }

        if (xCoord <= xCoord4) {
            rv = (xCoord - xCoord0) * factor0to4;
        } else if (xCoord <= xCoord10) {
            rv = (xCoord - xCoord4) * factor4to10 + 4.0;
        } else if (xCoord <= xCoord30) {
            rv = (xCoord - xCoord10) * factor10to30 + 10.0;
        } else if (xCoord >= xCoord31Plus) {
            return Double.NaN;
        }

        return rv;
    }

    /**
     * Calculate value to X coordinate.
     * 
     * @param val
     *            Wind speed value.
     * @return X coordinate.
     */
    private int calcValueToXCoord(Double val) {
        int rv = 0;

        if (val > 30.0 || val < 0.0) {
            return INVALID_INT;
        }

        if (val <= 4.0) {
            rv = (int) Math.round(val / factor0to4 + xCoord0);
        } else if (val <= 10.0) {
            rv = (int) Math.round((val - 4.0) / factor4to10 + xCoord4);
        } else if (val <= 30.0) {
            rv = (int) Math.round((val - 10.0) / factor10to30 + xCoord10);
        }

        return rv;
    }

    /**
     * Validate the user inputs in the value text control.
     */
    @Override
    public void validateValueInputs() {

        if (checkInput(valueTF.getText()) == false) {
            userInformation("Must enter a wind speed in the form D.D, DD.D, or 31+");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        if (valueTF.getText().compareTo("31+") == 0) {
            arrowCenterXCoord = xCoord31Plus;
            drawingCanvas.redraw();
            return;
        }

        try {
            Double val = Double.valueOf(valueTF.getText());

            arrowCenterXCoord = calcValueToXCoord(val);
            setValueText(val);
            drawingCanvas.redraw();
        } catch (NumberFormatException nfe) {
            nfe.printStackTrace();
        }
    }

    /**
     * Validate the user inputs in the range text control.
     */
    @Override
    public void validateRangeInputs() {

        if (rangeTF.getText().indexOf("-") < 0) {
            userInformation("Must enter wind speed range in the form D{D}.D-D{D}.D|31+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String[] ranges = rangeTF.getText().split("-");

        if (checkInput(ranges[0]) == false || checkInput(ranges[1]) == false) {
            userInformation("Must enter wind speed range in the form D{D}.D-D{D}.D|31+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String startStr = ranges[0];
        String endStr = ranges[1];

        if (startStr.compareTo("31+") == 0) {
            userInformation("Start Range cannot have a 31+ value");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        try {
            Double startVal = Double.valueOf(startStr);

            if (startVal < 0.0 || startVal > 30.0) {
                userInformation("Start Range must be from 0.0 to 30.0");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }

            int newXCoord = calcValueToXCoord(startVal);
            barRect.x = newXCoord;
            barRect.width = barRightXCoord - newXCoord;

            Double endVal = 0.0;
            if (endStr.compareTo("31+") == 0) {
                barRightXCoord = xCoord31Plus;
                barRect.width = barRightXCoord - barRect.x;
                endVal = Double.NaN;
            } else {
                endVal = Double.valueOf(endStr);

                if (endVal < startVal) {
                    userInformation("Start Range value must be less than the End Range value.");
                    rangeTF.setFocus();
                    rangeTF.selectAll();
                    return;
                }

                barRightXCoord = calcValueToXCoord(endVal);
                barRect.width = barRightXCoord - barRect.x;
            }

            setRangeText(startVal, endVal); // TODO

            drawingCanvas.redraw();
        } catch (NumberFormatException nfe) {
            nfe.printStackTrace();
        }
    }

    /**
     * Set the text in the value text control.
     * 
     * @param val
     *            Value to be converted in to wind speed.
     */
    public void setValueText(Double val) {
        if (val.isNaN() == true || val > 30) {
            valueTF.setText("31+");
        } else {
            valueTF.setText(String.format("%3.1f", val));
        }

        if (valueTF.getText().compareTo("31+") == 0) {
            arrowCenterXCoord = xCoord31Plus;
        } else {
            arrowCenterXCoord = calcValueToXCoord(val);
        }

        drawingCanvas.redraw();
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startVal
     *            Start value to be converted in to wind speed.
     * @param endVal
     *            End value to be converted in to wind speed.
     */
    public void setRangeText(Double startVal, Double endVal) {
        if (startVal.isNaN() == true || startVal > 30) {
            rangeTF.setText("31+-31+");
            return;
        } else if (endVal.isNaN() == true || endVal > 30) {
            rangeTF.setText(String.format("%2.1f-31+", startVal));
        } else {
            rangeTF.setText(String.format("%2.1f-%2.1f", startVal, endVal));
        }

        String[] ranges = rangeTF.getText().split("-");
        String startStr = ranges[0];
        String endStr = ranges[1];
        int newXCoord = calcValueToXCoord(startVal);
        barRect.x = newXCoord;
        barRect.width = barRightXCoord - newXCoord;

        if (endStr.compareTo("31+") == 0) {
            barRightXCoord = xCoord31Plus;
            barRect.width = barRightXCoord - barRect.x;
            endVal = Double.NaN;
        } else {
            barRightXCoord = calcValueToXCoord(endVal);
            barRect.width = barRightXCoord - barRect.x;
        }

        drawingCanvas.redraw();
    }

    public String getRange() {
        return rangeTF.getText();
    }

    /**
     * Validate the user input.
     * 
     * @param input
     *            Input string.
     * @return True if the input is good, false otherwise.
     */
    private boolean checkInput(String input) {
        return input
                .matches("(\\d\\u002E[0-9])|(\\u002E[0-9])|(\\d{2})|(\\d{1})|"
                        + "(\\u002E[0-9][0-9])|(\\d{2}\\u002E[0-9])|([3][1]\\u002B)");
    }

    public String[] getWindSpdRange() {
        String[] windSpdRange = new String[2];
        String[] str = rangeTF.getText().split("-");
        windSpdRange[0] = str[0];
        windSpdRange[1] = str[1];

        if (windSpdRange[1].endsWith("+")) {
            windSpdRange[1] = windSpdRange[1].substring(0, (windSpdRange[1]
                    .length() - 1));
        }

        return windSpdRange;
    }
}
