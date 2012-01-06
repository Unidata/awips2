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
 * TrendVisCanvasComp class draws the slider information on the Visibility
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
public class TrendVisCanvasComp extends TrendSliderComp implements
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
    int xCoord0 = hLineStartXCoord;

    /**
     * X coordinate of the 1/4 value.
     */
    int xCoord1_4 = xCoord0 + 30;

    /**
     * X coordinate of the 1 value.
     */
    int xCoord1 = xCoord1_4 + 40;

    /**
     * X coordinate of the 3 value.
     */
    int xCoord3 = xCoord1 + 60;

    /**
     * X coordinate of the 5 value.
     */
    int xCoord5 = xCoord3 + 40;

    /**
     * X coordinate of the 6 value.
     */
    int xCoord6 = xCoord5 + 15;

    /**
     * X coordinate of the 10 value.
     */
    int xCoord10 = xCoord6 + 70;

    /**
     * X coordinate of the 11+ value.
     */
    int xCoord11Plus = hLineEndXCoord;

    /**
     * X coordinate difference between 10 and 11+.
     */
    private int xCoord10_11Diff = ((xCoord11Plus - xCoord10) / 2) + xCoord10;

    /**
     * Value to pixel factor from 0 to 1/4.
     */
    private double factor0to1_4 = .25 / (xCoord1_4 - xCoord0);

    /**
     * Value to pixel factor from 1/4 to 1.
     */
    private double factor1_4to1 = .75 / (xCoord1 - xCoord1_4);

    /**
     * Value to pixel factor from 1 to 3.
     */
    private double factor1to3 = 2.0 / (xCoord3 - xCoord1);

    /**
     * Value to pixel factor from 3 to 5.
     */
    private double factor3to5 = 2.0 / (xCoord5 - xCoord3);

    /**
     * Value to pixel factor from 5 to 6.
     */
    private double factor5to6 = 1.0 / (xCoord6 - xCoord5);

    /**
     * Value to pixel factor from 6 to 10.
     */
    private double factor6to10 = 4.0 / (xCoord10 - xCoord6);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public TrendVisCanvasComp(Composite parent) {
        super(parent, "Visibility");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        arrowCenterXCoord = 150;

        barWidth = 100;

        barRect = new Rectangle(xCoord1, hLineYcoord + 3, barWidth, 15);

        barRightXCoord = barWidth + barRect.x;

        moveAll(150);

        drawingCanvas.addMouseListener(this);
        drawingCanvas.addMouseMoveListener(this);

        validateValueListeners();
        validateRangeListeners();
    }

    /**
     * Draw the dial information on the visibility canvas.
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
        gc.drawLine(xCoord1_4, hLineYcoord, xCoord1_4, hLineYcoord - dashLine);
        gc.drawLine(xCoord1, hLineYcoord, xCoord1, hLineYcoord - dashLine);
        gc.drawLine(xCoord3, hLineYcoord, xCoord3, hLineYcoord - dashLine);
        gc.drawLine(xCoord5, hLineYcoord, xCoord5, hLineYcoord - dashLine);
        gc.drawLine(xCoord6, hLineYcoord, xCoord6, hLineYcoord - dashLine);
        gc.drawLine(xCoord10, hLineYcoord, xCoord10, hLineYcoord - dashLine);
        gc.drawLine(xCoord11Plus, hLineYcoord, xCoord11Plus, hLineYcoord
                - dashLine);

        // ---------------------------------------------
        // Draw the labels
        // ---------------------------------------------
        gc.drawString("0", xCoord0 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("1/4", xCoord1_4 - (fontWidth * 3) / 2, hLineYcoord
                - dashLine - 1 - fontHeight, true);
        gc.drawString("1", xCoord1 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("3", xCoord3 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("5", xCoord5 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("6", xCoord6 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("10", xCoord10 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("11+", xCoord11Plus - (fontWidth * 3) / 2 - 2,
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
        if (xCoord > xCoord10) {
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

        if (currBarRightX != xCoord11Plus) {
            if (currBarRightX + offset > xCoord10) {
                return;
            }
        }

        moveArrow(xCoord);
        moveLeftSideOfRange(currBarLeftX + offset);

        if (barRightXCoord != xCoord11Plus) {
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
        } else if (xCoord > xCoord11Plus) {
            newXCoord = xCoord11Plus;
        } else if (xCoord >= xCoord10 && xCoord < xCoord10_11Diff) {
            newXCoord = xCoord10;
        } else if (xCoord >= xCoord10_11Diff && xCoord <= xCoord11Plus) {
            newXCoord = xCoord11Plus;
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
     * @return Visibility value.
     */
    private Double calcXCoordToValue(int xCoord) {
        double rv = 0.0;

        if (xCoord < xCoord0) {
            return rv;
        }

        if (xCoord <= xCoord1_4) {
            rv = (xCoord - xCoord0) * factor0to1_4;
        } else if (xCoord <= xCoord1) {
            rv = (xCoord - xCoord1_4) * factor1_4to1 + .25;
        } else if (xCoord <= xCoord3) {
            rv = (xCoord - xCoord1) * factor1to3 + 1.0;
        } else if (xCoord <= xCoord5) {
            rv = (xCoord - xCoord3) * factor3to5 + 3.0;
        } else if (xCoord <= xCoord6) {
            rv = (xCoord - xCoord5) * factor5to6 + 5.0;
        } else if (xCoord <= xCoord10) {
            rv = (xCoord - xCoord6) * factor6to10 + 6.0;
        } else if (xCoord >= xCoord11Plus) {
            return Double.NaN;
        }

        return rv;
    }

    /**
     * Calculate value to X coordinate.
     * 
     * @param val
     *            Visibility value.
     * @return X coordinate.
     */
    private int calcValueToXCoord(Double val) {
        int rv = 0;

        if (val > 10.0 || val < 0.0) {
            return INVALID_INT;
        }

        if (val <= .25) {
            rv = (int) Math.round(val / factor0to1_4 + xCoord0);
        } else if (val <= 1.0) {
            rv = (int) Math.round((val - .25) / factor1_4to1 + xCoord1_4);
        } else if (val <= 3.0) {
            rv = (int) Math.round((val - 1.0) / factor1to3 + xCoord1);
        } else if (val <= 5.0) {
            rv = (int) Math.round((val - 3.0) / factor3to5 + xCoord3);
        } else if (val <= 6.0) {
            rv = (int) Math.round((val - 5.0) / factor5to6 + xCoord5);
        } else if (val <= 10.0) {
            rv = (int) Math.round((val - 6.0) / factor6to10 + xCoord6);
        }

        return rv;
    }

    /**
     * Validate the user inputs in the value text control.
     */
    @Override
    public void validateValueInputs() {
        if (checkInput(valueTF.getText()) == false) {
            userInformation("Must enter a visibility in the form 0.DD, D.D, DD.D or 11+");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        if (valueTF.getText().compareTo("11+") == 0) {
            arrowCenterXCoord = xCoord11Plus;
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
            userInformation("Must enter visibility range in the form 0.DD, D.D, DD.D or 11+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String[] ranges = rangeTF.getText().split("-");

        if (checkInput(ranges[0]) == false || checkInput(ranges[1]) == false) {
            userInformation("Must enter visibility range in the form 0.DD, D.D, DD.D or 11+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String startStr = ranges[0];
        String endStr = ranges[1];

        if (startStr.compareTo("11+") == 0) {
            userInformation("Start Range cannot have a 11+ value");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        try {
            Double startVal = Double.valueOf(startStr);

            if (startVal < 0.0 || startVal > 30.0) {
                userInformation("Start Range must be from 0.0 to 10.0");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }

            int newXCoord = calcValueToXCoord(startVal);
            barRect.x = newXCoord;
            barRect.width = barRightXCoord - newXCoord;

            Double endVal = 0.0;
            if (endStr.compareTo("11+") == 0) {
                barRightXCoord = xCoord11Plus;
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
     *            Value to be converted in to visibility.
     */
    public void setValueText(Double val) {
        if (val.isNaN() == true || val > 10) {
            valueTF.setText("11+");
        } else if (val < 1.0) {
            valueTF.setText(String.format("%2.2f", val));
        } else {
            valueTF.setText(String.format("%2.1f", val));
        }

        if (valueTF.getText().compareTo("11+") == 0) {
            arrowCenterXCoord = xCoord11Plus;
            drawingCanvas.redraw();
        } else {
            arrowCenterXCoord = calcValueToXCoord(val);
            drawingCanvas.redraw();
        }
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startVal
     *            Start value to be converted in to visibility.
     * @param endVal
     *            End value to be converted in to visibility.
     */
    public void setRangeText(Double startVal, Double endVal) {
        if (startVal.isNaN() == true) {
            rangeTF.setText("10.0-11+");
            return;
        } else if (endVal.isNaN() == true || endVal > 10) {
            rangeTF.setText(String.format("%2.1f-11+", startVal));
        } else {
            StringBuilder format = new StringBuilder();

            if (startVal < 1.0) {
                format.append("%1.2f-");
            } else {
                format.append("%2.1f-");
            }

            if (endVal < 1.0) {
                format.append("%1.2f");
            } else {
                format.append("%2.1f");
            }

            rangeTF.setText(String.format(format.toString(), startVal, endVal));
        }

        int newXCoord = calcValueToXCoord(startVal);
        barRect.x = newXCoord;

        if (rangeTF.getText().endsWith("11+")) {
            barRightXCoord = xCoord11Plus;
            barRect.width = barRightXCoord - barRect.x;
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
                .matches("(0\\u002E\\d{2})|(\\u002E\\d{2})|(0\\u002E\\d{1})|"
                        + "(\\u002E\\d{1})|([1-9]\\u002E\\d)|"
                        + "(\\d{2}\\u002E\\d)|(\\d{2})|(\\d{1})|(11\\u002B)");
    }

    public String[] getVsbyRange() {
        String[] vsbyRange = new String[2];
        String[] str = rangeTF.getText().split("-");
        vsbyRange[0] = str[0];
        vsbyRange[1] = str[1];

        if (vsbyRange[0].endsWith("+")) {
            vsbyRange[0] = vsbyRange[0].substring(0,
                    (vsbyRange[0].length() - 1));
        }

        if (vsbyRange[1].endsWith("+")) {
            vsbyRange[1] = vsbyRange[1].substring(0,
                    (vsbyRange[1].length() - 1));
        }

        return vsbyRange;
    }
}
