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
 * TrendCeilingCanvasComp class draws the slider information on the ceiling
 * canvas and handles the user inputs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation
 * 12 Aug 2013  #2256      lvenable    Moved calcArrow() to parent abstract class
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TrendCeilingCanvasComp extends TrendSliderComp implements
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
     * X coordinate of the 2 value.
     */
    private int xCoord2 = xCoord0 + 25;

    /**
     * X coordinate of the 6 value.
     */
    private int xCoord6 = xCoord2 + 35;

    /**
     * X coordinate of the 10 value.
     */
    private int xCoord10 = xCoord6 + 30;

    /**
     * X coordinate of the 31 value.
     */
    private int xCoord31 = xCoord10 + 90;

    /**
     * X coordinate of the 50 value.
     */
    private int xCoord50 = xCoord31 + 50;

    /**
     * X coordinate of the 51+ value.
     */
    private int xCoord51Plus = xCoord50 + 25;

    /**
     * X coordinate of the UNL (Unlimited) value.
     */
    private int xCoordUNL = hLineEndXCoord;

    /**
     * X coordinate difference between 50 and 51+.
     */
    private int xCoord50_51PlusDiff = ((xCoord51Plus - xCoord50) / 2)
            + xCoord50;

    /**
     * X coordinate difference between 51+ and UNL.
     */
    private int xCoord51Plus_UnlDiff = ((xCoordUNL - xCoord51Plus) / 2)
            + xCoord51Plus;

    /**
     * Value to pixel factor from 0 to 4.
     */
    private double factor0to2 = 2.0 / (xCoord2 - xCoord0);

    /**
     * Value to pixel factor from 2 to 6.
     */
    private double factor2to6 = 4.0 / (xCoord6 - xCoord2);

    /**
     * Value to pixel factor from 6 to 10.
     */
    private double factor6to10 = 4.0 / (xCoord10 - xCoord6);

    /**
     * Value to pixel factor from 10 to 31.
     */
    private double factor10to31 = 21.0 / (xCoord31 - xCoord10);

    /**
     * Value to pixel factor from 31 to 50.
     */
    private double factor31to50 = 19.0 / (xCoord50 - xCoord31);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public TrendCeilingCanvasComp(Composite parent) {
        super(parent, "Ceiling");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        arrowCenterXCoord = 150;

        barWidth = 100;

        barRect = new Rectangle(xCoord2, hLineYcoord + 3, barWidth, 15);

        barRightXCoord = barWidth + barRect.x;

        moveAll(150);

        drawingCanvas.addMouseListener(this);
        drawingCanvas.addMouseMoveListener(this);

        validateValueListeners();
        validateRangeListeners();
    }

    /**
     * Draw the dial information on the ceiling canvas.
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
        gc.drawLine(10, hLineYcoord, hLineEndXCoord, hLineYcoord);

        // Draw hash marks.
        gc.drawLine(xCoord0, hLineYcoord, xCoord0, hLineYcoord - dashLine);
        gc.drawLine(xCoord2, hLineYcoord, xCoord2, hLineYcoord - dashLine);
        gc.drawLine(xCoord6, hLineYcoord, xCoord6, hLineYcoord - dashLine);
        gc.drawLine(xCoord10, hLineYcoord, xCoord10, hLineYcoord - dashLine);
        gc.drawLine(xCoord31, hLineYcoord, xCoord31, hLineYcoord - dashLine);
        gc.drawLine(xCoord50, hLineYcoord, xCoord50, hLineYcoord - dashLine);
        gc.drawLine(xCoord51Plus, hLineYcoord, xCoord51Plus, hLineYcoord
                - dashLine);
        gc.drawLine(xCoordUNL, hLineYcoord, xCoordUNL, hLineYcoord - dashLine);

        // ---------------------------------------------
        // Draw the labels
        // ---------------------------------------------
        gc.drawString("0", xCoord0 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("2", xCoord2 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("6", xCoord6 - fontWidth / 2, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("10", xCoord10 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("31", xCoord31 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("50", xCoord50 - fontWidth, hLineYcoord - dashLine - 3
                - fontHeight, true);
        gc.drawString("51+", xCoord51Plus - fontWidth, hLineYcoord - dashLine
                - 3 - fontHeight, true);
        gc.drawString("UNL", xCoordUNL - (fontWidth * 3) / 2, hLineYcoord
                - dashLine - 3 - fontHeight, true);

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

        setValueText(val * 100.0);
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

        setRangeText(startVal * 100.0, endVal * 100.0);
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

        setRangeText(startVal * 100.0, endVal * 100.0);
    }

    /**
     * Move the arrow and the range.
     * 
     * @param xCoord
     */
    private void moveAll(int xCoord) {
        if (xCoord > xCoord50) {
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

        if (currBarRightX != xCoord51Plus && currBarRightX != xCoordUNL) {
            if (currBarRightX + offset > xCoord50) {
                return;
            }
        }

        moveArrow(xCoord);
        moveLeftSideOfRange(currBarLeftX + offset);

        if (barRightXCoord <= xCoord50) {
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
        } else if (xCoord >= xCoord50 && xCoord < xCoord50_51PlusDiff) {
            newXCoord = xCoord50;
        } else if (xCoord >= xCoord50_51PlusDiff && xCoord < xCoord51Plus) {
            newXCoord = xCoord51Plus;
        } else if (xCoord >= xCoord51Plus && xCoord <= xCoord51Plus_UnlDiff) {
            newXCoord = xCoord51Plus;
        } else if (xCoord >= xCoord51Plus_UnlDiff && xCoord <= xCoordUNL) {
            newXCoord = xCoordUNL;
        } else if (xCoord > xCoordUNL) {
            newXCoord = xCoordUNL;
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

        if (xCoord <= xCoord2) {
            rv = (xCoord - xCoord0) * factor0to2;
        } else if (xCoord <= xCoord6) {
            rv = (xCoord - xCoord2) * factor2to6 + 2.0;
        } else if (xCoord <= xCoord10) {
            rv = (xCoord - xCoord6) * factor6to10 + 6.0;
        } else if (xCoord <= xCoord31) {
            rv = (xCoord - xCoord10) * factor10to31 + 10.0;
        } else if (xCoord <= xCoord50) {
            rv = (xCoord - xCoord31) * factor31to50 + 31.0;
        } else if (xCoord <= xCoord51Plus) {
            rv = 51.0;
        } else if (xCoord <= xCoordUNL) {
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
    private int calcValueToXCoord(Double valArg) {
        int rv = 0;

        double val = valArg / 100.0;

        if (val > 51.0 || val < 0.0) {
            return INVALID_INT;
        }

        if (val <= 2.0) {
            rv = (int) Math.round(val / factor0to2 + xCoord0);
        } else if (val <= 6.0) {
            rv = (int) Math.round((val - 2.0) / factor2to6 + xCoord2);
        } else if (val <= 10.0) {
            rv = (int) Math.round((val - 6.0) / factor6to10 + xCoord6);
        } else if (val <= 31.0) {
            rv = (int) Math.round((val - 10.0) / factor10to31 + xCoord10);
        } else if (val <= 50.0) {
            rv = (int) Math.round((val - 31.0) / factor31to50 + xCoord31);
        } else if (val == 51.0) {
            rv = xCoord51Plus;
        }

        return rv;
    }

    /**
     * Validate the user inputs in the value text control.
     */
    @Override
    public void validateValueInputs() {
        if (checkInput(valueTF.getText()) == false) {
            userInformation("Must enter a ceiling in the form D, DD, DDD, [1-4]DDD, 50DD, 5100+ or UNL");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        if (valueTF.getText().compareTo("5100+") == 0) {
            arrowCenterXCoord = xCoord51Plus;
            drawingCanvas.redraw();
            return;
        } else if (valueTF.getText().compareTo("UNL") == 0) {
            arrowCenterXCoord = xCoordUNL;
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
            userInformation("Must enter a ceiling range in the form:\n"
                    + "D, DD, DDD, [1-4]DDD, 50DD, 5100+ or UNL\n\n"
                    + "Examples: 600-3100, 1150-UNL, 2000-5100+, 5000-5100+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String[] ranges = rangeTF.getText().split("-");

        if (checkInput(ranges[0]) == false || checkInput(ranges[1]) == false) {
            userInformation("Must enter a ceiling range in the form:\n"
                    + "D, DD, DDD, [1-4]DDD, 50DD, 5100+ or UNL\n\n"
                    + "Examples: 600-3100, 1150-UNL, 2000-5100+, 5000-5100+");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        String startStr = ranges[0];
        String endStr = ranges[1];

        if (startStr.compareTo("UNL") == 0) {
            userInformation("Start Range cannot have a UNL value");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        try {
            Double startVal = 0.0;
            if (startStr.compareTo("5100+") == 0) {
                startVal = 5100.0;
            } else {
                startVal = Double.valueOf(startStr);

                if (startVal > 5000.0) {
                    startVal = 5000.0;
                }
            }

            if (startVal < 0.0 || startVal > 5100) {
                userInformation("Start Range must be from 0 to 5000 or 5100+");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }

            int newXCoord = calcValueToXCoord(startVal);
            barRect.x = newXCoord;
            barRect.width = barRightXCoord - newXCoord;

            Double endVal = 0.0;
            if (endStr.compareTo("5100+") == 0) {
                barRightXCoord = xCoord51Plus;
                barRect.width = barRightXCoord - barRect.x;
                if (barRect.width == 0) {
                    barRect.width = 1;
                }
                endVal = 5100.0;
            } else if (endStr.compareTo("UNL") == 0) {
                barRightXCoord = xCoordUNL;
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

                if (barRect.width == 0) {
                    barRect.width = 1;
                }
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
     *            Value to be converted in to ceiling value.
     */
    public void setValueText(Double val) {
        if (val.isNaN() == true || val > 5100) {
            valueTF.setText("UNL");
        } else if (val >= 5050) {
            valueTF.setText("5100+");
        } else {
            int intVal = (int) Math.round(val);
            valueTF.setText(String.format("%d", intVal));
        }

        if (valueTF.getText().compareTo("5100+") == 0) {
            arrowCenterXCoord = xCoord51Plus;
        } else if (valueTF.getText().compareTo("UNL") == 0) {
            arrowCenterXCoord = xCoordUNL;
        } else {
            arrowCenterXCoord = calcValueToXCoord(val);
        }

        drawingCanvas.redraw();
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startVal
     *            Start value to be converted in to ceiling value.
     * @param endVal
     *            End value to be converted in to ceiling value.
     */
    public void setRangeText(Double startVal, Double endVal) {
        StringBuilder rangeStr = new StringBuilder();

        if (startVal >= 5050) {
            rangeStr.append("5100+");
        } else {
            rangeStr.append(String.format("%d", (int) Math.round(startVal)));
        }

        rangeStr.append("-");

        if (endVal.isNaN() == true || endVal > 5100) {
            rangeStr.append("UNL");
        } else if (endVal >= 5050) {
            rangeStr.append("5100+");
        } else {
            rangeStr.append(String.format("%d", (int) Math.round(endVal)));
        }

        rangeTF.setText(rangeStr.toString());

        String[] ranges = rangeTF.getText().split("-");
        String startStr = ranges[0];
        String endStr = ranges[1];

        if (startStr.compareTo("5100+") == 0) {
            startVal = 5100.0;
        } else {
            startVal = Double.valueOf(startStr);

            if (startVal > 5000.0) {
                startVal = 5000.0;
            }
        }

        int newXCoord = calcValueToXCoord(startVal);
        barRect.x = newXCoord;
        barRect.width = barRightXCoord - newXCoord;

        if (endStr.compareTo("5100+") == 0) {
            barRightXCoord = xCoord51Plus;
            barRect.width = barRightXCoord - barRect.x;
            if (barRect.width == 0) {
                barRect.width = 1;
            }
            endVal = 5100.0;
        } else if (endStr.compareTo("UNL") == 0) {
            barRightXCoord = xCoordUNL;
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

            if (barRect.width == 0) {
                barRect.width = 1;
            }
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
        return input.matches("(\\d)|(\\d{2})|(\\d{3})|(\\dd{3})|([1-4]\\d{3})|"
                + "(50\\d{2})|(5100\\u002B)|(UNL)");
    }

    public String[] getCigRange() {
        String[] cigRange = new String[2];
        String[] str = rangeTF.getText().split("-");
        cigRange[0] = str[0];
        cigRange[1] = str[1];

        if (cigRange[0].endsWith("+")) {
            cigRange[0] = "5100";
        }

        if (cigRange[1].endsWith("+")) {
            cigRange[1] = cigRange[1].substring(0, (cigRange[1].length() - 1));
        }

        if (cigRange[1].equals("UNL")) {
            cigRange[1] = "99999";
        }

        return cigRange;
    }
}
