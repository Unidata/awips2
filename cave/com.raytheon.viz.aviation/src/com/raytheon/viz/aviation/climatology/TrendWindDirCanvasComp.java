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
 * TrendWindDirCanvasComp class draws the dial information on the wind direction
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
public class TrendWindDirCanvasComp extends TrendDialComp implements
        MouseMoveListener, MouseListener {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Flag indicating if the dial is in 'calm' mode (no wind).
     */
    private boolean inCalmMode = false;

    /**
     * Selection area of the calm circle. When the mouse is clicked in this area
     * the dial goes back into regular wind direction mode.
     */
    private Rectangle calmSelectionRec = new Rectangle(centerX - 5,
            centerY - 5, 10, 10);

    /**
     * Flag indicating the mouse button is down.
     */
    private boolean mouseIsDown = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public TrendWindDirCanvasComp(Composite parent) {
        super(parent, "Wind Direction");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        // Need to adjust the circle radius since this dial is not
        // an arrow but a wind barb.
        dialArmRadius = (canvasWidth - 20 * 2) / 2 - 10;

        // ---------- TMP SETTINGS ------------------
        dialArmDegree = 1.0;
        rangeStartDegree = 1.0;
        rangeEndDegree = 90.0;
        // ------------------------------------------

        drawingCanvas.addMouseMoveListener(this);
        drawingCanvas.addMouseListener(this);

        validateValueListeners();
        validateRangeListeners();
    }

    /**
     * Draw the wind direction dial information.
     * 
     * @param gc
     *            Graphical context.
     */
    @Override
    public void drawCanvas(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();
        int fontHeight = gc.getFontMetrics().getHeight();

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, canvasWidth + 5, canvasHeight + 5);

        // -----------------------------------
        // Draw the dial circle
        // -----------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawOval(dialXYVal, dialXYVal, canvasWidth - dialXYVal * 2,
                canvasHeight - dialXYVal * 2);

        // ---------------------------------------
        // Draw Range Arc
        // ---------------------------------------
        if (inCalmMode == true) {
            drawCalmRangeArc(gc);
        } else {
            drawRangeArc(gc);
        }

        // ------------------------------------
        // Draw the dial line
        // ------------------------------------

        if (inCalmMode == false) {
            drawDialArm(gc);
        }

        // ------------------------------------
        // Draw dashes
        // ------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        double radius = (canvasWidth - dialXYVal * 2) / 2;
        double radius2 = radius + dashLength;

        int centerX = canvasWidth / 2;
        int centerY = canvasHeight / 2;

        double offset = 360.0 / 8.0;

        for (double i = 0; i < 360.0; i += offset) {
            double rad2deg = 180.0 / Math.PI;
            double theta = i / rad2deg;
            int x = (int) (radius * Math.sin(theta));
            int y = (int) (-radius * Math.cos(theta));

            int x2 = (int) (radius2 * Math.sin(theta));
            int y2 = (int) (-radius2 * Math.cos(theta));

            gc.drawLine(Math.round(centerX + x2), Math.round(centerY + y2),
                    Math.round(centerX + x), Math.round(centerY + y));
        }

        // --------------------------------------
        // Draw string
        // --------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // North
        gc.drawString("N", canvasWidth / 2 - fontAveWidth / 2, 0, true);

        gc.drawString("NE", canvasWidth / 2 - fontAveWidth / 2 + canvasWidth
                / 4 + 8, 13, true);

        gc.drawString("NW", canvasWidth / 2 - fontAveWidth - canvasWidth / 4
                - 10, 13, true);

        // West
        gc.drawString("W", dialXYVal - dashLength * 2 - fontAveWidth,
                canvasHeight / 2 - fontHeight / 2, true);

        // East
        gc.drawString("E", canvasWidth - dialXYVal + dashLength * 2,
                canvasHeight / 2 - fontHeight / 2, true);

        // South
        gc.drawString("S", canvasWidth / 2 - fontAveWidth / 2, canvasHeight
                - fontHeight, true);

        gc.drawString("SE", canvasWidth / 2 - fontAveWidth / 2 + canvasWidth
                / 4 + 8, 28 + canvasHeight / 2, true);

        gc.drawString("SW", canvasWidth / 2 - fontAveWidth - canvasWidth / 4
                - 10, 28 + canvasHeight / 2, true);
    }

    /**
     * Draw the calm range arc (filled circle).
     * 
     * @param gc
     */
    private void drawCalmRangeArc(GC gc) {
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.setAlpha(75);

        gc.fillArc((canvasWidth - (canvasWidth - dialXYVal * 2)) / 2,
                (canvasHeight - (canvasHeight - dialXYVal * 2)) / 2,
                canvasWidth - dialXYVal * 2, canvasHeight - dialXYVal * 2, 0,
                360);

        gc.setAlpha(255);

        gc.setLineWidth(2);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.drawOval(dialXYVal, dialXYVal, canvasWidth - dialXYVal * 2,
                canvasHeight - dialXYVal * 2);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        gc.drawOval(centerX - 5, centerY - 5, 11, 11);

        gc.setLineWidth(1);

        valueTF.setText("CALM");
        setRangeText(1.0, 360.0);
    }

    /**
     * Draw the wind barb dial arm.
     */
    @Override
    public void drawDialArm(GC gc) {
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));

        gc.setLineCap(SWT.CAP_ROUND);
        gc.setLineWidth(2);

        double neg180to180Degree = dialArmDegree;
        if (neg180to180Degree > 180.0) {
            neg180to180Degree += -360.0;
        }

        double theta = neg180to180Degree / rad2deg;

        int x = (int) (dialArmRadius * Math.sin(theta));
        int y = (int) (-dialArmRadius * Math.cos(theta));

        int x2 = (int) (zeroRadius * Math.sin(theta));
        int y2 = (int) (-zeroRadius * Math.cos(theta));

        gc.drawLine(Math.round(centerX + x2), Math.round(centerY + y2), Math
                .round(centerX + x), Math.round(centerY + y));

        int[] tipEnd2 = getPointOnCircle(centerX, centerY, dialArmRadius + 8,
                neg180to180Degree - 75);

        gc.drawLine(Math.round(centerX + x), Math.round(centerY + y),
                tipEnd2[0], tipEnd2[1]);

        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));

        gc.setLineWidth(1);

        setValueText(dialArmDegree);
    }

    /**
     * Set the text in the value text control.
     * 
     * @param degrees
     *            Degree to be converted in to a 3 digit value.
     */
    @Override
    public void setValueText(double degrees) {
        int degreeInt = (int) Math.round(degrees);

        if (degreeInt == 0) {
            degreeInt = 360;
        }

        valueTF.setText(String.format("%03d", degreeInt));

        dialArmDegree = degrees;
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startdegrees
     *            Start degree to be converted in to a 3 digit value.
     * @param enddegrees
     *            End degree to be converted in to a 3 digit value.
     */
    @Override
    public void setRangeText(double startDegrees, double endDegrees) {
        int start = (int) Math.round(startDegrees);
        int end = (int) Math.round(endDegrees);

        if (start == 0) {
            start = 360;
        }

        if (end == 0) {
            end = 360;
        }

        rangeTF.setText(String.format("%03d-%03d", start, end));

        rangeStartDegree = startDegrees;
        rangeEndDegree = endDegrees;
    }

    public void setCalm() {
        inCalmMode = true;
        valueTF.setText("CALM");
        setRangeText(1, 360);
        drawingCanvas.redraw();

    }

    /**
     * Called when the mouse is moved over the control.
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseMove(MouseEvent e) {
        if (mouseIsDown == true) {
            mousePos.x = e.x;
            mousePos.y = e.y;
            moveLine();
        }
    }

    /**
     * Not Implemented...
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
        // Not used...

    }

    /**
     * Called when a mouse button is pressed.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        mouseIsDown = false;

        double localRadian = Math.atan2(e.x - centerX, centerY - e.y);
        double i = Math.toDegrees(localRadian);

        double degree0to360 = i;

        if (degree0to360 < 0) {
            degree0to360 += 360.0;
        }

        // If the right mouse button is pressed then return as the
        // right mouse button isn't used.
        if (e.button == 3) {
            if (inCalmMode == false) {
                if (dialArmDegree > degree0to360 - 5
                        && dialArmDegree < degree0to360 + 5) {
                    inCalmMode = true;
                    valueTF.setEditable(false);
                    rangeTF.setEditable(false);
                }
            } else {
                if (calmSelectionRec.contains(e.x, e.y)) {
                    inCalmMode = false;
                    valueTF.setEditable(true);
                    rangeTF.setEditable(true);
                }
            }
            drawingCanvas.redraw();
            return;
        }

        if (inCalmMode == true) {
            return;
        }

        moveDialArm = false;
        moveRangeStart = false;
        moveRangeEnd = false;
        moveAll = false;

        // Move the dial and the range.
        if (e.button == 2) {
            if (dialArmDegree > degree0to360 - 5
                    && dialArmDegree < degree0to360 + 5) {
                mouseIsDown = true;
                moveAll = true;
            }

            return;
        }

        if (dialArmDegree > degree0to360 - 5
                && dialArmDegree < degree0to360 + 5) {
            mouseIsDown = true;
            moveDialArm = true;
            return;
        } else if (rangeStartDegree > degree0to360 - 5
                && rangeStartDegree < degree0to360 + 5) {
            mouseIsDown = true;
            moveRangeStart = true;
            return;
        } else if (rangeEndDegree > degree0to360 - 5
                && rangeEndDegree < degree0to360 + 5) {
            mouseIsDown = true;
            moveRangeEnd = true;
            return;
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
        mouseIsDown = false;
        moveDialArm = false;
        moveRangeStart = false;
        moveRangeEnd = false;
        moveAll = false;
    }

    /**
     * Validate the user inputs in the value text control.
     */
    @Override
    public void validateValueInputs() {
        if (valueTF.getText().equals("CALM")) {
            setCalm();
            return;
        } else {
            inCalmMode = false;
        }

        if (inCalmMode == true) {
            return;
        }

        int charCount = valueTF.getCharCount();
        if (!(charCount == 0 || charCount == 3)) {
            userInformation("Must enter a wind direction in the form DDD");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        } else if (charCount == 3) {
            valueTF.setText(valueTF.getText());
            if (valueTF.getText().matches("\\d{3}") == false) {
                userInformation("Must enter a wind direction in the form DDD");
                valueTF.setFocus();
                valueTF.selectAll();
                return;
            }
        }

        Double degree = checkDegree(valueTF.getText());

        if (degree.isNaN() == true) {
            userInformation("Wind Direction is out of range (001 to 360)");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        dialArmDegree = degree;

        // Redraw the canvas.
        drawingCanvas.redraw();
    }

    /**
     * Validate the user inputs in the range text control.
     */
    @Override
    public void validateRangeInputs() {
        if (inCalmMode == true) {
            return;
        }

        int charCount = rangeTF.getCharCount();
        if (!(charCount == 0 || charCount == 7)) {
            userInformation("Must enter wind direction range in the form DDD-DDD");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        } else if (charCount == 7) {
            rangeTF.setText(rangeTF.getText());
            if (rangeTF.getText().matches("\\d{3}-\\d{3}") == false) {
                userInformation("Must enter wind direction range in the form DDD-DDD");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }
        }

        String[] str = rangeTF.getText().split("-");

        Double startDegree = checkDegree(str[0]);

        Double endDegree = checkDegree(str[1]);

        if (startDegree.isNaN() == true) {
            userInformation("Wind Direction is out of range (001 to 360)");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        if (endDegree.isNaN() == true) {
            userInformation("Wind Direction is out of range (001 to 360)");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        rangeStartDegree = startDegree;

        rangeEndDegree = endDegree;

        // Redraw the canvas.
        drawingCanvas.redraw();
    }

    /**
     * Check the degree. The range is from 001 to 360.
     * 
     * @param degree
     *            Wind degree.
     * @return Degree with double precision.
     */
    private double checkDegree(String degree) {
        double degreeDbl = Double.valueOf(degree);

        if (degreeDbl > 360.0 || degreeDbl < 1.0) {
            return Double.NaN;
        }

        return degreeDbl;
    }

    public String[] getWindDirRange() {
        String[] windDirRange = new String[2];
        String[] str = rangeTF.getText().split("-");
        windDirRange[0] = str[0];
        windDirRange[1] = str[1];
        return windDirRange;
    }
}
