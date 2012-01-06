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
import org.eclipse.swt.widgets.Composite;

/**
 * TrendHourCanvasComp class draws the dial on the hour canvas and handles the
 * user inputs.
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
public class TrendHourCanvasComp extends TrendDialComp implements
        MouseMoveListener, MouseListener {
    /**
     * Parent composite.
     */
    private Composite parent;

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
    public TrendHourCanvasComp(Composite parent) {
        super(parent, "Hour");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        drawingCanvas.addMouseMoveListener(this);
        drawingCanvas.addMouseListener(this);

        validateValueListeners();
        validateRangeListeners();
    }

    /**
     * Draw the hour dial information.
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

        drawRangeArc(gc);

        // ------------------------------------
        // Draw the dial line
        // ------------------------------------
        drawDialArm(gc);

        // ------------------------------------
        // Draw dashes around the dial
        // ------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        double radius = (canvasWidth - dialXYVal * 2) / 2;
        double radius2 = radius + dashLength;

        double offset = 360.0 / 24.0;

        for (double i = 0; i < 360.0; i += offset) {
            double rad2deg = 180.0 / Math.PI;
            double theta = i / rad2deg;
            int x = (int) Math.rint((radius * Math.sin(theta)));
            int y = (int) Math.rint((-radius * Math.cos(theta)));

            int x2 = (int) Math.rint((radius2 * Math.sin(theta)));
            int y2 = (int) Math.rint((-radius2 * Math.cos(theta)));

            gc.drawLine(Math.round(centerX + x2), Math.round(centerY + y2),
                    Math.round(centerX + x), Math.round(centerY + y));
        }

        // --------------------------------------
        // Draw string
        // --------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // 00 Hour
        gc.drawString("00", canvasWidth / 2 - fontAveWidth, 0, true);

        // 04 Hour
        gc.drawString("04", canvasWidth - dialXYVal + 3,
                (int) (canvasHeight * .25), true);

        // 08 Hour
        gc.drawString("08", canvasWidth - dialXYVal + 3,
                (int) (canvasHeight * .65), true);

        // 12 Hour
        gc.drawString("12", canvasWidth / 2 - fontAveWidth, canvasHeight
                - fontHeight, true);

        // 16 Hour
        gc.drawString("16", 3, (int) (canvasHeight * .65), true);

        // 20 Hour
        gc.drawString("20", 3, (int) (canvasHeight * .25), true);
    }

    /**
     * Set the text in the value text control.
     * 
     * @param degrees
     *            Degree to be converted in to hours:minutes.
     */
    @Override
    public void setValueText(double degrees) {
        double numMinutes = degrees * 4.0;
        int min = (int) (numMinutes % 60);
        int hour = (int) (numMinutes / 60);

        valueTF.setText(String.format("%02d:%02d", hour, min));

        String[] valueTime = valueTF.getText().split(":");
        Double degree = checkTime(valueTime);
        dialArmDegree = degree;
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startdegrees
     *            Start degree to be converted in to hours:minutes.
     * @param enddegrees
     *            End degree to be converted in to hours:minutes.
     */
    @Override
    public void setRangeText(double startDegrees, double endDegrees) {
        double numStartMinutes = startDegrees * 4;
        int startMin = (int) (numStartMinutes % 60);
        int startHr = (int) (numStartMinutes / 60);

        double numEndMinutes = endDegrees * 4;
        int endMin = (int) (numEndMinutes % 60);
        int endHr = (int) (numEndMinutes / 60);

        rangeTF.setText(String.format("%02d:%02d-%02d:%02d", startHr, startMin,
                endHr, endMin));

        String[] str = rangeTF.getText().split("-");
        String[] startTime = str[0].split(":");
        String[] endTime = str[1].split(":");
        Double startDegree = checkTime(startTime);
        Double endDegree = checkTime(endTime);
        rangeStartDegree = startDegree;
        rangeEndDegree = endDegree;
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
    }

    /**
     * Called when a mouse button is pressed.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        // If the right mouse button is pressed then return as the
        // right mouse button isn't used.
        if (e.button == 3) {
            return;
        }

        mouseIsDown = false;

        double localRadian = Math.atan2(e.x - centerX, centerY - e.y);
        double i = Math.toDegrees(localRadian);

        double degree0to360 = i;

        if (degree0to360 < 0) {
            degree0to360 += 360.0;
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
        int charCount = valueTF.getCharCount();
        if (!(charCount == 0 || charCount == 5)) {
            userInformation("Must enter an hour in the form HH:MM");
            valueTF.setFocus();
            valueTF.selectAll();
            return;
        } else if (charCount == 5) {
            valueTF.setText(valueTF.getText());
            if (valueTF.getText().matches("\\d{2}:\\d{2}") == false) {
                userInformation("Must enter an hour in the form HH:MM");
                valueTF.setFocus();
                valueTF.selectAll();
                return;
            }
        }

        String[] valueTime = valueTF.getText().split(":");

        Double degree = checkTime(valueTime);

        if (degree.isNaN() == true) {
            userInformation("Value time is out of hour/minute range (00:00 to 23:59)");
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
        int charCount = rangeTF.getCharCount();
        if (!(charCount == 0 || charCount == 11)) {
            userInformation("Must enter an hour range in the form HH:MM-HH:MM");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        } else if (charCount == 11) {
            rangeTF.setText(rangeTF.getText());
            if (rangeTF.getText().matches("\\d{2}:\\d{2}-\\d{2}:\\d{2}") == false) {
                userInformation("Must enter an hour range in the form HH:MM-HH:MM");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }
        }

        String[] str = rangeTF.getText().split("-");

        String[] startTime = str[0].split(":");

        String[] endTime = str[1].split(":");

        Double startDegree = checkTime(startTime);

        Double endDegree = checkTime(endTime);

        if (startDegree.isNaN() == true) {
            userInformation("Start time is out of hour/minute range (00:00 to 23:59)");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        if (endDegree.isNaN() == true) {
            userInformation("End time is out of hour/minute range (00:00 to 23:59)");
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
     * Validate the time.
     * 
     * @param time
     *            Array of time (hours, minutes).
     * @return Time as a degree.
     */
    private double checkTime(String[] time) {
        int hour = Integer.valueOf(time[0]).intValue();
        int min = Integer.valueOf(time[1]).intValue();

        if (hour > 23 || hour < 0) {
            return Double.NaN;
        }

        if (min > 59 || min < 0) {
            return Double.NaN;
        }

        double rv = (hour * 60.0 + min) / 4.0;

        return rv;
    }

    /**
     * Get the hours and minutes.
     * 
     * @return A 2 element integer array. Element 0 is hours, 1 is minutes.
     */
    public int[] getHoursAndMinutes() {
        int[] hoursMins = new int[2];

        String[] str = valueTF.getText().split(":");

        hoursMins[0] = Integer.valueOf(str[0]).intValue();
        hoursMins[1] = Integer.valueOf(str[1]).intValue();

        return hoursMins;
    }

    public String[] getHourRange() {
        String[] hourRange = new String[2];
        String[] str = rangeTF.getText().split("[:-]");
        int h0 = Integer.valueOf(str[0]);
        int m0 = Integer.valueOf(str[1]);
        int h1 = Integer.valueOf(str[2]);
        int m1 = Integer.valueOf(str[3]);

        hourRange[0] = Double.toString(h0 + m0 / 60);
        hourRange[1] = Double.toString(h1 + m1 / 60);

        return hourRange;
    }
}
