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

import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

/**
 * TrendDateCanvasComp class draws the dial on the date canvas and handles the
 * user inputs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 7/29/2008    1342       grichard    Get calendar instance in UTC time zone.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TrendDateCanvasComp extends TrendDialComp implements
        MouseMoveListener, MouseListener {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Days in a year.
     */
    private int daysInYear;

    /**
     * Days per degree.
     */
    private double daysPerDegree;

    /**
     * Calendar instance.
     */
    private Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));

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
    public TrendDateCanvasComp(Composite parent) {
        super(parent, "Date");

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        daysInYear = daysInAYear(Calendar.YEAR);
        daysPerDegree = 360.0 / daysInYear;

        validateValueListeners();
        validateRangeListeners();

        drawingCanvas.addMouseMoveListener(this);
        drawingCanvas.addMouseListener(this);
    }

    /**
     * Draw the date dial information.
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
        // Draw dashes
        // ------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        double radius = (canvasWidth - dialXYVal * 2) / 2;
        double radius2 = radius + dashLength;

        int centerX = canvasWidth / 2;
        int centerY = canvasHeight / 2;

        double offset = 360.0 / 12.0;

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

        // January & December
        gc.drawString("J", canvasWidth / 2 + 5 + fontAveWidth, 3, true);

        gc.drawString("D", canvasWidth / 2 - 5 - fontAveWidth * 2, 3, true);

        // February & November
        gc.drawString("F", canvasWidth / 2 + 25 + fontAveWidth, 16, true);

        gc.drawString("N", canvasWidth / 2 - 25 - fontAveWidth * 2, 16, true);

        // March & October
        gc.drawString("M", canvasWidth / 2 + 35 + fontAveWidth, 35, true);

        gc.drawString("O", canvasWidth / 2 - 35 - fontAveWidth * 2, 35, true);

        // April & September
        gc.drawString("A", canvasWidth / 2 + 35 + fontAveWidth, canvasHeight
                - fontHeight - 35, true);

        gc.drawString("S", canvasWidth / 2 - 35 - fontAveWidth * 2,
                canvasHeight - fontHeight - 35, true);

        // May & August
        gc.drawString("M", canvasWidth / 2 + 25 + fontAveWidth, canvasHeight
                - fontHeight - 16, true);

        gc.drawString("A", canvasWidth / 2 - 25 - fontAveWidth * 2,
                canvasHeight - fontHeight - 16, true);

        // June & July
        gc.drawString("J", canvasWidth / 2 + 5 + fontAveWidth, canvasHeight
                - fontHeight - 3, true);

        gc.drawString("J", canvasWidth / 2 - 5 - fontAveWidth * 2, canvasHeight
                - fontHeight - 3, true);
    }

    /**
     * Set the text in the value text control.
     * 
     * @param degrees
     *            Degree to be converted in to month/day (mmdd).
     */
    @Override
    public void setValueText(double degrees) {
        int dayOfYear = (int) Math.round(degrees / daysPerDegree);

        if (dayOfYear == 0) {
            ++dayOfYear;
        }

        cal.set(Calendar.DAY_OF_YEAR, dayOfYear);

        int month = cal.get(Calendar.MONTH) + 1;
        int day = cal.get(Calendar.DAY_OF_MONTH);

        valueTF.setText(String.format("%02d%02d", month, day));
    }

    public void setValueText(int dayOfYear) {
        cal.set(Calendar.DAY_OF_YEAR, dayOfYear);

        int month = cal.get(Calendar.MONTH) + 1;
        int day = cal.get(Calendar.DAY_OF_MONTH);

        valueTF.setText(String.format("%02d%02d", month, day));

        Double degree = checkDate(valueTF.getText());
        dialArmDegree = degree;
    }

    /**
     * Set the text in the range text control.
     * 
     * @param startDdegrees
     *            Start degree to be converted in to month/day (mmdd).
     * @param endDegrees
     *            End degree to be converted in to month/day (mmdd).
     */
    @Override
    public void setRangeText(double startDegrees, double endDegrees) {
        int startDayOfYear = (int) Math.round(startDegrees / daysPerDegree);

        if (startDayOfYear == 0) {
            ++startDayOfYear;
        }

        cal.set(Calendar.DAY_OF_YEAR, startDayOfYear);
        int startMonth = cal.get(Calendar.MONTH) + 1;
        int startDay = cal.get(Calendar.DAY_OF_MONTH);

        int endDayOfYear = (int) Math.round(endDegrees / daysPerDegree);

        if (endDayOfYear == 0) {
            ++endDayOfYear;
        }

        cal.set(Calendar.DAY_OF_YEAR, endDayOfYear);
        int endMonth = cal.get(Calendar.MONTH) + 1;
        int endDay = cal.get(Calendar.DAY_OF_MONTH);

        rangeTF.setText(String.format("%02d%02d-%02d%02d", startMonth,
                startDay, endMonth, endDay));

        String[] dates = rangeTF.getText().split("-");
        Double startDegree = checkDate(dates[0]);
        Double endDegree = checkDate(dates[1]);
        rangeStartDegree = startDegree;
        rangeEndDegree = endDegree;
    }

    public void setRangeText(int startDayOfYear, int endDayOfYear) {
        cal.set(Calendar.DAY_OF_YEAR, startDayOfYear);
        int startMonth = cal.get(Calendar.MONTH) + 1;
        int startDay = cal.get(Calendar.DAY_OF_MONTH);
        cal.set(Calendar.DAY_OF_YEAR, endDayOfYear);
        int endMonth = cal.get(Calendar.MONTH) + 1;
        int endDay = cal.get(Calendar.DAY_OF_MONTH);

        rangeTF.setText(String.format("%02d%02d-%02d%02d", startMonth,
                startDay, endMonth, endDay));

        String[] dates = rangeTF.getText().split("-");
        Double startDegree = checkDate(dates[0]);
        Double endDegree = checkDate(dates[1]);
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
        if (!(charCount == 0 || charCount == 4)) {
            userInformation("Must enter a date in the form MMDD");
            valueTF.setFocus();
            return;
        } else if (charCount == 4) {
            valueTF.setText(valueTF.getText());
            if (valueTF.getText().matches("\\d{4}") == false) {
                userInformation("Must enter a date in the form MMDD");
                valueTF.setFocus();
                valueTF.selectAll();
                return;
            }
        }

        Double degree = checkDate(valueTF.getText());

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
        if (!(charCount == 0 || charCount == 9)) {
            userInformation("Must enter date range in the form MMDD-MMDD");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        } else if (charCount == 4) {
            rangeTF.setText(rangeTF.getText());
            if (rangeTF.getText().matches("\\d{4}-\\d{4}") == false) {
                userInformation("Must enter a date range in the form MMDD-MMDD");
                rangeTF.setFocus();
                rangeTF.selectAll();
                return;
            }
        }

        String[] dates = rangeTF.getText().split("-");

        Double startDegree = checkDate(dates[0]);

        Double endDegree = checkDate(dates[1]);

        if (startDegree.isNaN() == true) {
            userInformation("Start date is out of month/day range.");
            rangeTF.setFocus();
            rangeTF.selectAll();
            return;
        }

        if (endDegree.isNaN() == true) {
            userInformation("End date is out of month/day range.");
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
     * Validate the date.
     * 
     * @param time
     *            Array of date values (month, day).
     * @return Date as a degree.
     */
    private double checkDate(String mmdd) {
        double degrees = 0.0;

        int month = Integer.valueOf(mmdd.substring(0, 2)) - 1;
        int day = Integer.valueOf(mmdd.substring(2, 4));

        try {
            cal.set(Calendar.MONTH, month);
            cal.set(Calendar.DAY_OF_MONTH, day);
        } catch (ArrayIndexOutOfBoundsException ex) {
            ex.printStackTrace();
            return Double.NaN;
        }

        degrees = cal.get(Calendar.DAY_OF_YEAR) * daysPerDegree;

        return degrees;
    }

    /**
     * Get the number of days in a year.
     * 
     * @param year
     *            Current year.
     * @return Number of days.
     */
    public int daysInAYear(int year) {
        if (year % 4 == 0) {
            return 366;
        }

        if (year % 100 != 0) {
            return 366;
        }

        if (year % 400 == 0) {
            return 366;
        }

        return 365;
    }

    public String[] getDayRange() {
        String[] dayRange = new String[2];
        String[] str = rangeTF.getText().split("-");
        int m0 = Integer.parseInt(str[0].substring(0, 2));
        int d0 = Integer.parseInt(str[0].substring(2));
        int m1 = Integer.parseInt(str[1].substring(0, 2));
        int d1 = Integer.parseInt(str[1].substring(2));

        Calendar c = Calendar.getInstance();
        c.set(Calendar.MONTH, m0 - 1);
        c.set(Calendar.DAY_OF_MONTH, d0);
        dayRange[0] = Integer.toString(c.get(Calendar.DAY_OF_YEAR));
        c.set(Calendar.MONTH, m1 - 1);
        c.set(Calendar.DAY_OF_MONTH, d1);
        dayRange[1] = Integer.toString(c.get(Calendar.DAY_OF_YEAR));

        return dayRange;
    }
}
