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
package com.raytheon.uf.viz.monitor.hodograph;

import java.util.Calendar;
import java.util.Date;
import java.util.Map.Entry;
import java.util.SortedMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.monitor.data.ObTrendDataSet;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Hodograph dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 26, 2012  1280     skorolev  Changes for non-blocking dialog.
 * Nov. 1, 2012  1297     skorolev  Cleaned code
 * Jul 10, 2018  6766     randerso  Fixed time labels. Improved placement of
 *                                  direction labels.
 * Aug 16, 2018  6766     randerso  Fix plotting of wind direction
 * Aug 16, 2018  7410     randerso  Fix regression form DR #6766. Code cleanup.
 *
 * </pre>
 *
 * @author wkwock
 */
public class HodographDlg extends CaveSWTDialog {

    /** direction names **/
    private static final String dirName[] = { "N", "NNE", "NE", "ENE", "E",
            "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW",
            "NNW" };

    /** Center x pixel point **/
    private static final int CENTER_X = 300;

    /** Center y pixel point **/
    private static final int CENTER_Y = 300;

    /** Radius **/
    private static final int RADIUS = 260;

    /** red threshold in degrees **/
    private Point redThreshold;

    /** yellow threshold in degrees **/
    private Point yellowThreshold;

    /** green threshold in degrees **/
    private Point greenThreshold;

    /** current time **/
    private Date curTime = null;

    /** Hodograph data **/
    private SortedMap<Date, Float> hodographData;

    /**
     * Constructor
     *
     * @param parent
     * @param siteName
     * @param dataName
     * @param title
     */
    public HodographDlg(Shell parent, String siteName, String dataName,
            String title, ObTrendDataSet dataset) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        setText(title);
        setReturnValue(this.getText());
        this.hodographData = dataset.getDataSet();
        this.curTime = SimulatedTime.getSystemTime().getTime();
        setThresholds(dataset.getDualValuedThresholds());
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createHodoGraph();
        addCloseBttn();
    }

    /**
     * Convert wind direction which is clockwise from North, to geometric angle
     * which is counter-clockwise from East
     *
     * @param degree
     *            clockwise from North
     * @return degree counter-clockwise from East
     */
    private float convertDegree(float degree) {
        return (360 + 90 - degree) % 360;
    }

    private void setThresholds(float[] thresholds) {
        if (thresholds.length != 4) {
            throw new IllegalArgumentException(
                    "The thresholds array must be of length 4");
        }

        if (Float.isNaN(thresholds[0]) || Float.isNaN(thresholds[1])
                || Float.isNaN(thresholds[2]) || Float.isNaN(thresholds[3])) {
            greenThreshold = calculateThreshold(0, 0);
        } else {
            greenThreshold = calculateThreshold(0, 360);
        }
        redThreshold = calculateThreshold(thresholds[0], thresholds[1]);
        yellowThreshold = calculateThreshold(thresholds[2], thresholds[3]);
    }

    /**
     * Calculate threshold angles
     *
     * @param from
     *            starting angle in degrees clockwise from North
     * @param to
     *            ending angle in degrees clockwise from North
     * @return point with x = start angle, and y = arc angle, both in degrees
     */
    private Point calculateThreshold(float from, float to) {

        float span = to - from;
        if (span < 0) {
            span += 360.0f;
            from = to;
        }
        return new Point(Math.round(convertDegree(from)), Math.round(span));
    }

    /**
     * Convert polar coordinate to rectangular for plotting
     *
     * @param angle
     *            in degrees clockwise from North
     * @param distance
     *            distance in pixels from center
     * @return point defining the pixel location
     */
    private Point polarToRectangular(double angle, int distance) {
        double radians = Math.toRadians(angle);
        int x = CENTER_X + (int) Math.round(Math.sin(radians) * distance);
        int y = CENTER_Y - (int) Math.round(Math.cos(radians) * distance);

        return new Point(x, y);
    }

    /**
     * Plots time labels
     *
     * @param gc
     */
    private void plotTimeLbls(GC gc) {
        Calendar currentTime = TimeUtil.newGmtCalendar(this.curTime);
        for (int i = 13; i > 1; i--) {
            String timeLabel = String.format("%tH:30", currentTime);
            int x = CENTER_X - (gc.textExtent(timeLabel).x / 2) - 2;
            int y = CENTER_Y + RADIUS / 13 * i - gc.textExtent(timeLabel).y + 2;
            gc.drawText(timeLabel, x, y, true);
            currentTime.add(Calendar.HOUR_OF_DAY, -2);
        }
    }

    /**
     * Plots direction names.
     *
     * @param gc
     */
    private void plotDirNames(GC gc) {
        for (int i = 0; i < dirName.length; i++) {
            double angle = i * 360.0 / dirName.length;
            Point p = polarToRectangular(angle, RADIUS + 5);

            /*
             * Adjust text placement per octant
             */
            Point extent = gc.textExtent(dirName[i]);
            int dx = 0;
            int dy = 0;
            if (angle < 180.0 / 4) {
                dx = -extent.x / 2;
                dy = -extent.y;
            } else if (angle < 180.0 / 2) {
                dx = 0;
                dy = -extent.y / 2;
            } else if (angle < 180.0 * 3 / 4) {
                dx = 0;
                dy = -extent.y / 2;
            } else if (angle < 180.0) {
                dx = -extent.x / 2;
                dy = 0;
            } else if (angle < 180.0 * 5 / 4) {
                dx = -extent.x / 2;
                dy = 0;
            } else if (angle < 180.0 * 3 / 2) {
                dx = -extent.x;
                dy = 0;
            } else if (angle < 180.0 * 7 / 4) {
                dx = -extent.x;
                dy = -extent.y / 2;
            } else if (angle < 180.0 * 2) {
                dx = -extent.x / 2;
                dy = -extent.y;
            }
            gc.drawText(dirName[i], p.x + dx, p.y + dy, true);
        }
    }

    /**
     * Paint Background
     *
     * @param gc
     */
    private void paintBackground(GC gc) {
        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_GRAY));
        gc.fillOval(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2,
                RADIUS * 2);

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_GREEN));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2, RADIUS * 2,
                greenThreshold.x, greenThreshold.y);

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2, RADIUS * 2,
                yellowThreshold.x, yellowThreshold.y);

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2, RADIUS * 2,
                redThreshold.x, redThreshold.y);

        // goes back to original background color
        gc.setBackground(
                getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

    }

    /**
     * Plots wind direction
     *
     * @param gc
     */
    private void plotWindDir(GC gc) {
        if (hodographData == null) {
            return;
        }
        long curSec = curTime.getTime();

        boolean lastOneExist = false;
        int lastX = 0;
        int lastY = 0;

        gc.setLineWidth(1);
        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_DARK_BLUE));
        for (Entry<Date, Float> entry : hodographData.entrySet()) {
            Date obsTime = entry.getKey();
            float obsVal = entry.getValue();
            /*
             * When wind speed is zero, wind direction is set to be
             * ObConst.MISSING, which is -9999.0f;
             */
            if (obsVal < 0.0f) {
                // wind direction is missing
                lastOneExist = false;

                // missing value
                continue;
            }
            long thisSec = obsTime.getTime();
            if (thisSec > curSec) {
                lastOneExist = false;

                // not in the last 24 hour
                continue;
            }
            if (thisSec < (curSec - TimeUtil.MILLIS_PER_DAY)) {
                lastOneExist = false;

                // not in the last 24 hour
                continue;
            }

            int offsetFromCenter = (int) ((TimeUtil.MILLIS_PER_DAY - curSec
                    + thisSec) * RADIUS / TimeUtil.MILLIS_PER_DAY);

            Point p = polarToRectangular(obsVal, offsetFromCenter);

            gc.fillOval(p.x, p.y, 5, 6);
            if (lastOneExist) {
                gc.drawLine(p.x, p.y, lastX, lastY);
            }

            lastX = p.x;
            lastY = p.y;
            lastOneExist = true;
        }

    }

    /**
     * Creates canvas
     *
     * @param drawBoard
     */
    private void drawCanvas(Canvas drawBoard) {
        drawBoard.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                e.gc.setAntialias(SWT.ON);
                e.gc.setLineWidth(1);
                paintBackground(e.gc);
                // draw circles
                e.gc.setForeground(
                        getDisplay().getSystemColor(SWT.COLOR_DARK_MAGENTA));
                int numCircle = 13;
                for (int i = 1; i <= numCircle; i++) {
                    e.gc.drawOval(CENTER_X - RADIUS / numCircle * i,
                            CENTER_Y - RADIUS / numCircle * i,
                            RADIUS / numCircle * 2 * i,
                            RADIUS / numCircle * 2 * i);
                }

                // draw direction lines
                e.gc.setForeground(
                        getDisplay().getSystemColor(SWT.COLOR_BLACK));
                for (int i = 0; i < dirName.length; i++) {
                    double angle = i * 360 / dirName.length;
                    Point p = polarToRectangular(angle, RADIUS);
                    e.gc.drawLine(CENTER_X, CENTER_Y, p.x, p.y);
                }

                plotDirNames(e.gc);
                plotTimeLbls(e.gc);
                plotWindDir(e.gc);
            }
        });
    }

    /**
     * Adds Close button
     */
    private void addCloseBttn() {
        Composite c = new Composite(getShell(), SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        c.setLayout(gl);
        c.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(c, SWT.CLOSE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Plots trend graph
     */
    public void createHodoGraph() {

        Composite c = new Composite(this.getShell(), SWT.NONE);
        Canvas canvas = new Canvas(c, SWT.DIALOG_TRIM);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        canvas.setSize(600, 600);
        c.setLayoutData(data);
        drawCanvas(canvas);

    }
}
