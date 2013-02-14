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
import java.util.SortedMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Hodograph dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012 1280       skorolev    Changes for non-blocking dialog.
 * Nov. 1, 2012 1297       skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public class HodographDlg extends CaveSWTDialog {

    /** direction names **/
    private final String dirName[] = { "N", "NNE", "NE", "ENE", "E", "ESE",
            "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW" };

    /** red start threshold in degree **/
    private float redStartDegree = 0;

    /** red threshold in degree **/
    private float redDegree = 0;

    /** yellow start threshold in degree **/
    private float yellowStartDegree = 0;

    /** yellow threshold in degree **/
    private float yellowDegree = 0;

    /** green start threshold in degree **/
    private float greenStartDegree = 0;

    /** green threshold in degree **/
    private float greenDegree = 0;

    /** Center x pixel point **/
    private final int CENTER_X = 285;

    /** Center y pixel point **/
    private final int CENTER_Y = 280;

    /** Radius **/
    private final int RADIUS = 260;

    /** current time **/
    private Calendar curTime = null;

    /** Hodograph data **/
    private SortedMap<Date, Float> hodographData;

    /** constant milliseconds in a day **/
    private final double milliSecInADay = 24.0 * 60 * 60 * 1000.0;

    /** display **/
    private Display display;

    /**
     * Constructor
     * 
     * @param parent
     * @param siteName
     * @param dataName
     * @param title
     */
    public HodographDlg(Shell parent, String siteName, String dataName,
            String title) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        this.display = parent.getDisplay();
        setText(title);
        setReturnValue(this.getText());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createHodoGraph();
        addCloseBttn();
    }

    /**
     * In Hodogrpah, the 0 degree is at North, and increase clockwise, opposite
     * in geometry.
     * 
     * @param degree
     * @return
     */
    private float convertDegree(float degree) {
        return (360 + 90 - degree) % 360;
    }

    /**
     * Sets Green Threshold
     * 
     * @param greenFromDegree
     * @param greenToDegree
     */
    public void setGreenThreshold(float greenFromDegree, float greenToDegree) {
        if (greenToDegree < greenFromDegree)
            this.greenDegree = ((greenFromDegree - greenToDegree) - 360) % 360;
        else
            this.greenDegree = greenFromDegree - greenToDegree;
        this.greenStartDegree = convertDegree(greenFromDegree);
    }

    /**
     * Sets Yellow Threshold
     * 
     * @param yellowFromDegree
     * @param yellowToDegree
     */
    public void setYellowThreshold(float yellowFromDegree, float yellowToDegree) {
        if (yellowToDegree < yellowFromDegree)
            this.yellowDegree = ((yellowFromDegree - yellowToDegree) - 360) % 360;
        else
            this.yellowDegree = yellowFromDegree - yellowToDegree;
        this.yellowStartDegree = convertDegree(yellowFromDegree);
    }

    /**
     * Sets Red Threshold
     * 
     * @param redFromDegree
     * @param redToDegree
     */
    public void setRedThreshold(float redFromDegree, float redToDegree) {
        if (redToDegree < redFromDegree)
            this.redDegree = ((redFromDegree - redToDegree) - 360) % 360;
        else
            this.redDegree = redFromDegree - redToDegree;

        this.redStartDegree = convertDegree(redFromDegree);
    }

    /**
     * Converts degrees to plot coordinate Y
     * 
     * @param degree
     * @return
     */
    private int degreeToY(double degree) {
        int y = CENTER_Y - (int) Math.round((Math.cos(degree) * RADIUS));
        return y;
    }

    /**
     * Converts degrees to plot coordinate X
     * 
     * @param degree
     * @return
     */
    private int degreeToX(double degree) {
        int x = CENTER_X + (int) Math.round((Math.sin(degree) * RADIUS));
        return x;
    }

    /**
     * Plots time labels
     * 
     * @param gc
     */
    private void plotTimeLbls(GC gc) {
        Calendar currentTime = (Calendar) curTime.clone();
        String timeLabel = String.format("%tH:30", currentTime);
        for (int i = 13; i > 1; i--) {
            int x = CENTER_X - (gc.textExtent(timeLabel).x / 2) - 2;
            int y = CENTER_Y + RADIUS / 13 * i - gc.textExtent(timeLabel).y + 2;
            gc.drawText(timeLabel, x, y, true);
            currentTime.add(Calendar.HOUR_OF_DAY, -2);
        }
        gc.setLineWidth(1);
    }

    /**
     * Plots direction names.
     * 
     * @param gc
     */
    private void plotDirNames(GC gc) {
        for (int i = 0; i < dirName.length; i++) {
            double radian = Math.PI * 2.0 * i / dirName.length;
            int x = CENTER_X
                    + (int) Math.round((Math.sin(radian) * (RADIUS + 10 + gc
                            .textExtent(dirName[i]).x)));
            int y = CENTER_Y
                    - (int) Math.round((Math.cos(radian) * (RADIUS + 10 + gc
                            .textExtent(dirName[i]).x)));
            gc.drawText(dirName[i], x, y);
        }
    }

    /**
     * Paint Background
     * 
     * @param gc
     */
    private void paintBackground(GC gc) {
        gc.setBackground(display.getSystemColor(SWT.COLOR_GRAY));
        gc.fillOval(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2,
                RADIUS * 2);

        gc.setBackground(display.getSystemColor(SWT.COLOR_GREEN));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2,
                RADIUS * 2, (int) Math.round(greenStartDegree),
                (int) Math.round(greenDegree));

        gc.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2,
                RADIUS * 2, (int) Math.round(yellowStartDegree),
                (int) Math.round(yellowDegree));

        gc.setBackground(display.getSystemColor(SWT.COLOR_RED));
        gc.fillArc(CENTER_X - RADIUS, CENTER_Y - RADIUS, RADIUS * 2,
                RADIUS * 2, (int) Math.round(redStartDegree),
                (int) Math.round(redDegree));

        // goes back to original background color
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

    }

    /**
     * Plots wind direction
     * 
     * @param gc
     */
    private void plotWindDir(GC gc) {
        gc.setBackground(display.getSystemColor(SWT.COLOR_DARK_BLUE));
        if (hodographData == null)
            return;
        long curSec = curTime.getTimeInMillis();

        boolean lastOneExist = false;
        int lastX = 0;
        int lastY = 0;

        for (Date obsTime : hodographData.keySet()) {
            float obsVal = hodographData.get(obsTime);
            /*
             * When wind speed is zero, wind direction is set to be
             * ObConst.MISSING, which is -9999.0f;
             */
            if (obsVal < 0.0f) { // wind direction is missing
                lastOneExist = false;
                continue; // missing value
            }
            double obsValInRadian = convertDegree(obsVal) * Math.PI * 2.0
                    / 360.0;
            long thisSec = obsTime.getTime();
            if (thisSec > curSec) {
                lastOneExist = false;
                continue; // not in the last 24 hour
            }
            if (thisSec < (curSec - milliSecInADay)) {
                lastOneExist = false;
                continue; // not in the last 24 hour
            }

            double offsetFromCenter = (milliSecInADay - curSec + thisSec)
                    / milliSecInADay * RADIUS;

            int x = CENTER_X
                    + (int) Math.round(offsetFromCenter
                            * Math.cos(obsValInRadian));
            int y = CENTER_Y
                    - (int) Math.round(offsetFromCenter
                            * Math.sin(obsValInRadian));

            gc.fillOval(x, y, 5, 6);
            if (lastOneExist)
                gc.drawLine(x, y, lastX, lastY);

            lastX = x;
            lastY = y;
            lastOneExist = true;
        }

    }

    /**
     * Sets hodograph data
     * 
     * @param hodographData
     */
    public void setHodoGraphData(SortedMap<Date, Float> hodographData) {
        this.hodographData = hodographData;
    }

    /**
     * Creates canvas
     * 
     * @param drawBoard
     */
    private void drawCanvas(Canvas drawBoard) {
        drawBoard.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAntialias(SWT.ON);
                e.gc.setLineWidth(1);
                paintBackground(e.gc);
                // draw circles
                e.gc.setForeground(display
                        .getSystemColor(SWT.COLOR_DARK_MAGENTA));
                int numCircle = 13;
                for (int i = 1; i <= numCircle; i++)
                    e.gc.drawOval(CENTER_X - RADIUS / numCircle * i, CENTER_Y
                            - RADIUS / numCircle * i, RADIUS / numCircle * 2
                            * i, RADIUS / numCircle * 2 * i);

                // draw direction lines
                e.gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
                for (int i = 0; i < 16; i++) {
                    int x2 = degreeToX(i / 16.0 * Math.PI * 2.0);
                    int y2 = degreeToY(i / 16.0 * Math.PI * 2.0);
                    e.gc.drawLine(CENTER_X, CENTER_Y, x2, y2);
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
        display = c.getDisplay();
        Canvas canvas = new Canvas(c, SWT.DIALOG_TRIM);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        canvas.setSize(600, 600);
        c.setLayoutData(data);
        drawCanvas(canvas);

    }

    /**
     * Sets current time
     * 
     * @param currentTime
     */
    public void setCurrentTime(Calendar currentTime) {
        this.curTime = currentTime;
    }
}
