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
package com.raytheon.viz.hydrocommon.ratingcurve;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * This class contains the Rating Curve Canvas and draws the data on the canvas.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Jul 15, 2013 2088        rferrel     Code clean part of non-blocking dialogs.
 * 09 Sep 2013  #2349       lvenable    Fixed Font memory leak.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RatingCurveCanvasComp extends Canvas {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Reference back to parent dialog.
     */
    private RatingCurveDlg parentDlg = null;

    /**
     * Font used when drawing text on the canvas.
     */
    private Font canvasFont;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 625;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 500;

    /**
     * Y coordinate of the horizontal graph line.
     */
    private final int HLINE_YCOORD = CANVAS_HEIGHT - 50;

    /**
     * X coordinate of the vertical graph line.
     */
    private final int VLINE_XCOORD = 100;

    /**
     * Length of the horizontal graph line.
     */
    private final int HLINE_LENGTH = 500;

    /**
     * Length of the vertical graph line.
     */
    private final int VLINE_LENGTH = 400;

    /**
     * Length of the graph line hash mark.
     */
    private final int HASH_MARK = 5;

    /**
     * Stage feet hash label x coordinate.
     */
    private final int STAGE_FEET_HASH_LABEL_X = 40;

    /**
     * Stage feet text.
     */
    private final String STAGE_FEET = "STAGE FEET";

    /**
     * Flow KCFS text.
     */
    private final String FLOW_KCFS = "FLOW KCFS";

    /**
     * Maximum stage value in feet.
     */
    private double maxStageFeet = 0.0;

    /**
     * Minimum stage value in feet.
     */
    private double minStageFeet = 0.0;

    /**
     * Maximum flow KCFS value.
     */
    private double maxFlowKcfs = 0.0;

    /**
     * Minimum flow KCFS value.
     */
    private double minFlowKcfs = 0.0;

    /**
     * Divisor for the flow value.
     */
    private double FLOW_DIVISOR = 10000.0;

    /**
     * Flood value.
     */
    private double floodValDbl = 0.0;

    /**
     * Record value.
     */
    private double recordValDbl = 0.0;

    /**
     * Array of curve data.
     */
    private List<RatingCurveData> curveDataArray;

    /**
     * Number of hashes on the vertical graph line.
     */
    double vNumHashs = 0.0;

    /**
     * Maximum stage feet label value.
     */
    int maxStageFeetLabelVal = 0;

    private double minStageFeetLabelVal = 0;

    /**
     * Number of hashes on the horizontal graph line.
     */
    double hNumHashs = 0.0;

    /**
     * Maximum KCFS label value.
     */
    int maxKcfsLabelVal = 0;

    /**
     * Pixels per major increment on the vertical graph line (multiple of 10).
     */
    double vPixelsPerIncBase10 = 0.0;

    /**
     * Pixels per increment on the vertical graph line.
     */
    double vPixelsPerInc = 0.0;

    /**
     * Pixels per major increment on the horizontal graph line (multiple of 10).
     */
    double hPixelsPerIncBase10 = 0.0;

    /**
     * Pixels per increment on the horizontal graph line.
     */
    double hPixelsPerInc = 0.0;

    /**
     * Flag for mouse down or not.
     */
    boolean isMouseDown = false;

    /**
     * Current X location of mouse pointer.
     */
    int currentX;

    /**
     * Current Y location of mouse pointer.
     */
    int currentY;

    /**
     * Shift amount
     */
    private double shiftAmount = 0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param curveDataArray
     *            Array of curve data.
     * @param floodVal
     *            Flood value.
     * @param recordDbl
     *            Record value.
     */
    public RatingCurveCanvasComp(Composite parent, RatingCurveDlg parentDlg,
            List<RatingCurveData> curveDataArray, double floodVal,
            double recordDbl, double shiftAmount) {
        super(parent, SWT.DOUBLE_BUFFERED);

        this.parent = parent;

        floodValDbl = floodVal;
        recordValDbl = recordDbl;
        this.curveDataArray = curveDataArray;
        this.parentDlg = parentDlg;
        this.shiftAmount = shiftAmount;

        init();
        this.pack();
    }

    /**
     * Update on the fly
     * 
     * @param curveDataArray
     * @param floodVal
     * @param recordDbl
     */
    public void updateCurveData(List<RatingCurveData> curveDataArray,
            double floodVal, double recordDbl, double shiftAmount) {
        floodValDbl = floodVal;
        recordValDbl = recordDbl;
        this.curveDataArray = curveDataArray;
        this.shiftAmount = shiftAmount;

        calcMaxStageMaxFlow();
        calcPixelsPerIncrement();
        setupCanvas();

        this.redraw();
    }

    /**
     * Initialize method.
     */
    private void init() {
        setLayout(new RowLayout());

        canvasFont = new Font(parent.getDisplay(), "Monospace", 10, SWT.NORMAL);

        calcMaxStageMaxFlow();
        calcPixelsPerIncrement();
        setupCanvas();

        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (canvasFont != null) {
                    canvasFont.dispose();
                }
            }
        });
    }

    /**
     * Setup the canvas.
     */
    private void setupCanvas() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        setLayoutData(gd);
        addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                if (curveDataArray != null) {
                    drawRatingCurve(e.gc);
                }
            }
        });

        addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Intentionally blank
            }

            @Override
            public void mouseDown(MouseEvent e) {
                handleMouseDownEvent(e);
            }

            @Override
            public void mouseUp(MouseEvent e) {
                handleMouseUpEvent(e);
            }
        });

        addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                handleMouseMoveEvent(e);
            }
        });

    }

    /**
     * Calculate the min/max stage and min/max flow values.
     */
    private void calcMaxStageMaxFlow() {
        maxStageFeet = -999;
        maxFlowKcfs = 0.0;
        minStageFeet = 0.0;
        minFlowKcfs = 0.0;

        if (curveDataArray != null) {
            for (RatingCurveData curveData : curveDataArray) {
                if (curveData.getStage() > maxStageFeet) {
                    maxStageFeet = curveData.getStage();
                }

                if (curveData.getDischarge() > maxFlowKcfs) {
                    maxFlowKcfs = curveData.getDischarge();
                }

                if (curveData.getStage() < minStageFeet) {
                    minStageFeet = curveData.getStage();
                }

                if (curveData.getDischarge() < minFlowKcfs) {
                    minFlowKcfs = curveData.getDischarge();
                }
            }

            // Check the flood and record stage values
            if (maxStageFeet < floodValDbl) {
                maxStageFeet = floodValDbl;
            }

            if (maxStageFeet < recordValDbl) {
                maxStageFeet = recordValDbl;
            }

            if (maxFlowKcfs > 100000) {
                FLOW_DIVISOR = 100000;
            } else if (maxFlowKcfs > 10000) {
                FLOW_DIVISOR = 10000;
            } else {
                FLOW_DIVISOR = 1000;
            }
            maxFlowKcfs /= FLOW_DIVISOR;

            if (minStageFeet >= 0) {
                minStageFeetLabelVal = 0;
            } else {
                minStageFeetLabelVal = Math.round((minStageFeet - 5) / 10) * 10;
            }
        }

        vNumHashs = Math
                .round((((maxStageFeet - minStageFeetLabelVal) / 10) + 0.5d));
        maxStageFeetLabelVal = (int) ((vNumHashs * 10) - Math
                .abs(minStageFeetLabelVal));
    }

    /**
     * Calculate the pixels per increment for the horizontal and vertical graph
     * lines.
     */
    private void calcPixelsPerIncrement() {
        vPixelsPerInc = VLINE_LENGTH
                / (maxStageFeetLabelVal - minStageFeetLabelVal);
        vPixelsPerIncBase10 = (VLINE_LENGTH / vNumHashs);

        hNumHashs = Math.round(((maxFlowKcfs - minFlowKcfs) + 0.5d));
        maxKcfsLabelVal = (int) (hNumHashs * 10);
        hPixelsPerIncBase10 = HLINE_LENGTH / hNumHashs;
        hPixelsPerInc = HLINE_LENGTH / maxKcfsLabelVal;
    }

    /**
     * Draw the rating curve.
     * 
     * @param e
     *            Paint event.
     */
    private void drawRatingCurve(GC gc) {
        gc.setFont(canvasFont);
        int fontHeightMid = (gc.getFontMetrics().getHeight() / 2);
        int fontHeight = (gc.getFontMetrics().getHeight());
        int fontAveWidth = (gc.getFontMetrics().getAverageCharWidth());

        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        if (isMouseDown) {
            drawCrossHairs(gc);
        }
        // ----------------------------------------
        // Draw X & Y graph labels
        // ----------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));

        char[] charArray = STAGE_FEET.toCharArray();

        int tmpY = 150;
        for (int i = 0; i < charArray.length; i++) {
            gc.drawString(String.valueOf(charArray[i]), 5, tmpY, true);
            tmpY += fontHeight;
        }

        gc.drawString(FLOW_KCFS, CANVAS_WIDTH / 2 - fontAveWidth * 5,
                CANVAS_HEIGHT - fontHeight - 3, true);

        // ----------------------------------------
        // Draw horizontal and vertical axis lines
        // ----------------------------------------

        gc.drawLine(VLINE_XCOORD, HLINE_YCOORD, VLINE_XCOORD, HLINE_YCOORD
                - VLINE_LENGTH);

        gc.drawLine(VLINE_XCOORD, HLINE_YCOORD, VLINE_XCOORD + HLINE_LENGTH,
                HLINE_YCOORD);

        // ------------------------------------------
        // Draw Flood and Record lines
        // ------------------------------------------

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        int floodYCoord = (int) (HLINE_YCOORD - Math
                .round(((floodValDbl - minStageFeetLabelVal) * vPixelsPerInc)));

        gc.drawLine(VLINE_XCOORD + 1, floodYCoord, VLINE_XCOORD + HLINE_LENGTH,
                floodYCoord);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));

        int recordYCoord = (int) (HLINE_YCOORD - Math
                .round(((recordValDbl - minStageFeetLabelVal) * vPixelsPerInc)));

        gc.drawLine(VLINE_XCOORD + 1, recordYCoord,
                VLINE_XCOORD + HLINE_LENGTH, recordYCoord);

        // ----------------------------------------
        // Draw Stage Feet hash marks and labels
        // ----------------------------------------

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        for (int x = 0; x <= vNumHashs; ++x) {
            int yCoord = (int) (HLINE_YCOORD - Math
                    .round((x * vPixelsPerIncBase10)));

            if (yCoord >= HLINE_YCOORD - VLINE_LENGTH) {
                // draw hashmark
                gc.drawLine(VLINE_XCOORD, yCoord, VLINE_XCOORD - HASH_MARK,
                        yCoord);

                // draw label
                gc.drawString(
                        String.format("%5.1f", minStageFeetLabelVal + (x * 10)),
                        STAGE_FEET_HASH_LABEL_X, yCoord - fontHeightMid, true);
            }
        }

        // ----------------------------------------
        // Draw KCFS hash marks and labels
        // ----------------------------------------

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));

        String kcfsStr;

        for (int x = 0; x <= hNumHashs; ++x) {
            int xCoord = (int) (VLINE_XCOORD + Math
                    .round((x * hPixelsPerInc * 10)));

            if (xCoord <= VLINE_XCOORD + HLINE_LENGTH) {
                gc.drawLine(xCoord, HLINE_YCOORD, xCoord, HLINE_YCOORD
                        + HASH_MARK);

                kcfsStr = String.format("%5.1f",
                        (float) (x * FLOW_DIVISOR / 1000));
                gc.drawString(kcfsStr, xCoord
                        - ((kcfsStr.length() * fontAveWidth) / 2)
                        - fontAveWidth, HLINE_YCOORD + HASH_MARK * 2, true);
            }
        }

        // --------------------------------------------
        // Draw the Rating Curve
        // --------------------------------------------

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_CYAN));

        int oldX = 0;
        int oldY = 0;

        boolean firstTime = true;

        int circleWidth = 4;
        int circleHeight = 4;
        for (RatingCurveData curveData : curveDataArray) {
            int xCoord = (int) (VLINE_XCOORD + Math.round((((curveData
                    .getDischarge() / FLOW_DIVISOR) * 10) * hPixelsPerInc)));

            int yCoord = 0;
            if (shiftAmount < 0) {
                yCoord = (int) (HLINE_YCOORD - Math.round(((curveData
                        .getStage() - minStageFeetLabelVal) * vPixelsPerInc)));
            } else {
                yCoord = (int) (HLINE_YCOORD - Math.round(((curveData
                        .getStage()) * vPixelsPerInc)));
            }

            if (firstTime) {
                oldX = xCoord;
                oldY = yCoord;
                firstTime = false;
            }

            gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_CYAN));
            gc.drawLine(oldX, oldY, xCoord, yCoord);

            gc.setForeground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_YELLOW));

            gc.drawOval(xCoord - circleWidth / 2, yCoord - circleHeight / 2,
                    circleWidth, circleHeight);

            oldX = xCoord;
            oldY = yCoord;
        }
    }

    /**
     * Draw the cross hairs on the graph when the left mouse button is clicked.
     * 
     * @param gc
     *            The current GC
     */
    private void drawCrossHairs(GC gc) {
        if ((currentX > VLINE_XCOORD)
                && (currentX < VLINE_XCOORD + HLINE_LENGTH)
                && (currentY > HLINE_YCOORD - VLINE_LENGTH)
                && (currentY < HLINE_YCOORD)) {
            int x1 = VLINE_XCOORD;
            int y1 = currentY;
            int x2 = currentX;
            int y2 = currentY;
            gc.setForeground(parent.getDisplay()
                    .getSystemColor(SWT.COLOR_WHITE));
            gc.setLineWidth(1);
            gc.drawLine(x1, y1, x2, y2);

            x1 = currentX;
            y1 = HLINE_YCOORD - VLINE_LENGTH;
            x2 = currentX;
            y2 = currentY;
            gc.drawLine(x1, y1, x2, y2);

            x1 = currentX;
            y1 = currentY;
            x2 = currentX;
            y2 = HLINE_YCOORD;
            gc.drawLine(x1, y1, x2, y2);

            x1 = currentX;
            y1 = currentY;
            x2 = VLINE_XCOORD + HLINE_LENGTH;
            y2 = currentY;
            gc.drawLine(x1, y1, x2, y2);

            gc.setLineWidth(1);
        }
    }

    /**
     * Update the Stage and KCFS labels when moving mouse and left mouse button
     * is pressed.
     */
    private void updateLabels() {
        if ((currentX > VLINE_XCOORD)
                && (currentX < VLINE_XCOORD + HLINE_LENGTH)
                && (currentY > HLINE_YCOORD - VLINE_LENGTH)
                && (currentY < HLINE_YCOORD)) {
            double q = (((currentX - VLINE_XCOORD) / (hPixelsPerInc * 100) * FLOW_DIVISOR)) / 100;
            double stage = (((currentY - (minStageFeetLabelVal * vPixelsPerInc) - HLINE_YCOORD)
                    / vPixelsPerInc * -1));
            parentDlg.getStageDataLbl().setText(
                    String.valueOf(String.format("%5.1f", stage)));
            parentDlg.getKcfsDataLbl().setText(
                    String.valueOf(String.format("%5.1f", q)));
        }
    }

    private void handleMouseDownEvent(MouseEvent e) {
        isMouseDown = true;
    }

    private void handleMouseUpEvent(MouseEvent e) {
        isMouseDown = false;
        redraw();
        parentDlg.getStageDataLbl().setText("");
        parentDlg.getKcfsDataLbl().setText("");
    }

    private void handleMouseMoveEvent(MouseEvent e) {
        currentX = e.x;
        currentY = e.y;
        redraw();
        if (isMouseDown) {
            updateLabels();
        }
    }
}
