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

package com.raytheon.viz.hydro.staffgage;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * Canvas displaying the staff gage.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 18 Nov 2008  1628       dhladky     Adjustments and such. 
 * 03 Aug 2010  4383       lbousaidi   fixed missing flows for Staff 
 * 									   Gage Window. 
 * 02 Feb 2011  4383 	   lbousaidi   added calcMaxStage and calcMinStage
 * 									   routines, also added several conditions 
 * 									   for the drawing on gage
 * 09 Sep 2013  #2349      lvenable   Fixed Font memory leak.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StaffGageCanvasComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Canvas font.
     */
    private Font canvasFont;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 440;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 500;

    /**
     * Width of the gage.
     */
    private final int GAGE_WIDTH = 50;

    /**
     * X coordinate of the canvas middle.
     */
    private final int CANVAS_MID_WIDTH = CANVAS_WIDTH / 2;

    /**
     * X coordinate of the gage's left side.
     */
    private final int GAGE_LEFT_X = CANVAS_MID_WIDTH - (GAGE_WIDTH / 2);

    /**
     * X coordinate of the gage's right side.
     */
    private final int GAGE_RIGHT_X = GAGE_LEFT_X + GAGE_WIDTH;

    /**
     * Height of the gage.
     */
    private final int GAGE_HEIGHT = CANVAS_HEIGHT - 40;

    /**
     * Number of pixels between the top of the gage and the top of the canvas.
     */
    private final int GAGE_PIX_FROM_TOP = 20;

    /**
     * Length of the large hash mark.
     */
    private final int LARGE_HASH_MARK = 10;

    /**
     * Length of the small hash mark.
     */
    private final int SMALL_HASH_MARK = 3;

    /**
     * X coordinate of the labels on the left side of the gage.
     */
    private final int LEFT_LABEL_XCOORD = 5;

    /**
     * X coordinate of the vertical stage line.
     */
    private final int VERT_STAGE_LINE_XCOORD = 100;

    /**
     * X coordinate of the stage label on the left side of the gage.
     */
    private final int LEFT_STAGE_LABEL_XCOORD = 120;

    /**
     * Canvas displaying the staff gage.
     */
    private Canvas stageCanvas;

    /**
     * Gage data to be displayed.
     */
    private StaffGageData gageData;

    /**
     * No STAGE Data Available string.
     */
    private static final String noDataString = "STAGE DATA UNAVAILABLE FOR STAFF GAGE DISPLAY.";

    /**
     * maximum stage default value
     */
    public static final int DEFAULT_MAX = 0;

    /**
     * minimum stage default value
     */
    public static final int DEFAULT_MIN = 0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param gageData
     *            Gage data to be displayed.
     */
    public StaffGageCanvasComp(Composite parent, StaffGageData gageData) {
        super(parent, SWT.NONE);

        this.parent = parent;
        this.gageData = gageData;

        init();
        this.pack();
    }

    /**
     * Initialize the canvas, layout, and font.
     */
    private void init() {
        this.setLayout(new RowLayout());

        canvasFont = new Font(parent.getDisplay(), "Monospace", 10, SWT.NORMAL);

        setupCanvas();

        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (canvasFont != null) {
                    canvasFont.dispose();
                    canvasFont = null;
                }
            }
        });
    }

    /**
     * Setup the canvas so it can be used.
     */
    private void setupCanvas() {
        stageCanvas = new Canvas(parent, SWT.DOUBLE_BUFFERED);
        stageCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        stageCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawStaffGage(e);
            }
        });
    }

    /**
     * Update the Staff Gage data and redraw the canvas.
     * 
     * @param gageData
     *            Staff Gage data.
     */
    public void updateStaffGageData(StaffGageData gageData) {

        this.gageData = gageData;

        stageCanvas.redraw();
    }

    /**
     * Draw the staff gage information on the canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawStaffGage(PaintEvent e) {
        // Get the flood , stage and action stage numbers.

        double recordStg = Double.valueOf(gageData.getRecordStage());
        double majorStg = Double.valueOf(gageData.getMajorCatStage());
        double moderStg = Double.valueOf(gageData.getModCatStage());
        double minorStg = Double.valueOf(gageData.getMinorCatStage());
        double floodStgNum = Double.valueOf(gageData.getFloodStage());
        double actionStgNum = Double.valueOf(gageData.getActionStage());
        double BankFullStg = Double.valueOf(gageData.getBankfullStage());
        double zeroDatum = Double.valueOf(gageData.getZeroDatum());

        // --------------------------------------------------------------
        // NOTE: the max and min stage values will need to be calculated
        // --------------------------------------------------------------
        double maxStageVal = calcMaxStage();
        double minStageVal = calcMinStage();

        e.gc.setFont(canvasFont);
        int fontMid = (int) (e.gc.getFontMetrics().getHeight() / 2);

        e.gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        e.gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // if Stage missing Draw the missing error message

        if ((maxStageVal == DEFAULT_MAX) && (minStageVal == DEFAULT_MIN)) {

            e.gc.setForeground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));
            int label_X = CANVAS_WIDTH - 400;
            int label_Y = CANVAS_HEIGHT / 2;
            e.gc.drawString(noDataString, label_X, label_Y - fontMid, true);

            return;

        } else {

            double maxPixValue = GAGE_HEIGHT + GAGE_PIX_FROM_TOP;
            double numHashs = maxStageVal - minStageVal;
            double pixelsPerInc = GAGE_HEIGHT / numHashs;

            // -----------------------------------------------------------------
            // Create RED area on GAGE representing the area above flood stage
            // -----------------------------------------------------------------
            if (floodStgNum > 0.0) {
                e.gc.setBackground(parent.getDisplay().getSystemColor(
                        SWT.COLOR_RED));

                e.gc.fillRectangle(GAGE_LEFT_X, GAGE_PIX_FROM_TOP, GAGE_WIDTH,
                        GAGE_HEIGHT);
            }
            // ----------------------------------------------------------------
            // Create YELLOW areas on gage only if an action stage is defined
            // ----------------------------------------------------------------
            if (actionStgNum > 0.0) {

                double ActionFloodStg = (floodStgNum > 0.0) ? floodStgNum
                        : maxStageVal;

                int yCoord1 = (int) (maxPixValue - Math
                        .round(((ActionFloodStg - minStageVal) * pixelsPerInc)));

                e.gc.setBackground(parent.getDisplay().getSystemColor(
                        SWT.COLOR_YELLOW));

                e.gc.fillRectangle(GAGE_LEFT_X, yCoord1, GAGE_WIDTH,
                        GAGE_HEIGHT - yCoord1 + GAGE_PIX_FROM_TOP);
            }

            // ------------------------------------------------------
            // Create GREEN area on gage, this color is always drawn
            // ------------------------------------------------------

            double floodActionMax = 0.0;
            if (actionStgNum > 0.0) {
                floodActionMax = actionStgNum;
            } else if (floodStgNum > 0.0) {
                floodActionMax = floodStgNum;
            } else {
                floodActionMax = maxStageVal;
            }

            int yCoord2 = (int) (maxPixValue - Math
                    .round(((floodActionMax - minStageVal) * pixelsPerInc)));
            e.gc.setBackground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_GREEN));

            e.gc.fillRectangle(GAGE_LEFT_X, yCoord2, GAGE_WIDTH, GAGE_HEIGHT
                    - yCoord2 + GAGE_PIX_FROM_TOP);

            // ------------------------------------------
            // Draw WHITE rectangle around gage
            // ------------------------------------------
            e.gc.setForeground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));

            e.gc.drawRectangle(GAGE_LEFT_X, GAGE_PIX_FROM_TOP, GAGE_WIDTH,
                    GAGE_HEIGHT);

            // ----------------------------------------
            // Draw gage hash marks
            // ----------------------------------------
            double counter = 0.0;

            for (int x = (int) minStageVal; x <= maxStageVal; ++x) {
                int yCoord = (int) (maxPixValue - Math
                        .round((counter * pixelsPerInc)));

                if (x % 5 == 0) {
                    // draw big hash
                    e.gc.drawLine(GAGE_RIGHT_X, yCoord, GAGE_RIGHT_X
                            + LARGE_HASH_MARK, yCoord);
                    e.gc.drawLine(GAGE_LEFT_X, yCoord, GAGE_LEFT_X
                            - LARGE_HASH_MARK, yCoord);
                } else {
                    // draw little hash
                    e.gc.drawLine(GAGE_RIGHT_X, yCoord, GAGE_RIGHT_X
                            + SMALL_HASH_MARK, yCoord);
                    e.gc.drawLine(GAGE_LEFT_X, yCoord, GAGE_LEFT_X
                            - SMALL_HASH_MARK, yCoord);
                }

                counter += 1.0;
            }

            // -------------------------------------
            // Draw labels
            // -------------------------------------

            //
            // Draw record label
            //
            double recordY = recordStg - minStageVal;

            int recordYCoord = (int) (maxPixValue - Math
                    .round((recordY * pixelsPerInc)));

            if (recordStg > 0.0) {
                e.gc.drawString("Record", LEFT_LABEL_XCOORD, recordYCoord
                        - fontMid, true); // 1
                if (majorStg > 0.0) {
                    e.gc.drawLine(VERT_STAGE_LINE_XCOORD, recordYCoord,
                            VERT_STAGE_LINE_XCOORD + LARGE_HASH_MARK,
                            recordYCoord);
                }
                double recDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    recDbl = recordStg + zeroDatum;
                }

                String recStr = String.format("%7.2f", recDbl);
                e.gc.drawString(recStr, LEFT_STAGE_LABEL_XCOORD, recordYCoord
                        - fontMid, true);

                e.gc.drawString(gageData.getRecordStage(), GAGE_RIGHT_X
                        + LARGE_HASH_MARK * 2, recordYCoord - fontMid, true);
            }
            //
            // Draw minimum label
            //
            int minYCoord = (int) (maxPixValue);

            e.gc.drawString(String.valueOf(minStageVal), GAGE_RIGHT_X
                    + LARGE_HASH_MARK * 2, minYCoord - fontMid, true);

            double minDbl = minStageVal;
            if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                minDbl = minStageVal + zeroDatum;
            }

            String minStr = String.format("%7.2f", minDbl);
            e.gc.drawString(minStr, LEFT_STAGE_LABEL_XCOORD, minYCoord
                    - fontMid, true);

            //
            // Draw maximum label
            //
            int maxYCoord = (int) (GAGE_PIX_FROM_TOP);
            e.gc.drawString(String.valueOf(maxStageVal), GAGE_RIGHT_X
                    + LARGE_HASH_MARK * 2, maxYCoord - fontMid, true);

            double maxDbl = maxStageVal;
            if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                maxDbl = maxStageVal + zeroDatum;
            }

            String maxStr = String.format("%7.2f", maxDbl);
            e.gc.drawString(maxStr, LEFT_STAGE_LABEL_XCOORD, maxYCoord
                    - fontMid, true);

            //
            // Draw Major Category
            //
            double majorY = majorStg - minStageVal;

            int majorYCoord = (int) (maxPixValue - Math
                    .round((majorY * pixelsPerInc)));

            if (majorStg > 0.0) {
                e.gc.drawString("Major", LEFT_LABEL_XCOORD, majorYCoord
                        - fontMid, true);
                if (moderStg > 0.0) {
                    e.gc.drawLine(VERT_STAGE_LINE_XCOORD, majorYCoord,
                            VERT_STAGE_LINE_XCOORD + LARGE_HASH_MARK,
                            majorYCoord);
                }
                double majDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    majDbl = majorStg + zeroDatum;
                }

                String majStr = String.format("%7.2f", majDbl);
                e.gc.drawString(majStr, LEFT_STAGE_LABEL_XCOORD, majorYCoord
                        - fontMid, true);

                e.gc.drawString(gageData.getMajorCatStage(), GAGE_RIGHT_X
                        + LARGE_HASH_MARK * 2, majorYCoord - fontMid, true);
            }
            //
            // Draw Moderate Category
            //
            double moderY = moderStg - minStageVal;

            int modYCoord = (int) (maxPixValue - Math
                    .round((moderY * pixelsPerInc)));

            if (moderStg > 0.0) {
                e.gc.drawString("Moderate", LEFT_LABEL_XCOORD, modYCoord
                        - fontMid, true);
                if (minorStg > 0.0) {
                    e.gc.drawLine(VERT_STAGE_LINE_XCOORD, modYCoord,
                            VERT_STAGE_LINE_XCOORD + LARGE_HASH_MARK, modYCoord);
                }

                double modDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    modDbl = moderStg + zeroDatum;
                }

                String modStr = String.format("%7.2f", modDbl);
                e.gc.drawString(modStr, LEFT_STAGE_LABEL_XCOORD, modYCoord
                        - fontMid, true);
                e.gc.drawString(gageData.getModCatStage(), GAGE_RIGHT_X
                        + LARGE_HASH_MARK * 2, modYCoord - fontMid, true);
            }

            // Draw Minor Category
            //

            double minorY = minorStg - minStageVal;

            int minorYCoord = (int) (maxPixValue - Math
                    .round((minorY * pixelsPerInc)));

            if (minorStg > 0.0) {
                e.gc.drawString("Minor", LEFT_LABEL_XCOORD, minorYCoord
                        - fontMid, true);
                if (moderStg > 0.0) {
                    e.gc.drawLine(VERT_STAGE_LINE_XCOORD, minorYCoord,
                            VERT_STAGE_LINE_XCOORD + LARGE_HASH_MARK,
                            minorYCoord);
                }
                double minorDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    minorDbl = minorStg + zeroDatum;
                }

                String minorStr = String.format("%7.2f", minorDbl);
                e.gc.drawString(minorStr, LEFT_STAGE_LABEL_XCOORD, minorYCoord
                        - fontMid, true);

                e.gc.drawString(gageData.getMinorCatStage(), GAGE_RIGHT_X
                        + LARGE_HASH_MARK * 2, minorYCoord - fontMid, true);
            }

            // Draw vertical stage line
            //

            if (minorStg > 0.0 && majorStg > 0.0) {
                e.gc.drawLine(VERT_STAGE_LINE_XCOORD, minorYCoord,
                        VERT_STAGE_LINE_XCOORD, majorYCoord);
            }

            // draw the line from rec to major in case the record is
            // outside the major value
            if (recordStg > 0.0 && majorStg > 0.0) {
                e.gc.drawLine(VERT_STAGE_LINE_XCOORD, recordYCoord,
                        VERT_STAGE_LINE_XCOORD, majorYCoord);
            }
            //
            // Draw flood stage
            //
            double floodY = floodStgNum - minStageVal;

            int floodYCoord = (int) (maxPixValue - Math
                    .round((floodY * pixelsPerInc)));
            if (floodStgNum > 0.0) {
                String floodStr = String.valueOf(gageData.getFloodStage())
                        + " Flood Stg";
                e.gc.drawString(floodStr, GAGE_RIGHT_X + LARGE_HASH_MARK * 2,
                        floodYCoord - fontMid, true);
            }
            // Draw action stage

            double actionY = actionStgNum - minStageVal;

            int actionYCoord = (int) (maxPixValue - Math
                    .round((actionY * pixelsPerInc)));

            if (actionStgNum > 0.0) {
                String actionStr = String.valueOf(gageData.getActionStage())
                        + " Action Stg";
                e.gc.drawString(actionStr, GAGE_RIGHT_X + LARGE_HASH_MARK * 2,
                        actionYCoord - fontMid, true);

                double actionDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    actionDbl = actionStgNum + zeroDatum;
                }
                String actionStr1 = String.format("%7.2f", actionDbl);
                e.gc.drawString(actionStr1, LEFT_STAGE_LABEL_XCOORD,
                        actionYCoord - fontMid, true);
            }
            // Draw Bankfull stage

            double BankFullStgY = BankFullStg - minStageVal;

            int BankFullYCoord = (int) (maxPixValue - Math
                    .round((BankFullStgY * pixelsPerInc)));

            if (BankFullStg > 0.0) {
                String BankFullStr = String
                        .valueOf(gageData.getBankfullStage());
                e.gc.drawString(BankFullStr,
                        GAGE_RIGHT_X + LARGE_HASH_MARK * 2, BankFullYCoord
                                - fontMid, true);

                double bankfDbl = 0.0;
                if (!gageData.getZeroDatum().equals(StaffGageDlg.MISSING)) {
                    bankfDbl = BankFullStg + zeroDatum;
                }

                String bankfStr = String.format("%7.2f", bankfDbl);
                e.gc.drawString(bankfStr, LEFT_STAGE_LABEL_XCOORD,
                        BankFullYCoord - fontMid, true);
            }
        }
    }

    /**
     * calculate minimum stage value
     **/

    public double calcMinStage() {
        double minStageVal = DEFAULT_MIN;
        double lMin = 0.0;
        double tmpMin = 0.0;
        /*
         * Compare min to each of the significant stage values and determine the
         * minimum.
         */
        if ((Double.valueOf(gageData.getMinorCatStage()) < minStageVal)
                && (Double.valueOf(gageData.getMinorCatStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getMinorCatStage());

        if ((Double.valueOf(gageData.getModCatStage()) < minStageVal)
                && (Double.valueOf(gageData.getModCatStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getModCatStage());

        if ((Double.valueOf(gageData.getMajorCatStage()) < minStageVal)
                && (Double.valueOf(gageData.getMajorCatStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getMajorCatStage());

        if ((Double.valueOf(gageData.getRecordStage()) < minStageVal)
                && (Double.valueOf(gageData.getRecordStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getRecordStage());

        if ((Double.valueOf(gageData.getFloodStage()) < minStageVal)
                && (Double.valueOf(gageData.getFloodStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getFloodStage());

        if ((Double.valueOf(gageData.getActionStage()) < minStageVal)
                && (Double.valueOf(gageData.getActionStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getActionStage());

        if ((Double.valueOf(gageData.getBankfullStage()) < minStageVal)
                && (Double.valueOf(gageData.getBankfullStage()) > 0.0))
            minStageVal = Double.valueOf(gageData.getBankfullStage());

        // Round to the nearest multiple of five (5)

        if ((minStageVal > 0.0) && (minStageVal < DEFAULT_MIN)) {

            tmpMin = minStageVal;
            lMin = (long) Math.floor(minStageVal / 5);
            minStageVal = lMin * 5;

            // extend the max value if min value and the nearest stage are
            // within 1.0 ft of each other

            if ((tmpMin - minStageVal) < 1.0) {
                minStageVal -= 2.0;
            }

        } else {
            minStageVal = 0.0;
        }

        return minStageVal;
    }

    /**
     * calculate maximum stage value
     **/

    public double calcMaxStage() {

        double maxStageVal = DEFAULT_MAX;
        double lMax = 0.0;
        double tmpMax = 0.0;
        /*
         * Compare max to each of the significant stage values and determine the
         * maximum.
         */
        if ((Double.valueOf(gageData.getMinorCatStage()) > maxStageVal)
                && (Double.valueOf(gageData.getMinorCatStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getMinorCatStage());

        if ((Double.valueOf(gageData.getModCatStage()) > maxStageVal)
                && (Double.valueOf(gageData.getModCatStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getModCatStage());

        if ((Double.valueOf(gageData.getMajorCatStage()) > maxStageVal)
                && (Double.valueOf(gageData.getMajorCatStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getMajorCatStage());

        if ((Double.valueOf(gageData.getRecordStage()) > maxStageVal)
                && (Double.valueOf(gageData.getRecordStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getRecordStage());

        if ((Double.valueOf(gageData.getFloodStage()) > maxStageVal)
                && (Double.valueOf(gageData.getFloodStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getFloodStage());

        if ((Double.valueOf(gageData.getActionStage()) > maxStageVal)
                && (Double.valueOf(gageData.getActionStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getActionStage());

        if ((Double.valueOf(gageData.getBankfullStage()) > maxStageVal)
                && (Double.valueOf(gageData.getBankfullStage()) > 0.0))
            maxStageVal = Double.valueOf(gageData.getBankfullStage());

        if (maxStageVal > DEFAULT_MAX) {
            tmpMax = maxStageVal;
            lMax = (long) Math.floor(maxStageVal / 5);
            maxStageVal = (lMax + 1) * 5;

            /*
             * If the max value and the nearest stage are within 1.0 ft of each
             * other, extend the max value.
             */
            if ((maxStageVal - tmpMax) < 1.0) {
                maxStageVal += 2.0;
            }

        }

        return maxStageVal;
    }

}
