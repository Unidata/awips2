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
package com.raytheon.viz.hydrocommon.cresthistory;

import java.text.DecimalFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class draws the crest data on the canvas.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 3, 2008				lvenable	Initial creation
 * Nov 20, 2008   1682  dhladky   made interactive.
 * Jul 16, 2012  15181      wkwock      Init the counts
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CrestHistoryCanvas extends Canvas {
    /**
     * Parent component.
     */
    private Composite parentComp;

    /**
     * Text font on the canvas.
     */
    private Font canvasFont;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 700;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 520;

    /**
     * Y coordinate of the horizontal axis line.
     */
    private final int HLINE_YCOORD = CANVAS_HEIGHT - 40;

    /**
     * X coordinate of the vertical axis line.
     */
    private final int VLINE_XCOORD = 80;

    private final int HLINE_LEFT_OFFSET = 20;

    /**
     * Length of the horizontal axis line.
     */
    private final int HLINE_LENGTH = CANVAS_WIDTH - VLINE_XCOORD
            - HLINE_LEFT_OFFSET;

    /**
     * Length of the vertical axis line.
     */
    private final int VLINE_LENGTH = 450;

    /**
     * The number of pixels the vertical lines is from the top of the canvas.
     */
    private final int VLINE_PIXELS_FROM_TOP = CANVAS_HEIGHT - 40 - VLINE_LENGTH;

    /**
     * "STAGE IN FEET" text.
     */
    private final String STAGE_IN_FEET = "STAGE IN FEET";

    /**
     * Hash mark length.
     */
    private final int HASH_MARK = 10;

    /**
     * Small hash mark length.
     */
    private final int SMALL_HASH_MARK = 5;

    /**
     * Label Y coordinate offset.
     */
    private final int LABEL_Y_OFFSET = 15;

    /**
     * Label X coordinate offset.
     */
    private final int LABEL_X_OFFSET = 5;

    /**
     * Starting X coordinate for the year hash mark.
     */
    private final int YEAR_HASH_X_COORD_START = VLINE_XCOORD + 15;

    /**
     * Maximum stage value.
     */
    private double stageMaxVal = 0.0;

    /**
     * Minimum stage value.
     */
    private double stageMinVal = 0.0;

    /**
     * Number of MAJOR data items.
     */
    private int majorCount = 0;

    /**
     * Number of MODERATE data items.
     */
    private int modCount = 0;

    /**
     * Number of ACTION data items.
     */
    private int actionCount = 0;

    /**
     * Number of MINOR data items.
     */
    private int minorCount = 0;

    /**
     * Crest history data.
     */
    private CrestHistoryData crestHistoryData;

    /**
     * Map of the crest data (value) and the rectangle area on the canvas the
     * data occupies (key).
     */
    private HashMap<Rectangle, CrestDrawData> crestDataMap;

    /**
     * Callback for selecting crest data.
     */
    private ISelectedCrestData selectCrestDataCB;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param crestHistoryData
     *            Crest history data.
     * @param callback
     *            Crest data selection callback.
     */
    public CrestHistoryCanvas(Composite parent,
            CrestHistoryData crestHistoryData, ISelectedCrestData callback) {
        super(parent, SWT.DOUBLE_BUFFERED);

        parentComp = parent;
        this.crestHistoryData = crestHistoryData;
        // work around for missing data
        if (crestHistoryData != null) {
            selectCrestDataCB = callback;
            stageMaxVal = crestHistoryData.getMaxStageLevel();
            stageMinVal = 0.0;
        } else {
            stageMaxVal = 0.0;
            stageMinVal = 0.0;
        }
        // another work around
        if ((crestHistoryData.getEndingYear() == 0)
                || (crestHistoryData.getStartingYear() == 0)) {
            Calendar cal = new GregorianCalendar();
            cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            Date date = SimulatedTime.getSystemTime().getTime();
            cal.setTime(date);
            crestHistoryData.setStartingYear(1900);
            crestHistoryData.setEndingYear(cal.get(Calendar.YEAR));
        }

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        if (crestHistoryData != null) {
            generateCrestDataMap();
        }

        canvasFont = new Font(parentComp.getDisplay(), "Monospace", 9,
                SWT.NORMAL);

        setupCanvas();

        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                processMouseEvent(e);
            }
        });
    }

    /**
     * Regenerate the Crest history canvas
     * 
     * @param crestHistoryData
     */
    public void updateCrestHistotryData(CrestHistoryData crestHistoryData) {
        this.crestHistoryData = crestHistoryData;

        if (crestHistoryData != null) {
            stageMaxVal = crestHistoryData.getMaxStageLevel();
            stageMinVal = 0.0;
        } else {
            stageMaxVal = 0.0;
            stageMinVal = 0.0;
        }

        generateCrestDataMap();
        redraw();
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        this.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        setLayoutData(gd);
        addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCrestHistory(e);
            }
        });

        addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                canvasFont.dispose();
            }
        });
    }

    /**
     * Draw the crest history data and the graph lines & labels on the canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawCrestHistory(PaintEvent e) {
        e.gc.setFont(canvasFont);
        int fontHeightMid = (e.gc.getFontMetrics().getHeight() / 2);
        int fontHeight = (e.gc.getFontMetrics().getHeight());

        e.gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK));

        e.gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WHITE));

        // ----------------------------------------
        // Draw horizontal and vertical axis lines
        // ----------------------------------------

        e.gc.drawLine(VLINE_XCOORD, HLINE_YCOORD, VLINE_XCOORD, HLINE_YCOORD
                - VLINE_LENGTH);

        e.gc.drawLine(VLINE_XCOORD, HLINE_YCOORD, VLINE_XCOORD + HLINE_LENGTH,
                HLINE_YCOORD);

        // ------------------------------------------------
        // Draw STAGE IN FEET vertical label
        // ------------------------------------------------
        char[] charArray = STAGE_IN_FEET.toCharArray();

        int tmpY = 120;
        for (int i = 0; i < charArray.length; i++) {
            e.gc.drawString(String.valueOf(charArray[i]), 5, tmpY, true);
            tmpY += fontHeight;
        }

        if (crestHistoryData != null) {
            // ------------------------------------------------
            // Draw the years and the hash marks
            // ------------------------------------------------
            int startyear = crestHistoryData.getStartingYear();
            int endyear = crestHistoryData.getEndingYear();
            int years = (endyear - startyear);
            double pixelsPerIncX;
            if (years == 0) {
                pixelsPerIncX = 0;
            } else {
                pixelsPerIncX = HLINE_LENGTH / years;
            }

            int yearXcoord = VLINE_XCOORD;
            int yearHashXcoord = YEAR_HASH_X_COORD_START;
            DecimalFormat df = new DecimalFormat();
            df.setMinimumIntegerDigits(1);
            df.setMaximumFractionDigits(0);
            df.setGroupingUsed(false);

            for (int i = 0; i <= years; i++) {
                int inc = new Double(Math.rint(pixelsPerIncX * i)).intValue();
                yearXcoord = YEAR_HASH_X_COORD_START + inc - 15;
                yearHashXcoord = YEAR_HASH_X_COORD_START + inc;
                if (years < 75) {
                    if (i % 5 == 0) {
                        e.gc.drawString(df.format(startyear + i), yearXcoord,
                                CANVAS_HEIGHT - fontHeight - 3, true);
                        e.gc.drawLine(yearHashXcoord, HLINE_YCOORD, yearHashXcoord,
                                HLINE_YCOORD + HASH_MARK);
                    }
                } else {
                    if (i % 10 == 0) {
                        e.gc.drawString(df.format(startyear + i), yearXcoord,
                                CANVAS_HEIGHT - fontHeight - 3, true);
                        e.gc.drawLine(yearHashXcoord, HLINE_YCOORD, yearHashXcoord,
                                HLINE_YCOORD + HASH_MARK);
                    }
                }
            }

            // -----------------------------------------
            // Draw stage hash marks
            // -----------------------------------------

            double maxPixValue = VLINE_LENGTH + VLINE_PIXELS_FROM_TOP;
            double numHashsY = stageMaxVal;
            double pixelsPerIncY = VLINE_LENGTH / numHashsY;

            for (int x = 0; x <= stageMaxVal; ++x) {
                int yCoord = HLINE_YCOORD
                        - new Double(Math.round(x * pixelsPerIncY)).intValue();

                if (x % 5 == 0) {
                    // draw little hash
                    e.gc.drawLine(VLINE_XCOORD, yCoord, VLINE_XCOORD
                            - SMALL_HASH_MARK, yCoord);

                    String recStr = String.format("%5.1f", new Float(x)
                            .floatValue());
                    e.gc.drawString(recStr, 35, yCoord - fontHeightMid, true);
                }
            }

            // -----------------------------------------
            // Draw MAJOR line and label
            // -----------------------------------------

            double majorLevel = crestHistoryData.getMajorLevel();

            e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_MAGENTA));

            int majorYCoord = (int) (maxPixValue - Math
                    .round(((majorLevel - stageMinVal) * pixelsPerIncY)));

            e.gc.drawLine(VLINE_XCOORD + 1, majorYCoord, VLINE_XCOORD
                    + HLINE_LENGTH, majorYCoord);

            String majorLabel = String.format("%5.1f MAJOR (%d)", majorLevel,
                    majorCount);
            e.gc.drawString(majorLabel, VLINE_XCOORD + LABEL_X_OFFSET,
                    majorYCoord - LABEL_Y_OFFSET, true);

            // -----------------------------------------
            // Draw MODERATE line and label
            // -----------------------------------------

            double moderateLevel = crestHistoryData.getModerateLevel();

            e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_BLUE));

            int modYCoord = (int) (maxPixValue - Math
                    .round(((moderateLevel - stageMinVal) * pixelsPerIncY)));

            e.gc.drawLine(VLINE_XCOORD + 1, modYCoord, VLINE_XCOORD
                    + HLINE_LENGTH, modYCoord);

            String modLabel = String.format("%5.1f MODERATE (%d)",
                    moderateLevel, modCount);
            e.gc.drawString(modLabel, VLINE_XCOORD + LABEL_X_OFFSET, modYCoord
                    - LABEL_Y_OFFSET, true);

            // -----------------------------------------
            // Draw MINOR line and label
            // -----------------------------------------

            double minorLevel = crestHistoryData.getMinorLevel();

            e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_RED));

            int minorYCoord = (int) (maxPixValue - Math
                    .round(((minorLevel - stageMinVal) * pixelsPerIncY)));

            e.gc.drawLine(VLINE_XCOORD + 1, minorYCoord, VLINE_XCOORD
                    + HLINE_LENGTH, minorYCoord);

            String minorLabel = String.format("%5.1f MINOR (%d)", minorLevel,
                    minorCount);
            e.gc.drawString(minorLabel, VLINE_XCOORD + LABEL_X_OFFSET,
                    minorYCoord - LABEL_Y_OFFSET, true);

            e.gc.drawString("FLOOD", VLINE_XCOORD + HLINE_LENGTH - 40,
                    minorYCoord - LABEL_Y_OFFSET, true);

            // -----------------------------------------
            // Draw ACTION line and label
            // -----------------------------------------

            double actionLevel = crestHistoryData.getActionLevel();

            e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_YELLOW));

            int actionYCoord = (int) (maxPixValue - Math
                    .round(((actionLevel - stageMinVal) * pixelsPerIncY)));

            e.gc.drawLine(VLINE_XCOORD + 1, actionYCoord, VLINE_XCOORD
                    + HLINE_LENGTH, actionYCoord);

            String actionLabel = String.format("%5.1f ACTION (%d)",
                    actionLevel, actionCount);
            e.gc.drawString(actionLabel, VLINE_XCOORD + LABEL_X_OFFSET,
                    actionYCoord - LABEL_Y_OFFSET, true);

            // -------------------------------------------
            // Draw crest data
            // -------------------------------------------

            CrestDrawData drawData;
            Rectangle rec;

            Set<Rectangle> keys = crestDataMap.keySet();

            for (Iterator<Rectangle> iterator = keys.iterator(); iterator
                    .hasNext();) {
                rec = iterator.next();
                drawData = crestDataMap.get(rec);

                if (drawData.getDrawData() == false) {
                    continue;
                }

                if (drawData.isSelected() == true) {
                    e.gc.setBackground(parentComp.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));
                } else {
                    e.gc.setBackground(drawData.getColor());
                }

                e.gc.fillRectangle(rec.x, rec.y, rec.width, rec.height);
            }
        }
    }

    /**
     * Generate the map of crest data (value) and the rectangle areas on the
     * canvas (key).
     */
    private void generateCrestDataMap() {
        crestDataMap = new HashMap<Rectangle, CrestDrawData>();
        CrestDrawData drawData;
        Rectangle rec;

        int circleWidth = 8;
        int circleHeight = 8;

        double maxPixValue = VLINE_LENGTH + VLINE_PIXELS_FROM_TOP;
        double numHashs = crestHistoryData.getMaxStageLevel();
        double pixelsPerIncY = VLINE_LENGTH / numHashs;
        int startyear = crestHistoryData.getStartingYear();
        int endyear = crestHistoryData.getEndingYear();
        int years = endyear - startyear;
        int pixelsPerIncX;
        if (years == 0) {
            pixelsPerIncX = 0;
        } else {
            pixelsPerIncX = HLINE_LENGTH / years;
        }

        //Init the counts
        this.majorCount=0;
        this.minorCount=0;
        this.actionCount=0;
        this.modCount=0;
        
        for (CrestData crestData : crestHistoryData.getCrestDataArray()) {

            int yCoord = (int) (maxPixValue - Math
                    .round((crestData.getStage() * pixelsPerIncY)));

            double yearXOffset = crestData.getYear() - startyear;
            int xCoord = YEAR_HASH_X_COORD_START
                    + new Double(Math.round(yearXOffset * pixelsPerIncX))
                            .intValue();

            rec = new Rectangle(xCoord - circleWidth / 2, yCoord - circleHeight
                    / 2, circleWidth, circleHeight);

            drawData = new CrestDrawData(crestData.getStage(), crestData
                    .getYear(), calculateDataColor(crestData.getStage()));

            crestDataMap.put(rec, drawData);
        }
    }

    /**
     * Calculate color of the crest data.
     * 
     * @param stage
     *            Stage (in feet).
     * @return The color associated with the data.
     */
    private Color calculateDataColor(double stage) {
        if ((stage > crestHistoryData.getMajorLevel()) && 
                (crestHistoryData.getMajorLevel() != HydroConstants.MISSING_VALUE)) {
            ++majorCount;
            return parentComp.getDisplay().getSystemColor(SWT.COLOR_MAGENTA);
        } else if ((stage > crestHistoryData.getModerateLevel()) && 
                (crestHistoryData.getModerateLevel() != HydroConstants.MISSING_VALUE)) {
            ++modCount;
            return parentComp.getDisplay().getSystemColor(SWT.COLOR_BLUE);
        } else if ((stage > crestHistoryData.getMinorLevel()) &&
                (crestHistoryData.getMinorLevel() != HydroConstants.MISSING_VALUE)) {
            ++minorCount;
            return parentComp.getDisplay().getSystemColor(SWT.COLOR_RED);
        } else if ((stage > crestHistoryData.getActionLevel()) &&
                (crestHistoryData.getActionLevel() != HydroConstants.MISSING_VALUE)) {
            ++actionCount;
            return parentComp.getDisplay().getSystemColor(SWT.COLOR_YELLOW);
        }

        return parentComp.getDisplay().getSystemColor(SWT.COLOR_GREEN);
    }

    /**
     * Select the crest data if the mouse click is in the data rectangle.
     * 
     * @param e
     *            Mouse event.
     */
    public void processMouseEvent(MouseEvent e) {
        Rectangle rec;
        Rectangle foundRec = null;
        Rectangle selectedRec = null;

        Set<Rectangle> keys = crestDataMap.keySet();

        for (Iterator<Rectangle> iterator = keys.iterator(); iterator.hasNext();) {
            rec = iterator.next();

            // Check if the mouse's x & y coords is in the rectangle area or on
            // the
            // edge of the rectangle
            if (recContains(e.x, e.y, rec)) {
                foundRec = rec;
                // crestDataMap.get(rec).setSelected(true);
            } else if (crestDataMap.get(rec).isSelected() == true) {
                selectedRec = rec;
            }

            if (foundRec != null) {
                crestDataMap.get(foundRec).setSelected(true);

                if (selectedRec != null) {
                    crestDataMap.get(selectedRec).setSelected(false);
                }

                if (selectCrestDataCB != null) {
                    selectCrestDataCB.crestDataSelected(crestDataMap.get(
                            foundRec).getStage(), crestDataMap.get(foundRec)
                            .getYear());
                }
            }
        }
        this.redraw();
    }

    /**
     * This method is used in place of the Rectangle.contains() method. The
     * contains method only checks if the x,y is inside the rectangle area. This
     * method accounts for the x,y being on the edges of the rectangle.
     * 
     * @param x
     *            Mouse's x coordinate.
     * @param y
     *            Mouse's y coordinate.
     * @param rec
     *            Rectangle the mouse coordinates will be checked against.
     * @return True if the mouse's x & y coordinates are in/on the rectangle.
     */
    private boolean recContains(int x, int y, Rectangle rec) {
        if ((x < rec.x) || (x > (rec.x + rec.width))) {
            return false;
        }

        if ((y < rec.y) || (y > (rec.y + rec.height))) {
            return false;
        }

        return true;
    }

    /**
     * Select the crest data based on the stage nad year passed in.
     * 
     * @param stage
     *            Stage in feet.
     * @param year
     *            Year.
     */
    public void selectCrestData(double stage, int year) {
        Rectangle rec;

        Set<Rectangle> keys = crestDataMap.keySet();

        for (Iterator<Rectangle> iterator = keys.iterator(); iterator.hasNext();) {
            rec = iterator.next();

            crestDataMap.get(rec).setSelected(false);

            if ((crestDataMap.get(rec).getStage() == stage)
                    && (crestDataMap.get(rec).getYear() == year)) {
                crestDataMap.get(rec).setSelected(true);
            }
        }
        this.redraw();
    }

    /**
     * Draw all of the crest data on the canvas.
     */
    public void drawAllCrestData() {
        Rectangle rec;

        Set<Rectangle> keys = crestDataMap.keySet();

        for (Iterator<Rectangle> iterator = keys.iterator(); iterator.hasNext();) {
            rec = iterator.next();

            crestDataMap.get(rec).setDrawData(true);
        }

        this.redraw();
    }

    /**
     * Draw only the crest data below the action stage.
     */
    public void drawDataBelowActionStage() {
        Rectangle rec;

        Set<Rectangle> keys = crestDataMap.keySet();

        for (Iterator<Rectangle> iterator = keys.iterator(); iterator.hasNext();) {
            rec = iterator.next();

            if (crestDataMap.get(rec).getStage() > crestHistoryData
                    .getActionLevel()) {
                crestDataMap.get(rec).setDrawData(false);
            } else {
                crestDataMap.get(rec).setDrawData(true);
            }
        }

        this.redraw();
    }

    /**
     * Draw only the crest data above the action stage.
     */
    public void drawDataAboveActionStage() {
        Rectangle rec;

        Set<Rectangle> keys = crestDataMap.keySet();

        for (Iterator<Rectangle> iterator = keys.iterator(); iterator.hasNext();) {
            rec = iterator.next();

            if (crestDataMap.get(rec).getStage() < crestHistoryData
                    .getActionLevel()) {
                crestDataMap.get(rec).setDrawData(false);
            } else {
                crestDataMap.get(rec).setDrawData(true);
            }
        }

        this.redraw();
    }
}
