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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;

/**
 * 
 * Canvas to display the trend graph.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??????????              lvenable    Initial creation
 * Oct 9, 2013  #2447      lvenable    Replaced creating a new color with using a
 *                                     system color (white).  This fixes a memory
 *                                     leak.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TrendGraphCanvas {
    private final Composite parentComp;

    private final Display display;

    /*
     * Canvas information
     */
    private Canvas canvas;

    private final int CANVAS_WIDTH = 280;

    private final int CANVAS_HEIGHT = 180;

    private final int graphXcoord = 50;

    private final int graphYcoord = 30;

    private final int graphWidth = 200;

    private final int graphHeight = 120;

    private final int hashMarkLength = 10;

    private Font titleFont;

    private Font labelFont;

    private int textWidth = 0;

    private int textHeight = 0;

    private TrendGraphData trendGraphData;

    private Date currentDate;

    private Integer vcp;

    private final String[] timeLabels = new String[] { "xxxxx", "xxxxx",
            "xxxxx", "xxxxx", "xxxxx" };

    private final int[] timeLabelXCoords = new int[timeLabels.length];

    /**
     * Millisecond values
     */
    private long startTimeMillis = Long.MIN_VALUE;

    private long endTimeMillis = Long.MAX_VALUE;

    private long millisRange = Long.MIN_VALUE;

    private final long fewExtraMillis = 60000;

    private double pxPerMillis = Double.NaN;

    private Color canvasBgColor;

    private Color overLimitColor;

    private final ScanTables scanTable;

    private String attrName;

    private SCANConfig scanCfg;

    private SCANAttributesXML attributeData;

    private StringBuilder labelText;

    private double minValue = Double.NaN;

    private double rangeValue = Double.NaN;

    private double incValue = Double.NaN;

    private String valueFormatStr = null;

    private double valueLabelIncPerPix = Double.NaN;

    private boolean overMaxLimit = false;

    private ArrayList<Double> labelDisplayVals;

    private boolean displayAsInt = true;

    private IRequestTrendGraphData requestDataCallback;

    /**
     * the ID of this row
     */
    private String ident = null;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param trendGraphData
     *            Trend graph data.
     * @param currentDate
     *            Current date.
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @param vcp
     *            Volume coverage pattern.
     * @param requestDataCallback
     *            Request callback.
     * @param ident
     *            Identification.
     */
    public TrendGraphCanvas(Composite parentComp,
            TrendGraphData trendGraphData, Date currentDate,
            ScanTables scanTable, String attrName, Integer vcp,
            IRequestTrendGraphData requestDataCallback, String ident) {
        this.parentComp = parentComp;
        this.currentDate = currentDate;
        this.trendGraphData = trendGraphData;
        this.scanTable = scanTable;
        this.attrName = attrName;
        display = this.parentComp.getDisplay();
        this.vcp = vcp;
        this.requestDataCallback = requestDataCallback;
        this.ident = ident;
        init();
    }

    public void init() {
        scanCfg = SCANConfig.getInstance();

        labelDisplayVals = new ArrayList<Double>();

        labelFont = new Font(display, "Courier", 9, SWT.BOLD);
        titleFont = new Font(display, "Courier", 11, SWT.BOLD);
        canvasBgColor = new Color(display, 225, 225, 225);
        overLimitColor = new Color(display, 225, 192, 203);

        createDates();
        calcFontMetrics();
        createCanvas();

        updateAttribute(attrName, trendGraphData, currentDate);
    }

    private void createCanvas() {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        // canvas = new Canvas(shell, SWT.NO_BACKGROUND);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        canvas.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                labelFont.dispose();
                titleFont.dispose();
                canvasBgColor.dispose();
                overLimitColor.dispose();
            }
        });
    }

    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);

        // Fill the canvas background
        gc.setBackground(canvasBgColor);
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        if (labelDisplayVals.size() == 0) {
            return;
        }

        gc.setFont(labelFont);

        // Draw the elevation angles here
        // so they render behind the graph lines
        drawElevationAngle(gc);

        // Draw grey graph lines
        drawGraphLines(gc);

        // Draw the label values
        drawValueLabels(gc);

        gc.setLineWidth(3);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        // Draw vertical black label line
        gc.drawLine(graphXcoord, graphYcoord - 1, graphXcoord, graphYcoord
                + graphHeight);

        // Draw vertical black label line
        gc.drawLine(graphXcoord, graphYcoord + graphHeight, graphXcoord
                + graphWidth, graphYcoord + graphHeight);

        drawTimeLabels(gc);

        drawGraphData(gc);

        // Draw the top label
        drawTopLabel(gc);
    }

    private void drawTopLabel(GC gc) {
        gc.setFont(titleFont);

        gc.setBackground(display.getSystemColor(SWT.COLOR_DARK_GRAY));
        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, textHeight + 5);
        gc.drawString(labelText.toString(), 5, 0, true);
    }

    private void drawGraphLines(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
        gc.setLineWidth(1);

        /*
         * Horizontal value graph lines
         */
        valueLabelIncPerPix = (graphHeight) / this.rangeValue;

        int yCoord = graphYcoord + graphHeight;

        for (int i = 0; i < labelDisplayVals.size(); i++) {
            double adjustedValue = labelDisplayVals.get(i);

            if (this.minValue < 0.0) {
                adjustedValue += Math.abs(this.minValue);
            } else if (this.minValue > 0.0) {
                adjustedValue -= this.minValue;
            }

            yCoord = (int) (graphYcoord + graphHeight - (Math
                    .round(valueLabelIncPerPix * adjustedValue)));

            gc.drawLine(graphXcoord, yCoord, graphXcoord + graphWidth, yCoord);
        }

        /*
         * Vertical time graph lines
         */

        for (int i = 0; i < timeLabels.length; i++) {
            int xCoord = timeLabelXCoords[i];
            if (xCoord < graphXcoord || xCoord > graphXcoord + graphWidth)
                continue;
            // Draw the time hash mark
            gc.drawLine(xCoord, graphYcoord + graphHeight, xCoord, graphYcoord);
        }
    }

    private void drawValueLabels(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setLineWidth(2);

        /*
         * TODO : need to fix this as the range and value labels may not be
         * synced (if you mod the range by the interval the result would be
         * zero).
         */
        valueLabelIncPerPix = (graphHeight) / this.rangeValue;

        if (overMaxLimit == true) {
            gc.setBackground(overLimitColor);

            gc.fillRectangle(3, graphYcoord - 8, graphXcoord - hashMarkLength
                    - 4, graphHeight + 16);
        }

        int yCoord = graphYcoord + graphHeight;

        for (int i = 0; i < labelDisplayVals.size(); i++) {
            double adjustedValue = labelDisplayVals.get(i);

            if (this.minValue < 0.0) {
                adjustedValue += Math.abs(this.minValue);
            } else if (this.minValue > 0.0) {
                adjustedValue -= this.minValue;
            }

            yCoord = (int) (graphYcoord + graphHeight - (Math
                    .round(valueLabelIncPerPix * adjustedValue)));

            gc.drawLine(graphXcoord - hashMarkLength, yCoord, graphXcoord,
                    yCoord);
            gc.drawString(getFormattedValueString(labelDisplayVals.get(i)), 5,
                    yCoord - textHeight / 2, true);
        }
    }

    /*
     * Draw the elevation angles
     */
    private void drawElevationAngle(GC gc) {
        double valuePerPix = (graphHeight) / this.rangeValue;

        TreeSet<Date> dateSet = new TreeSet<Date>();
        dateSet.addAll(trendGraphData.getGraphData().keySet());

        if (vcp == null)
            return;

        if (!attrName.equalsIgnoreCase("dbzHt")
                && !attrName.equalsIgnoreCase("top")
                && !attrName.equalsIgnoreCase("base")
                && !attrName.equalsIgnoreCase("htMxVr"))
            return; // only height base attributes

        if (requestDataCallback == null)
            return;

        // get the ranges of this row
        TrendGraphData tgd = requestDataCallback.requestTrendGraphData(
                scanTable, "rng", ident);
        LinkedHashMap<Date, Double> rngDateMap = tgd.getGraphData();

        ElevationInfo eleInfo = new ElevationInfo();
        int[] elevationAngles = eleInfo.getScanElevations(null, vcp);
        if (elevationAngles == null)
            return;

        ArrayList<ArrayList<Double>> bmHtLsts = new ArrayList<ArrayList<Double>>();

        for (Date rngDate : rngDateMap.keySet()) {
            double rngVal = rngDateMap.get(rngDate);
            /* convert to NMI for DMD rng */
            if (scanTable.name().equalsIgnoreCase("DMD"))
                rngVal *= ScanUtils.KM_TO_NMI;

            ArrayList<Double> bmHts = new ArrayList<Double>();
            for (int angleVal : elevationAngles) {
                double bmHt = ScanUtils.getRadarBeamHeight(rngVal,
                        angleVal / 10.0);
                bmHts.add(bmHt);
            }
            bmHtLsts.add(bmHts);
        }

        if (bmHtLsts.size() <= 0)
            return;

        double topAngle = attributeData.getMin() + rangeValue;
        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        Object[] rngValLst = rngDateMap.values().toArray();
        Object[] dates = rngDateMap.keySet().toArray();
        for (int dateIndex = 0; dateIndex < (dates.length - 1); dateIndex++) {
            for (int bmHtIndex = 0; bmHtIndex < (bmHtLsts.get(dateIndex).size()); bmHtIndex++) {
                /*
                 * Check if the data is outside the time range. If so then skip
                 * to the next point.
                 */
                if ((((Date) dates[dateIndex]).getTime() < startTimeMillis)
                        || (((Date) dates[dateIndex]).getTime() > currentDate
                                .getTime() + fewExtraMillis)) {
                    continue;
                }

                double rngVal = (Double) rngValLst[dateIndex + 1];

                /* convert to NMI for DMD rng */
                if (scanTable.name().equalsIgnoreCase("DMD")
                        && (attrName.equalsIgnoreCase("base") || attrName
                                .equalsIgnoreCase("htMxVr")))
                    rngVal *= ScanUtils.KM_TO_NMI;

                int lineWidth = Math.round((float) (rngVal * 1.852
                        * Math.tan(.95 * .01745) * valuePerPix));

                gc.setLineWidth(lineWidth);
                Double bmHt = bmHtLsts.get(dateIndex).get(bmHtIndex);
                Double nextBmHt = bmHtLsts.get(dateIndex + 1).get(bmHtIndex);
                if ((bmHt > topAngle) && (nextBmHt > topAngle))
                    break;

                if ((bmHt < attributeData.getMin())
                        && (nextBmHt < attributeData.getMin()))
                    continue;

                if ((((Date) dates[dateIndex + 1]).getTime() < startTimeMillis)
                        || (((Date) dates[dateIndex + 1]).getTime() > currentDate
                                .getTime() + fewExtraMillis)
                        || (bmHt < this.minValue)
                        || (nextBmHt > (rangeValue * 1.1 + minValue))) {
                    continue;
                }

                long millisOffset = ((Date) dates[dateIndex]).getTime()
                        - startTimeMillis;
                int x1 = (int) Math.round(millisOffset / pxPerMillis)
                        + graphXcoord;
                int y1 = (int) Math.round(graphHeight + graphYcoord
                        - (bmHt * valuePerPix));
                millisOffset = ((Date) dates[dateIndex + 1]).getTime()
                        - startTimeMillis;
                int x2 = (int) Math.round(millisOffset / pxPerMillis)
                        + graphXcoord;
                int y2 = (int) Math.round(graphHeight + graphYcoord
                        - (nextBmHt * valuePerPix));
                gc.drawLine(x1, y1, x2, y2);
            }
        }
    }

    private void drawTimeLabels(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setLineWidth(2);

        for (int i = 0; i < timeLabels.length; i++) {
            int xCoord = timeLabelXCoords[i];
            if (xCoord < graphXcoord || xCoord > graphXcoord + graphWidth)
                continue;
            // Draw the time hash mark
            gc.drawLine(xCoord, graphYcoord + graphHeight, xCoord, graphYcoord
                    + graphHeight + hashMarkLength);

            // Draw the time label
            gc.drawString(
                    timeLabels[i],
                    xCoord
                            - (Math.round(textWidth * timeLabels[i].length()
                                    / 2)), graphYcoord + graphHeight
                            + hashMarkLength + 2, true);
        }
    }

    private void drawGraphData(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setLineWidth(1);

        double val = Double.NaN;
        long millisOffset = 0;
        int xCoord = 0;
        int yCoord = 0;

        int prevXCoord = Integer.MIN_VALUE;
        int prevYCoord = Integer.MIN_VALUE;

        double valuePerPix = (graphHeight) / this.rangeValue;

        TreeSet<Date> dateSet = new TreeSet<Date>();
        LinkedHashMap<Date, Color> dataColors = trendGraphData
                .getGraphDataColors();
        LinkedHashMap<Date, Double> dataMap = trendGraphData.getGraphData();
        dateSet.addAll(dataMap.keySet());

        /*
         * Loop and draw the data lines.
         */
        for (Date d : dateSet) {
            val = dataMap.get(d);

            /* convert to NMI for DMD rng */
            if (scanTable.name().equalsIgnoreCase("DMD")
                    && attrName.equalsIgnoreCase("rng"))
                val *= ScanUtils.KM_TO_NMI;

            /*
             * Check if the data is outside the time range. If so then skip to
             * the next point.
             */
            // if (d.getTime() < startTimeMillis || d.getTime() > endTimeMillis
            // || val < this.minValue) {
            // continue;
            // }

            if ((d.getTime() < startTimeMillis)
                    || (d.getTime() > currentDate.getTime() + fewExtraMillis)
                    || (val < this.minValue)) {
                continue;
            }

            /*
             * Adjust the value to plot correctly if the minimum is not zero. It
             * the minimum is less than zero then add the absolute value of the
             * minimum to the value. If the minimum is greater that zero then
             * subtract the minimum value from the value.
             */
            double adjustedValue = val;

            if (this.minValue < 0.0) {
                adjustedValue += Math.abs(this.minValue);
            } else if (this.minValue > 0.0) {
                adjustedValue -= this.minValue;
            }

            millisOffset = d.getTime() - startTimeMillis;

            xCoord = (int) Math.round(millisOffset / pxPerMillis) + graphXcoord;
            yCoord = (int) Math.round(graphHeight + graphYcoord
                    - (adjustedValue * valuePerPix));

            if ((prevXCoord != Integer.MIN_VALUE)
                    && (prevXCoord != Integer.MIN_VALUE)) {
                gc.drawLine(xCoord, yCoord, prevXCoord, prevYCoord);
            }

            prevXCoord = xCoord;
            prevYCoord = yCoord;
        }

        /*
         * Loop and draw the data points.
         */
        for (Date d : dateSet) {
            val = dataMap.get(d);

            /* convert to NMI for DMD rng */
            if (scanTable.name().equalsIgnoreCase("DMD")
                    && attrName.equalsIgnoreCase("rng"))
                val *= ScanUtils.KM_TO_NMI;

            // if (d.getTime() < startTimeMillis || d.getTime() > endTimeMillis
            // || val < this.minValue) {
            // continue;
            // }

            if ((d.getTime() < startTimeMillis)
                    || (d.getTime() > currentDate.getTime() + fewExtraMillis)
                    || (val < this.minValue)) {
                continue;
            }

            /*
             * Adjust the value to plot correctly if the minimum is not zero. It
             * the minimum is less than zero then add the absolute value of the
             * minimum to the value. If the minimum is greater that zero then
             * subtract the minimum value from the value.
             */
            double adjustedValue = val;

            if (this.minValue < 0.0) {
                adjustedValue += Math.abs(this.minValue);
            } else if (this.minValue > 0.0) {
                adjustedValue -= this.minValue;
            }

            millisOffset = d.getTime() - startTimeMillis;
            xCoord = (int) Math.round(millisOffset / pxPerMillis) + graphXcoord;
            yCoord = (int) Math.round(graphHeight + graphYcoord
                    - (adjustedValue * valuePerPix));

            gc.setBackground(dataColors.get(d));
            gc.fillOval(xCoord - 4, yCoord - 4, 8, 8);

            gc.drawOval(xCoord - 4, yCoord - 4, 8, 8);
        }
    }

    private void createDates() {
        SimpleDateFormat dateFmt = new SimpleDateFormat("HH:mm");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        Calendar cal = Calendar.getInstance();
        cal.setTime(currentDate);

        /*
         * Calculate the ending time (greatest time)
         */
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        endTimeMillis = cal.getTimeInMillis();

        /*
         * Round up to the next 15 minutes
         */
        int min = cal.get(Calendar.MINUTE);
        int minOffset = 0;

        if (min % 15 != 0) {
            if (min < 15) {
                minOffset = 15 - min;
            } else if (min < 30) {
                minOffset = 30 - min;
            } else if (min < 45) {
                minOffset = 45 - min;
            } else if (min < 60) {
                minOffset = 60 - min;
            }
        }

        cal.add(Calendar.MINUTE, minOffset);

        startTimeMillis = endTimeMillis - 3600 * 1000;

        millisRange = endTimeMillis - startTimeMillis;

        pxPerMillis = millisRange / graphWidth;

        for (int i = timeLabels.length - 1; i >= 0; --i) {
            timeLabels[i] = dateFmt.format(cal.getTime());
            timeLabelXCoords[i] = graphXcoord
                    + (int) Math
                            .round((cal.getTimeInMillis() - startTimeMillis)
                                    / pxPerMillis);
            cal.add(Calendar.MINUTE, -15);
        }
    }

    private void calcFontMetrics() {
        Image image = new Image(display, 100, 100);
        GC gc = new GC(image);
        gc.setFont(labelFont);

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();

        gc.dispose();
        image.dispose();
    }

    private void generateLabelValues() {
        overMaxLimit = false;
        labelDisplayVals.clear();

        this.minValue = attributeData.getMin();
        this.incValue = attributeData.getInterval();

        this.rangeValue = attributeData.getRange();

        // Find the maximum display value.
        double maxDisplayValue = rangeValue + minValue;
        double maxDataValue = Double.MIN_VALUE;

        /*
         * Loop through the data and determine if there are values that are
         * higher than the maximum display value. If so then a flag is set so
         * the label background is colored pink.
         */
        LinkedHashMap<Date, Double> dataMap = trendGraphData.getGraphData();
        Set<Date> dates = dataMap.keySet();

        for (Date d : dates) {

            if ((d.getTime() > startTimeMillis)
                    && (d.getTime() < endTimeMillis + fewExtraMillis)) {
                if (maxDisplayValue < dataMap.get(d)) {
                    overMaxLimit = true;

                    if (maxDataValue < dataMap.get(d)) {
                        maxDataValue = dataMap.get(d);
                    }
                }
            }
        }

        if (overMaxLimit == true) {

            double currentDisplayValue = this.minValue;
            while (currentDisplayValue <= maxDataValue) {
                labelDisplayVals.add(currentDisplayValue);
                currentDisplayValue += this.incValue;
            }

            /*
             * Need to adjust the range value as the higher data value has
             * increased the range - NOTE: the range will never be smaller than
             * the original value
             */

            this.rangeValue = this.rangeValue + currentDisplayValue
                    - (this.minValue + this.rangeValue);
        } else {
            double currentDisplayValue = this.minValue;
            while (currentDisplayValue <= maxDisplayValue) {
                labelDisplayVals.add(currentDisplayValue);
                currentDisplayValue += this.incValue;
            }
        }
    }

    private String getFormattedValueString(double val) {
        if (displayAsInt == true) {
            return String.format(valueFormatStr, Math.round(val));
        }

        return String.format(valueFormatStr, val);
    }

    /**
     * Determine how the value labels will be formatted (integer or decimal).
     */
    private void setValueFormatString() {
        displayAsInt = scanCfg.displayAsIntTrend(scanTable, attrName);

        if (displayAsInt == false) {
            valueFormatStr = "%4.1f";
        } else {
            valueFormatStr = "%4d";
        }
    }

    /**
     * Update the attribute and reset the data.
     * 
     * @param attributeName
     *            Attribute name.
     */
    public void updateAttribute(String attributeName,
            TrendGraphData trendGraphData, Date date) {
        // System.out.println("*  TrendGraphCanvas.updateAttribute() - Updating graph data - "
        // + attributeName);

        this.currentDate = date;
        this.trendGraphData = trendGraphData;
        this.attrName = attributeName;

        // printDataMap();

        attributeData = scanCfg.getTrendAttrData(scanTable, attrName);

        if (attributeData == null) {
            System.out.println("*** attribute data is null");
        } else {
            labelText = new StringBuilder();
            labelText.append(attributeName).append(" (");
            labelText.append(attributeData.getUnits()).append(")");
        }

        setValueFormatString();
        generateLabelValues();
        createDates();

        canvas.redraw();
    }

    public void redrawCanvas() {
        canvas.redraw();
    }

    public void setIndent(String ident) {
        this.ident = ident;
    }

    /*
     * TODO : remove when no longer needed...
     */
    private void printDataMap() {
        SimpleDateFormat dateFmt = new SimpleDateFormat("E MMM dd HH:mm yyyy");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));

        System.out
                .println("-------------TrendGraphCanvas.printDataMap()-----------------");
        System.out.println("+++++ " + scanTable.name() + "  ---  " + attrName);

        System.out.println("Current Date = " + dateFmt.format(currentDate));

        Set<Date> keys = trendGraphData.getGraphData().keySet();

        for (Date key : keys) {
            double dbl = trendGraphData.getGraphData().get(key);
            System.out.println(dbl + "  " + dateFmt.format(key));
        }
    }
}
