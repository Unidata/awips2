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
package com.raytheon.uf.viz.monitor.trendplot;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.SortedMap;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObTrendDataSet;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * The Trend Canvas
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2009-12-02			   vkorolev    Initial creation.
 * 2010-01-12   4268 	   vkorolev    Finish the Trend Plot graph – displays correct data, scale
 * 									   lables are correct, displays 24 hrs, etc.
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1.0
 */
public class TrendPlotCanvas {
    private String selZone = null;

    private boolean noData = false;

    private final Display display;

    private final Composite parentComp;

    private Canvas canvas;

    private Calendar curdate;

    private SortedMap<Date, Float> trendData;

    // canvas dimensions
    private final int CANVAS_WIDTH = 850;

    private final int CANVAS_HEIGHT = 180;

    private final int graphXcoord = 70;

    private final int graphYcoord = 30;

    private final int graphWidth = 768;

    private final int graphHeight = 120;

    private Font titleFont;

    private Font labelFont;

    private int textWidth = 0;

    private int textHeight = 0;

    private long startTimeMillis = Long.MIN_VALUE;

    // ???? Do we need colors?
    private Color red;

    private Color yellow;

    private Color green;

    private Color canvasBgColor;

    // private Color overLimitColor;

    private double minValue = 0; // ?

    private double maxValue = 0; // ?

    private double incValue = 0; // ?

    private Double rangeValue = Double.MIN_VALUE;

    private double valuePerPix = Double.MIN_VALUE;

    private String valueFormatStr = null;

    private double valueLabelIncPerPix = Double.NaN;

    double minDisplayValue = 0; // ?

    double maxDisplayValue = 0;

    private ArrayList<Double> labelDisplayVals;

    private String varNameText = "";

    private boolean displayAsInt = false;

    float redThreshold = 0; // ?

    float yellowThreshold = 0; // ?

    private String displayUnit = "";

    private final ObConst.VarName varName;
    
    private final ObConst.ProductName productName;

    private final String displayVarName;

    private String noDataMsg = "";

    private final double millisecinday = 24.0 * 60.0 * 60.0 * 1000.0;

    /**
     * application name (snow, fog, safeseas, etc)
     */
    private CommonConfig.AppName appName;

    /**
     * @param parent
     * @param zone
     * @param station
     * @param dispVarName
     */
    public TrendPlotCanvas(Composite parentComp, String zone, String station,
            String displayVarName, String dataSource, ObMultiHrsReports obData) {
        this.parentComp = parentComp;
        this.selZone = zone;
        this.displayVarName = displayVarName;
        int stInd = displayVarName.indexOf("_");
        varName = ObConst.VarName.valueOf(displayVarName.substring(stInd + 1));
        if ( displayVarName.startsWith("SCA") ) {
        	productName = ObConst.ProductName.SCA; 
        } else if ( displayVarName.startsWith("GALE") ) {
        	productName = ObConst.ProductName.GALE_WARNING; 
        } else if ( displayVarName.startsWith("STORM") ) {
        	productName = ObConst.ProductName.STORM_WARNING; 
        } else if ( displayVarName.startsWith("HURRICANE") ) {
        	productName = ObConst.ProductName.HFWW; 
        } else if ( displayVarName.startsWith("BLIZ") ) {
        	productName = ObConst.ProductName.BLIZZARD_WARNING; 
        } else if ( displayVarName.startsWith("FRZ") ) {
        	productName = ObConst.ProductName.ICE_WARNING; 
        } else if ( displayVarName.startsWith("HSW") ) {
        	productName = ObConst.ProductName.HEAVY_SNOW_WARNING; 
        } else {
        	productName = ObConst.ProductName.UNDEFINED_PRODUCT; 
        } 
        System.out.println("varName = " + varName + "; productName = " + productName);
        display = this.parentComp.getDisplay();
        ObTrendDataSet trendDataSet = obData.getTrendDataSet(zone, station,
                varName, productName);
        if (trendDataSet != null && !trendDataSet.isEmpty()) {
            float[] thresholds = trendDataSet.getSingleValuedThresholds();
            if (!Float.isNaN(thresholds[0])) {
                redThreshold = thresholds[0];
            }
            if (!Float.isNaN(thresholds[1])) {
                yellowThreshold = thresholds[1];
            }
            curdate = ObUtil.getTimeNow(); // Calendar.getInstance();
            
            if (trendDataSet.getYAxisMinMaxIncrement() != null) {
                float[] minMaxIncr = trendDataSet.getYAxisMinMaxIncrement();
                if (!Float.isNaN(minMaxIncr[0])) {
                    minValue = minMaxIncr[0];
                }
                if (!Float.isNaN(minMaxIncr[1])) {
                    maxValue = minMaxIncr[1];
                }
                if (!Float.isNaN(minMaxIncr[2])) {
                    incValue = minMaxIncr[2];
                }
                System.out.println("============\n minValue = " + minMaxIncr[0]
                                                                             + "\n maxValue =" + minMaxIncr[1] + "\n incValue = "
                                                                             + minMaxIncr[2]);
                System.out.println(" redThreshold = " + redThreshold
                        + " yellowThreshold = " + yellowThreshold
                        + "\n============");
            } else {
                noDataMsg = "No data is available!";
                if (trendDataSet.getCeilingCond() != "") {
                    noDataMsg = "All observations are "
                        + trendDataSet.getCeilingCond() + "!";
                }
                noData = true;
            }
            this.trendData = trendDataSet.getDataSet();
            System.out.println("trendData = " + trendData);
        } else {
            noDataMsg = "No data is available";
            noData = true;
        }
        appName = obData.getAppName();
        init();
    }

    private void init() {
        titleFont = new Font(display, "Courier", 11, SWT.BOLD);
        labelFont = new Font(display, "Courier", 9, SWT.BOLD);
        int displayVarNameOrd = ObConst.DisplayVarName.valueOf(displayVarName)
        .ordinal();
        varNameText = ObConst.DISPLAY_VAR_NAME_TEXT[displayVarNameOrd];
        displayUnit = "(" + ObConst.DISPLAY_VAR_UNITS_TEXT[displayVarNameOrd]
                                                           + ")";
        /**
         * DR#10406: There is a special case: for Snow and Fog, 
         * the unit for "Visibility" is statute mile ("mi"), 
         * not nautical mile ("nm") 
         */
        if ( varNameText.equals("Visibility") && ( appName == AppName.SNOW || appName == AppName.FOG ) ) {
        	displayUnit = "(mi)";
        }
        
        labelDisplayVals = new ArrayList<Double>();
        canvasBgColor = new Color(display, 255, 222, 173);
        red = new Color(display, 230, 0, 0);
        yellow = new Color(display, 245, 245, 0);
        green = new Color(display, 0, 235, 0);

        rangeValue = maxValue - minValue;
        // If data not available noData = true
        if (!noData) {
            generateLabelValues();
            // Adjust value range to Y-axis
            valuePerPix = graphHeight / rangeValue;
            // Set format for label values
            setValueFormatString();
            // Set label font
            calcFontMetrics();
        }
        createCanvas(noData);
    }

    private void createCanvas(final boolean noData) {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                if (noData) {
                    drawEmpty(e.gc);
                } else {
                    drawCanvas(e.gc);
                }
            }
        });

        canvas.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                labelFont.dispose();
                titleFont.dispose();
                canvasBgColor.dispose();
                red.dispose();
                green.dispose();
                yellow.dispose();
                // System.out.println("canvas disposing");
            }
        });
    }

    protected void drawEmpty(GC gc) {
        gc.setAntialias(SWT.ON);
        // Fill canvas background
        gc.setBackground(canvasBgColor);
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
        // Draw top label
        drawTopLabel(gc);
        gc.setForeground(red);
        FontMetrics fm = gc.getFontMetrics();
        int h = fm.getHeight();
        int w = fm.getAverageCharWidth();
        gc.drawString(noDataMsg, CANVAS_WIDTH / 2 - w * 10, CANVAS_HEIGHT / 2
                - h);
    }

    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        // Fill canvas background
        gc.setBackground(canvasBgColor);
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
        // Draw top label
        drawTopLabel(gc);
        // Draw threshold
        drawThresholds(gc);
        // Draw axis graph lines
        drawGraphLines(gc);
        // Draw label values
        gc.setFont(labelFont);
        drawValueLabels(gc);
        drawTimeLabels(gc);
        // Draw data
        drawGraphData(gc);
    }

    private void drawTopLabel(GC gc) {
        gc.setFont(titleFont);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, textHeight + 5);
        gc.drawString(varNameText, 5, 0, true);
        gc
        .drawString(displayUnit, 2,
                0 + gc.getFontMetrics().getHeight(), true);
    }

    private void drawThresholds(GC gc) {
        if (redThreshold == 0 && yellowThreshold == 0) {
            redThreshold = (float) minDisplayValue;
            yellowThreshold = (float) minDisplayValue;
        }
        if (redThreshold > yellowThreshold) {
            gc.setBackground(green);
            gc.fillRectangle(graphXcoord, graphYcoord, graphWidth, graphHeight);
            if (maxDisplayValue > yellowThreshold) {
                if (yellowThreshold > minDisplayValue) {
                    gc.setBackground(yellow);
                    gc.fillRectangle(graphXcoord, graphYcoord, graphWidth,
                            (int) Math.round(graphHeight
                                    - (yellowThreshold - minDisplayValue)
                                    * valuePerPix));
                }
                if (yellowThreshold <= minDisplayValue) {
                    gc.setBackground(yellow);
                    gc.fillRectangle(graphXcoord, graphYcoord, graphWidth,
                            graphHeight);
                }
            }
            if (maxDisplayValue > redThreshold) {
                gc.setBackground(red);
                gc.fillRectangle(graphXcoord, graphYcoord, graphWidth,
                        (int) Math.round(graphHeight
                                - (redThreshold - minDisplayValue)
                                * valuePerPix));
            }
        } else {
            gc.setBackground(red);
            gc.fillRectangle(graphXcoord, graphYcoord, graphWidth, graphHeight);
            if (maxDisplayValue > redThreshold) {
                if (redThreshold > minDisplayValue) {
                    gc.setBackground(yellow);
                    gc.fillRectangle(graphXcoord, graphYcoord, graphWidth,
                            (int) Math.round(graphHeight
                                    - (redThreshold - minDisplayValue)
                                    * valuePerPix));
                }
                if (redThreshold <= minDisplayValue) {
                    gc.setBackground(yellow);
                    gc.fillRectangle(graphXcoord, graphYcoord, graphWidth,
                            graphHeight);
                }
            }

            if (maxDisplayValue > yellowThreshold) {
                if (yellowThreshold > minDisplayValue) {
                    gc.setBackground(green);
                    gc
                    .fillRectangle(graphXcoord, graphYcoord, graphWidth, (int) Math
                            .round(graphHeight - (yellowThreshold - minDisplayValue)
                                    * valuePerPix));
                }
                if (yellowThreshold <= minDisplayValue) {
                    gc.setBackground(green);
                    gc.fillRectangle(graphXcoord, graphYcoord, graphWidth, graphHeight);
                }
            }
        }
    }

    private void drawGraphLines(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        // Horizontal value graph lines
        valueLabelIncPerPix = valuePerPix * incValue;
        int yCoord = graphYcoord + graphHeight;
        for (int i = 0; i < labelDisplayVals.size(); i++) {
            yCoord = (int) (graphYcoord + graphHeight - Math
                    .round(valueLabelIncPerPix * i));
            gc.drawLine(graphXcoord, yCoord, graphXcoord + graphWidth, yCoord);
        }
        // Vertical time graph lines
        int timePxInc = graphWidth / 24;
        for (int i = 0; i < 25; i++) {
            gc.drawLine(graphXcoord + timePxInc * i, graphYcoord + graphHeight,
                    graphXcoord + timePxInc * i, graphYcoord);
        }
    }

    private void drawValueLabels(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        int yCoord = graphYcoord + graphHeight;
        for (int i = 0; i < labelDisplayVals.size(); i++) {
            yCoord = (int) (graphYcoord + graphHeight - Math
                    .round(valueLabelIncPerPix * i));
            gc.drawString(getFormattedValueString(labelDisplayVals.get(i)),
                    graphXcoord - 30, yCoord - textHeight / 2, true);
        }
    }

    private void drawTimeLabels(GC gc) {
        // Round up to the next 30 minutes
        Calendar labelTime = (Calendar) curdate.clone();
        int min = labelTime.get(Calendar.MINUTE);
        int minOffset = 0;
        if (min < 30) {
            minOffset = 30 - min;
        } else if (min < 60) {
            minOffset = 60 - min;
        }
        // Calculate the hour which is the closest to current. It will be start
        // time for dates.
        labelTime.add(Calendar.MINUTE, minOffset);
        labelTime.set(Calendar.SECOND, 0);
        labelTime.set(Calendar.MILLISECOND, 0);
        startTimeMillis = labelTime.getTimeInMillis();
        SimpleDateFormat dateFmt = new SimpleDateFormat("HH:mm");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        int timeLablePxInc = graphWidth / 24;
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        // Start put labels on one hour later than start time
        labelTime.add(Calendar.HOUR_OF_DAY, -1);
        // Put label on time axis through every 2 hours
        for (int j = 23; j > 0; j -= 2) {
            String timeLabel = dateFmt.format(labelTime.getTime());
            int x = graphXcoord + timeLablePxInc * j
            - Math.round(textWidth * timeLabel.length() / 2);
            int y = graphYcoord + graphHeight + 2;
            gc.drawString(timeLabel, x, y, true);
            labelTime.add(Calendar.HOUR_OF_DAY, -2);
        }
    }

    private void drawGraphData(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setLineWidth(1);

        int xCoord = 0;
        int yCoord = 0;

        boolean lastExist = false;
        int prevXCoord = Integer.MIN_VALUE;
        int prevYCoord = Integer.MIN_VALUE;
        // Time X-Axis is 24 hours
        double pxPerMillis = graphWidth / millisecinday;
        // Loop and draw the data.
        for (Date d : trendData.keySet()) {
            Float val = trendData.get(d);
            long curTime = d.getTime();
            // Skip data newer than start time ?
            if (curTime > startTimeMillis) {
                lastExist = false;
                continue;
            }
            // Skip data older than start time ?
            if (curTime < startTimeMillis - millisecinday) {
                lastExist = false;
                continue;
            }
            // Skip missed data
            if (val == -9999.0) {
                lastExist = false;
                continue;
            }
            //If current data less then graph minimum.
            if (val < minDisplayValue){
                minDisplayValue = val;
            }
            /*
             * Adjust the value to plot correctly if the minimum is not zero.
             * If minimum is less than zero then add the absolute value of the
             * minimum to the value. If the minimum is greater that zero then
             * subtract the minimum value from the value.
             */
            double adjustedValue = val;
            if (minDisplayValue < 0.0) {
                adjustedValue += Math.abs(minDisplayValue);
            } else if (minDisplayValue > 0.0) {
                adjustedValue -= Math.abs(minDisplayValue);
            }
            long millisOffset = startTimeMillis - curTime;
            xCoord = (int) (graphWidth + graphXcoord - millisOffset
                    * pxPerMillis);
            yCoord = (int) Math.round(graphHeight + graphYcoord - adjustedValue
                    * valuePerPix);

            gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
            gc.fillOval(xCoord - 3, yCoord - 3, 6, 6);
            gc.drawOval(xCoord - 3, yCoord - 3, 6, 6);
            if (lastExist) {
                gc.setLineWidth(2);
                gc.drawLine(xCoord, yCoord, prevXCoord, prevYCoord);
            }
            prevXCoord = xCoord;
            prevYCoord = yCoord;
            lastExist = true;
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
        if (labelDisplayVals != null) {
            labelDisplayVals.clear();
        }
        maxDisplayValue = maxValue;
        minDisplayValue = Math.floor(minValue);
        double currentDisplayValue = minDisplayValue;
        // Fill data Y-axis label array
        while (currentDisplayValue <= maxValue) {
            labelDisplayVals.add(currentDisplayValue);
            currentDisplayValue += incValue;
        }
        // Add extra label on the top of Y-axis
        labelDisplayVals.add(currentDisplayValue);
        maxDisplayValue = currentDisplayValue;
        // Need to adjust the range value
        rangeValue = maxDisplayValue - minDisplayValue;
        System.out.println("maxDisplayValue = " + maxDisplayValue
                + "\nminDisplayValue = " + minDisplayValue + "\nrangeValue = "
                + rangeValue);
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
        // If incValue not an integer
        if (incValue - Math.floor(incValue) == 0) {
            displayAsInt = true;
        }
        if (displayAsInt == false) {
            valueFormatStr = "%4.1f";
        } else {
            valueFormatStr = "%4d";
        }
    }

    /**
     * @param redThreshold
     *            the redThreshold to set
     */
    public void setRedThreshold(float redThreshold) {
        this.redThreshold = redThreshold;
    }

    /**
     * @param yellowThreshold
     *            the yellowThreshold to set
     */
    public void setYellowThreshold(float yellowThreshold) {
        this.yellowThreshold = yellowThreshold;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public void setMinValue(double minValue) {
        this.minValue = minValue;
    }

    /**
     * @param incValue
     *            the incValue to set
     */
    public void setIncValue(double incValue) {
        this.incValue = incValue;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    public void setCurrentTime(Calendar currentTime) {
        this.curdate = currentTime;
    }

    /**
     * @return the selZone
     */
    public String getSelZone() {
        return selZone;
    }

    /**
     * @param selZone
     *            the selZone to set
     */
    public void setSelZone(String selZone) {
        this.selZone = selZone;
    }

    /**
     * @param noData
     *            the noData to set
     */
    public void setNoData() {
        this.noData = true;
    }
}
