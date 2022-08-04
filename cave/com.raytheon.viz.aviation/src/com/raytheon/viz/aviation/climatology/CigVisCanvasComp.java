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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;

import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.Element;
import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.FlightCategory;
import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.GraphType;

/**
 * This class contains the "base" canvas used to draw Ceiling/Visibility graphs.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 28, 2008  938      lvenable  Initial creation
 * Aug 12, 2013  2256     lvenable  Disposed of image when composite is
 *                                  disposed.
 * Jan 29, 2016  18439    zhao      Modified 'months' (changed "null" to "Dec")
 * Feb 24, 2017  6119     tgurney   Fix legend sizing
 * May 14, 2020  8067     randerso  Major refactor and code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class CigVisCanvasComp extends Canvas {

    private class GraphSettings {
        private Element element = Element.VISIBILITY;

        private int maxPercent = 100;

        private int startHour = 0;

        private int numHours = 1;

        private int startMonth = 1;

        private int numMonths = 1;

        private CigVisDistDataManager data;

        public GraphSettings() {

        }

        private GraphSettings(GraphSettings other) {
            this.element = other.element;
            this.maxPercent = other.maxPercent;
            this.startHour = other.startHour;
            this.numHours = other.numHours;
            this.startMonth = other.startMonth;
            this.numMonths = other.numMonths;
            this.data = other.data;
        }

        @Override
        public GraphSettings clone() {
            return new GraphSettings(this);
        }
    }

    private int SET = 0;

    private int DISPLAYED = 1;

    private GraphSettings[] graphSettings = new GraphSettings[2];

    /**
     * Parent composite.
     */
    private CigVisTabComp parent;

    private GraphType graphType;

    /**
     * Large canvas font.
     */
    private Font largeFont;

    /**
     * Medium canvas font.
     */
    private Font mediumFont;

    /**
     * Small canvas font.
     */
    private Font smallFont;

    /**
     * Canvas width.
     */
    private static final int CANVAS_WIDTH = 900;

    /**
     * Canvas height.
     */
    private static final int CANVAS_HEIGHT = 650;

    /**
     * A copy of the image to save to file.
     */
    private Image image;

    private int graphWidth;

    private int cellWidth;

    private int graphHeight = 500;

    private int graphXCoord = 100;

    private int graphYCoord = 50;

    private int cellHeight = 15;

    private int cellCharXOffset = 15;

    private int cellCharYOffset = 1;

    private int cellLabelWidth = 60;

    private int cellLabelCharOffset = 4;

    private int barOffset = 4;

    private int barWidth;

    private Color[] legendColors;

    private float maxPercentInData = -1;

    private boolean drawLegend = true;

    private double pixelPerInc;

    private float[][] dataArray;

    private float[] hoursArray;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent CigVisTabComp
     * @param graphType
     * @param data
     * @param element
     */
    public CigVisCanvasComp(CigVisTabComp parent, GraphType graphType,
            CigVisDistDataManager data, Element element) {
        super(parent, SWT.BORDER | SWT.DOUBLE_BUFFERED);
        this.parent = parent;
        graphSettings[SET] = new GraphSettings();
        graphSettings[SET].data = data;
        graphSettings[SET].element = element;
        this.graphType = graphType;
        setAutoRedraw(true);

        init();
    }

    /**
     * Initialize the canvas and composite.
     */
    private void init() {
        setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));

        int graphWidthOffset = 780 % graphType.getNumColumns();
        graphWidth = 780 - graphWidthOffset;
        cellWidth = graphWidth / graphType.getNumColumns();
        barWidth = cellWidth - 8;

        largeFont = new Font(parent.getDisplay(), "Monospace", 14, SWT.NORMAL);
        mediumFont = new Font(parent.getDisplay(), "Monospace", 12, SWT.NORMAL);
        smallFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        legendColors = new Color[FlightCategory.values().length];
        for (FlightCategory fc : FlightCategory.values()) {
            if (fc.isDisplayed()) {
                legendColors[fc.ordinal()] = new Color(getDisplay(),
                        fc.getLegendColor());
            }
        }

        addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                e.gc.setAntialias(SWT.OFF);
                drawCanvas(e.gc);
            }
        });

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                largeFont.dispose();
                mediumFont.dispose();
                smallFont.dispose();

                for (Color color : legendColors) {
                    if (color != null) {
                        color.dispose();
                    }
                }

                if (image != null) {
                    image.dispose();
                }
            }
        });
    }

    public void setCigVisData(CigVisDistDataManager data) {
        graphSettings[SET].data = data;
        redraw();
    }

    public void setElement(Element element) {
        graphSettings[SET].element = element;
        redraw();
    }

    /**
     * Set the maxPercent on the graph scale.
     *
     * @param maxPercent
     *            Maximum Percent
     */
    public void setMaxPercent(int maxPercent) {
        graphSettings[SET].maxPercent = maxPercent;
        redraw();
    }

    /**
     * @param startHour
     *            the startHour to set
     */
    public void setStartHour(int startHour) {
        graphSettings[SET].startHour = startHour;
        redraw();
    }

    /**
     * @param numHours
     *            the numHours to set
     */
    public void setNumHours(int numHours) {
        graphSettings[SET].numHours = numHours;
        redraw();
    }

    /**
     * @param startMonth
     *            the startMonth to set
     */
    public void setStartMonth(int startMonth) {
        graphSettings[SET].startMonth = startMonth;
        redraw();
    }

    /**
     * @param numMonths
     *            the numMonths to set
     */
    public void setNumMonths(int numMonths) {
        graphSettings[SET].numMonths = numMonths;
        redraw();
    }

    /**
     * @param autoRedraw
     *            the autoRedraw to set
     */
    public void setAutoRedraw(boolean autoRedraw) {
        if (autoRedraw) {
            graphSettings[DISPLAYED] = graphSettings[SET];
        } else {
            /* if DISPLAYED is the same object as SET */
            if (graphSettings[DISPLAYED] == graphSettings[SET]) {
                /*
                 * make a separate copy so the display no longer reflects the
                 * set values
                 */
                graphSettings[DISPLAYED] = graphSettings[SET].clone();
            }
        }
        redraw();
    }

    /**
     * Force the display to update to the latest settings
     */
    public void forceUpdate() {
        /* if DISPLAYED is not the same object as SET */
        if (graphSettings[DISPLAYED] != graphSettings[SET]) {
            graphSettings[DISPLAYED] = graphSettings[SET].clone();
        }
        redraw();
    }

    /**
     * The text color returned is either gray or black depending on how dark the
     * background RGB value is.
     *
     * The color returned is a system color and should not be disposed.
     *
     * @param color
     *            background color.
     * @return White or Black system color.
     */
    private Color getTextColor(Color color) {
        RGB rgb = color.getRGB();
        if (rgb.red < 165 && rgb.green < 165 && rgb.blue < 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        if (rgb.red < 165 && rgb.green < 165 && rgb.blue > 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        return parent.getDisplay().getSystemColor(SWT.COLOR_BLACK);
    }

    private String getMainLabel() {
        int startYear = graphSettings[DISPLAYED].data.getStartYear();
        int endYear = graphSettings[DISPLAYED].data.getEndYear();
        String site = graphSettings[DISPLAYED].data.getSite();

        String format = graphType.getLegendFormat();
        String dataType = graphSettings[DISPLAYED].element.getDisplayString();

        String hourStr = CigVisDistDataManager.hourStr(
                graphSettings[DISPLAYED].startHour,
                graphSettings[DISPLAYED].numHours);
        String monthStr = CigVisDistDataManager.monthStr(
                graphSettings[DISPLAYED].startMonth,
                graphSettings[DISPLAYED].numMonths);

        String mainLabel = String.format(format, site, dataType, hourStr,
                monthStr, startYear, endYear);

        return mainLabel;
    }

    /**
     * Draw the legend on the graph.
     *
     * @param gc
     *            Graphical context.
     * @param graphXCoord
     *            Graph's X coordinate.
     * @param graphYCoord
     *            Graph's Y coordinate.
     * @param graphWidth
     *            Graph width.
     * @param mvfrColor
     *            MVFR color.
     * @param ifrColor
     *            IFR color.
     * @param lifrColor
     *            LIFR color.
     * @param vlifrColor
     *            VLIFR color.
     */
    private void drawGraphLegend(GC gc, int graphXCoord, int graphYCoord,
            int graphWidth) {
        int charWidth = gc.getCharWidth('x');
        int charHeight = gc.getFontMetrics().getHeight();
        int legendBarWidth = charWidth * 6;
        int legendBarHeight = charHeight;
        int legendWidth = legendBarWidth + charWidth * 8;
        int legendHeight = (int) (charHeight * 6.5);
        int legendXCoord = graphXCoord + graphWidth - legendWidth - 10;
        int legendYCoord = graphYCoord + 10;
        int legendXOffset = charWidth;
        int legendYOffset = charHeight / 2;
        //
        // Draw Legend Box
        //
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(legendXCoord, legendYCoord, legendWidth, legendHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord, legendYCoord, legendWidth, legendHeight);

        //
        // Draw Legend information
        //
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        int y = legendYCoord + legendHeight;
        for (FlightCategory fc : FlightCategory.values()) {
            if (fc.isDisplayed()) {
                y -= legendYOffset + legendBarHeight;

                gc.setBackground(legendColors[fc.ordinal()]);
                gc.fillRectangle(legendXCoord + legendXOffset, y,
                        legendBarWidth, legendBarHeight);

                gc.drawRectangle(legendXCoord + legendXOffset, y,
                        legendBarWidth, legendBarHeight);

                gc.drawString(fc.name(),
                        legendXCoord + legendXOffset * 2 + legendBarWidth, y,
                        true);

            }
        }

    }

    public Image getCigVisDistImage() {
        if (image != null) {
            image.dispose();
        }

        image = new Image(parent.getDisplay(), CANVAS_WIDTH, CANVAS_HEIGHT);

        GC gc = new GC(image);
        drawCanvas(gc);

        gc.dispose();

        return image;
    }

    /**
     * Draw the graph on the canvas.
     *
     * @param gc
     *            Graphical context.
     */
    private void drawCanvas(GC gc) {

        gc.setFont(smallFont);
        int fontHeightSm = gc.getFontMetrics().getHeight();

        gc.setFont(largeFont);

        // -------------------------------
        // Fill in the canvas background
        // -------------------------------
        gc.setBackground(parent.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // -----------------------------------------------
        // Draw the graph area.
        // -----------------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(graphXCoord, graphYCoord, graphWidth, graphHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphXCoord, graphYCoord, graphWidth, graphHeight);

        // ----------------------------------------------
        // Draw values grid
        // ----------------------------------------------

        drawValuesGrid(gc);

        String mainLabel = getMainLabel();
        int mainLabelXCoord = graphXCoord
                + (graphWidth - gc.textExtent(mainLabel).x) / 2;
        gc.drawString(mainLabel, mainLabelXCoord, 10, true);

        // Draw the column Labels
        gc.setFont(smallFont);
        int x = graphXCoord;
        int y = graphYCoord + graphHeight + cellCharYOffset;
        for (String dir : graphType.getLabels()) {
            int tmpCellOffset = (cellWidth - gc.textExtent(dir).x) / 2;
            gc.drawString(dir, x + tmpCellOffset, y, true);
            x += cellWidth;
        }

        // ----------------------------------------------
        // Draw the cell label colors and text
        // ----------------------------------------------
        y = graphYCoord + graphHeight + cellHeight;
        List<FlightCategory> cats = Arrays.asList(FlightCategory.values());
        Collections.reverse(cats);
        for (FlightCategory fc : cats) {
            if (fc.isDisplayed()) {
                Color color = legendColors[fc.ordinal()];
                gc.setBackground(color);
                gc.fillRectangle(graphXCoord - cellLabelWidth, y,
                        cellLabelWidth, cellHeight);

                gc.setForeground(
                        parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
                gc.drawRectangle(graphXCoord - cellLabelWidth, y,
                        cellLabelWidth, cellHeight);

                // Add the label text
                gc.setForeground(getTextColor(color));
                gc.drawString(fc.name(),
                        graphXCoord - cellLabelWidth + cellLabelCharOffset,
                        y + cellCharYOffset, true);

                y += cellHeight;
            }
        }

        // Hours cell label
        if (graphType.isDisplayHours()) {
            Color color = getDisplay().getSystemColor(SWT.COLOR_WHITE);
            gc.setBackground(color);
            gc.fillRectangle(graphXCoord - cellLabelWidth, y, cellLabelWidth,
                    cellHeight);

            gc.setForeground(
                    parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
            gc.drawRectangle(graphXCoord - cellLabelWidth, y, cellLabelWidth,
                    cellHeight);

            gc.setForeground(getTextColor(color));
            gc.drawString("Hours",
                    graphXCoord - cellLabelWidth + cellLabelCharOffset,
                    y + cellCharYOffset, true);
        }

        calculateGraphData();

        int graphMax = determineGraphMax();
        pixelPerInc = graphHeight / (double) graphMax;

        // ------------------------------------------------------------
        // Draw the bars and the data values in the cells
        // ------------------------------------------------------------
        drawGraphBarsAndDataCellValues(gc);

        // ------------------------------------------------------------
        // Draw the Percent labels and the dash lines across the graph
        // ------------------------------------------------------------
        gc.setLineStyle(SWT.LINE_DOT);

        int offset = 10;

        if (graphMax <= 10) {
            offset = 1;
        } else if (graphMax <= 20) {
            offset = 2;
        } else if (graphMax <= 50) {
            offset = 5;
        }

        for (x = 0; x <= graphMax; x += offset) {
            y = (int) Math.round(graphYCoord + graphHeight - pixelPerInc * x);

            if (y >= graphYCoord) {
                gc.drawString(String.format("%3d", x), graphXCoord - 25,
                        y - fontHeightSm / 2, true);
                gc.drawLine(graphXCoord, y, graphXCoord + graphWidth, y);
            }
        }

        if (graphMax % 10 != 0) {
            y = (int) Math
                    .round(graphYCoord + graphHeight - pixelPerInc * graphMax);
            gc.drawString(String.format("%3d", graphMax), graphXCoord - 25,
                    y - fontHeightSm / 2, true);
            gc.drawLine(graphXCoord, y, graphXCoord + graphWidth, y);
        }

        gc.setLineStyle(SWT.LINE_SOLID);

        // ------------------------------------------------
        // Draw the legend
        // ------------------------------------------------

        if (drawLegend) {
            drawGraphLegend(gc, graphXCoord, graphYCoord, graphWidth);
        }

        // ----------------------------------------------
        // Draw the Percent Occurrence label
        // ----------------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setFont(mediumFont);
        Transform t = new Transform(gc.getDevice());
        // new origin
        t.translate(35, 375);
        t.rotate(-90f);
        gc.setTransform(t);
        gc.drawText("Percent Occurrence", 0, 0, true);
        t.dispose();
    }

    private int determineGraphMax() {
        int graphMax = graphSettings[DISPLAYED].maxPercent;
        if (graphMax < 0) {
            graphMax = (int) Math.ceil(maxPercentInData / 5) * 5;
        }
        if (graphMax < 5) {
            graphMax = 5;
        }
        return graphMax;
    }

    private void calculateGraphData() {

        dataArray = graphSettings[DISPLAYED].data.getDataByGraphType(graphType,
                graphSettings[DISPLAYED].element,
                graphSettings[DISPLAYED].startHour,
                graphSettings[DISPLAYED].numHours,
                graphSettings[DISPLAYED].startMonth,
                graphSettings[DISPLAYED].numMonths);
        hoursArray = new float[dataArray[0].length];

        for (int i = 0; i < dataArray[0].length; i++) {
            float t = 0;
            for (float[] f : dataArray) {
                t += f[i];
            }
            hoursArray[i] = t;
            for (float[] f : dataArray) {
                f[i] = (f[i] / t) * 100;
            }

            maxPercentInData = Math.max(
                    (100 - dataArray[FlightCategory.VFR.ordinal()][i]),
                    maxPercentInData);
            if (Float.isNaN(maxPercentInData)) {
                maxPercentInData = 0;
            }
        }

    }

    /**
     * Draw the graph legend based on the flag passed in.
     *
     * @param flag
     *            True draws the legend, false does not.
     */
    public void drawLegend(boolean flag) {
        drawLegend = flag;
        redraw();
    }

    /**
     * This method draws all of the grid cells where the data will be located.
     *
     * @param gc
     *            Graphical context.
     */
    private void drawValuesGrid(GC gc) {
        int numRows = 5;
        if (graphType.isDisplayHours()) {
            numRows++;
        }

        // Draw white background
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(graphXCoord, graphYCoord + graphHeight, graphWidth,
                cellHeight * numRows);

        // Draw the black outline
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphXCoord, graphYCoord + graphHeight, graphWidth,
                cellHeight * numRows);

        // Draw the horizontal grid lines
        for (int x = 1; x < numRows; ++x) {
            gc.drawLine(graphXCoord, graphYCoord + graphHeight + cellHeight * x,
                    graphXCoord + graphWidth,
                    graphYCoord + graphHeight + cellHeight * x);
        }

        // Draw the vertical grid lines
        for (int x = 0; x < graphType.getNumColumns(); ++x) {
            gc.drawLine(graphXCoord + cellWidth * x, graphYCoord + graphHeight,
                    graphXCoord + cellWidth * x,
                    graphYCoord + graphHeight + cellHeight * numRows);
        }
    }

    /**
     * Draw the bar graphs and the data in the grid cells.
     *
     * @param gc
     *            Graphical context.
     */
    private void drawGraphBarsAndDataCellValues(GC gc) {
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        for (int x = 0; x < dataArray[0].length; ++x) {
            int y = graphYCoord + graphHeight + cellHeight * 5
                    + cellCharYOffset;

            // Draw the Hours data
            if (graphType.isDisplayHours()) {
                gc.drawString(String.format("%02d", Math.round(hoursArray[x])),
                        graphXCoord + cellWidth * x + cellCharXOffset, y, true);
            }

            int yStartPos = graphYCoord + graphHeight;
            for (FlightCategory fc : FlightCategory.values()) {
                float dataValue = dataArray[fc.ordinal()][x];
                if (!Float.isNaN(dataValue)) {
                    if (fc.isDisplayed()) {
                        y -= cellHeight;
                        gc.drawString(
                                String.format("%02d", Math.round(dataValue)),
                                graphXCoord + cellWidth * x + cellCharXOffset,
                                y, true);

                        // draw the bar
                        gc.setBackground(legendColors[fc.ordinal()]);
                        int barHeight = (int) Math
                                .round(dataValue * pixelPerInc);
                        yStartPos -= barHeight;

                        gc.fillRectangle(
                                cellWidth * x + graphXCoord + barOffset,
                                yStartPos, barWidth, barHeight);

                        gc.drawRectangle(
                                cellWidth * x + graphXCoord + barOffset,
                                yStartPos, barWidth, barHeight);
                    }
                }
            }
        }
    }
}
