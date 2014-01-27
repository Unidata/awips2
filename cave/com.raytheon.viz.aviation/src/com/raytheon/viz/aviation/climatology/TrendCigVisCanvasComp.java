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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * This class draws the graph for the Ceiling/Visibility Trend dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation 
 * 14 JUN 2008  1119       lvenable    Updated to draw graph data.
 * 12 Aug 2013  #2256      lvenable    Disposed of image when the composite is disposed of.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TrendCigVisCanvasComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 800;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 350;

    /**
     * Drawing canvas.
     */
    private Canvas graphCanvas;

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
     * Width of the graph.
     */
    private int graphWidth = 690;

    /**
     * Width of the graph data cell (where data is displayed in number format).
     */
    private int graphDataCellWidth = 0;

    /**
     * Data cell height.
     */
    private int graphDataCellHeight = 15;

    /**
     * Height of the graph.
     */
    private int graphHeight = 200;

    /**
     * Graph X coordinate.
     */
    private int graphXCoord = 100;

    /**
     * Graph Y coordinate.
     */
    private int graphYCoord = 50;

    /**
     * Cell label width.
     */
    private int cellLabelWidth = 60;

    /**
     * Cell label character offset.
     */
    private int cellLabelCharOffset = 4;

    /**
     * Cell character Y offset.
     */
    private int cellCharYOffset = 1;

    /**
     * Cell character X offset.
     */
    private int cellCharXOffset = 15;

    /**
     * Graph bar offset.
     */
    private int barOffset = 4;

    /**
     * Average width of the large font.
     */
    private int fontAveWidthLrg;

    /**
     * Number of data cells displayed on the graph. This is the number of hours
     * plus 1 hour (for the current hour).
     */
    private int numDataCells = 0;

    /**
     * The data to be displayed as a bar graph and numerical data.
     */
    private TrendCigVisGraphData graphData;

    /**
     * MVFR color.
     */
    private Color mvfrColor;

    /**
     * IFR color.
     */
    private Color ifrColor;

    /**
     * LIFR color.
     */
    private Color lifrColor;

    /**
     * VLIFR color.
     */
    private Color vlifrColor;

    /**
     * Graph information header.
     */
    private String graphHeader = "";

    private Image image;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param graphData
     *            Graph data.
     */
    public TrendCigVisCanvasComp(Composite parent,
            TrendCigVisGraphData graphData) {
        super(parent, SWT.BORDER);

        this.parent = parent;

        this.graphData = graphData;

        init();
    }

    /**
     * Initialization method.
     */
    private void init() {
        // Setup the colors for the graph and data labels.
        mvfrColor = new Color(parent.getDisplay(), 0, 255, 0);
        ifrColor = new Color(parent.getDisplay(), 255, 255, 0);
        lifrColor = new Color(parent.getDisplay(), 255, 0, 0);
        vlifrColor = new Color(parent.getDisplay(), 171, 0, 255);

        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));

        // Setup the fonts.
        largeFont = new Font(parent.getDisplay(), "Monospace", 14, SWT.NORMAL);
        mediumFont = new Font(parent.getDisplay(), "Monospace", 12, SWT.NORMAL);
        smallFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        setupCanvas();

        // Add a dispose listener to the canvas so the colors and
        // font can get cleaned up.
        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                largeFont.dispose();
                mediumFont.dispose();
                smallFont.dispose();
                mvfrColor.dispose();
                ifrColor.dispose();
                lifrColor.dispose();
                vlifrColor.dispose();

                if (image != null) {
                    image.dispose();
                }
            }
        });
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        graphCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        graphCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        graphCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAntialias(SWT.OFF);
                drawCanvas(e.gc);
            }
        });
    }

    /**
     * Draw the data on the canvas.
     * 
     * @param gc
     *            The graphic context.
     */
    public void drawCanvas(GC gc) {
        gc.setFont(smallFont);

        gc.setFont(largeFont);
        fontAveWidthLrg = gc.getFontMetrics().getAverageCharWidth();

        // -------------------------------
        // Fill in the canvas background
        // -------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // -----------------------------------------------
        // Draw the graph area.
        // -----------------------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(graphXCoord, graphYCoord, graphWidth, graphHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphXCoord, graphYCoord, graphWidth, graphHeight);

        // -------------------------------------------
        // Check if there is any graph data, if not
        // then return.
        // -------------------------------------------
        if (graphData == null) {
            return;
        }

        // ----------------------------------------------
        // Draw values grid
        // ----------------------------------------------
        drawValuesGrid(gc);

        // ----------------------------------------------
        // Draw the cell label colors and text
        // ----------------------------------------------
        drawCellLabelsAndColors(gc);

        // ----------------------------------------------
        // Draw the cell label colors and text
        // ----------------------------------------------
        drawGraphBarsAndDataCellValues(gc);

        // -------------------------------------------------------
        // Draw the main graph label located above the chart
        // -------------------------------------------------------
        gc.setFont(largeFont);
        int mainLabelXCoord = graphWidth / 2 + graphXCoord
                - graphHeader.length() / 2 * fontAveWidthLrg;
        gc.drawString(graphHeader, mainLabelXCoord, 10, true);

        // ----------------------------------------------
        // Draw the Percent Occurrence label
        // ----------------------------------------------
        drawPercentGraphLabels(gc);
    }

    /**
     * This method draws all of the grid cells where the data will be located.
     * 
     * @param gc
     *            Graphical context.
     */
    private void drawValuesGrid(GC gc) {
        // Draw white background
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(graphXCoord, graphYCoord + graphHeight, graphWidth,
                graphDataCellHeight * 6);

        // Draw the black outline
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphXCoord, graphYCoord + graphHeight, graphWidth,
                graphDataCellHeight * 6);

        // Draw the horizontal grid lines for the numerical data.
        for (int x = 1; x < 6; ++x) {
            gc.drawLine(graphXCoord, graphYCoord + graphHeight
                    + graphDataCellHeight * x, graphXCoord + graphWidth,
                    graphYCoord + graphHeight + graphDataCellHeight * x);
        }

        // Draw the vertical grid lines for the numerical data.
        for (int x = 0; x < numDataCells; ++x) {
            gc.drawLine(graphXCoord + graphDataCellWidth * x, graphYCoord
                    + graphHeight, graphXCoord + graphDataCellWidth * x,
                    graphYCoord + graphHeight + graphDataCellHeight * 6);
        }
    }

    /**
     * Draw the MVFR, IFR, LIFR, VLIFR, and COUNT colors and labels.
     * 
     * @param gc
     *            The graphics contexts.
     */
    private void drawCellLabelsAndColors(GC gc) {
        gc.setFont(smallFont);

        // MVFR cell color and black outline
        gc.setBackground(mvfrColor);
        gc.fillRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight, cellLabelWidth,
                graphDataCellHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight, cellLabelWidth,
                graphDataCellHeight);

        // IFR cell color and black outline
        gc.setBackground(ifrColor);
        gc.fillRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 2, cellLabelWidth,
                graphDataCellHeight);

        gc.drawRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 2, cellLabelWidth,
                graphDataCellHeight);

        // LIFR cell color and black outline
        gc.setBackground(lifrColor);
        gc.fillRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 3, cellLabelWidth,
                graphDataCellHeight);

        gc.drawRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 3, cellLabelWidth,
                graphDataCellHeight);

        // VLIFR cell color and black outline
        gc.setBackground(vlifrColor);
        gc.fillRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 4, cellLabelWidth,
                graphDataCellHeight);

        gc.drawRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 4, cellLabelWidth,
                graphDataCellHeight);

        // COUNT cell color and black outline
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 5, cellLabelWidth,
                graphDataCellHeight);

        gc.drawRectangle(graphXCoord - cellLabelWidth, graphYCoord
                + graphHeight + graphDataCellHeight * 5, cellLabelWidth,
                graphDataCellHeight);

        // -------------------------------------------------------------
        // Add the text to the MVFR, IFR, LIFR, VLIFR, and COUNT cells
        // -------------------------------------------------------------
        gc.setForeground(getTextColor(mvfrColor.getRGB()));
        gc.drawString("MVFR", graphXCoord - cellLabelWidth
                + cellLabelCharOffset, graphYCoord + graphHeight
                + graphDataCellHeight + cellCharYOffset, true);

        gc.setForeground(getTextColor(ifrColor.getRGB()));
        gc.drawString("IFR",
                graphXCoord - cellLabelWidth + cellLabelCharOffset, graphYCoord
                        + graphHeight + graphDataCellHeight * 2
                        + cellCharYOffset, true);

        gc.setForeground(getTextColor(lifrColor.getRGB()));
        gc.drawString("LIFR", graphXCoord - cellLabelWidth
                + cellLabelCharOffset, graphYCoord + graphHeight
                + graphDataCellHeight * 3 + cellCharYOffset, true);

        gc.setForeground(getTextColor(vlifrColor.getRGB()));
        gc.drawString("VLIFR", graphXCoord - cellLabelWidth
                + cellLabelCharOffset, graphYCoord + graphHeight
                + graphDataCellHeight * 4 + cellCharYOffset, true);

        gc.setForeground(getTextColor(vlifrColor.getRGB()));
        gc.drawString("COUNT", graphXCoord - cellLabelWidth
                + cellLabelCharOffset, graphYCoord + graphHeight
                + graphDataCellHeight * 5 + cellCharYOffset, true);
    }

    /**
     * Draw the data graph bars and the numerical data for all of the hours.
     * 
     * @param gc
     *            The graphical context.
     */
    private void drawGraphBarsAndDataCellValues(GC gc) {
        // Set the text font and the number of pixels per unit for the bar graph
        gc.setFont(smallFont);
        int pixelPerInc = graphHeight / 100;

        // Get the arrays for each data category.
        float[] mvfrArray = graphData.getDataArray()[graphData.getMvfrIndex()];
        float[] ifrArray = graphData.getDataArray()[graphData.getIfrIndex()];
        float[] lifrArray = graphData.getDataArray()[graphData.getLifrIndex()];
        float[] vlifrArray = graphData.getDataArray()[graphData.getVlifr()];
        float[] countArray = graphData.getDataArray()[graphData.getCountIndex()];

        // Check how many hours are to be displayed and set the cell
        // and bar offsets.
        if (graphData.getNumberOfHours() == 3) {
            cellCharXOffset = 75;
            barOffset = 30;
        } else if (graphData.getNumberOfHours() == 6) {
            cellCharXOffset = 35;
            barOffset = 15;
        } else if (graphData.getNumberOfHours() == 12) {
            cellCharXOffset = 20;
            barOffset = 4;
        }

        int displayHour = graphData.getStartingHour();

        // Loop and draw all of the hour labels.
        for (int i = 0; i < countArray.length; i++) {
            if (displayHour > 23) {
                displayHour = 0;
            }

            gc.drawString(String.format("%02dZ", displayHour), graphXCoord
                    + graphDataCellWidth * i + cellCharXOffset, graphYCoord
                    + graphHeight + cellCharYOffset, true);

            ++displayHour;
        }

        float total = 0;

        // Loop and draw the MVFR data and bar
        for (int x = 0; x < mvfrArray.length; ++x) {
            // Check if there is any data to be drawn on the bar graph.
            if (mvfrArray[x] != 0) {
                total = mvfrArray[x] + ifrArray[x] + lifrArray[x]
                        + vlifrArray[x];

                gc.setBackground(mvfrColor);

                gc.fillRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));

                gc.drawRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));
            }

            // Draw the MVFR label
            gc.drawString(String.format("%02d", Math.round(mvfrArray[x])),
                    graphXCoord + graphDataCellWidth * x + cellCharXOffset,
                    graphYCoord + graphHeight + graphDataCellHeight
                            + cellCharYOffset, true);
        }

        // Loop and draw the IFR data and bar
        for (int x = 0; x < ifrArray.length; ++x) {
            // Check if there is any data to be drawn on the bar graph.
            if (ifrArray[x] != 0) {
                total = ifrArray[x] + lifrArray[x] + vlifrArray[x];

                gc.setBackground(ifrColor);

                gc.fillRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));

                gc.drawRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));
            }

            // Draw the IFR label
            gc.drawString(String.format("%02d", Math.round(ifrArray[x])),
                    graphXCoord + graphDataCellWidth * x + cellCharXOffset,
                    graphYCoord + graphHeight + (graphDataCellHeight * 2)
                            + cellCharYOffset, true);
        }

        // Loop and draw the LIFR data and bar
        for (int x = 0; x < lifrArray.length; ++x) {
            // Check if there is any data to be drawn on the bar graph.
            if (lifrArray[x] != 0) {
                total = lifrArray[x] + vlifrArray[x];

                gc.setBackground(lifrColor);

                gc.fillRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));

                gc.drawRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));
            }

            // Draw the LIFR data
            gc.drawString(String.format("%02d", Math.round(lifrArray[x])),
                    graphXCoord + graphDataCellWidth * x + cellCharXOffset,
                    graphYCoord + graphHeight + (graphDataCellHeight * 3)
                            + cellCharYOffset, true);
        }

        // Loop and draw the VLIFR data and bar
        for (int x = 0; x < vlifrArray.length; ++x) {
            // Check if there is any data to be drawn on the bar graph.
            if (vlifrArray[x] != 0) {
                total = vlifrArray[x];

                gc.setBackground(vlifrColor);

                gc.fillRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));

                gc.drawRectangle(
                        graphXCoord + graphDataCellWidth * x + barOffset,
                        Math.round(graphHeight + graphYCoord - total
                                * pixelPerInc), graphDataCellWidth - barOffset
                                * 2, Math.round(total * pixelPerInc));
            }

            // Draw the VLIFR data
            gc.drawString(String.format("%02d", Math.round(vlifrArray[x])),
                    graphXCoord + graphDataCellWidth * x + cellCharXOffset,
                    graphYCoord + graphHeight + (graphDataCellHeight * 4)
                            + cellCharYOffset, true);
        }

        // Loop and draw the COUNT data
        for (int x = 0; x < countArray.length; ++x) {
            // Draw the COUNT data
            gc.drawString(String.format("%02d", Math.round(countArray[x])),
                    graphXCoord + graphDataCellWidth * x + cellCharXOffset,
                    graphYCoord + graphHeight + (graphDataCellHeight * 5)
                            + cellCharYOffset, true);
        }
    }

    /**
     * Draw the "Percent" labels on the left side of the graph.
     * 
     * @param gc
     *            The graphic context.
     */
    private void drawPercentGraphLabels(GC gc) {
        gc.setFont(smallFont);
        int aveFontHeight = gc.getFontMetrics().getHeight() / 2;
        // --------------------------------------------
        // Draw the graph percent labels (increments)
        // --------------------------------------------
        int xCoord = 75;
        int yCoord = 0;
        int lineYCoord = 0;
        int yCoordOffset = (int) Math.round(graphHeight / 10.0);

        for (int i = 0; i < 11; i++) {
            yCoord = (graphYCoord + graphHeight) - (yCoordOffset * i)
                    - aveFontHeight;
            gc.drawText(String.format("%3d", (i * 10)), xCoord, yCoord, true);

            gc.setLineStyle(SWT.LINE_DOT);
            lineYCoord = (graphYCoord + graphHeight) - (yCoordOffset * i);
            gc.drawLine(graphXCoord, lineYCoord, graphXCoord + graphWidth,
                    lineYCoord);
            gc.setLineStyle(SWT.LINE_SOLID);
        }

        // --------------------------------------------
        // Draw the "Percent Occurrence" label
        // --------------------------------------------
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setFont(mediumFont);
        Transform t = new Transform(gc.getDevice());
        t.translate(35, 235); // new origin
        t.rotate(-90f);
        gc.setTransform(t);
        gc.drawText("Percent Occurrence", 0, 0, true);
        t.dispose();
    }

    /**
     * The text color returned is either white or black depending on how dark
     * the background RGB value is. The color returned does not get disposed.
     * 
     * @param rgb
     *            RGB background color.
     * @return White or Black system color.
     */
    private Color getTextColor(RGB rgb) {
        if (rgb.red < 165 && rgb.green < 165 && rgb.blue < 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        if (rgb.red < 165 && rgb.green < 165 && rgb.blue > 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        return parent.getDisplay().getSystemColor(SWT.COLOR_BLACK);
    }

    /**
     * Update the graph data and redraw the canvas.
     * 
     * @param graphData
     *            Graph data to be displayed on the canvas.
     */
    public void updateDataAndRedraw(TrendCigVisGraphData graphData) {
        this.graphData = graphData;

        // ----------------------------------------------------
        // Get the number of data cells to be displayed and
        // calculate the width of the graph data cells.
        // ----------------------------------------------------
        numDataCells = graphData.getNumberOfHours() + 1;
        graphDataCellWidth = (int) Math.round((double) graphWidth
                / (double) numDataCells);

        // -------------------------------
        // Build the graph header
        // -------------------------------
        StringBuilder strBld = new StringBuilder();
        strBld.append(graphData.getSite()).append(" ");
        strBld.append(String.format("%02dZ ", graphData.getStartingHour()));

        if (graphData.getSelectedElement() == TrendCigVisGraphData.trendElement.Visibility) {
            strBld.append("Visibility Forecast");
        } else if (graphData.getSelectedElement() == TrendCigVisGraphData.trendElement.Ceiling) {
            strBld.append("Ceiling Forecast");
        } else {
            strBld.append("Flight Category Forecast");
        }

        graphHeader = strBld.toString();

        // -------------------------------
        // Redraw the canvas
        // -------------------------------
        graphCanvas.redraw();
    }

    public Image getCigVisTrendImage() {
        // Redraw the wind rose on an image.
        if (image != null) {
            image.dispose();
        }

        image = new Image(parent.getDisplay(), CANVAS_WIDTH, CANVAS_HEIGHT);

        GC gc = new GC(image);
        drawCanvas(gc);

        gc.dispose();

        return image;

    }
}