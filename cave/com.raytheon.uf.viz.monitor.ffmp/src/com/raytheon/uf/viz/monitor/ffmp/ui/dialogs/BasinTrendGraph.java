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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.PlotItems;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.TimeDuration;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.Underlays;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPGraphData;

/**
 * This class draws the data on the graph plus all of the data labels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            lvenable     Initial creation
 * Oct 10, 2015  4756      dhladky      Dynamic bounding of Y axis.
 * Feb 11, 2016  5360      tjensen      Fix scaling issues
 * Mar 26, 2016  5493      dhladky      Removed code that prevented drawing 12/24hr FFG
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class BasinTrendGraph {
    /**
     * Parent composite
     */
    private Composite parentComp;

    /**
     * Main composite
     */
    private Composite mainComp;

    /**
     * The display device
     */
    private Display display;

    /**
     * Main canvas composite that holds all of the canvases
     */
    private Composite mainCanvasComp;

    /**
     * Data graph canvas
     */
    private Canvas graphCanvas;

    /**
     * The canvas containing the left side label.
     */
    private Canvas leftLabelCanvas;

    /**
     * Left side increment canvas.
     */
    private Canvas leftIncCanvas;

    /**
     * Right side increment canvas.
     */
    private Canvas rightIncCanvas;

    /**
     * Bottom increment canvas.
     */
    private Canvas bottomIncCanvas;

    /**
     * Graph view width - the amount of graph width that is visible
     */
    private final int graphViewWidth = 600;

    /**
     * Graph view height - the amount of graph height that is visible.
     */
    private final int graphViewHeight = 400;

    /**
     * Graph canvas width.
     */
    private final int graphCanvasWidth = 2016;

    /**
     * Graph canvas height.
     */
    private final int graphCanvasHeight = 400;

    /**
     * Graph image width
     */
    private int graphImageWidth = graphCanvasWidth;

    /**
     * Graph image height
     */
    private int graphImageHeight = graphCanvasHeight;

    /**
     * Label canvas width & height.
     */
    private final int labelCanvasWidth = 25;

    /**
     * Inches per hour canvas height.
     */
    private final int labelCanvasHeight = graphViewHeight;

    /**
     * Left increment canvas width.
     */
    private final int leftIncCanvasWidth = 50;

    /**
     * Right increment canvas width.
     */
    private final int rightIncCanvasWidth = 50;

    /**
     * Bottom increment canvas width and height.
     */
    private final int bottomIncCanvasWidth = labelCanvasWidth
            + leftIncCanvasWidth + graphCanvasWidth + rightIncCanvasWidth;

    /**
     * Bottom hours increment canvas height.
     */
    private final int bottomIncCanvasHeight = 50;

    /**
     * Number of pixels for each inch per hour increment.
     */
    private double pixPerInc_YCoord = 0;

    /**
     * The number of pixels for each hour increment.
     */
    private double pixPerInc_XCoord = 0;

    /**
     * Horizontal scale control.
     */
    private Scale hScale;

    /**
     * Vertical scale control.
     */
    private Scale vScale;

    /**
     * X coordinate used to move the graph via the mouse.
     */
    private int mainXCoord = 0;

    /**
     * Y coordinate used to move the graph via the mouse.
     */
    private int mainYCoord = 0;

    /**
     * Mouse down flag.
     */
    private boolean mouseDown = false;

    /**
     * Current mouse point.
     */
    private Point mousePt = new Point(Integer.MIN_VALUE, Integer.MIN_VALUE);

    /**
     * Font used for the Y axis label.
     */
    private Font labelFont;

    /**
     * Font used for the Y Axis values
     */
    private Font scaleFont;

    /**
     * Label text for the Y axis label.
     */
    private String scaleLabelText = "Inches/hr";

    /**
     * Left increment image.
     */
    private Image leftIncImage;

    /**
     * Right increment image.
     */
    private Image rightIncImage;

    /**
     * Graph image.
     */
    private Image graphImage;

    /**
     * Bottom hour increment image.
     */
    private Image bottomIncImage;

    /**
     * Background color.
     */
    private Color bgColor;

    /**
     * Length of the dashes on the graph scales.
     */
    private final int dashLength = 8;

    /**
     * Flag indicating if the X axis should be reversed.
     */
    private boolean reverseXAxis = false;

    /**
     * Time duration.
     */
    private TimeDuration timeDur = TimeDuration.ALL;

    /**
     * Basin Trend Graph Bounds.
     */
    private BasinTrendGraphBounds graphBounds;

    /**
     * Data to graph.
     */
    private FFMPGraphData graphData;

    /**
     * Graph Rate flag.
     */
    private boolean graphRate = false;

    /**
     * Graph QPE flag.
     */
    private boolean graphQPE = false;

    /**
     * Graph QPF flag.
     */
    private boolean graphQPF = false;

    /**
     * Graph Guidance flag.
     */
    private boolean graphGuid = false;

    /**
     * Graph VBG flag.
     */
    private boolean graphVGB = false;

    /**
     * The underlay to graph.
     */
    private Underlays underlay;

    /**
     * The circle X & Y offset.
     */
    private final int circleXYOffset = 2;

    /**
     * The circle width & height.
     */
    private final int circleWthHgt = 5;

    /**
     * Color used for graphing the Rate line.
     */
    private Color rateColor;

    /**
     * Color used for graphing the QPE line.
     */
    private Color qpeColor;

    /**
     * Color used for graphing the QPF line.
     */
    private Color qpfColor;

    /**
     * Color used for graphing the guidance line..
     */
    private Color guidColor;

    /**
     * Color used for graphing the VGB lines.
     */
    private Color vgbColor;

    private Calendar cal = Calendar.getInstance();

    /**
     * Adjusted time offset. This will either 0.0 with QPF turned off or 1.0 for
     * QPF tuned on. This value will be applied to adjust the plotting of the
     * data.
     */
    double adjustedTimeOffset = 0.0;

    /**
     * Simple date format in hours and minutes.
     */
    private SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param bgColor
     *            Background color.
     * @param graphData
     *            Graph data.
     * @param rateColor
     *            Rate color.
     * @param qpeColor
     *            QPE color.
     * @param qpfColor
     *            QPF color.
     * @param guidColor
     *            Guidance color.
     * @param reverseXAxis
     *            Reverse X axis flag.
     */
    public BasinTrendGraph(Composite parentComp, Color bgColor,
            FFMPGraphData graphData, Color rateColor, Color qpeColor,
            Color qpfColor, Color guidColor, Color vgbColor,
            boolean reverseXAxis) {
        this.parentComp = parentComp;
        this.bgColor = bgColor;
        this.graphData = graphData;

        this.rateColor = rateColor;
        this.qpeColor = qpeColor;
        this.qpfColor = qpfColor;
        this.guidColor = guidColor;
        this.vgbColor = vgbColor;

        this.reverseXAxis = reverseXAxis;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {

        determineGraphBounds();
        display = parentComp.getDisplay();
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        recalcPixelsIncrements();

        mainComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);
        mainComp.setBackground(bgColor);

        gl = new GridLayout(4, false);
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        mainCanvasComp = new Composite(mainComp, SWT.BORDER);
        mainCanvasComp.setLayout(gl);

        initializeComponents();

        mainComp.pack();

        vScale.setSelection(vScale.getMaximum());
        mainYCoord = 0 - vScale.getSelection();
        graphCanvas.redraw();
        rightIncCanvas.redraw();
        leftIncCanvas.redraw();

        /*
         * When the shell disposes then dispose of the necessary widgets.
         */
        parentComp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                labelFont.dispose();
                scaleFont.dispose();

                leftIncImage.dispose();
                rightIncImage.dispose();
                graphImage.dispose();
                bottomIncImage.dispose();
            }
        });
    }

    /**
     * Initialize the canvases and other controls.
     */
    private void initializeComponents() {
        labelFont = new Font(display, "Monospace", 14, SWT.NORMAL);
        scaleFont = new Font(display, "Monospace", 10, SWT.BOLD);

        createLeftIncImage();
        createRightIncImage();

        // System.out.println("in BasinTrendGraph.initializeComponents()");
        createGraphImage();
        createAllHoursBottomIncImage();

        addLabelCanvas();
        addLeftIncCanvas();
        addGraphCanvas();
        addRightIncCanvas();
        addBottomIncCanvas();
        addScaleControls();
    }

    /**
     * Add the Y axis label canvas (left side of the display). This label never
     * changes. Will be the same for All Hours, 1, 3, 6, 12, 24.
     */
    private void addLabelCanvas() {
        leftLabelCanvas = new Canvas(mainCanvasComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = labelCanvasWidth;
        gd.heightHint = labelCanvasHeight;
        leftLabelCanvas.setBackground(bgColor);
        leftLabelCanvas.setLayoutData(gd);
        leftLabelCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAdvanced(true);
                drawLabelCanvas(e.gc);
            }
        });
    }

    /**
     * Add the left side scale increment canvas.
     */
    private void addLeftIncCanvas() {
        Composite canvasComp = new Composite(mainCanvasComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);
        GridData gd = new GridData(leftIncCanvasWidth, graphViewHeight);
        canvasComp.setLayoutData(gd);

        leftIncCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = leftIncCanvasWidth;
        gd.heightHint = graphCanvasHeight;
        leftIncCanvas.setBackground(bgColor);
        leftIncCanvas.setLayoutData(gd);
        leftIncCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAdvanced(true);
                drawLeftIncCanvas(e.gc);
            }
        });
    }

    /**
     * Add the graph canvas to the display.
     */
    private void addGraphCanvas() {
        Composite canvasComp = new Composite(mainCanvasComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);
        GridData gd = new GridData(graphViewWidth, graphViewHeight);
        canvasComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = graphCanvasWidth;
        gd.heightHint = graphCanvasHeight;
        graphCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        graphCanvas.setBackground(bgColor);
        graphCanvas.setLayoutData(gd);

        /*
         * Add a mouse listener to the graph canvas to trap the mouse clicks.
         */
        graphCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                mouseDown = true;
            }

            @Override
            public void mouseUp(MouseEvent e) {
                mouseDown = false;
                mousePt.x = Integer.MIN_VALUE;
                mousePt.y = Integer.MIN_VALUE;
            }
        });

        /*
         * Add a mouse move listener to the graph canvas to get the x,y
         * coordinates when the mouse is moved (with the mouse button down).
         */
        graphCanvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                if (timeDur != TimeDuration.ALL) {
                    return;
                }

                if (mouseDown == true) {
                    if (mousePt.x != Integer.MIN_VALUE) {
                        vScale.setSelection(vScale.getSelection() + mousePt.y
                                - e.y);
                        hScale.setSelection(hScale.getSelection() + mousePt.x
                                - e.x);

                        mainXCoord = 0 - hScale.getSelection();
                        mainYCoord = 0 - vScale.getSelection();

                        leftIncCanvas.redraw();
                        graphCanvas.redraw();
                        rightIncCanvas.redraw();
                        bottomIncCanvas.redraw();
                    }

                    mousePt.x = e.x;
                    mousePt.y = e.y;
                }
            }
        });

        /*
         * Add a paint listener to the graph canvas.
         */
        graphCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAdvanced(true);
                drawGraphCanvas(e.gc);
            }
        });
    }

    /**
     * Add the right scale increment canvas to the display.
     */
    private void addRightIncCanvas() {
        Composite canvasComp = new Composite(mainCanvasComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        canvasComp.setLayout(gl);
        GridData gd = new GridData(rightIncCanvasWidth, graphViewHeight);
        canvasComp.setLayoutData(gd);

        rightIncCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = rightIncCanvasWidth;
        gd.heightHint = graphCanvasHeight;
        rightIncCanvas.setBackground(bgColor);
        rightIncCanvas.setLayoutData(gd);

        /*
         * Add a paint listener to the right increment canvas.
         */
        rightIncCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAdvanced(true);
                drawRightIncCanvas(e.gc);
            }
        });
    }

    /**
     * Add the bottom hour increment canvas to the display.
     */
    private void addBottomIncCanvas() {
        Composite canvasComp = new Composite(mainCanvasComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        canvasComp.setLayout(gl);
        int bottomViewWidth = labelCanvasWidth + leftIncCanvasWidth
                + graphViewWidth + rightIncCanvasWidth;
        GridData gd = new GridData(bottomViewWidth, bottomIncCanvasHeight);
        gd.horizontalSpan = ((GridLayout) mainCanvasComp.getLayout()).numColumns;
        canvasComp.setLayoutData(gd);

        bottomIncCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = bottomIncCanvasWidth;
        gd.heightHint = bottomIncCanvasHeight;
        bottomIncCanvas.setBackground(bgColor);
        bottomIncCanvas.setLayoutData(gd);
        bottomIncCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAdvanced(true);
                drawBottomIncCanvas(e.gc);
            }
        });
    }

    /**
     * Add the vertical and horizontal scale controls used to move the increment
     * and data graphs..
     */
    private void addScaleControls() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        vScale = new Scale(mainComp, SWT.VERTICAL);
        vScale.setMinimum(0);
        vScale.setMaximum(graphCanvasHeight - graphViewHeight + 1);
        vScale.setLayoutData(gd);
        vScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                mainYCoord = 0 - vScale.getSelection();
                graphCanvas.redraw();
                rightIncCanvas.redraw();
                leftIncCanvas.redraw();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        hScale = new Scale(mainComp, SWT.HORIZONTAL);
        hScale.setMinimum(0);
        hScale.setMaximum(graphCanvasWidth - graphViewWidth + 1);
        hScale.setLayoutData(gd);
        hScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                mainXCoord = 0 - hScale.getSelection();
                graphCanvas.redraw();
                bottomIncCanvas.redraw();
            }
        });
    }

    /**
     * Draw the Y axis label canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawLabelCanvas(GC gc) {
        gc.setFont(labelFont);
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        int labelInPixels = gc.getFontMetrics().getAverageCharWidth()
                * scaleLabelText.length();
        int yCoord = (labelCanvasHeight / 2) + (labelInPixels / 2);

        Transform t = new Transform(gc.getDevice());
        t.translate(2, yCoord); // new origin
        t.rotate(-90f);
        gc.setTransform(t);
        gc.drawString(scaleLabelText, 0, 0, true);
        t.dispose();
    }

    /**
     * Draw the left scale increment canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawLeftIncCanvas(GC gc) {
        if (timeDur == TimeDuration.ALL) {
            gc.drawImage(leftIncImage, 0, mainYCoord); // Time duration is ALL
        } else {
            // Draw at the 0,0 of the "view port"
            gc.drawImage(leftIncImage, 0, 0);
        }
    }

    /**
     * Draw the data graph canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawGraphCanvas(GC gc) {
        if (timeDur == TimeDuration.ALL) {
            gc.drawImage(graphImage, mainXCoord, mainYCoord); // Time duration
                                                              // is ALL
        } else {
            // Draw at the corner of the "view port"
            gc.drawImage(graphImage, 0, 1);
        }
    }

    /**
     * Draw the right scale increment canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawRightIncCanvas(GC gc) {
        if (timeDur == TimeDuration.ALL) {
            gc.drawImage(rightIncImage, 0, mainYCoord); // Time duration is ALL
        } else {
            // Draw at the 0,0 of the "view port"
            gc.drawImage(rightIncImage, 0, 0);
        }
    }

    /**
     * Draw the bottom hours increment canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawBottomIncCanvas(GC gc) {
        gc.drawImage(bottomIncImage, mainXCoord, 0);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.setLineWidth(2);
        gc.drawLine(labelCanvasWidth + leftIncCanvasWidth - 2, 1,
                graphViewWidth + labelCanvasWidth + leftIncCanvasWidth + 2, 1);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, labelCanvasWidth + (leftIncCanvasWidth / 2),
                bottomIncCanvasHeight);
        gc.fillRectangle(graphViewWidth + labelCanvasWidth + leftIncCanvasWidth
                + (rightIncCanvasWidth / 2), 0, graphViewWidth
                + labelCanvasWidth + leftIncCanvasWidth + rightIncCanvasWidth,
                bottomIncCanvasHeight);
    }

    /**
     * Create the increment image on the left side of the graph.
     */
    private void createLeftIncImage() {
        if (leftIncImage != null) {
            leftIncImage.dispose();
        }

        leftIncImage = new Image(display, leftIncCanvasWidth,
                graphImageHeight + 1);

        GC gc = new GC(leftIncImage);

        gc.setFont(scaleFont);
        int fontWidth = gc.getFontMetrics().getAverageCharWidth();
        int fontHeight = gc.getFontMetrics().getHeight();
        int aveFontHeight = (int) Math.round(fontHeight / 2.0);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, leftIncCanvasWidth, graphImageHeight + 1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.setLineWidth(2);
        gc.drawLine(leftIncCanvasWidth - 1, 0, leftIncCanvasWidth - 1,
                graphImageHeight + 1);

        double yCoord = 0.0;
        String inchLbl;
        double inchDbl = graphBounds.getYCoordValues();
        int skip = graphBounds.getSkipValue();
        int count = 0;

        while (yCoord < graphImageHeight) {

            // When counter == skip, allow graph to draw a label.
            if (count == skip || yCoord == 0.0) {
                // resets the counter
                if (yCoord > 1.0) {
                    count = 0;
                }

                if (inchDbl == graphBounds.getYCoordValues()) {
                    gc.drawLine(leftIncCanvasWidth, doubleToInt(yCoord + 1.0),
                            leftIncCanvasWidth - dashLength,
                            doubleToInt(yCoord + 5.0));
                    inchLbl = String.format("%1.1f", inchDbl);
                    gc.drawString(inchLbl, leftIncCanvasWidth - dashLength - 3
                            - (inchLbl.length() * fontWidth),
                            doubleToInt(yCoord + 5.0), true);

                } else {
                    if (inchDbl != 0.0) {
                        gc.drawLine(leftIncCanvasWidth,
                                doubleToInt(yCoord + 1.0), leftIncCanvasWidth
                                        - dashLength, doubleToInt(yCoord + 1.0));
                    }
                }

                if (inchDbl != 0.0 && inchDbl != graphBounds.getYCoordValues()) {
                    inchLbl = String.format("%1.1f", inchDbl);
                    gc.drawString(inchLbl, leftIncCanvasWidth - dashLength - 3
                            - (inchLbl.length() * fontWidth),
                            doubleToInt(yCoord - aveFontHeight), true);
                }
            }

            yCoord += pixPerInc_YCoord;
            inchDbl -= 1.0;
            count++;
        }

        gc.drawLine(leftIncCanvasWidth, graphImageHeight + 1,
                leftIncCanvasWidth - dashLength, graphImageHeight - 5);

        inchLbl = String.format("%1.1f", 0.0);
        gc.drawString(inchLbl,
                leftIncCanvasWidth - dashLength - 3
                        - (inchLbl.length() * fontWidth), (graphImageHeight
                        - aveFontHeight - 5), true);

        gc.dispose();
    }

    /**
     * Create the increment image on the right side of the graph.
     */
    private void createRightIncImage() {
        if (rightIncImage != null) {
            rightIncImage.dispose();
        }

        rightIncImage = new Image(display, rightIncCanvasWidth,
                graphImageHeight + 1);

        GC gc = new GC(rightIncImage);

        gc.setFont(scaleFont);
        int fontHeight = gc.getFontMetrics().getHeight();
        int aveFontHeight = (int) Math.round(fontHeight / 2.0);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, rightIncCanvasWidth, graphImageHeight + 1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.setLineWidth(2);
        gc.drawLine(1, 0, 1, graphImageHeight + 1);

        double yCoord = 0.0;
        String inchLbl;
        double inchDbl = graphBounds.getYCoordValues();
        int skip = graphBounds.getSkipValue();
        int count = 0;

        while (yCoord < graphImageHeight) {

            // When counter == skip, allow graph to draw a label.
            if (count == skip || yCoord == 0.0) {
                // resets the counter
                if (yCoord > 1.0) {
                    count = 0;
                }

                if (inchDbl == graphBounds.getYCoordValues()) {
                    gc.drawLine(0, doubleToInt(yCoord + 1.0), dashLength,
                            doubleToInt(yCoord + 5.0));
                    inchLbl = String.format("%1.1f", inchDbl);
                    gc.drawString(inchLbl, 10, doubleToInt(yCoord + 5.0), true);

                } else {
                    if (inchDbl != 0.0) {
                        gc.drawLine(0, doubleToInt(yCoord + 1.0), dashLength,
                                doubleToInt(yCoord + 1.0));
                    }
                }

                if (inchDbl != 0.0 && inchDbl != graphBounds.getYCoordValues()) {
                    inchLbl = String.format("%1.1f", inchDbl);
                    gc.drawString(inchLbl, 10, doubleToInt(yCoord
                            - aveFontHeight), true);
                }
            }

            yCoord += pixPerInc_YCoord;
            inchDbl -= 1.0;
            count++;
        }

        gc.drawLine(0, graphImageHeight + 1, dashLength, graphImageHeight - 5);

        inchLbl = String.format("%1.1f", 0.0);
        gc.drawString(inchLbl, 10, (graphImageHeight - aveFontHeight - 5), true);

        gc.dispose();
    }

    /**
     * Create the graph image.
     */
    private void createGraphImage() {

        if (graphImage != null) {
            graphImage.dispose();
        }

        graphImage = new Image(display, graphImageWidth, graphImageHeight + 1);

        GC gc = new GC(graphImage);

        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, graphImageWidth, graphImageHeight + 1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.setAntialias(SWT.ON);

        /*
         * Draw the current hour white line.
         */
        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        gc.setLineWidth(10);

        if (timeDur == TimeDuration.ALL) {
            gc.drawLine(0, 0, 0, graphImageHeight);
        } else {
            if (graphQPF == true) {
                gc.drawLine(graphImageWidth - doubleToInt(pixPerInc_XCoord), 0,
                        graphImageWidth - doubleToInt(pixPerInc_XCoord),
                        graphImageHeight);
            } else {
                gc.drawLine(graphImageWidth, 0, graphImageWidth,
                        graphImageHeight);
            }
        }

        // Reset the line width.
        gc.setLineWidth(1);

        /*
         * Draw the underlays
         */
        if (underlay == Underlays.RATE && graphRate == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursRateUnderlay(gc);
            } else {
                drawHoursRateUnderlay(gc);
            }
        } else if (underlay == Underlays.QPE && graphQPE == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursQPEUnderlay(gc);
            } else {
                drawHoursQPEUnderlay(gc);
            }
        } else if (underlay == Underlays.RATIO && graphQPE == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursRatioUnderlay(gc);
            } else {
                drawHoursRatioUnderlay(gc);
            }
        } else if (underlay == Underlays.DIFF && graphQPE == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursDiffUnderlay(gc);
            } else {
                drawHoursDiffUnderlay(gc);
            }
        }

        /*
         * Draw horizontal black & grey grid lines.
         */
        double yCoord = 0.0;
        int hours = graphBounds.getYCoordValues();

        for (int i = 0; i < hours; i++) {
            gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
            gc.drawLine(0, doubleToInt(yCoord + (pixPerInc_YCoord / 2.0)),
                    graphImageWidth, doubleToInt(yCoord
                            + (pixPerInc_YCoord / 2.0)));

            gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            gc.drawLine(0, doubleToInt(yCoord), graphImageWidth,
                    doubleToInt(yCoord));

            yCoord += pixPerInc_YCoord;
        }

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawLine(0, graphImageHeight, graphImageWidth, graphImageHeight);

        /*
         * Draw vertical grey & black grid lines.
         */
        int xCoord = 0;
        int pix25percent = (int) Math.round(pixPerInc_XCoord * .25);
        int pix50percent = (int) Math.round(pixPerInc_XCoord * .50);
        int pix75percent = (int) Math.round(pixPerInc_XCoord * .75);

        while (xCoord <= graphImageWidth) {
            gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));

            if (timeDur != TimeDuration.HR_12 && timeDur != TimeDuration.HR_24) {
                gc.drawLine(xCoord + pix25percent, 0, xCoord + pix25percent,
                        graphImageHeight);
                gc.drawLine(xCoord + pix75percent, 0, xCoord + pix75percent,
                        graphImageHeight);
            }

            if (timeDur != TimeDuration.HR_24) {
                gc.drawLine(xCoord + pix50percent, 0, xCoord + pix50percent,
                        graphImageHeight);
            }

            gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            gc.drawLine(xCoord, 0, xCoord, graphImageHeight);

            xCoord += pixPerInc_XCoord;
        }

        /*
         * Draw the QPE graph line.
         */
        if (graphQPE == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursQPEGraphLine(gc);
            } else {
                drawHoursQPEGraphLine(gc);
            }
        }

        /*
         * Draw the Rate graph line.
         */
        if (graphRate == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursRateGraphLine(gc);
            } else {
                drawHoursRateGraphLine(gc);
            }
        }

        /*
         * Draw the Guidance graph line.
         */
        if (graphGuid == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursGuidGraphLine(gc);
            } else {
                drawHoursGuidGraphLine(gc);
            }
        }

        /*
         * Draw the QPF line.
         */
        if (graphQPF == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursQPFGraphLine(gc);
            } else {
                drawHoursQPFGraphLine(gc);
            }
        }

        /*
         * Draw the VGB line.
         */
        if (graphVGB == true) {
            if (timeDur == TimeDuration.ALL) {
                drawAllHoursVGBGraphLine(gc);
            } else {
                drawHoursVGBGraphLine(gc);
            }
        }

        gc.dispose();

        if (reverseXAxis == true) {
            ImageData data = graphImage.getImageData();
            graphImage.dispose();
            graphImage = new Image(display, reverseImage(data));
        }
    }

    /**
     * Create the bottom increment image for the All Hours selection.
     */
    private void createAllHoursBottomIncImage() {
        if (bottomIncImage != null) {
            bottomIncImage.dispose();
        }

        int width = graphCanvasWidth + labelCanvasWidth + leftIncCanvasWidth
                + rightIncCanvasWidth;
        bottomIncImage = new Image(display, width, bottomIncCanvasHeight);

        GC gc = new GC(bottomIncImage);

        gc.setFont(scaleFont);
        int fontWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, width, bottomIncCanvasHeight);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.setAntialias(SWT.ON);

        gc.setLineWidth(2);

        int xCoord = labelCanvasWidth + leftIncCanvasWidth;
        double hour = 0.0;
        double hourOffset = -1.0;

        if (reverseXAxis == true) {
            hour = -24.0;
            hourOffset = 1.0;
        }

        String displayTime;
        boolean transparent = false;

        while (xCoord <= graphCanvasWidth) {
            if (hour != 0.0) {
                gc.setBackground(bgColor);
                transparent = true;
            } else {
                gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
                transparent = false;
            }

            gc.drawLine(xCoord, 0, xCoord, dashLength);

            displayTime = String.format("%1.1f", hour);
            gc.drawString(
                    displayTime,
                    xCoord
                            - (int) Math.round(displayTime.length() * fontWidth
                                    / 2.0), dashLength, transparent);

            xCoord += pixPerInc_XCoord;
            hour += hourOffset;
        }

        xCoord = labelCanvasWidth + leftIncCanvasWidth + graphCanvasWidth;
        gc.drawLine(xCoord, 0, xCoord, 6);

        displayTime = String.format("%1.1f", hour);
        gc.drawString(
                displayTime,
                xCoord
                        - (int) Math.round(displayTime.length() * fontWidth
                                / 2.0), dashLength, true);

        gc.dispose();
    }

    /**
     * Create the bottom increment image for the 1, 3, 6, 12, or 24 selection.
     */
    private void createHoursBottomIncImage() {
        if (bottomIncImage != null) {
            bottomIncImage.dispose();
        }

        int width = graphCanvasWidth + labelCanvasWidth + leftIncCanvasWidth
                + rightIncCanvasWidth;
        bottomIncImage = new Image(display, width, bottomIncCanvasHeight);

        GC gc = new GC(bottomIncImage);

        gc.setFont(scaleFont);
        int fontWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.setBackground(bgColor);
        gc.fillRectangle(0, 0, width, bottomIncCanvasHeight);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.setAntialias(SWT.ON);

        gc.setLineWidth(2);

        int xCoordStart = labelCanvasWidth + leftIncCanvasWidth;
        int newXCoord = xCoordStart + graphImageWidth;
        boolean transparent = false;

        // Use the display date so the time on the x-axis matches the
        // time on the dialogs.
        cal.setTime(graphData.getDisplayDate());

        if (graphQPF == true) {
            cal.add(Calendar.HOUR, 1);
        }

        String displayTime;

        /*
         * Determine how many hours to subtract. 1, 3, 6, 12 hours will have
         * labels for each hour. 24 hour display will have labels every 2 hours.
         */
        int hoursToSubtract = -1;
        int pixelMultiplier = 1;

        if (timeDur == TimeDuration.HR_24) {
            hoursToSubtract = -2;
            pixelMultiplier = 2;
        }

        int counter = 0;

        /*
         * Loop and draw the remaining hour labels.
         */
        for (int i = 0; i <= graphBounds.getHours(); i += pixelMultiplier) {
            if (graphQPF == true && counter == 1) {
                transparent = false;
                gc.drawLine(newXCoord, 0, newXCoord, 6);
                gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
                displayTime = sdf.format(cal.getTime());
                gc.drawString(
                        displayTime,
                        newXCoord
                                - (int) Math.round(displayTime.length()
                                        * fontWidth / 2.0), dashLength,
                        transparent);

                gc.setBackground(bgColor);
                transparent = true;
            } else if (graphQPF == false && counter == 0) {
                transparent = false;
                gc.drawLine(newXCoord, 0, newXCoord, 6);
                gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
                displayTime = sdf.format(cal.getTime());
                gc.drawString(
                        displayTime,
                        newXCoord
                                - (int) Math.round(displayTime.length()
                                        * fontWidth / 2.0), dashLength,
                        transparent);

                gc.setBackground(bgColor);
                transparent = true;
            } else {
                gc.drawLine(newXCoord, 0, newXCoord, 6);
                displayTime = sdf.format(cal.getTime());
                gc.drawString(
                        displayTime,
                        newXCoord
                                - (int) Math.round(displayTime.length()
                                        * fontWidth / 2.0), dashLength,
                        transparent);
            }

            cal.add(Calendar.HOUR, hoursToSubtract);

            newXCoord -= (pixPerInc_XCoord * pixelMultiplier);
            ++counter;
        }

        gc.dispose();
    }

    /**
     * Draw the All Hours QPE graph line.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursQPEGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(qpeColor);
        gc.setForeground(qpeColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpeTimes());
        // Collections.reverse(timesArray);

        // Double largestTime = Double.MIN_VALUE;
        Double largestQPE = Double.MIN_VALUE;

        for (Double time : timesArray) {
            largestQPE = (largestQPE > graphData.getQpe(time)) ? largestQPE
                    : graphData.getQpe(time);
            // largestTime = (time > largestTime) ? time : largestTime;
        }

        // System.out.println("---------------------------");
        for (Double time : timesArray) {
            Double qpe = graphData.getQpe(time);

            if (qpe == null || qpe.isNaN()) {
                continue;
            }

            // qpe = largestQPE - qpe;

            // System.out.println("--- AH time   = " + time);
            // System.out.println("--- AH qpe    = " + qpe);

            x = (int) Math.round(time * pixPerInc_XCoord);
            // x = (int) Math.round((largestTime - time) * pixPerHourInc);
            y = (int) Math.round(graphImageHeight - (qpe * pixPerInc_YCoord));

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the other hours QPE graph line.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursQPEGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(qpeColor);
        gc.setForeground(qpeColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = new ArrayList<Double>(getQPEGraphTimes());

        Collections.reverse(timesArray);

        double offset = getQPEValueNearStartTimeOffset();

        // System.out.println("*************************************");

        // Double largestTime = Double.MIN_VALUE;
        //
        // for (Double time : timesArray) {
        // largestTime = (time > largestTime) ? time : largestTime;
        // }

        // System.out.println("---------------------------");

        for (Double time : timesArray) {
            Double qpe = graphData.getQpe(time);

            if (qpe == null || qpe.isNaN()) {
                continue;
            }

            // System.out.println("---");
            // System.out.println("--- H time   = " + time);
            // System.out.println("--- H qpe    = " + qpe);
            // System.out.println("--- H offset = " + offset);

            // qpe += offset;
            // qpe -= offset;

            qpe = offset - qpe;

            // System.out.println("--- H adjusted qpe = " + qpe);

            x = graphImageWidth
                    - ((int) Math.round((time + adjustedTimeOffset)
                            * pixPerInc_XCoord));

            // System.out.println("qpe xcoord = " + x);
            // System.out.println("graphImageWidth = " + graphImageWidth);

            // x = graphImageWidth - ((int) Math.round(((largestTime - time) +
            // adjustedTimeOffset) * pixPerHourInc));

            y = (int) Math.round(graphImageHeight - (qpe * pixPerInc_YCoord));

            // System.out.println("(qpe * pixPerInc_YCoord) = "
            // + (qpe * pixPerInc_YCoord));
            //
            // System.out.println("qpe ycoord = " + y);
            // System.out.println("graphImageHeight = " + graphImageHeight);
            // System.out.println("pixPerInc_YCoord = " + pixPerInc_YCoord);

            // Adjust the zero value line "up" one pixel so it is more visible.
            if (y == graphImageHeight) {
                --y;
            }

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the QPE underlay for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursQPEUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String qpeName = ThreshColNames.QPE.name();
        Color ulColor = null;
        Double qpe = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpeTimes());

        // Reverse the array so the underlay colors will change in the correct
        // order
        Collections.sort(timesArray);
        int timesArraySize = timesArray.size();

        Double largestTime = Double.MIN_VALUE;

        // Need to reverse the graph data
        Double largestQPE = Double.MIN_VALUE;
        for (Double time : timesArray) {
            largestQPE = (largestQPE > graphData.getQpe(time)) ? largestQPE
                    : graphData.getQpe(time);
            largestTime = (time > largestTime) ? time : largestTime;
        }

        for (int i = 0; i < timesArraySize; i++) {
            // Double qpeOrigValue = graphData.getQpe(timesArray.get(i));
            // qpe = largestQPE - qpeOrigValue;

            qpe = graphData.getQpe(timesArray.get(i));

            ulColor = ffmpCfg.getBasinThresholdColor(qpeName, qpe);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                // nextQPE = largestQPE - graphData.getQpe(nextTime);
                nextQPE = graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                x = (int) Math.round(timesArray.get(i) * pixPerInc_XCoord);
                // x = (int) Math.round((largestTime - timesArray.get(i)) *
                // pixPerHourInc);
                y = (int) Math.round(graphImageHeight
                        - (qpe * pixPerInc_YCoord));

                nextX = (int) Math.round(nextTime * pixPerInc_XCoord);
                // nextX = (int) Math.round((largestTime - nextTime) *
                // pixPerHourInc);
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the QPE underlay for 1, 3, 6, 12, 24 hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursQPEUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String qpeName = ThreshColNames.QPE.name();
        Color ulColor = null;
        Double qpe = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        ArrayList<Double> timesArray = new ArrayList<Double>(getQPEGraphTimes());
        double offset = getQPEValueNearStartTimeOffset();

        Collections.reverse(timesArray);
        int timesArraySize = timesArray.size();

        for (int i = 0; i < timesArraySize; i++) {
            qpe = graphData.getQpe(timesArray.get(i));

            if (qpe == null || qpe.isNaN()) {
                continue;
            }
            // Adjust to qpe value for zero point.
            // qpe += offset;
            qpe = offset - qpe;

            ulColor = ffmpCfg.getBasinThresholdColor(qpeName, qpe);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                // nextQPE = graphData.getQpe(nextTime) + offset;
                nextQPE = offset - graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                // x = graphImageWidth - ((int) Math.round((timesArray.get(i) +
                // adjustedTimeOffset) * pixPerHourInc));
                x = graphImageWidth
                        - ((int) Math
                                .round((timesArray.get(i) + adjustedTimeOffset)
                                        * pixPerInc_XCoord));
                y = (int) Math.round(graphImageHeight
                        - (qpe * pixPerInc_YCoord));

                // nextX = graphImageWidth - ((int) Math.round((nextTime +
                // adjustedTimeOffset) * pixPerHourInc));
                nextX = graphImageWidth
                        - ((int) Math.round((nextTime + adjustedTimeOffset)
                                * pixPerInc_XCoord));
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the rate graph line for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursRateGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(rateColor);
        gc.setForeground(rateColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = graphData.getRateTimes();

        for (Double time : timesArray) {
            Double rate = graphData.getRate(time);

            if (rate == null || rate.isNaN()) {
                continue;
            }

            x = (int) Math.round(time * pixPerInc_XCoord);
            y = (int) Math.round(graphImageHeight - (rate * pixPerInc_YCoord));

            // System.out.println("AH Rate time = " + rate);
            // System.out.println("AH Rate = " + rate);

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the rate graph line for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursRateGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(rateColor);
        gc.setForeground(rateColor);
        gc.setLineWidth(2);

        // ArrayList<Double> timesArray = new ArrayList<Double>(
        // graphData.getRateTimes());

        ArrayList<Double> timesArray = new ArrayList<Double>(
                getRateGraphTimes());

        for (Double time : timesArray) {
            Double rate = graphData.getRate(time);

            if (rate == null || rate.isNaN()) {
                continue;
            }

            x = graphImageWidth
                    - ((int) Math.round((time + adjustedTimeOffset)
                            * pixPerInc_XCoord));
            y = (int) Math.round(graphImageHeight - (rate * pixPerInc_YCoord));

            // System.out.println("H Rate time = " + rate);
            // System.out.println("H Rate = " + rate);

            // If the y coordinate is at the graph height then subtract a pixel
            // so you can see the point better.
            if (y == graphImageHeight) {
                --y;
            }

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the virtual gage basin graph line for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursVGBGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(vgbColor);
        gc.setForeground(vgbColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = graphData.getVirtualTimes();

        // System.out.println("All Hours VGB array size = " +
        // timesArray.size());

        for (Double time : timesArray) {
            Double vbgVal = graphData.getVirtual(time);

            // System.out.println("All Hours vbgVal = " + vbgVal);

            if (vbgVal == null || vbgVal.isNaN()) {
                continue;
            }

            x = (int) Math.round(time * pixPerInc_XCoord);
            y = (int) Math
                    .round(graphImageHeight - (vbgVal * pixPerInc_YCoord));

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the VGB graph line for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursVGBGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(vgbColor);
        gc.setForeground(vgbColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = graphData.getVirtualTimes();

        // System.out.println("Hours VGB array size = " + timesArray.size());

        for (Double time : timesArray) {
            Double vgbVal = graphData.getVirtual(time);

            // System.out.println("Hours VGB vgbVal = " + vgbVal);

            if (vgbVal == null || vgbVal.isNaN()) {
                continue;
            }

            x = graphImageWidth
                    - ((int) Math.round((time + adjustedTimeOffset)
                            * pixPerInc_XCoord));
            y = (int) Math
                    .round(graphImageHeight - (vgbVal * pixPerInc_YCoord));

            // If the y coordinate is at the graph height then subtract a pixel
            // so you can see the point better.
            if (y == graphImageHeight) {
                --y;
            }

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the all hours QPE graph line.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawAllHoursQPFGraphLine(GC gc) {
        // double qpfOffset = findAllHoursQPFOffset();

        double qpfOffset = graphData.getQpfValue();

        /*
         * If the offset is NaN or zero then return because 1.) we do not have
         * the data to draw the QPF graph line. 2.) QPF should not be less than
         * zero.
         */
        if (Double.isNaN(qpfOffset) || qpfOffset < 0.0) {
            return;
        }

        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(qpfColor);
        gc.setForeground(qpfColor);
        gc.setLineWidth(2);

        // Create a new array for sorting so we preserve the
        // original order.
        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpeTimes());
        Collections.sort(timesArray);

        Double largestQPE = Double.MIN_VALUE;

        for (Double time : timesArray) {
            largestQPE = (largestQPE > graphData.getQpe(time)) ? largestQPE
                    : graphData.getQpe(time);
        }

        Double qpe = 0.0;
        double qpeWithQpfOffset = 0;
        double qpfOffsetTime = 0.0;

        for (Double time : timesArray) {
            qpe = graphData.getQpe(time);

            if (qpe == null || qpe.isNaN()) {
                continue;
            }

            // qpe = largestQPE - qpe;

            qpfOffsetTime = time + 1.0;

            // Accumulate the QPF value
            qpeWithQpfOffset = qpe + qpfOffset;

            // System.out.println("*** qpe = " + qpe);

            x = (int) Math.round(qpfOffsetTime * pixPerInc_XCoord);
            y = (int) Math.round(graphImageHeight
                    - (qpeWithQpfOffset * pixPerInc_YCoord));

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the QPF graph line.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawHoursQPFGraphLine(GC gc) {
        /*
         * Draw the 1 hour QPF at the current QPE time and the 1 hour QPF into
         * the future
         */

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpfTimes());

        if (timesArray.size() == 0) {
            return;
        }

        Collections.sort(timesArray);

        ArrayList<Double> qpeTimesArray = new ArrayList<Double>(
                getQPEGraphTimes());
        Collections.sort(qpeTimesArray);

        Double currentTime = qpeTimesArray.get(0);

        gc.setBackground(qpfColor);
        gc.setForeground(qpfColor);
        gc.setLineWidth(2);

        ArrayList<Double> times = new ArrayList<Double>(getQPEGraphTimes());

        Collections.sort(times);

        // Drawing needs to start where QPE left off.
        double offset = getQPEValueNearStartTimeOffset();
        Double qpeCurrTimeVal = graphData.getQpe(times.get(0)) + offset;

        Double qpfOneHrTimeVal = (double) graphData.getQpfValue();

        qpfOneHrTimeVal += qpeCurrTimeVal;

        int xCur = graphImageWidth
                - ((int) Math.round((currentTime + adjustedTimeOffset)
                        * pixPerInc_XCoord));
        int x1Hr = graphImageWidth;

        int yCur = (int) Math.round(graphImageHeight
                - (qpeCurrTimeVal * pixPerInc_YCoord));
        int y1Hr = (int) Math.round(graphImageHeight
                - (qpfOneHrTimeVal * pixPerInc_YCoord));

        gc.fillOval(xCur - circleXYOffset, yCur - circleXYOffset, circleWthHgt,
                circleWthHgt);
        gc.fillOval(x1Hr - circleXYOffset, y1Hr - circleXYOffset, circleWthHgt,
                circleWthHgt);
        gc.drawLine(xCur, yCur, x1Hr, y1Hr);
    }

    /**
     * Draw the rate underlay for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursRateUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String rateName = ThreshColNames.RATE.name();
        Color ulColor = null;
        Double rate = null;
        Double nextRate = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getRateTimes());

        // Reverse the array so the underlay colors will change in the correct
        // order
        Collections.reverse(timesArray);

        int timesArraySize = timesArray.size();

        for (int i = 0; i < timesArraySize; i++) {

            rate = graphData.getRate(timesArray.get(i));
            ulColor = ffmpCfg.getBasinThresholdColor(rateName, rate);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextRate = null;
            } else {
                nextTime = timesArray.get(i + 1);
                nextRate = graphData.getRate(nextTime);
            }

            if (nextRate != null) {
                x = (int) Math.round(timesArray.get(i) * pixPerInc_XCoord);
                y = (int) Math.round(graphImageHeight
                        - (rate * pixPerInc_YCoord));

                nextX = (int) Math.round(nextTime * pixPerInc_XCoord);
                nextY = (int) Math.round(graphImageHeight
                        - (nextRate * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the rate underlay for All Hours.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursRateUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String rateName = ThreshColNames.RATE.name();
        Color ulColor = null;
        Double rate = null;
        Double nextRate = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        // ArrayList<Double> timesArray = new ArrayList<Double>(
        // graphData.getRateTimes());

        ArrayList<Double> timesArray = new ArrayList<Double>(
                getRateGraphTimes());

        Collections.sort(timesArray);
        int timesArraySize = timesArray.size();

        for (int i = 0; i < timesArraySize; i++) {
            rate = graphData.getRate(timesArray.get(i));
            ulColor = ffmpCfg.getBasinThresholdColor(rateName, rate);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextRate = null;
            } else {
                nextTime = timesArray.get(i + 1);
                nextRate = graphData.getRate(nextTime);
            }

            if (nextRate != null) {
                x = graphImageWidth
                        - ((int) Math
                                .round((timesArray.get(i) + adjustedTimeOffset)
                                        * pixPerInc_XCoord));
                y = (int) Math.round(graphImageHeight
                        - (rate * pixPerInc_YCoord));

                nextX = graphImageWidth
                        - ((int) Math.round((nextTime + adjustedTimeOffset)
                                * pixPerInc_XCoord));
                nextY = (int) Math.round(graphImageHeight
                        - (nextRate * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the ratio underlay that will be drawn in place of the QPE underlay.
     * 
     * @param gc
     */
    private void drawAllHoursRatioUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        Color ulColor = null;
        Double qpe = null;
        Double ratio = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpeTimes());

        // Reverse the array so the underlay colors will change in the correct
        // order
        Collections.reverse(timesArray);

        int timesArraySize = timesArray.size();

        Double largestQPE = Double.MIN_VALUE;
        for (Double time : timesArray) {
            largestQPE = (largestQPE > graphData.getQpe(time)) ? largestQPE
                    : graphData.getQpe(time);
        }

        for (int i = 0; i < timesArraySize; i++) {
            qpe = graphData.getQpe(timesArray.get(i));

            ratio = graphData.getRatio(timesArray.get(i));

            if (ratio == null) {
                ratio = Double.NaN;
            }

            ulColor = ffmpCfg.getBasinThresholdColor(
                    ThreshColNames.RATIO.name(), ratio);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                nextQPE = graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                x = (int) Math.round(timesArray.get(i) * pixPerInc_XCoord);
                y = (int) Math.round(graphImageHeight
                        - (qpe * pixPerInc_YCoord));

                nextX = (int) Math.round(nextTime * pixPerInc_XCoord);
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the difference underlay for the specified number of hours.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawHoursRatioUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String ratioName = ThreshColNames.RATIO.name();
        Color ulColor = null;
        Double qpe = null;
        Double ratio = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        Double guid = this.getGuidForHour(graphBounds.getHours());

        ArrayList<Double> timesArray = new ArrayList<Double>(getQPEGraphTimes());
        double offset = getQPEValueNearStartTimeOffset();

        Collections.reverse(timesArray);
        int timesArraySize = timesArray.size();

        for (int i = 0; i < timesArraySize; i++) {
            qpe = graphData.getQpe(timesArray.get(i));

            if (qpe == null || qpe.isNaN()) {
                continue;
            }

            // Adjust to qpe value for zero point.
            // qpe += offset;
            double qpePlusOffset = offset - qpe;

            if (Double.isNaN(guid) == true) {
                ratio = Double.NaN;
            } else {
                // ratio = qpe / guid * 100.0;
                ratio = qpePlusOffset / guid * 100.0;
            }

            // TODO : remove debug code
            // System.out.println("--------");
            // double oldRatio = graphData.getRatio(timesArray.get(i));
            // System.out.println("time = " + timesArray.get(i));
            // System.out.println("qpe  = " + qpe);
            // System.out.println("giud = " + guid);
            // System.out.println("ratio    = " + ratio);
            // System.out.println("oldRatio = " + oldRatio);

            ulColor = ffmpCfg.getBasinThresholdColor(ratioName, ratio);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                nextQPE = offset - graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                x = graphImageWidth
                        - (int) Math
                                .round((timesArray.get(i) + adjustedTimeOffset)
                                        * pixPerInc_XCoord);
                y = (int) Math.round(graphImageHeight
                        - (qpePlusOffset * pixPerInc_YCoord));

                // nextX = graphImageWidth - (int) Math.round((nextTime +
                // adjustedTimeOffset) * pixPerHourInc);
                nextX = graphImageWidth
                        - (int) Math.round((nextTime + adjustedTimeOffset)
                                * pixPerInc_XCoord);
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the difference underlay for all hours.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawAllHoursDiffUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String diffName = ThreshColNames.DIFF.name();
        Color ulColor = null;
        Double qpe = null;
        Double diff = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getQpeTimes());

        // Reverse the array so the underlay colors will change in the correct
        // order
        Collections.reverse(timesArray);

        int timesArraySize = timesArray.size();

        /*
         * Loop and find the largest QPE. This is used to draw the graph line
         * correctly as it needs to be flipped to draw the accumulating to the
         * right on the graph.
         */
        Double largestQPE = Double.MIN_VALUE;
        for (Double time : timesArray) {
            largestQPE = (largestQPE > graphData.getQpe(time)) ? largestQPE
                    : graphData.getQpe(time);
        }

        for (int i = 0; i < timesArraySize; i++) {
            qpe = graphData.getQpe(timesArray.get(i));

            diff = graphData.getDiff(timesArray.get(i));

            if (diff == null) {
                diff = Double.NaN;
            }

            ulColor = ffmpCfg.getBasinThresholdColor(diffName, diff);
            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                nextQPE = graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                x = (int) Math.round(timesArray.get(i) * pixPerInc_XCoord);
                y = (int) Math.round(graphImageHeight
                        - (qpe * pixPerInc_YCoord));

                nextX = (int) Math.round(nextTime * pixPerInc_XCoord);
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the difference underlay for a specified number of hours hour.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawHoursDiffUnderlay(GC gc) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        gc.setLineWidth(1);
        String diffName = ThreshColNames.DIFF.name();
        Color ulColor = null;
        Double qpe = null;
        Double diff = null;
        Double nextQPE = null;
        Double nextTime = null;

        int nextX = -9999;
        int nextY = -9999;
        int x = 0;
        int y = 0;

        Double guid = this.getGuidForHour(graphBounds.getHours());

        ArrayList<Double> timesArray = new ArrayList<Double>(getQPEGraphTimes());
        double offset = getQPEValueNearStartTimeOffset();

        Collections.reverse(timesArray);
        int timesArraySize = timesArray.size();

        for (int i = 0; i < timesArraySize; i++) {
            qpe = graphData.getQpe(timesArray.get(i));

            if (qpe == null || qpe.isNaN()) {
                continue;
            }

            // Adjust to qpe value for zero point.
            double qpeWithOffset = offset - qpe;

            if (Double.isNaN(guid) == true) {
                diff = Double.NaN;
            } else {
                // diff = qpe - guid;
                diff = qpeWithOffset - guid;
            }

            // TODO - Remove Debug code.
            // System.out.println("--------");
            // double oldDiff = graphData.getDiff(timesArray.get(i));
            // System.out.println("time = " + timesArray.get(i));
            // System.out.println("offset         = " + offset);
            // System.out.println("qpe            = " + qpe);
            // System.out.println("qpePlusOffset  = " + qpeWithOffset);
            // System.out.println("giud = " + guid);
            // System.out.println("diff    = " + diff);
            // System.out.println("oldDiff = " + oldDiff);

            ulColor = ffmpCfg.getBasinThresholdColor(diffName, diff);

            gc.setBackground(ulColor);

            if ((i + 1) == timesArraySize) {
                nextQPE = null;
            } else {
                nextTime = timesArray.get(i + 1);
                // nextQPE = graphData.getQpe(nextTime);
                // nextQPE = graphData.getQpe(nextTime) + offset;
                nextQPE = offset - graphData.getQpe(nextTime);
            }

            if (nextQPE != null) {
                x = graphImageWidth
                        - (int) Math
                                .round((timesArray.get(i) + adjustedTimeOffset)
                                        * pixPerInc_XCoord);
                // System.out.println("x = " + x);
                y = (int) Math.round(graphImageHeight
                        - (qpeWithOffset * pixPerInc_YCoord));

                nextX = graphImageWidth
                        - (int) Math.round((nextTime + adjustedTimeOffset)
                                * pixPerInc_XCoord);
                nextY = (int) Math.round(graphImageHeight
                        - (nextQPE * pixPerInc_YCoord));

                int[] pointArray = new int[] { x, graphImageHeight, x, y,
                        nextX, nextY, nextX, graphImageHeight, x,
                        graphImageHeight };
                gc.fillPolygon(pointArray);
            }
        }
    }

    /**
     * Draw the guidance graph line.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawAllHoursGuidGraphLine(GC gc) {
        int prevX = Integer.MIN_VALUE;
        int prevY = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;

        gc.setBackground(guidColor);
        gc.setForeground(guidColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = new ArrayList<Double>(
                graphData.getGuidanceTimes());

        Collections.sort(timesArray);

        for (Double time : timesArray) {
            Double guid = graphData.getGuid(time);

            if (guid == null || guid.isNaN() || guid < 0.0) {
                prevX = Integer.MIN_VALUE;
                prevY = Integer.MIN_VALUE;
                continue;
            }

            x = (int) Math.round(time * pixPerInc_XCoord);
            y = (int) Math.round(graphImageHeight - (guid * pixPerInc_YCoord));

            gc.fillOval(x - circleXYOffset, y - circleXYOffset, circleWthHgt,
                    circleWthHgt);

            if (prevX != Integer.MIN_VALUE && prevY != Integer.MIN_VALUE) {
                gc.drawLine(x, y, prevX, prevY);
            }

            prevX = x;
            prevY = y;
        }
    }

    /**
     * Draw the guidance graph line.
     * 
     * @param gc
     *            Graphics Context.
     */
    private void drawHoursGuidGraphLine(GC gc) {

        int y = 0;

        gc.setBackground(guidColor);
        gc.setForeground(guidColor);
        gc.setLineWidth(2);

        ArrayList<Double> timesArray = graphData.getGuidanceTimes();

        double maxGuid = Double.MIN_VALUE;

        if (timesArray.contains((double) graphBounds.getHours()) == true) {
            maxGuid = graphData.getGuid((double) graphBounds.getHours());

            if (Double.isNaN(maxGuid) == true || maxGuid < 0.0) {
                return;
            }
        } else {
            return;
        }

        y = (int) Math.round(graphImageHeight - (maxGuid * pixPerInc_YCoord));

        gc.fillOval(0 - circleXYOffset, y - circleXYOffset, circleWthHgt,
                circleWthHgt);
        gc.fillOval(graphImageWidth - circleXYOffset, y - circleXYOffset,
                circleWthHgt, circleWthHgt);
        gc.drawLine(0, y, graphImageWidth, y);
    }

    /**
     * Get the guidance for the specified hour.
     * 
     * @param hour
     * @return
     */
    private double getGuidForHour(double hour) {

        ArrayList<Double> timesArray = graphData.getGuidanceTimes();
        if (timesArray.contains(hour) == true) {
            Double guid = graphData.getGuid(hour);
            return guid;
        }

        return Double.NaN;
    }

    /**
     * Get the offset that will be used to plot a "zero hour" for QPE. Find the
     * smallest value and then subtract the value from zero to get the offset.
     * 
     * @return Offset.
     */
    private double getQPEValueNearStartTimeOffset() {

        double offset = 0.0;
        ArrayList<Double> timeArray = new ArrayList<Double>(getQPEGraphTimes());

        if (timeArray.size() == 0) {
            return offset;
        }

        Collections.reverse(timeArray);

        // Find the minimum QPE value.
        Double time = Double.NaN;
        for (int i = 0; i < timeArray.size(); i++) {
            time = timeArray.get(i);
            offset = Math.max(offset, graphData.getQpe(time));
        }

        return offset;
    }

    /**
     * Get QPE graph times for points that will appear in the graph window.
     * 
     * @return Array of QPE graph times.
     */
    private ArrayList<Double> getQPEGraphTimes() {
        int hrs = graphBounds.getHours();

        /*
         * If qpf is selected then adjust the hours. For example: hour = 24 then
         * hour should be 23
         */
        if (graphQPF == true) {
            hrs -= 1;
        }

        ArrayList<Double> qpeGraphTimes = new ArrayList<Double>();

        ArrayList<Double> tmpQpeTimes = new ArrayList<Double>(
                graphData.getQpeTimes());
        Collections.sort(tmpQpeTimes);

        for (Double time : tmpQpeTimes) {
            if (time <= hrs) {
                qpeGraphTimes.add(time);
            }
        }

        return qpeGraphTimes;
    }

    /**
     * Get Rate graph times for points that will appear in the graph window.
     * 
     * @return Array of QPE graph times.
     */
    private ArrayList<Double> getRateGraphTimes() {
        int hrs = graphBounds.getHours();

        /*
         * If qpf is selected then adjust the hours. For example: hour = 24 then
         * hour should be 23
         */
        if (graphQPF == true) {
            hrs -= 1;
        }

        ArrayList<Double> rateGraphTimes = new ArrayList<Double>();

        ArrayList<Double> tmpRateTimes = new ArrayList<Double>(
                graphData.getRateTimes());
        Collections.sort(tmpRateTimes);

        for (Double time : tmpRateTimes) {
            if (time <= hrs) {
                rateGraphTimes.add(time);
            }
        }

        return rateGraphTimes;
    }

    /**
     * Reverse the image (for reverse X axis).
     * 
     * @param srcData
     *            Image data.
     * @return New image data used to create a new image.
     */
    private ImageData reverseImage(ImageData srcData) {
        int bytesPerPixel = srcData.bytesPerLine / srcData.width;
        int destBytesPerLine = srcData.width * bytesPerPixel;
        byte[] newData = new byte[srcData.data.length];

        for (int srcY = 0; srcY < srcData.height; srcY++) {
            for (int srcX = 0; srcX < srcData.width; srcX++) {
                int destX = 0, destY = 0, destIndex = 0, srcIndex = 0;

                destX = srcData.width - srcX - 1;
                destY = srcY;

                destIndex = (destY * destBytesPerLine)
                        + (destX * bytesPerPixel);
                srcIndex = (srcY * srcData.bytesPerLine)
                        + (srcX * bytesPerPixel);
                System.arraycopy(srcData.data, srcIndex, newData, destIndex,
                        bytesPerPixel);
            }
        }

        /*
         * destBytesPerLine is used as scanlinePad to ensure that no padding is
         * required
         */
        return new ImageData(srcData.width, srcData.height, srcData.depth,
                srcData.palette, destBytesPerLine, newData);
    }

    /**
     * Recalculate the pixel increments. The increments are for the number of
     * pixels in one hour (X time axis) and the number of pixels for inch per
     * hour (Y axis).
     */
    private void recalcPixelsIncrements() {
        if (timeDur == TimeDuration.ALL) {
            graphImageWidth = graphCanvasWidth;
            graphImageHeight = graphCanvasHeight;
        } else {
            graphImageWidth = graphViewWidth;
            graphImageHeight = graphViewHeight;
        }

        pixPerInc_XCoord = graphImageWidth / (double) graphBounds.getHours();
        pixPerInc_YCoord = graphImageHeight
                / (double) graphBounds.getYCoordValues();
    }

    /**
     * Redraw the graph canvas.
     */
    public void redrawGraphCanvas() {
        // System.out.println("in redrawGraphCanvas()");
        createGraphImage();
        graphCanvas.redraw();
    }

    /**
     * Redraw the bottom increment canvas.
     */
    private void redrawBottomIncCanvas() {
        if (timeDur == TimeDuration.ALL) {
            createAllHoursBottomIncImage();
            mainXCoord = 0 - hScale.getSelection();
        } else {
            mainXCoord = 0;
            hScale.setSelection(0);
            createHoursBottomIncImage();
        }
        // createAllHoursBottomIncImage();
        bottomIncCanvas.redraw();
    }

    /**
     * Regenerate the left & right value increment labels images, the graph
     * image, and the bottom time label image.
     */
    private void regenerateImages() {
        createLeftIncImage();
        createRightIncImage();

        // System.out.println("in regenerateImages()");
        createGraphImage();

        if (timeDur == TimeDuration.ALL) {
            createAllHoursBottomIncImage();
        } else {
            createHoursBottomIncImage();
        }
    }

    /**
     * Redraw all of the canvases.
     */
    private void redrawAllCanvases() {
        leftIncCanvas.redraw();
        rightIncCanvas.redraw();
        bottomIncCanvas.redraw();
        graphCanvas.redraw();
    }

    /**
     * Convert a double to an integer using Math.round()
     * 
     * @param dblVal
     *            Double value.
     * @return An integer
     */
    private int doubleToInt(double dblVal) {
        int i = (int) Math.round(dblVal);
        return i;
    }

    /**
     * Reverse the X axis.
     */
    public void reverseXAxis() {
        reverseXAxis = !reverseXAxis;

        if (reverseXAxis == true) {
            hScale.setSelection(hScale.getMaximum());
        } else {
            hScale.setSelection(hScale.getMinimum());
        }
        mainXCoord = 0 - hScale.getSelection();

        redrawGraphCanvas();
        redrawBottomIncCanvas();
    }

    /**
     * Reset the Reverse X Axis flag.
     */
    public void resetReverseXAxis() {
        reverseXAxis = false;
    }

    /**
     * Set the time duration.
     * 
     * @param timeDur
     *            Time duration.
     */
    public void setTimeDurHours(TimeDuration timeDur) {
        this.timeDur = timeDur;

        if (this.timeDur == TimeDuration.ALL) {
            hScale.setEnabled(true);
            vScale.setEnabled(true);
        } else {
            hScale.setSelection(hScale.getMinimum());
            mainXCoord = 0 - hScale.getSelection();
            hScale.setEnabled(false);
            vScale.setEnabled(false);
        }

        refreshGraph();
    }

    /**
     * Set the graph data.
     * 
     * @param graphData
     *            The graph data.
     */
    public void setGraphData(FFMPGraphData graphData) {

        this.graphData = graphData;
        refreshGraph();
    }

    private void refreshGraph() {
        determineGraphBounds();
        recalcPixelsIncrements();
        regenerateImages();
        redrawAllCanvases();
    }

    /**
     * Set the graphing parameters.
     * 
     * @param timeDur
     *            Time duration.
     * @param underlay
     *            Underlay.
     * @param plots
     *            Array on selected plot items.
     */
    public void setGraphingParameters(TimeDuration timeDur, Underlays underlay,
            ArrayList<PlotItems> plots) {

        // System.out.println("in setGraphingParameters");
        this.underlay = underlay;
        this.timeDur = timeDur;

        graphRate = false;
        graphQPE = false;
        graphQPF = false;
        graphGuid = false;
        graphVGB = false;
        adjustedTimeOffset = 0.0;

        for (PlotItems pi : plots) {
            if (pi == PlotItems.RATE) {
                graphRate = true;
            } else if (pi == PlotItems.QPE) {
                graphQPE = true;
            } else if (pi == PlotItems.QPF) {
                graphQPF = true;
                adjustedTimeOffset = 1.0;
            } else if (pi == PlotItems.GUID) {
                graphGuid = true;
            } else if (pi == PlotItems.VGB) {
                graphVGB = true;
            }
        }

        redrawGraphCanvas();
        redrawBottomIncCanvas();
    }

    /**
     * Set the X and Y bounds of the graph.
     */
    private void determineGraphBounds() {

        graphBounds = new BasinTrendGraphBounds();
        // maximum data value
        double max = graphData.getMaximumValue();
        int yMaxHeight = (int) Math.round(max + 1);
        graphBounds.setYCoordValues(yMaxHeight);
        graphBounds.setHours(this.timeDur.getHours());
        graphBounds.setTimeDurName(this.timeDur.getTimeDurName());

    }
}