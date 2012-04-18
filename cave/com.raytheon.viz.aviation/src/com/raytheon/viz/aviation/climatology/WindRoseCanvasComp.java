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
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * WindRoseCanvasComp class displays the Wind Rose diagram.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 18 JUN 2008  1119       lvenable    Updated to draw the Wind Rose diagram.
 * 19 AUG 2008  1454       lvenable    Fix saving wind rose image.
 * 09 MAR 2012  14530      zhao        Revised wind rose plot to match AWIPS-1
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WindRoseCanvasComp extends Composite {
    /**
     * Parent composite;
     */
    private Composite parent;

    /**
     * Font used for the canvas.
     */
    private Font canvasFont;

    /**
     * Large canvas font.
     */
    private Font canvasFontLrg;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 700;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 700;

    /**
     * Center X coordinate of the canvas.
     */
    private final int centerX = CANVAS_WIDTH / 2;

    /**
     * Center of the Wind Rose circle on the canvas.
     */
    private final int circleCenterY = CANVAS_HEIGHT / 2 - 30;

    /**
     * Maximum circle radius of the Wind Rose.
     */
    private int maxCircleRadius = 275;

    /**
     * Legend color rectangle width.
     */
    private int rectWidth = 30;

    /**
     * Legend color rectangle height.
     */
    private int rectHeight = 15;

    /**
     * Legend color rectangle X coordinate.
     */
    private int rectStartX = 10;

    /**
     * Legend color rectangle starting Y coordinate.
     */
    private int rectStartY = 590;

    /**
     * Legend color rectangle for the above wind direction (v3+).
     */
    private Rectangle aboveRect;

    /**
     * Legend color rectangle for wind variable 3.
     */
    private Rectangle windVar3Rect;

    /**
     * Legend color rectangle for wind variable 2.
     */
    private Rectangle windVar2Rect;

    /**
     * Legend color rectangle for wind variable 1.
     */
    private Rectangle windVar1Rect;

    /**
     * Legend color rectangle for variable winds.
     */
    private Rectangle variableRect;

    /**
     * Legend color rectangle for calm winds.
     */
    private Rectangle calmRect;

    /**
     * Wind Rose configuration data.
     */
    private WindRoseConfigData windRoseConfigData;

    /**
     * Wind Rose data manager class that contains the data to be drawn.
     */
    private WindRoseDataMgr windRoseDataMgr;

    /**
     * Wind Rose header - contains site, month, hour information.
     */
    private String windRoseHeader = "";

    /**
     * number of wind directions on the wind rose diagram
     */
    private int numWindDirections = 0;
    
    /**
     * radius of calm wind circle on the wind rose diagram
     */
    private double calmRingPercent = 0.0;
    
    /**
     * radius of variable wind on the wind rose diagram
     */
    private double variableRingPercent = 0.0;
    
    /**
     * Maximum percent of the outer Wind Rose ring.
     */
    private double maxRingPercent = 0.0;

    /**
     * Number of pixels to draw for each Wind Rose unit (wind direction/knot).
     */
    private double pixPerUnit = 0.0;

    /**
     * A copy of the image Wind Rose image to save to file.
     */
    private Image image;

    /**
     * Drawing canvas.
     */
    private Canvas windRoseCanvas;

    private WindRosePlotDlg windRoseDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param windRoseConfigData
     *            Wind Rose configuration data used for drawing the Wind Rose
     *            data.
     */
    public WindRoseCanvasComp(Composite parent,
            WindRoseConfigData windRoseConfigData) {
        super(parent, SWT.BORDER);

        this.parent = parent;

        this.windRoseConfigData = windRoseConfigData;

        image = new Image(parent.getDisplay(), CANVAS_WIDTH, CANVAS_HEIGHT);

        init();
        this.pack();
    }

    /**
     * Initialize method.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));

        canvasFont = new Font(parent.getDisplay(), "Monospace", 10, SWT.NORMAL);
        canvasFontLrg = new Font(parent.getDisplay(), "Monospace", 14,
                SWT.NORMAL);

        setupCanvas();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                canvasFont.dispose();
                canvasFontLrg.dispose();
                image.dispose();
            }
        });

        // --------------------------------------------------
        // Setup the color rectangle areas for the legend
        // --------------------------------------------------
        aboveRect = new Rectangle(rectStartX, rectStartY, rectWidth, rectHeight);
        windVar3Rect = new Rectangle(rectStartX, rectStartY + (rectHeight),
                rectWidth, rectHeight);
        windVar2Rect = new Rectangle(rectStartX, rectStartY + (rectHeight * 2),
                rectWidth, rectHeight);
        windVar1Rect = new Rectangle(rectStartX, rectStartY + (rectHeight * 3),
                rectWidth, rectHeight);
        variableRect = new Rectangle(rectStartX, rectStartY + (rectHeight * 4),
                rectWidth, rectHeight);
        calmRect = new Rectangle(rectStartX, rectStartY + (rectHeight * 5),
                rectWidth, rectHeight);
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        windRoseCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        windRoseCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        windRoseCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawWindRoseCanvas(e.gc);
            }
        });
    }

    /**
     * Draw the wind rose diagram.
     * 
     * @param e
     *            Paint Event.
     */
    private void drawWindRoseCanvas(GC gc) {
        // Turn on anti-aliasing
        gc.setAntialias(SWT.ON);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // If the Wind Rose data manager is null then return.
        if (windRoseDataMgr == null) {
            return;
        }

        // ----------------------------------------
        // Draw the Wind Rose information
        // ----------------------------------------
        drawWindRose(gc);

        // ----------------------------------------
        // Draw the Wind Rose circles
        // ----------------------------------------
        
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        
        String format = "%d%%";
        String percentLbl = "";
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();
       
        double ringPercent = 0.02; // start with 2%
        int radiusPix = 0; 
        
        for (;;) {
        	
        	radiusPix = (int) Math.round( pixPerUnit * Math.sqrt( ringPercent + variableRingPercent*variableRingPercent ) );
        	if ( radiusPix >= maxCircleRadius ) {
        		break;
        	}
        	gc.drawOval(centerX - radiusPix, circleCenterY - radiusPix, radiusPix * 2, radiusPix * 2);
        	
        	percentLbl = String.format(format, Math.round(ringPercent*100));
            gc.drawText(percentLbl, centerX - (percentLbl.length() * fontAveWidth / 2), circleCenterY + radiusPix + 3, true);
            
            if ( ringPercent < 0.09 ) {
            	ringPercent += 0.02; 
            } else if ( ringPercent < 0.29 ) {
            	ringPercent += 0.05;
            } else {
            	ringPercent += 0.1;
            }
        }

        // -------------------------------
        // Draw the Wind Rose header
        // -------------------------------
        drawHeader(gc);

        // -------------------------------
        // Draw the Wind Rose legend
        // -------------------------------
        drawLegend(gc);

    }

    /**
     * Draw the header at the top of the canvas.
     * 
     * @param gc
     *            The Graphics Context.
     */
    private void drawHeader(GC gc) {
        gc.setFont(canvasFontLrg);
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.drawText(windRoseHeader, centerX
                - (windRoseHeader.length() * fontAveWidth / 2), 10, true);
    }

    /**
     * Draw the Wind Rose legend (lower left part of the screen).
     * 
     * @param gc
     *            The Graphics Context.
     */
    private void drawLegend(GC gc) {
        // Set the font for the legend and set the foreground color to black for
        // the text.
        gc.setFont(canvasFont);
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // Get the percents for each knot range (0 to v1, v1 to v2, v2 to v3,
        // v3+, variable, and calm).
        double[] knotPercents = windRoseDataMgr.getKnotPercents();

        // --------------------------------------------
        // Draw Above information
        // --------------------------------------------
        Color tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getAboveRgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(aboveRect);
        gc.drawRectangle(aboveRect);

        String tmpStr = String.format("%4S+ kt: %4.1f%%",
                windRoseConfigData.getVar3Max(), knotPercents[5]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, aboveRect.y, true);

        // --------------------------------------------
        // Draw Wind speed var 3 information
        // --------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVar3Rgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(windVar3Rect);
        gc.drawRectangle(windVar3Rect);

        String tmpKt = String.format("%S-%S", windRoseConfigData.getVar2Max(),
                windRoseConfigData.getVar3Max());
        tmpStr = String.format("%5S kt: %4.1f%%", tmpKt, knotPercents[4]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, windVar3Rect.y, true);

        // --------------------------------------------
        // Draw Wind speed var 2 information
        // --------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVar2Rgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(windVar2Rect);
        gc.drawRectangle(windVar2Rect);

        tmpKt = String.format("%S-%S", windRoseConfigData.getVar1Max(),
                windRoseConfigData.getVar2Max());
        tmpStr = String.format("%5S kt: %4.1f%%", tmpKt, knotPercents[3]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, windVar2Rect.y, true);

        // --------------------------------------------
        // Draw Wind speed var 1 information
        // --------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVar1Rgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(windVar1Rect);
        gc.drawRectangle(windVar1Rect);

        tmpKt = String.format("%S-%S", "0", windRoseConfigData.getVar1Max());
        tmpStr = String.format("%5S kt: %4.1f%%", tmpKt, knotPercents[2]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, windVar1Rect.y, true);

        // --------------------------------------------
        // Draw variable information
        // --------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVariableRgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(variableRect);
        gc.drawRectangle(variableRect);

        tmpStr = String.format("variable: %4.1f%%", knotPercents[1]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, variableRect.y, true);

        // --------------------------------------------
        // Draw calm information
        // --------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getCalmRgb());

        gc.setBackground(tmpColor);
        gc.fillRectangle(calmRect);
        gc.drawRectangle(calmRect);

        tmpStr = String.format("    calm: %4.1f%%", knotPercents[0]);
        gc.drawString(tmpStr, rectStartX + rectWidth + 5, calmRect.y, true);

        // --------------------------------------------
        // Draw years & hours information
        // --------------------------------------------
        tmpStr = String.format("Years: %9S", windRoseDataMgr.getYears());
        gc.drawString(tmpStr, 425, variableRect.y, true);

        tmpStr = String.format("Total Hours: %S", windRoseDataMgr.getHours());
        gc.drawString(tmpStr, 425, calmRect.y, true);

        tmpColor.dispose();
    }

    /**
     * Draw the Wind Rose diagram.
     * 
     * @param gc
     *            The Graphical Context.
     */
    private void drawWindRose(GC gc) {
        // Setup the color object.
        Color tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVar2Rgb());

        // Calculate the highest percent for the outer display ring and
        // the percent increment for the internal display rings.
        calcPercentData();

        // Get the Wind Rose data that has been calculated over all hours.
        double[][] dataArray = windRoseDataMgr.getAllWindDirDataArray();

        // Get the number of Wind Directions that will be drawn of
        // the Wind Rose diagram.
        int directions = windRoseDataMgr.getNumOfWindDirections();

        int outerRadius = 0;

        // Loop through each wind direction and draw the wind information.
        for (int i = 0; i < dataArray.length; ++i) {
            // Get an array of calculate pixel values that will be used to
            // draw the current wind direction.
            int[] windDirPixels = getWindDirPixelValues(dataArray[i]);
            
            // Loop through the array of pixels for each knot range for
            // the current wind direction. We are looping from the outer
            // most range to the inner most range. This allows us to draw
            // the filledArcs on top of each other.
            for (int j = windDirPixels.length - 1; j >= 0; --j) {
                // If increment j is the last element in the array then we need
                // to save off the radius (largest set of pixels) so we can draw
                // the edges lines of the 'pie slice' -- which provides an
                // outline
                // look.
                if (j == windDirPixels.length - 1) {
                    outerRadius = windDirPixels[j];
                }

                // Check increment j to determine which color is used
                // to draw the knot range.
                switch (j) {
                case 0:
                    tmpColor.dispose();
                    tmpColor = new Color(parent.getDisplay(),
                            windRoseConfigData.getVar1Rgb());
                    break;
                case 1:
                    tmpColor.dispose();
                    tmpColor = new Color(parent.getDisplay(),
                            windRoseConfigData.getVar2Rgb());
                    break;
                case 2:
                    tmpColor.dispose();
                    tmpColor = new Color(parent.getDisplay(),
                            windRoseConfigData.getVar3Rgb());
                    break;
                case 3:
                    tmpColor.dispose();
                    tmpColor = new Color(parent.getDisplay(),
                            windRoseConfigData.getAboveRgb());
                    break;
                }

                // Set the background color to the selected knot range color.
                gc.setBackground(tmpColor);

                // Determine how many wind directions will be drawn on the
                // Wind Rose diagram.
                /**
                 * DR14530:
                 * Angle of wind direction is measured clockwise with North (12 o'clock) = 0 degree, 
                 * whereas, angle in GC.fillAra() and GC.drawArc() is measured counter-clockwise, 
                 * with East (3 o'clock) = 0 degree
                 * [zhao, 3/2/2012]
                 */
                if (directions == 8) {
                    // Draw a filled arc for the current knot range.
                    gc.fillArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - (i * 45) - 21, 42);

                    // Set the foreground color to black.
                    gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));

                    // Draw a black arc line on the filled arc.
                    gc.drawArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - (i * 45) - 21, 42);
                    
                } else if (directions == 16) {
                    int degree = (int) Math.round(i * 22.5);

                    // Draw a filled arc for the current knot range.
                    gc.fillArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - degree - 10, 20);

                    // Set the foreground color to black.
                    gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));

                    // Draw a black arc line on the filled arc.
                    gc.drawArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - degree - 10, 20);
                } else {
                    // Draw a filled arc for the current knot range.
                    gc.fillArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - (i * 10) - 4, 8);

                    // Set the foreground color to black.
                    gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));

                    // Draw a black arc line on the filled arc.
                    gc.drawArc(centerX - windDirPixels[j], circleCenterY
                            - windDirPixels[j], windDirPixels[j] * 2,
                            windDirPixels[j] * 2, 90 - (i * 10) - 4, 8);
                }
            }

            // Determine how many wind directions will be drawn on the
            // Wind Rose diagram. Here we are drawing the edges of the wind
            // direction 'pie slice'.
            if (directions == 8) {
                // Set the foreground color to black.
                gc.setForeground(parent.getDisplay().getSystemColor(
                        SWT.COLOR_BLACK));

                // Find the X & Y coordinates of the lowest angle of the
                // wind direction 'pie slice'.
                int x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - (i * 45) - 21))));
                int y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - (i * 45) - 21))));

                // Draw the edge of the lowest angle.
                gc.drawLine(centerX, circleCenterY, x, y);

                // Find the X & Y coordinates of the highest angle of the
                // wind direction 'pie slice'.
                x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - (i * 45) - 21 + 42))));
                y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - (i * 45) - 21 + 42))));

                // Draw the edge of the highest angle.
                gc.drawLine(centerX, circleCenterY, x, y);
            } else if (directions == 16) {
                // Set the foreground color to black.
                gc.setForeground(parent.getDisplay().getSystemColor(
                        SWT.COLOR_BLACK));

                // Calculate the middle degree for the current wind direction.
                int degree = (int) Math.round(i * 22.5);

                // Find the X & Y coordinates of the lowest angle of the
                // wind direction 'pie slice'.
                int x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - degree - 10))));
                int y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - degree - 10))));

                // Draw the edge of the lowest angle.
                gc.drawLine(centerX, circleCenterY, x, y);

                // Find the X & Y coordinates of the highest angle of the
                // wind direction 'pie slice'.
                x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - degree - 10 + 20))));
                y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - degree - 10 + 20))));

                // Draw the edge of the highest angle.
                gc.drawLine(centerX, circleCenterY, x, y);
            } else {
                // Set the foreground color to black.
                gc.setForeground(parent.getDisplay().getSystemColor(
                        SWT.COLOR_BLACK));

                // Find the X & Y coordinates of the lowest angle of the
                // wind direction 'pie slice'.
                int x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - (i * 10) - 4))));
                int y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - (i * 10) - 4))));

                // Draw the edge of the lowest angle.
                gc.drawLine(centerX, circleCenterY, x, y);

                // Find the X & Y coordinates of the highest angle of the
                // wind direction 'pie slice'.
                x = (int) Math.round(centerX
                        + (outerRadius * Math.cos(Math
                                .toRadians(90 - (i * 10) - 4 + 8))));
                y = (int) Math.round(circleCenterY
                        - (outerRadius * Math.sin(Math
                                .toRadians(90 - (i * 10) - 4 + 8))));

                // Draw the edge of the highest angle.
                gc.drawLine(centerX, circleCenterY, x, y);
            }
        }

        // ---------------------------------------------
        // Draw the variable wind information
        // ---------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getVariableRgb());
        gc.setBackground(tmpColor);

        int variablePix = (int) Math
                .round(variableRingPercent * pixPerUnit);
        gc.fillOval(centerX - variablePix, circleCenterY - variablePix,
                variablePix * 2, variablePix * 2);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        gc.drawOval(centerX - variablePix, circleCenterY - variablePix,
                variablePix * 2, variablePix * 2);

        // ---------------------------------------------
        // Draw the calm wind information
        // ---------------------------------------------
        tmpColor.dispose();
        tmpColor = new Color(parent.getDisplay(),
                windRoseConfigData.getCalmRgb());
        gc.setBackground(tmpColor);

        int calmPix = (int) Math.round( calmRingPercent * pixPerUnit);
        gc.fillOval(centerX - calmPix, circleCenterY - calmPix, calmPix * 2,
                calmPix * 2);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        gc.drawOval(centerX - calmPix, circleCenterY - calmPix, calmPix * 2,
                calmPix * 2);

        tmpColor.dispose();
    }

    /**
     * Get and array of pixels for each knot range. The pixels are cumulative
     * for each range. When the filled arcs are drawn they are drawn from the
     * outer most range to the inner most range and they will overlap.
     * 
     * The indexes of the array are as follows: 0 - 0 to wind variable 1 1 -
     * Wind variable 1 to wind variable 2 2 - Wind variable 2 to wind variable 3
     * 3 - Above wind variable 3.
     * 
     * @param knotsData
     * @return
     */
    private int[] getWindDirPixelValues(double[] knotsData) {
        // Create the pixel array.
        int[] pixelVals = { 0, 0, 0, 0 };

        // Get the Calm and Variable averages for all wind directions.
        double calmAve = windRoseDataMgr.getCalmAverage();
        double variableAve = windRoseDataMgr.getVariableAverage();
        double totalWindValue = windRoseDataMgr.getTotalWindDirCount();

        // Loop and fill the pixel array.
        for (int i = 0; i < pixelVals.length; i++) {
            double sum = 0;
            for (int j = 1; j <= i + 1; j++) {
                sum += knotsData[j];
            }
            
            pixelVals[i] = (int) Math.round( Math.sqrt( (calmAve + variableAve + sum)/totalWindValue )
                    * pixPerUnit);
        }

        return pixelVals;
    }

    /**
     * Calculate the maximum percent that will be displayed on the Wind Rose
     * diagram (percent of the outer ring).
     */
    private void calcPercentData() {
    	numWindDirections = windRoseDataMgr.getNumOfWindDirections();
    	double totalValue = windRoseDataMgr.getTotalWindDirCount();
    	double calmValue = windRoseDataMgr.getCalmValue();
    	double variableValue = windRoseDataMgr.getVariableValue();
    	calmRingPercent = Math.sqrt(calmValue/totalValue/numWindDirections);
    	variableRingPercent = Math.sqrt((calmValue+variableValue)/totalValue/numWindDirections);
    	
        double highestPercent = Math.sqrt(windRoseDataMgr
                .getLargestWindDirectionPercent()) * 100;

        if (highestPercent <= 5) {
            // Calculate max percent to nearest 2%
            for (int x = 0; x <= 5; ++x) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 10) {
            // Calculate max percent to nearest 2%
            for (int x = 0; x <= 10; x += 2) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 15) {
            // Calculate max percent to nearest 2%
            for (int x = 0; x <= 15; x += 3) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 20) {
            // Calculate max percent to nearest 2%
            for (int x = 0; x <= 20; x += 4) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 25) {
            // Calculate max percent to nearest 5%
            for (int x = 0; x <= 25; x += 5) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 30) {
            // Calculate max percent to nearest 5%
            for (int x = 0; x <= 30; x += 6) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 35) {
            // Calculate max percent to nearest 5%
            for (int x = 0; x <= 35; x += 7) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 40) {
            // Calculate max percent to nearest 5%
            for (int x = 0; x <= 40; x += 8) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 50) {
            // Calculate max percent to nearest 10%
            for (int x = 0; x <= 50; x += 10) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        } else if (highestPercent <= 100) {
            // Calculate max percent to nearest 20%
            for (int x = 0; x <= 100; x += 20) {
                if (x > highestPercent) {
                    maxRingPercent = (double) x / 100;
                    break;
                }
            }
        }
        
        pixPerUnit = maxCircleRadius / maxRingPercent;
    }

    /**
     * Set the header that will be drawn on the Wind Rose diagram.
     * 
     * @param header
     *            Header information.
     */
    public void setWindRoseHeader(String header) {
        windRoseHeader = header;
    }

    /**
     * Update the Wind Rose configuration data & Wind Rose header label and
     * redraw the Wind Rose diagram.
     * 
     * @param windRoseConfigData
     *            New configuration data.
     * @param header
     *            Header label information.
     */
    public void updateAndRedraw(WindRoseConfigData windRoseConfigData,
            String header, String month, String numMonths, String hour,
            String numHours, int flightCat, String site,
            WindRosePlotDlg windRoseDlg) {
        this.windRoseDlg = windRoseDlg;
        this.windRoseConfigData = windRoseConfigData;
        this.windRoseHeader = header;

        updateWindRoseData(month, numMonths, hour, numHours, flightCat, site);
    }

    /**
     * Reread the Wind Rose data. This needs to be done because the criteria for
     * 'binning' the data may have changed.
     */
    private void updateWindRoseData(String month, String numMonths,
            String hour, String numHours, int flightCat, String site) {
        windRoseDataMgr = WindRoseDataMgr.getInstance(
                windRoseConfigData.getPoints(),
                windRoseConfigData.getVar1Max(),
                windRoseConfigData.getVar2Max(),
                windRoseConfigData.getVar3Max(), hour, numHours);
        windRoseDataMgr.readData(month, numMonths, flightCat, site, this);
    }

    /**
     * Get the Wind Rose diagram in an image object.
     * 
     * @return The Wind Rose diagram.
     */
    public Image getWindRoseImage() {
        // Redraw the wind rose on an image.
        if (image != null) {
            image.dispose();
        }

        image = new Image(parent.getDisplay(), CANVAS_WIDTH, CANVAS_HEIGHT);

        GC gc = new GC(image);
        drawWindRoseCanvas(gc);

        gc.dispose();

        return image;

    }

    public void drawCanvas(GC gc) {
        drawWindRoseCanvas(gc);
    }

    public void saveStats(String filename) {
        if (windRoseDataMgr != null) {
            windRoseDataMgr.printData(filename);
        }
    }

    public void resetCursor() {
        windRoseCanvas.redraw();
        windRoseDlg.resetCursor();
    }
}
