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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

public class ThresholdCanvas extends Canvas
{
    private final int CANVAS_WIDTH = 650;   
    private final int CANVAS_HEIGHT = 110;
    
    private Color lightGrey;
    
    private Composite parentComp;
    private Font canvasFont;
    
    private boolean drawFog = true;
    
    private int fontHeightMid = 0;
    private int fontHeight = 0;
    private int fontAveWidth = 0;
    
    private int boxXOffset = 25;
    private int boxYOffset = 10;
    private int drawHeight = 75;
    private int boxWidth = CANVAS_WIDTH - boxXOffset*2;
    
    private int rangeTextYCoord = 90;
    
    private double fogPixPerInc = 3.0;
    private double visPixPerInc = 4.0;
    
    private double[] fogRanges;
    private double[] visRanges;
    
    private double fogRangeMin = -10.0;
    private double fogRangeMax = 10.0;
    
    private double visRangeMin = 50.0;
    private double visRangeMax = 200.0;
    
    private double[] fogIncrements;
    private double[] visIncrements;
    
    public static enum RangeEnum {YLo, RLo, RHi, YHi};
    private enum IncrementEnum {LrgDec, SmDec, SmInc, LrgInc};

    private RangeEnum currentRange = RangeEnum.YLo;
    
    public ThresholdCanvas(Composite parent)
    {
        super(parent, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        
        parentComp = parent;
        
        init();
    }
    
    private void init()
    {        
        
        canvasFont = new Font(parentComp.getDisplay(), "Monospace", 12, SWT.NORMAL);
        lightGrey = new Color(parentComp.getDisplay(), 210, 210, 210);
        
        setupRangesAndIncrements();        
        
        setupCanvas();        
    }
    
    private void setupRangesAndIncrements()
    {
        fogRanges = new double[RangeEnum.values().length];
        visRanges = new double[RangeEnum.values().length];
        
        fogRanges[RangeEnum.YLo.ordinal()] = 0.5;
        fogRanges[RangeEnum.RLo.ordinal()] = 1.5;
        fogRanges[RangeEnum.RHi.ordinal()] = 3.0;
        fogRanges[RangeEnum.YHi.ordinal()] = 4.0;
        
        visRanges[RangeEnum.YLo.ordinal()] = 100.0;
        visRanges[RangeEnum.RLo.ordinal()] = 110.0;
        visRanges[RangeEnum.RHi.ordinal()] = 130.0;
        visRanges[RangeEnum.YHi.ordinal()] = 140.0;
        
        fogIncrements = new double[IncrementEnum.values().length];
        visIncrements = new double[IncrementEnum.values().length];        
        
        fogIncrements[IncrementEnum.LrgDec.ordinal()] = -1.0;
        fogIncrements[IncrementEnum.SmDec.ordinal()] = -0.1;
        fogIncrements[IncrementEnum.SmInc.ordinal()] = 0.1;
        fogIncrements[IncrementEnum.LrgInc.ordinal()] = 1.0;
        
        for (int i = 0; i < fogIncrements.length; i++)
        {
            System.out.println(fogIncrements[i]);
        }
        
        visIncrements[IncrementEnum.LrgDec.ordinal()] = -5.0;
        visIncrements[IncrementEnum.SmDec.ordinal()] = -1.0;
        visIncrements[IncrementEnum.SmInc.ordinal()] = 1.0;
        visIncrements[IncrementEnum.LrgInc.ordinal()] = 5.0;
    }
    
    private void setupCanvas()
    {        
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;
        
        this.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        
        this.setLayoutData(gd);
        this.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {   
                drawCanvas(e.gc);
            }
        });
        
        this.addDisposeListener(new DisposeListener()
        {
            public void widgetDisposed(DisposeEvent e)
            {                
                canvasFont.dispose();
                lightGrey.dispose();
            }
        });
    }
    
    private void drawCanvas(GC gc)
    {
        gc.setFont(canvasFont);
        fontHeightMid = (int)(gc.getFontMetrics().getHeight() / 2);
        fontHeight = (int)(gc.getFontMetrics().getHeight());
        fontAveWidth = (int)(gc.getFontMetrics().getAverageCharWidth());
                
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
        
        // Draw gray box.
        gc.setBackground(lightGrey);
        gc.fillRectangle(boxXOffset, boxYOffset, boxWidth, drawHeight);
        
        // Draw the Fog Product or the VIS depending on the Draw Fog flag.
        if (drawFog == true)
        {
            drawFogProduct(gc);
        }
        else
        {
            drawVis(gc);
        }
    }
    
    private void drawFogProduct(GC gc)
    {        
        /*
         * Get the X coordinates for the YLo, RLo, RHi, and YHi ranges.
         */
        int xCoordYLo = getIntXCoord(fogRanges[RangeEnum.YLo.ordinal()]);
        int xCoordYHi = getIntXCoord(fogRanges[RangeEnum.YHi.ordinal()]);
        
        int xCoordRLo = getIntXCoord(fogRanges[RangeEnum.RLo.ordinal()]);
        int xCoordRHi = getIntXCoord(fogRanges[RangeEnum.RHi.ordinal()]);        
        
        /*
         * Draw the Y (yellow filled) rectangle.
         */
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_YELLOW));                
        gc.fillRectangle(xCoordYLo, boxYOffset, xCoordYHi - xCoordYLo, drawHeight);
        
        /*
         * Draw the R (red filled) rectangle.
         */
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_RED));                
        gc.fillRectangle(xCoordRLo, boxYOffset, xCoordRHi - xCoordRLo, drawHeight);
        
        /*
         * Draw the range labels for the YLo, RLo, RHi, and YHi ranges.
         */
        
        // YLo
        setFontColor(gc, RangeEnum.YLo);
        String yLoStr = String.format("%5.1f", fogRanges[RangeEnum.YLo.ordinal()]);
        gc.drawString(yLoStr, xCoordYLo - (fontAveWidth * 3), boxYOffset + 1, true);
        
        // RLo
        setFontColor(gc, RangeEnum.RLo);
        String rLoStr = String.format("%5.1f", fogRanges[RangeEnum.RLo.ordinal()]);
        gc.drawString(rLoStr, xCoordRLo - (fontAveWidth * 3), boxYOffset + fontHeight + 1, true);
        
        // RHi
        setFontColor(gc, RangeEnum.RHi);
        String rHiStr = String.format("%5.1f", fogRanges[RangeEnum.RHi.ordinal()]);
        gc.drawString(rHiStr, xCoordRHi - (fontAveWidth * 3), boxYOffset + (fontHeight * 2) + 1, true);
        
        // YHi
        setFontColor(gc, RangeEnum.YHi);
        String yHiStr = String.format("%5.1f", fogRanges[RangeEnum.YHi.ordinal()]);
        gc.drawString(yHiStr, xCoordYHi - (fontAveWidth * 3), boxYOffset + (fontHeight * 3) + 1, true);
        
        /*
         * Draw the minimum and maximum range labels
         */
        gc.setForeground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK)); 
        
        String lowRangeStr = String.format("%5.1f", fogRangeMin);
        gc.drawString(lowRangeStr, 5, rangeTextYCoord, true);
        
        String upperRangeStr = String.format("%5.1f", fogRangeMax);
        gc.drawString(upperRangeStr, CANVAS_WIDTH - 55, rangeTextYCoord, true);
    }
    
    private void drawVis(GC gc)
    {
        System.out.println("Drawing VIS");
        /*
         * Get the X coordinates for the YLo, RLo, RHi, and YHi ranges.
         */
        int xCoordYLo = getIntXCoord(visRanges[RangeEnum.YLo.ordinal()]);
        int xCoordYHi = getIntXCoord(visRanges[RangeEnum.YHi.ordinal()]);
        
        int xCoordRLo = getIntXCoord(visRanges[RangeEnum.RLo.ordinal()]);
        int xCoordRHi = getIntXCoord(visRanges[RangeEnum.RHi.ordinal()]);  
        
        System.out.println(xCoordYLo + "\t" + xCoordYHi + "\t" + xCoordRLo + "\t" + xCoordRHi);
        
        /*
         * Draw the Y (yellow filled) rectangle.
         */
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_YELLOW));                
        gc.fillRectangle(xCoordYLo, boxYOffset, xCoordYHi - xCoordYLo, drawHeight);
        
        /*
         * Draw the R (red filled) rectangle.
         */
        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_RED));                
        gc.fillRectangle(xCoordRLo, boxYOffset, xCoordRHi - xCoordRLo, drawHeight);
        
        /*
         * Draw the range labels for the YLo, RLo, RHi, and YHi ranges.
         */
        
        // YLo
        setFontColor(gc, RangeEnum.YLo);
        String yLoStr = String.format("%3d", (int)visRanges[RangeEnum.YLo.ordinal()]);
        gc.drawString(yLoStr, xCoordYLo - (int)Math.round(fontAveWidth * 1.5), boxYOffset + 1, true);
        
        // RLo
        setFontColor(gc, RangeEnum.RLo);
        String rLoStr = String.format("%3d", (int)visRanges[RangeEnum.RLo.ordinal()]);
        gc.drawString(rLoStr, xCoordRLo - (int)Math.round(fontAveWidth * 1.5),
                boxYOffset + fontHeight + 1, true);
        
        // RHi
        setFontColor(gc, RangeEnum.RHi);
        String rHiStr = String.format("%3d", (int)visRanges[RangeEnum.RHi.ordinal()]);
        gc.drawString(rHiStr, xCoordRHi - (int)Math.round(fontAveWidth * 1.5),
                boxYOffset + (fontHeight * 2) + 1, true);
        
        // YHi
        setFontColor(gc, RangeEnum.YHi);
        String yHiStr = String.format("%3d", (int)visRanges[RangeEnum.YHi.ordinal()]);
        gc.drawString(yHiStr, xCoordYHi - (int)Math.round(fontAveWidth * 1.5),
                boxYOffset + (fontHeight * 3) + 1, true);
        
        /*
         * Draw the minimum and maximum range labels
         */
        gc.setForeground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK)); 
        String lowRangeStr = String.format("%2d", (int)visRangeMin);
        gc.drawString(lowRangeStr, 15, rangeTextYCoord, true);
        
        String upperRangeStr = String.format("%3d", (int)visRangeMax);
        gc.drawString(upperRangeStr, CANVAS_WIDTH - 40, rangeTextYCoord, true);
    }
    
    private void setFontColor(GC gc, RangeEnum range)
    {
        if (currentRange == range)
        {
            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_BLUE)); 
            return;
        }
        
        gc.setForeground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK)); 
    }
    
    /**
     * Use the double value for fog or vis and find the integer value of the X Coordinate.
     * @param val Double value.
     * @return Integer X coordinate.
     */
    private int getIntXCoord(double val)
    {
        int rv = 0;
        
        if (drawFog == true)
        {
            rv = (int)Math.round(((val + 10.0)/0.1 * fogPixPerInc) + boxXOffset);
        }
        else
        {
            rv = (int)Math.round(((val - 50) * visPixPerInc) + boxXOffset);
        }
        
        return rv;
    }
    
    public void drawFog(boolean flag)
    {
        drawFog = flag;
        this.redraw();
    }
    
    public void setFogVisThreshholds(double fogYLo, double fogRLo, double fogRHi, 
            double fogYHi, double visYLo, double visRLo, double visRHi, double visYHi)
    {
        fogRanges[RangeEnum.YLo.ordinal()] = fogYLo;
        fogRanges[RangeEnum.RLo.ordinal()] = fogRLo;
        fogRanges[RangeEnum.RHi.ordinal()] = fogRHi;
        fogRanges[RangeEnum.YHi.ordinal()] = fogYHi;
        visRanges[RangeEnum.YLo.ordinal()] = visYLo;
        visRanges[RangeEnum.RLo.ordinal()] = visRLo;
        visRanges[RangeEnum.RHi.ordinal()] = visRHi;
        visRanges[RangeEnum.YHi.ordinal()] = visYHi;
        
        this.redraw();
    }
    
    public void setThresholdRange(RangeEnum range)
    {
        this.currentRange = range;
        this.redraw();
    }
    
    public void smallDecrement()
    {
        adjustRange(IncrementEnum.SmDec);
    }
    
    public void largeDecrement()
    {
        adjustRange(IncrementEnum.LrgDec);
    }
    
    public void smallIncrement()
    {
        adjustRange(IncrementEnum.SmInc);
    }
    
    public void largeIncrement()
    {
        adjustRange(IncrementEnum.LrgInc);
    }
    
    public double [] getFogProductValues()
    {
        return fogRanges;
    }
    
    public double [] getVisValues()
    {
        return visRanges;
    }
    
    private void adjustRange(IncrementEnum inc)
    {
        double[] tmpRanges = new double[RangeEnum.values().length];
        
        if (drawFog == true)
        {
            System.arraycopy(fogRanges, 0, tmpRanges, 0, RangeEnum.values().length);
            
            tmpRanges[currentRange.ordinal()] += fogIncrements[inc.ordinal()];
            tmpRanges[currentRange.ordinal()] = roundToTenths(tmpRanges[currentRange.ordinal()]);
            
            if (validateRanges(tmpRanges, fogRangeMin, fogRangeMax) == true)
            {
                System.arraycopy(tmpRanges, 0, fogRanges, 0, RangeEnum.values().length);
            }
        }
        else
        {
            System.arraycopy(visRanges, 0, tmpRanges, 0, RangeEnum.values().length);
            
            tmpRanges[currentRange.ordinal()] += visIncrements[inc.ordinal()];
            tmpRanges[currentRange.ordinal()] = roundToTenths(tmpRanges[currentRange.ordinal()]);
            
            System.out.println("visIncrements[inc.ordinal()] = " + visIncrements[inc.ordinal()]);
            System.out.println("tmpRanges[currentRange.ordinal()] = " + tmpRanges[currentRange.ordinal()]);
            
            if (validateRanges(tmpRanges, visRangeMin, visRangeMax) == true)
            {
                System.arraycopy(tmpRanges, 0, visRanges, 0, RangeEnum.values().length);
            }
        }
        
        this.redraw();
    }
    
    private boolean validateRanges(double[] range, double minRange, double maxRange)
    {        
        if (range[currentRange.ordinal()] < minRange || range[currentRange.ordinal()] > maxRange)
        {
            return false;
        }
        
        if (range[RangeEnum.YLo.ordinal()] <= range[RangeEnum.RLo.ordinal()] &&
                range[RangeEnum.RLo.ordinal()] <= range[RangeEnum.RHi.ordinal()] &&
                range[RangeEnum.RHi.ordinal()] <= range[RangeEnum.YHi.ordinal()])
        {
            System.out.println("Valid ranges");
            return true;
        }
        
        System.out.println("Invalid ranges");
        return false;
    }
    
    private double roundToTenths(double val)
    {
        double roundedVal = 0.0;
        
        roundedVal = Math.round(val * 10)/10.0;
        
        return roundedVal;
    }
}
