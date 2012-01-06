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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

public class TwoValueSliderCanvas
{
    private Composite parentComp;
    private Display display;
    
    private Color blackColor;
    private Color greyColor;
    private Color arrowAndLabelColor;
    private Color canvasBgColor;
    
    /*
     * Canvas information
     */
    private Canvas canvas;    
    private final int CANVAS_WIDTH = 250;  
    private final int CANVAS_HEIGHT = 50;
    
    /*
     * Bar information.
     */
    private int barBottomYCoord = 45;
    private int barWidth = 200;
    private int barHeight = 10;
    private int barXCoord = 25;
    private int barYCoord = 35;
    
    /*
     * Upper arrow, label and values
     */    
    private Region upperRegion;
    private int[] upperPtArray = new int[]{0,0};
    private int upperArrowXCoord = 150;
    private Rectangle upperLblRect;
    private String upperStr;
    private int upperLblYCoord = 1;
    private boolean moveUpper = false;
    private Double upperDisplayVal = 0.0;
    
    /*
     * Lower arrow, label and values
     */    
    private Region lowerRegion;
    private int[] lowerPtArray = new int[]{0,0};
    private int lowerArrowXCoord = 50;
    private Rectangle lowerLblRect;
    private String lowerStr;
    private int lowerLblYCoord = 14;
    private Color middleColor;
    private boolean moveLower = false;
    private Double lowerDisplayVal = 0.0;
    
    /*
     * Mouse information.
     */
    private Point mousePt;    
    private boolean mouseDown = false;
    
    /*
     * Font/text information
     */
    private Font labelFont;
    private int textWidth = 0;
    private int textHeight = 0;
    
    /*
     * Min/Range/Increment information
     */
    private double minValue = Double.NaN;
    private double rangeValue = Double.NaN;
    private double incValue = Double.NaN;
    
    private double incPerPixel = Double.NaN;
    private boolean displayAtInt = false;
    
    private String formatStr;
    
    public TwoValueSliderCanvas(Composite parentComp, double min, double range, double inc,
            double startingUpperVal, double startingLowerVal)
    {
        this.parentComp = parentComp;
        
        display = this.parentComp.getDisplay();
        
        setValuesNoRedraw(min, range, inc, startingUpperVal, startingLowerVal);
        
        init();
        createCanvas();
    }
    
    private void init()
    {
        canvasBgColor = new Color(display, 142, 122, 110);
        arrowAndLabelColor = display.getSystemColor(SWT.COLOR_WHITE);
        middleColor = new Color(display, 110, 150, 110);
        blackColor = display.getSystemColor(SWT.COLOR_BLACK);
        greyColor = display.getSystemColor(SWT.COLOR_GRAY);
        
        upperLblRect = new Rectangle(0,0,0,0);
        lowerLblRect = new Rectangle(0,0,0,0);
        
        labelFont = new Font(display, "Monospaced", 10, SWT.BOLD);
        mousePt = new Point(0,0);
        upperRegion = new Region(display);
        lowerRegion = new Region(display);
        
        makeCalculations();
    }
    
    private void createCanvas()
    {        
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        //canvas = new Canvas(shell, SWT.NO_BACKGROUND);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;
        
        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        
        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {                                
                drawCanvas(e.gc);
            }
        });
        
        canvas.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mouseDown(MouseEvent e)
            {
                if (upperRegion.contains(e.x, e.y) == true ||
                        upperLblRect.contains(e.x, e.y) == true)
                {
                    mousePt.x = e.x;
                    mousePt.y = e.y;
                    mouseDown = true;
                    moveUpper = true;
                }
                else if (lowerRegion.contains(e.x, e.y) == true ||
                        lowerLblRect.contains(e.x, e.y) == true)
                {
                    mousePt.x = e.x;
                    mousePt.y = e.y;
                    mouseDown = true;
                    moveLower = true;
                }
            }

            @Override
            public void mouseUp(MouseEvent e)
            {
                mouseDown = false;
                moveLower = false;
                moveUpper = false;
            }            
        });
        
        canvas.addMouseMoveListener(new MouseMoveListener()
        {
            @Override
            public void mouseMove(MouseEvent e)
            {
                handleMouseMove(e);                
            }            
        });
        
        canvas.addDisposeListener(new DisposeListener()
        {
            @Override
            public void widgetDisposed(DisposeEvent e)
            {
                upperRegion.dispose();
                lowerRegion.dispose();
                labelFont.dispose();
                canvasBgColor.dispose();
                middleColor.dispose();
            }            
        });
    }
    
    private void drawCanvas(GC gc)
    { 
        gc.setAntialias(SWT.ON);
        gc.setFont(labelFont);
        gc.setBackground(canvasBgColor);
        
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
        
        gc.setBackground(greyColor);
        gc.fillRectangle(barXCoord, barYCoord, barWidth, barHeight);
        
        gc.setBackground(middleColor);
        gc.fillRectangle(lowerArrowXCoord, barYCoord, barWidth+25-lowerArrowXCoord, barHeight);
                
        gc.setBackground(greyColor);
        gc.fillRectangle(upperArrowXCoord, barYCoord, barWidth+25-upperArrowXCoord, barHeight);
        
        gc.setForeground(blackColor);
        gc.drawRectangle(barXCoord, barYCoord, barWidth, barHeight);
        
        updateLowerArrow(gc);
        updateLowerLabel(gc);
        
        updateUpperArrow(gc);        
        updateUpperLabel(gc);
    }
    
    private void updateUpperArrow(GC gc)
    {
        upperRegion.subtract(upperPtArray);
        upperPtArray = new int[]{upperArrowXCoord, barBottomYCoord - barHeight - 3,
                upperArrowXCoord + 4, barBottomYCoord,
                upperArrowXCoord - 4, barBottomYCoord};
        upperRegion.add(upperPtArray);
        
        gc.setBackground(arrowAndLabelColor);
        gc.fillPolygon(upperPtArray);
        gc.setForeground(blackColor);
        gc.drawPolygon(upperPtArray);
    }
    
    private void updateUpperLabel(GC gc)
    {
        gc.setForeground(arrowAndLabelColor);
        upperStr = calcDisplayString(upperDisplayVal);
        int lblXCoord = upperArrowXCoord - (textWidth * upperStr.length() / 2);
        gc.drawString(upperStr, lblXCoord, upperLblYCoord, true);
        upperLblRect.x = lblXCoord;
        upperLblRect.y = upperLblYCoord;
        upperLblRect.width = textWidth * upperStr.length();
        upperLblRect.height = textHeight;
    }
    
    private void updateLowerArrow(GC gc)
    {
        lowerRegion.subtract(lowerPtArray);
        lowerPtArray = new int[]{lowerArrowXCoord, barBottomYCoord - barHeight - 3,
                lowerArrowXCoord + 4, barBottomYCoord,
                lowerArrowXCoord - 4, barBottomYCoord};
        lowerRegion.add(lowerPtArray);
        
        gc.setBackground(arrowAndLabelColor);
        gc.fillPolygon(lowerPtArray);        
        gc.setForeground(blackColor);
        gc.drawPolygon(lowerPtArray);
    }
    
    private void updateLowerLabel(GC gc)
    {
        gc.setForeground(arrowAndLabelColor);
        lowerStr = calcDisplayString(lowerDisplayVal);
        int lblXCoord = (int)(lowerArrowXCoord - ((double)textWidth * (double)lowerStr.length() / 2));
        gc.drawString(lowerStr, lblXCoord, lowerLblYCoord, true);
        lowerLblRect.x = lblXCoord;
        lowerLblRect.y = lowerLblYCoord;
        lowerLblRect.width = textWidth * lowerStr.length();
        lowerLblRect.height = textHeight;
    }
    
    private void handleMouseMove(MouseEvent e)
    {
        if (mouseDown == false)
        {          
            return;
        }
        
        if (moveUpper == true)
        {
            int xDiff = e.x - mousePt.x;
            upperArrowXCoord += xDiff;

            if (upperArrowXCoord > 225)
            {
                upperArrowXCoord = 225;
            }
            else if (upperArrowXCoord < 25)
            {
                upperArrowXCoord = 25;
            }
            else if (upperArrowXCoord < lowerArrowXCoord)
            {
                upperArrowXCoord = lowerArrowXCoord;
            }             
            
            upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        }
        else if (moveLower == true)
        {
            int xDiff = e.x - mousePt.x;
            lowerArrowXCoord += xDiff;

            if (lowerArrowXCoord > 225)
            {
                lowerArrowXCoord = 225;
            }
            else if (lowerArrowXCoord < 25)
            {
                lowerArrowXCoord = 25;
            }
            else if (lowerArrowXCoord > upperArrowXCoord)
            {
                lowerArrowXCoord = upperArrowXCoord;
            }          
            lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
        }
        
        mousePt.x = e.x;
        mousePt.y = e.y;
        canvas.redraw();
    }
    
    private void makeCalculations()
    {
        Image image = new Image(display, 100, 100);
        GC gc = new GC(image);
        gc.setFont(labelFont);
        
        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();   
        
        gc.dispose();
        image.dispose();
    }
    
    private double calcDisplayValue(int xCoord)
    {
        double xCoordAsValue = (xCoord - barXCoord) * incPerPixel + minValue;
        if (incValue == .25)
        {            
            return (Math.round(xCoordAsValue * 4.00)) / 4.00;
        }
        else if (incValue == .10)
        {
            return (Math.round(xCoordAsValue * 10.00)) / 10.00;
        }
        else
        {
            return (double)Math.round(xCoordAsValue);
        }
    }
    
    private String calcDisplayString(Double displVal)
    {
        if (displayAtInt == true)
        {
            return String.format(formatStr, Math.round(displVal));
        }
        else
        {
            return String.format(formatStr, displVal);
        }
    }
    
    private int calcValueToBarXCoord(double val)
    {
        int result = (int)Math.round((val - minValue)/incPerPixel + barXCoord);
        
        return result;
    }
    
    private void setValuesNoRedraw(double min, double range, double inc, double startingUpperVal, double startingLowerVal)
    {
        this.minValue = min;
        this.rangeValue = (double)Math.round(range);
        this.incValue = inc;
        
        incPerPixel = this.rangeValue / (double)barWidth;
        
        if (inc < 1.00)
        {
            displayAtInt = false;
            formatStr = "%1.2f";
        }
        else
        {
            displayAtInt = true;
            formatStr = "%d";
        }
        
        upperArrowXCoord = calcValueToBarXCoord(startingUpperVal);
        lowerArrowXCoord = calcValueToBarXCoord(startingLowerVal);
        
        upperDisplayVal = calcDisplayValue(upperArrowXCoord);
        lowerDisplayVal = calcDisplayValue(lowerArrowXCoord);
    }
    
    public void setValues(double min, double range, double inc, double startingUpperVal, double startingLowerVal)
    {
        setValuesNoRedraw(min, range, inc, startingUpperVal, startingLowerVal);
        canvas.redraw();
    }
    
    public double getUpperValue()
    {
        return upperDisplayVal;
    }
    
    public double getLowerValue()
    {
        return lowerDisplayVal;
    }
}
