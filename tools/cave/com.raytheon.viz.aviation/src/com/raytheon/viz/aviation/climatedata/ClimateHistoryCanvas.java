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
package com.raytheon.viz.aviation.climatedata;

import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * Canvas the draws the climate history graph.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009 #3438      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ClimateHistoryCanvas
{
    private Composite parentComp;
    private Display display;
    
    private Canvas graphCanvas;    
    private int DEFAULT_GRAPH_CANVAS_WIDTH = 675;      
    private int GRAPH_CANVAS_WIDTH = 1500;  
    private final int GRAPH_CANVAS_WIDTH_VIEW = 700;
    private final int GRAPH_CANVAS_HEIGHT = 425;  
    private final int GRAPH_LABEL_AREA_HEIGHT = 25;
    
    private Canvas headerCanvas;
    private final int HEADER_CANVAS_HEIGHT = 40;
    
    private Canvas leftLabelCanvas;
    private final int LEFT_CANVAS_WIDTH = 100;
    private final int LEFT_CANVAS_HEIGHT = GRAPH_CANVAS_HEIGHT + HEADER_CANVAS_HEIGHT;  
    
    private final int hashLength = 5;
    
    private Font headerFont;
    private Font numObsTextFont;
    private Font incLabelFont;
    
    private final String numObsText = "Number of Observations";
    private String headerText;
    
    private int maxObsCountNearest100 = 0;
    
    private double obsPixPerInc = 0.0;
    
    private Color barColor;
    
    private String stationName;
    private ArrayList<ObsGraphData> graphData;
    
    public ClimateHistoryCanvas(Composite parentComp, String stationName, ArrayList<ObsGraphData> graphData)
    {
        this.parentComp = parentComp;
        this.display = parentComp.getDisplay();
        
        this.stationName = stationName;
        this.graphData = graphData;
        
        init();
    }
    
    public void init()
    {
        initData();
        createLeftLabelCanvas();
        createTopHeaderCanvas();
        createGraphCanvas();
    }
    
    private void initData()
    {
        headerFont = new Font(display, "Monospace", 12, SWT.NORMAL);
        numObsTextFont = new Font(display, "Monospace", 12, SWT.BOLD);
        incLabelFont = new Font(display, "Monospace", 10, SWT.NORMAL);
        barColor = new Color(display, 50, 50, 245);
        
        Collections.sort(graphData);
        
        GRAPH_CANVAS_WIDTH = graphData.size() * 100 + 50;
        
        if (DEFAULT_GRAPH_CANVAS_WIDTH > GRAPH_CANVAS_WIDTH)
        {
            GRAPH_CANVAS_WIDTH = DEFAULT_GRAPH_CANVAS_WIDTH;
        }
        
        int maxYear = Integer.MIN_VALUE;
        int minYear = Integer.MAX_VALUE;
        int maxObsCount = Integer.MIN_VALUE;
        
        for (ObsGraphData ogd : graphData)
        {
            if (ogd.getYear() < minYear)
            {
                minYear = ogd.getYear();
            }
            
            if (ogd.getYear() > maxYear)
            {
                maxYear = ogd.getYear();
            }
            
            if (ogd.getNumObs() > maxObsCount)
            {
                maxObsCount = ogd.getNumObs();
            }
        }
        
        if (maxObsCount % 100 != 0)
        {
            maxObsCountNearest100 = (((int)((maxObsCount + 100)/100))*100);
        }
        else
        {
            maxObsCountNearest100 = maxObsCount + 100;
        }
        
        obsPixPerInc = ((double)(GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT) / (double)maxObsCountNearest100);
        
        headerText = minYear + "-" + maxYear + " data for " + stationName;
    }
    
    private void createLeftLabelCanvas()
    {
        leftLabelCanvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.verticalSpan = 2;
        gd.heightHint = LEFT_CANVAS_HEIGHT;
        gd.widthHint = LEFT_CANVAS_WIDTH;
        
        leftLabelCanvas.setSize(LEFT_CANVAS_WIDTH, LEFT_CANVAS_HEIGHT);
        
        leftLabelCanvas.setLayoutData(gd);
        leftLabelCanvas.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {           
                drawLeftCanvas(e.gc);
            }
        });
    }
    
    private void createTopHeaderCanvas()
    {
        headerCanvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = HEADER_CANVAS_HEIGHT;
        gd.widthHint = GRAPH_CANVAS_WIDTH_VIEW;
        
        headerCanvas.setSize(GRAPH_CANVAS_WIDTH_VIEW, HEADER_CANVAS_HEIGHT);
        
        headerCanvas.setLayoutData(gd);
        headerCanvas.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {           
                drawHeaderCanvas(e.gc);
            }
        });
    }
    
    private void createGraphCanvas()
    {
        ScrolledComposite scrolledComp = new ScrolledComposite(parentComp, SWT.H_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        scrolledComp.setLayout(gl);
        
        GridData gd = new GridData(700, GRAPH_CANVAS_HEIGHT);
        scrolledComp.setLayoutData(gd);
        
        graphCanvas = new Canvas(scrolledComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = GRAPH_CANVAS_HEIGHT;
        gd.widthHint = GRAPH_CANVAS_WIDTH + 10;
        
        graphCanvas.setSize(GRAPH_CANVAS_WIDTH + 10, GRAPH_CANVAS_HEIGHT);        
        graphCanvas.setLayoutData(gd);
        
        scrolledComp.setContent(graphCanvas);
        
        graphCanvas.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {                                
                drawGraphCanvas(e.gc);
            }
        });
        
        graphCanvas.addDisposeListener(new DisposeListener()
        {
            @Override
            public void widgetDisposed(DisposeEvent e)
            {
                headerFont.dispose();
                numObsTextFont.dispose();
                incLabelFont.dispose();
                barColor.dispose();
            }            
        });
    }
    
    private void drawLeftCanvas(GC gc)
    { 
        gc.setAntialias(SWT.ON);
        
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));        
        gc.fillRectangle(0, 0, LEFT_CANVAS_WIDTH, LEFT_CANVAS_HEIGHT);   
        
        /*
         * Draw the "left line" of the main graph on the right side of the canvas.
         */
        gc.drawLine(LEFT_CANVAS_WIDTH - 1, HEADER_CANVAS_HEIGHT, LEFT_CANVAS_WIDTH - 1,
                HEADER_CANVAS_HEIGHT + GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT);
        
        /*
         * Draw the hash marks and labels
         */
        gc.setFont(incLabelFont);
        int textHeight = gc.getFontMetrics().getHeight();
        int aveFontWidth = (int)gc.getFontMetrics().getAverageCharWidth();
        int xCoord = (LEFT_CANVAS_WIDTH - 1) - hashLength - (aveFontWidth * 6) - 3;
        
        int labelInc = maxObsCountNearest100 / 5;
        int yCoord = HEADER_CANVAS_HEIGHT + GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT;
        int ycoordDecrementVal = (GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT) / 5;
        
        for (int i = 0; i < 6; i++)
        {
            gc.drawLine(LEFT_CANVAS_WIDTH - 1 - hashLength, yCoord, LEFT_CANVAS_WIDTH - 1, yCoord);
            
            gc.drawString(String.format("%6s", labelInc * i), xCoord, yCoord - textHeight/2, true);
            
            yCoord -= ycoordDecrementVal;
        }
        
        /*
         * Draw the "Number of Observations" label
         */        
        gc.setFont(numObsTextFont);
        
        aveFontWidth = (int)gc.getFontMetrics().getAverageCharWidth();        
        int textInPix = numObsText.length() * aveFontWidth;        
        int textYcoord = (int)Math.round(LEFT_CANVAS_HEIGHT/2 + textInPix / 2);
        
        Transform t = new Transform(gc.getDevice());
        t.translate(1,textYcoord); // new origin
        t.rotate(-90f);
        gc.setTransform(t);        
        gc.drawText(numObsText, 0, 0);
        t.dispose();
        
        /*
         * Draw the line on the left side of the graph.
         */
        gc.drawLine(HEADER_CANVAS_HEIGHT, LEFT_CANVAS_WIDTH - 1,
                HEADER_CANVAS_HEIGHT + GRAPH_CANVAS_HEIGHT, LEFT_CANVAS_WIDTH - 1);
    }
    
    private void drawHeaderCanvas(GC gc)
    {
        gc.setAntialias(SWT.ON);
        
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));        
        gc.fillRectangle(0, 0, GRAPH_CANVAS_WIDTH, HEADER_CANVAS_HEIGHT);
        
        gc.setFont(headerFont);
        
        int aveFontWidth = (int)gc.getFontMetrics().getAverageCharWidth();
        
        int textInPix = headerText.length() * aveFontWidth;
        
        int textXcoord = (int)Math.round(GRAPH_CANVAS_WIDTH_VIEW/2 - textInPix / 2);
        
        gc.drawString(headerText, textXcoord, 10, true);
    }
    
    private void drawGraphCanvas(GC gc)
    { 
        gc.setAntialias(SWT.ON);
        
        gc.setFont(incLabelFont);
        int aveFontWidth = (int)gc.getFontMetrics().getAverageCharWidth();  
        
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));        
        gc.fillRectangle(0, 0, GRAPH_CANVAS_WIDTH, GRAPH_CANVAS_HEIGHT);   
        
        gc.setLineWidth(1);
        
        gc.setBackground(display.getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, GRAPH_CANVAS_WIDTH, GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT);
        
        /*
         * Draw the top, bottom, and right side lines of the graph.
         */
        gc.drawLine(0, 0, GRAPH_CANVAS_WIDTH, 0);
        
        gc.drawLine(0, GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT,
                GRAPH_CANVAS_WIDTH, GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT);

        gc.drawLine(GRAPH_CANVAS_WIDTH, 0,
                GRAPH_CANVAS_WIDTH, GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT);
        
        /*
         * Loop and draw the bar graph and the year label.
         */
        
        int graphOffset = 50;
        int barWidth = 50;
        int barHeight = 0;
        int textInPix = 4 * aveFontWidth;
        gc.setBackground(barColor);
        
        for (ObsGraphData ogd : graphData)
        {
            barHeight = (int)Math.rint(obsPixPerInc * ogd.getNumObs());            
            int barYcoord = GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT - barHeight;
            gc.fillRectangle(graphOffset, barYcoord, barWidth, barHeight);
            gc.drawRectangle(graphOffset, barYcoord, barWidth, barHeight);
            
            int textXcoord = (int)Math.round((graphOffset + barWidth/2.0) - (textInPix / 2.0));
            gc.drawString(String.valueOf(ogd.getYear()), textXcoord,
                    GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT + 2, true);
            
            graphOffset += 50 + barWidth; 
        }
        
        /*
         * Draw dashed lines across the graph for each increment.
         */
        gc.setLineStyle(SWT.LINE_DASH);
        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
        int ycoordDecrementVal = (GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT) / 5;
        int yCoord = GRAPH_CANVAS_HEIGHT - GRAPH_LABEL_AREA_HEIGHT - ycoordDecrementVal;       
        
        for (int i = 0; i < 4; i++)
        {
            gc.drawLine(0, yCoord, GRAPH_CANVAS_WIDTH - 1, yCoord);            
            yCoord -= ycoordDecrementVal;
        }        
    }
}