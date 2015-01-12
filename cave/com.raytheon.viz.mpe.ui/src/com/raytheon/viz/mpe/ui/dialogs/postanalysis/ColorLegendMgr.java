package com.raytheon.viz.mpe.ui.dialogs.postanalysis;


import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThreshold;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThresholdArray;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * Handles color legend display in MPE's PostAnalysis
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * December 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */
public class ColorLegendMgr {

	private Canvas legendCanvas = null;
	
	private NamedColorUseSet _namedColorUseSet1 = null;
	private NamedColorUseSet _namedColorUseSet2 = null;
	
	private String dateTimeForLegend = null;
	   
	private final static Color black = Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);

	
	
	public ColorLegendMgr(Canvas canvas, 
						  NamedColorUseSet namedColorUseSet1,
						  NamedColorUseSet namedColorUseSet2,
						  String dateTimeForLegend)
	{
	    legendCanvas = canvas;
	    this.setDateTimeStringForLegend(dateTimeForLegend);
	    setNamedColorUseSet1(namedColorUseSet1);
	    setNamedColorUseSet2(namedColorUseSet2);
	    
	    
	    if (getNamedColorUseSet2() != null)
	    {
	        int height = legendCanvas.getBounds().height * 2;
	        int width = legendCanvas.getBounds().width;
	        
	        Rectangle bounds = new Rectangle(0,0, width, height);
	        
	        legendCanvas.setBounds(bounds);
	    }
	    
	    legendCanvas.addPaintListener(new ColorLegendMgrPaintListener());

	}

	
	
	private static Color getColorByName(String colorName)
	{
		
		String header = "ColorLegendMgr.getColorByName() ";
		
		Color color = null;
		try
		{

			RGB rgbValue = RGBColors.getRGBColor(colorName);

			if (rgbValue == null)
			{
				System.out.println(header + " RGB value for " + colorName + " is null.");
			}
			
			color = new Color(Display.getCurrent(), rgbValue);
/*
			if (color == null)
			{
				color = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
			}
*/
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	    return color;
	}

	public void paintLegend(GC gc)
	{
	    int canvasWidth = legendCanvas.getSize().x;
        int canvasHeight = legendCanvas.getSize().y;
	
        gc.setBackground(black);
        gc.fillRectangle(0, 0, canvasWidth, canvasHeight);

	    
	    paintColorScale(gc, 0, getNamedColorUseSet1(), false, true, false);
	    
	    // This can be used to draw either 1 or 2 color scales
	    if (getNamedColorUseSet2() !=  null)
	    {
	        paintColorScale(gc, 75, getNamedColorUseSet2(), false, false, true);
	    }
	}
	
    private void paintColorScale(GC gc, int initialYPos,
                                 NamedColorUseSet namedColorUseSet,        
                                 boolean displaySpecialIdenticallyZeroValue,
                                 boolean displayMissingValue,
                                 boolean displayLabelAbove)
    { 	
    
        gc.setAntialias(SWT.ON);
        
        final Color black = legendCanvas.getDisplay().getSystemColor(SWT.COLOR_BLACK);
        final Color white = legendCanvas.getDisplay().getSystemColor(SWT.COLOR_WHITE);
        
        int xPos = 25;
        int yPos = initialYPos;
        
        gc.setForeground(white);
        
        if (displayLabelAbove)
        {
            gc.drawString(namedColorUseSet.getColor_use_display_string() , xPos, yPos);
            yPos = initialYPos + 13;
        }
        
     
        
        ColorThresholdArray colorThresholdArrayObject = namedColorUseSet.getThreshold_array();
        ColorThreshold[] colorThresholds = colorThresholdArrayObject.getThresholds();
              
   
        final int rectangleWidth = 40;
        final int rectangleHeight = 20;
        
        int stringXOffset = -10;
        int extraDigitStringXOffset = 0;
        int defaultXOffset = 7;
        
        final int stringYOffset = 4;
  
        xPos = 10;
        yPos += rectangleHeight;
        
        String missingColorName = colorThresholdArrayObject.getMissingColorName();
   //   String defaultColorName = colorThresholdArrayObject.getDefaultColorName();
        
        Color color =  null;
        
        if (displayMissingValue)
        {
            //draw the missing color rectangle

            final Color missingColor = getColorByName(missingColorName);     
            color = missingColor;
            gc.setBackground(black);
            gc.drawString("MSG", xPos + defaultXOffset, yPos - rectangleHeight + stringYOffset);
            gc.setBackground(color);
            gc.fillRectangle(xPos, yPos, rectangleWidth, rectangleHeight);
            xPos += rectangleWidth;
        }
        
        if (displaySpecialIdenticallyZeroValue)
        {
            //draw the identically 0 color rectangle       
            color = black;
            gc.setBackground(color);
            gc.drawString("0.0", xPos + defaultXOffset, yPos - rectangleHeight + stringYOffset);
            gc.fillRectangle(xPos, yPos, rectangleWidth, rectangleHeight);
            xPos += rectangleWidth;
        }
        


        //draw the default color rectangle
        final Color defaultColor = black;     
        gc.setBackground(defaultColor);      
        gc.fillRectangle(xPos, yPos, rectangleWidth, rectangleHeight);
        xPos += rectangleWidth;

        
        //draw each threshold color rectangle and the associated threshold value
        for (ColorThreshold threshold: colorThresholds)
        {
            color = getColorByName(threshold.getColorName());
             
            gc.setBackground(black);
            if (threshold.getValue() >= 10.00)
            {
                extraDigitStringXOffset = -3;
            }
            else
            {
                extraDigitStringXOffset = -0;
            }
            String thresholdValueString = String.format("%5.2f",threshold.getValue());
            gc.drawString(thresholdValueString, xPos + stringXOffset + extraDigitStringXOffset, yPos - rectangleHeight + stringYOffset);
            
            gc.setBackground(color);
            gc.fillRectangle(xPos, yPos, rectangleWidth, rectangleHeight);
            xPos += rectangleWidth;
           
        } //end for
  
        //draw infinity symbol
        String infinityString = "\u221D"; //unicode encoding for infinity symbol
        gc.setBackground(black);
        gc.drawString(infinityString, xPos + stringXOffset-3, yPos - rectangleHeight + stringYOffset);
        
        
        
        if (! displayLabelAbove)
        {
            xPos = 20;
            yPos += rectangleHeight + 10;
            gc.setForeground(white);
            String legendText = namedColorUseSet.getColor_use_display_string() + " " + this.getDateTimeForLegend();
            gc.drawString(legendText, xPos, yPos);
        }
        
        return;
    }
    

   
   

    private void setNamedColorUseSet1(NamedColorUseSet _namedColorUseSet1) {
		this._namedColorUseSet1 = _namedColorUseSet1;
	}

	public NamedColorUseSet getNamedColorUseSet1() {
		return _namedColorUseSet1;
	}





	private void setNamedColorUseSet2(NamedColorUseSet _namedColorUseSet2) {
		this._namedColorUseSet2 = _namedColorUseSet2;
	}

	public NamedColorUseSet getNamedColorUseSet2() {
		return _namedColorUseSet2;
	}


	public void finalize()
	{
	}


	public void setDateTimeStringForLegend(String dateTimeForLegend) {
		this.dateTimeForLegend = dateTimeForLegend;
	}

	public String getDateTimeForLegend() {
		return dateTimeForLegend;
	}


	class ColorLegendMgrPaintListener implements PaintListener
    {

        public void paintControl(PaintEvent e)
        {
            paintLegend(e.gc); 
        }        
    }
    

}
