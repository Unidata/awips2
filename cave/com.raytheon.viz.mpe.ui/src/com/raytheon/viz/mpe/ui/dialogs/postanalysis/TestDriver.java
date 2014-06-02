package com.raytheon.viz.mpe.ui.dialogs.postanalysis;


import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.SWT;

import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

public class TestDriver
{
    public static void main(String[] argArray)
    {
        Display display = new Display();
        Shell shell = new Shell(display);
        Label label = new Label(shell, SWT.CENTER);
        label.setText("Hello, World");
        
    //    label.setBounds(shell.getClientArea());
        
        int width = 800;
        int height = 100;
        Rectangle bounds = new Rectangle(0,0, width, height);
        
        Canvas canvas = new Canvas(shell, SWT.TOP);
        canvas.setBounds(bounds);
        canvas.setBackground(canvas.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        
        NamedColorUseSet namedColorUseSet1 = getNamedColorUseSet();
        NamedColorUseSet namedColorUseSet2 = getNamedColorUseSetForBias();
        
        ColorLegendMgr legendMgr = new ColorLegendMgr(canvas, namedColorUseSet1,
        											  namedColorUseSet2, "2001-01-01 12Z");
             
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        display.dispose();
    }

    public static NamedColorUseSet getNamedColorUseSet()
    {   
        double[] thresholdValueArray = {.000000001, 0.01, 0.1, 0.2, 0.4, 0.6,
        								 0.75, 1.0, 1.5, 5.0, 7.5};
       
        String[] colorNameArray = {"Pink", "DarkBlue", "Blue", "Cyan", 
        						   "DarkGreen", "Green",
        						   "Yellow", "Orange", "Red", "Magenta", "White"};

        String missingColorName =  "Gray";
        String defaultColorName = "DarkGray";
        int  duration = 3600 * 24;
        
        NamedColorUseSet namedColorUseSet = 
            new NamedColorUseSet("PRECIP_ACCUM", "24 Hour Precip Accumulation in inches", 
                    thresholdValueArray, colorNameArray,
                    missingColorName, defaultColorName, duration);
              
        return namedColorUseSet;
    }
    
    public static NamedColorUseSet getNamedColorUseSet1Hr()
    {  
        double[] thresholdValueArray = {0.0, .000000001, 0.01, 0.1, 0.2, 0.4,
        								0.6, 0.75, 1.0, 1.5, 5.0, 7.5};
       
        String[] colorNameArray = {"Magenta", "Pink", "DarkBlue", "Blue", 
        						   "Cyan", "DarkGreen", "Green",
        						   "Yellow", "Orange", "Red", "Magenta", "White"};
        
        String missingColorName =  "Gray";
        String defaultColorName = "DarkGray";
        int  duration = 3600;
        
        NamedColorUseSet namedColorUseSet = 
            new NamedColorUseSet("PRECIP_ACCUM", "1 Hour Precip Accumulation in inches", 
                    thresholdValueArray, colorNameArray,
                    missingColorName, defaultColorName, duration);
              
        return namedColorUseSet;
    }
    
    
    public static NamedColorUseSet getNamedColorUseSetForBias()
    {
        /*
        public NamedColorUseSet(final String color_use_db_name,
                final String color_use_display_string,
                final double threshold_values[], final String color_names[],
                final String missing_color_name, final String default_color_name,
                int default_duration)
        */
    
        //double[] thresholdValueArray = {-7.5, -5.0, -1.5, -1.0, -0.75, -0.6, -0.4, -0.2, -0.1, -0.01, 
        //								0.0, .25, 0.5, 0.9, 1.1, 2.0, 3.0};
        
        double[] thresholdValueArray = {0.0, 0.05, 0.1, .2, .4, 0.8, 1.2, 2.0, 3.0, 4.0, 5.0, 8.0, 16.0, 32.0, 100.0};
        String[] colorNameArray = {"DarkGray", "Gray", "Light Gray", "DarkBlue", "Blue", "DarkCyan", "Cyan",  "DarkGreen", "Green",
                "Yellow", "Orange", "Red", "Dark Red", "Magenta", "White" };
        
       // String[] colorNameArray = {"DarkBlue", "Blue", "Cyan", "DarkGreen", "Green", "Yellow", "Orange", "Red", "Magenta", "White"};
        
       // String[] colorNameArray = {"White", "Magenta",  "Red", "Orange",  "Yellow",   "Green", "DarkGreen", "Cyan", "Blue", "DarkBlue",  
       //         				    "Black", "DarkCyan", "DarkMagenta", "Yellow", "DarkGoldenrod", "Red", "DarkRed"};
        							// "Black", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow"};
        
        String missingColorName = "DarkGray";
        String defaultColorName = "Gray";
        int  duration = 3600;
        
        NamedColorUseSet namedColorUseSet = 
            new NamedColorUseSet("PRECIP_BIAS", "Grid Bias Legend", 
                    thresholdValueArray, colorNameArray,
                    missingColorName, defaultColorName, duration);
              
        return namedColorUseSet;
    }
    
 
    
}
