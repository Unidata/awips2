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

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class SCANSplash extends Dialog
{
    /**
     * Dialog shell.
     */
    private Shell shell;
    
    /**
     * The display control.
     */
    private Display display;
    
    /**
     * Canvas to display the information.
     */
    private Canvas canvas;
    
    private Font textFont;
    
    /**
     * Canvas width.
     */
    private int canvasWidth = 300;
    
    /**
     * Canvas height.
     */
    private int canvasHeight = 200;
    
    private Image loadImage = null;
    
    public SCANSplash(Shell parent)
    {
        super(parent, 0);
        
        open();
    }
    
    /**
     * Open method to show the dialog.
     * @return Null.
     */
    public Object open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
//        shell = new Shell(parent, SWT.NO_TRIM | SWT.ON_TOP);
        shell = new Shell(parent, SWT.NO_TRIM);
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);
        
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        shell.pack();    
        
        centerOnScreen();
        
        shell.setVisible(true);
        
        return null;
    }
    
    private void initializeComponents()
    {       
        textFont = new Font(display, "Monospace", 50, SWT.BOLD);
        String imageName = loadImage();
        
        if (imageName != null)
        {
             loadImage = new Image(display, imageName);
        }
        
        // Make the calculation for the canvas.
//        makeCalculations();
        
        // Create the canvas.
        createCanvas();
    }
    
    private void createCanvas()
    {                
        canvas = new Canvas(shell, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = canvasHeight;
        gd.widthHint = canvasWidth;        
        
        canvas.setSize(canvasWidth, canvasHeight);
        
        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener()
        {
            public void paintControl(PaintEvent e)
            {                
                drawCanvas(e.gc);
            }
        });
    }
    
    private void drawCanvas(GC gc)
    {                
        gc.setFont(textFont);
        gc.setTextAntialias(SWT.ON);
        
        if (loadImage != null)
        {
            gc.drawImage(loadImage, 0, 0);
        }
        else
        {
            gc.setBackground(display.getSystemColor(SWT.COLOR_BLUE));
            gc.fillRectangle(0, 0, canvasWidth, canvasHeight);
        }
        
        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));        
        gc.setLineWidth(3);
        gc.drawRectangle(1, 1, canvasWidth-3, canvasHeight-3);
    }
    
    /**
     * Center on the screen that CAVE is on.
     */
    private void centerOnScreen()
    {
        Shell tmpShell = getParent().getShell();
        
        Monitor[] monitors = getParent().getDisplay().getMonitors();
        int displayMonitor = 0;
        
        for (int i = 0; i < monitors.length; i++)
        {
            displayMonitor = i;
            int rightEdge = monitors[i].getBounds().x + monitors[i].getBounds().width;
            if (tmpShell.getBounds().x < rightEdge)
            {
                break;
            }
        }
        
        Monitor monitor = monitors[displayMonitor];
        Rectangle monRect = monitor.getBounds();
        
        int xCoord = ((monRect.x + (monRect.width / 2)) - (canvasWidth / 2));
        int yCoord = (monRect.height / 2) - (canvasHeight / 2);
        
        shell.setLocation(xCoord, yCoord);
    }
    
    private String loadImage()
    {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getStaticFile("scan" + File.separatorChar + "images" +
                File.separatorChar + "ScanLoading.png").getAbsolutePath();
        return path;
    }
    
    public void disposeDialog()
    {
        textFont.dispose();
        loadImage.dispose();
        shell.dispose();
    }
}
