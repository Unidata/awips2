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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * This class contains all of the rectangle layout coordinates.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class LayoutRectangles
{
    /**
     * Width of a small rectangle.
     */
    private final int smRecWidth = 60;    
    
    /**
     * Width of a large rectangle.
     */
    private final int lrgRecWidth = 100;    
    
    /**
     * Rectangle height.
     */
    private final int recHeight = 20;
    
    /**
     * Row 1 Y coordinate.
     */
    private final int row1YCoord = 30;
    
    /**
     * Row 2 Y coordinate.
     */
    private final int row2YCoord = 70;
    
    /**
     * Row 3 Y coordinate.
     */
    private final int row3YCoord = 110;
    
    /**
     * Row 4 Y coordinate.
     */
    private final int row4YCoord = 150;
    
    /**
     * Array of rectangles that make up the Quad 4 layout.
     */
    private ArrayList<Rectangle> q4;
    
    /**
     * Array of rectangles that make up the horizontal 1 text control layout.
     */
    private ArrayList<Rectangle> h1;
    
    /**
     * Array of rectangles that make up the horizontal 2 text controls layout.
     */
    private ArrayList<Rectangle> h2;
    
    /**
     * Array of rectangles that make up the vertical 2 text controls layout.
     */
    private ArrayList<Rectangle> v2;
    
    /**
     * Array of rectangles that make up the vertical 3 text controls layout.
     */
    private ArrayList<Rectangle> v3;
    
    /**
     * Array of rectangles that make up the vertical 4 text controls layout.
     */
    private ArrayList<Rectangle> v4;
    
    /**
     * Constructor.
     */
    public LayoutRectangles()
    {       
        createRectangles();
    }
    
    /**
     * Create the rectangles.
     */
    private void createRectangles()
    {        
        createQ4Rectangles();
        createH1Rectangles();
        createH2Rectangles();
        createV2Rectangles();
        createV3Rectangles();
        createV4Rectangles();
    }
    
    /**
     * Create the Quad 4 rectangles.
     */
    private void createQ4Rectangles()
    {
        q4 = new ArrayList<Rectangle>();
        
        q4.add(new Rectangle(20, row1YCoord, smRecWidth, recHeight));
        q4.add(new Rectangle(100, row1YCoord, smRecWidth, recHeight));
        q4.add(new Rectangle(20, row2YCoord, smRecWidth, recHeight));
        q4.add(new Rectangle(100, row2YCoord, smRecWidth, recHeight));
    }
    
    /**
     * Create the horizontal 1 text control rectangle.
     */
    private void createH1Rectangles()
    {
        h1 = new ArrayList<Rectangle>();
        h1.add(new Rectangle(40, row1YCoord, lrgRecWidth, recHeight));
    }
    
    /**
     * Create the horizontal 2 text controls rectangles.
     */
    private void createH2Rectangles()
    {
        h2 = new ArrayList<Rectangle>();
        h2.add(new Rectangle(20, row1YCoord, smRecWidth, recHeight));
        h2.add(new Rectangle(100, row1YCoord, smRecWidth, recHeight));
    }
    
    /**
     * Create the vertical 2 text controls rectangles.
     */
    private void createV2Rectangles()
    {
        v2 = new ArrayList<Rectangle>();
        v2.add(new Rectangle(40, row1YCoord, lrgRecWidth, recHeight));
        v2.add(new Rectangle(40, row2YCoord, lrgRecWidth, recHeight));
    }
    
    /**
     * Create the vertical 3 text controls rectangles.
     */
    private void createV3Rectangles()
    {
        v3 = new ArrayList<Rectangle>();
        v3.add(new Rectangle(40, row1YCoord, lrgRecWidth, recHeight));
        v3.add(new Rectangle(40, row2YCoord, lrgRecWidth, recHeight));
        v3.add(new Rectangle(40, row3YCoord, lrgRecWidth, recHeight));
    }
    
    /**
     * Create the vertical 4 text controls rectangles.
     */
    private void createV4Rectangles()
    {
        v4 = new ArrayList<Rectangle>();
        v4.add(new Rectangle(40, row1YCoord, lrgRecWidth, recHeight));
        v4.add(new Rectangle(40, row2YCoord, lrgRecWidth, recHeight));
        v4.add(new Rectangle(40, row3YCoord, lrgRecWidth, recHeight));
        v4.add(new Rectangle(40, row4YCoord, lrgRecWidth, recHeight));
    }
    
    /**
     * Get the rectangles associated with the selected mode.
     * @param m Selected mode.
     * @return Array of rectangles.
     */
    public ArrayList<Rectangle> getRectangles(TrayConfiguration.TrayMode m)
    {
        switch(m)
        {
        case Q4:
            return q4;
            
        case H1:
            return h1;
            
        case H2:
            return h2;
            
        case V2:
            return v2;
            
        case V3:
            return v3;
            
        case V4:
            return v4;            
        }
        
        return null;
    }

    /**
     * Get the row 1 Y coordinate.
     * @return The y coordinate.
     */
    public int getRow1YCoord()
    {
        return row1YCoord;
    }

    /**
     * Get the row 2 Y coordinate.
     * @return The y coordinate.
     */
    public int getRow2YCoord()
    {
        return row2YCoord;
    }

    /**
     * Get the row 3 Y coordinate.
     * @return The y coordinate.
     */
    public int getRow3YCoord()
    {
        return row3YCoord;
    }

    /**
     * Get the row 4 Y coordinate.
     * @return The y coordinate.
     */
    public int getRow4YCoord()
    {
        return row4YCoord;
    }
}
