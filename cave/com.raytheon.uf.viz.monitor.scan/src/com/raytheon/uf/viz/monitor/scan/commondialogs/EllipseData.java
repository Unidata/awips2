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

import org.eclipse.swt.graphics.Rectangle;

/**
 * Class that calculates the ellipse dimensions for drawing the time-height
 * circulation ellipses.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2010            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0	
 */
public class EllipseData
{
    /**
     * Ellipse type enum
     */
    public static enum EllipseType {E_6mn, E_3mn, E_0_5};
    
    /**
     * Ellipse height
     */
    private int ellipseHeight = 20;
    
    /**
     * Minimum ellipse width.
     */
    private float minWidth = 25.0f;
    
    /**
     * Maximum ellipse width.
     */
    private float maxWidth = 75.0f;
    
    /**
     * Pixel per increment from 0.5 to 10.0.
     */
    private float pixInc = 10.0f /(maxWidth - minWidth);
    
    /**
     * Constructor.
     */
    public EllipseData()
    {
    }
    
    /**
     * Calculate the ellipse width based on the value passed in.
     * @param value Value of the low level diameter.
     * @return Ellipse width.
     */
    private int calcEllipseWidth(Double value)
    {
        return (int)Math.round(minWidth + (value/pixInc));
    }
    
    /**
     * Get the value associated with the ellipse type.
     * @param type Ellipse type.
     * @return Value of the low level diameter.
     */
    public double getValueForType(EllipseType type)
    {
        if (type == EllipseType.E_0_5)
        {
            return 0.5;
        }
        else if (type == EllipseType.E_3mn)
        {
            return 3.0;
        }
        else if (type == EllipseType.E_6mn)
        {
            return 6.0;
        }
        
        return 0.0;
    }
    
    /**
     * Get the dimensions of the ellipse based on the center coordinate and the value.
     * @param centerX Center X coord.
     * @param centerY Center Y coord.
     * @param value Value of the low level diameter.
     * @return Rectangle (dimensions).
     */
    public Rectangle getEllipseDrawData(int centerX, int centerY, Double value)
    {
        int x = 0;
        int y = 0;
        int width = 0;
        
        if (value < 0.5)
        {
            width = (int)minWidth;
        }
        else if (value > 10.0)
        {
            width = (int)maxWidth;
        }
        else
        {
            width = calcEllipseWidth(value);
        }
        
        x = centerX - (int)Math.round(width/2.0f);
        
        y = centerY - (ellipseHeight / 2);
        
        Rectangle dataRec = new Rectangle(x, y, width, ellipseHeight);       
        
        return dataRec;
    }
    
    /**
     * Get the middle width value of the ellipse. 
     * @param type Ellipse type.
     * @return Middle width value of the ellipse.
     */
    public int getEllipseMiddleWidth(EllipseType type)
    {
        double value = getValueForType(type);
        
        int middleWidth = calcEllipseWidth(value);
        
        return (int)Math.round(middleWidth/2.0);
    }
    
    /**
     * Get the width of the ellipse based on the ellipse type.
     * @param type Ellipse type.
     * @return Ellipse width.
     */
    public int getEllipseWidth(EllipseType type)
    {
        double value = getValueForType(type);
        
        int width = calcEllipseWidth(value);
        
        return width;
    }
    
    /**
     * Get the height of the ellipse (one height for all ellipses).
     * @return Ellipse height.
     */
    public int getEllipseHeight()
    {
        return ellipseHeight;
    }
    
    /**
     * Get the X coordinate offset for the specified ellipse type.
     * @param type Ellipse type.
     * @return X offset.
     */
    public int getXOffsetForType(EllipseType type)
    {
        int xOffset = (int)Math.round(getEllipseWidth(type)/2.0);
        xOffset *= -1;
        
        return xOffset;
    }
}
