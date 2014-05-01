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
package com.raytheon.uf.common.colormap;

import java.util.List;

/**
 * Defines a colormap interface
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 5, 2007              chammack    Initial Creation.
 * Jul 24, 2007             njensen     Moved to EDEX.
 * Aug 20, 2008			    dglazesk	Altered the interface to include getColors
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IColorMap {

    /**
     * Get the size of the colormap
     * 
     * @return the number of discrete colors in the map
     */
    public abstract int getSize();

    /**
     * Get the red components
     * 
     * @return
     */
    public abstract float[] getRed();

    /**
     * Get the blue components
     * 
     * @return
     */
    public abstract float[] getBlue();

    /**
     * Get the green components
     * 
     * @return
     */
    public abstract float[] getGreen();

    /**
     * Get the alpha components
     * 
     * @return
     */
    public abstract float[] getAlpha();

    /**
     * Get the list of colors
     * 
     * @return
     */
    public abstract List<Color> getColors();

    /**
     * Get the name of the colormap
     * 
     * @return colormap name
     */
    public abstract String getName();

    /**
     * Sets the red components
     * 
     * @param aRed
     */
    public abstract void setRed(float[] aRed);

    /**
     * Sets the green components
     * 
     * @param aGreen
     */
    public abstract void setGreen(float[] aGreen);

    /**
     * Sets the blue components
     * 
     * @param aBlue
     */
    public abstract void setBlue(float[] aBlue);

    /**
     * Sets the alpha components
     * 
     * @param anAlpha
     */
    public abstract void setAlpha(float[] anAlpha);

    /**
     * Gets if the colormap has changed
     * 
     * @return
     */
    public abstract boolean isChanged();

    /**
     * Sets if the color map has changed (i.e. needs reloaded)
     * 
     * @param aChanged
     */
    public abstract void setChanged(boolean aChanged);

    /**
     * The colormap was renamed
     * 
     * @param name
     */
    public void setName(String name);

    /**
     * Clone the colormap
     * 
     * @return
     */
    public abstract IColorMap clone();
}
