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
package com.raytheon.uf.viz.core.drawables;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;

/**
 * Describes a Colormapped Image
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 13, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IColormappedImage extends IImage {

    /**
     * 
     * @return the colormap parameters
     */
    public abstract ColorMapParameters getColorMapParameters();

    /**
     * Set the colormap parameters
     * 
     * @param params
     *            the parameters to set
     */
    public abstract void setColorMapParameters(ColorMapParameters params);

    /**
     * Get the pixel value of the staged data
     * 
     * @param x
     *            the x coordinate
     * @param y
     *            the y coordinate
     * 
     * @return the value
     */
    public abstract double getValue(int x, int y);

    /**
     * Get the unit associated with the data in the image. Values returned from
     * {@link #getValue(int, int)} will be in this unit
     * 
     * @return
     */
    public abstract Unit<?> getDataUnit();

}