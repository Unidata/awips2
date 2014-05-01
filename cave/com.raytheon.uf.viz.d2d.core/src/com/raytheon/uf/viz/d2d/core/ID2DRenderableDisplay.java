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
package com.raytheon.uf.viz.d2d.core;

import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.map.D2DColorBarResource;
import com.raytheon.uf.viz.d2d.core.map.D2DSelectedPaneResource;
import com.raytheon.uf.viz.d2d.core.sampling.D2DSamplingResource;

/**
 * Common interface for D2D renderable displays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface ID2DRenderableDisplay {

    public static final GenericResourceData colorBarRscData = new GenericResourceData(
            D2DColorBarResource.class);

    public static final GenericResourceData legendRscData = new GenericResourceData(
            D2DLegendResource.class);

    public static final GenericResourceData selectedRscData = new GenericResourceData(
            D2DSelectedPaneResource.class);

    public static final GenericResourceData samplingRscData = new GenericResourceData(
            D2DSamplingResource.class);

    /**
     * Get the magnification
     * 
     * @return
     */
    public double getMagnification();

    /**
     * Get the density
     * 
     * @return
     */
    public double getDensity();

    /**
     * Set the magnification
     * 
     * @param magnification
     */
    public void setMagnification(double magnification);

    /**
     * Set the density
     * 
     * @param density
     */
    public void setDensity(double density);

    /**
     * Get the scale
     * 
     * @return
     */
    public String getScale();

    /**
     * Set the scale
     * 
     * @param scale
     */
    public void setScale(String scale);

}
