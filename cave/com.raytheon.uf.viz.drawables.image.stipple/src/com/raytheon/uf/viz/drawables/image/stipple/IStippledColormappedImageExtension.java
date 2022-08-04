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
package com.raytheon.uf.viz.drawables.image.stipple;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;

/**
 * 
 * This extension allows for creation of images which apply one or more fill
 * patterns to a colormapped image. For more information on how the fill pattern
 * is applied see {@link IStippledColormappedImage}
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 01, 2016  5957     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public interface IStippledColormappedImageExtension extends IImagingExtension {

    /**
     * Initializes an IColormappedImage given the dataCallback and colormap
     * parameters
     * 
     * @param dataCallback
     * @param colorMapParameters
     * @return
     */
    public IStippledColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters);
}
