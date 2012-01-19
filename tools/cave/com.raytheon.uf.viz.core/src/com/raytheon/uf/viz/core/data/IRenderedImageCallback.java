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
package com.raytheon.uf.viz.core.data;

import java.awt.image.RenderedImage;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Callback interface for initializing a RenderedImage object from scratch.
 * Implementing classes should create RenderedImage from scratch when getImage
 * is called and NOT keep in memory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRenderedImageCallback {

    /**
     * Get the rendered image. IMPORTANT NOTE: This method should retrieve the
     * renderedImage from wherever it lives. RenderedImages should not be stored
     * as member variables of the classes implementing this interface.
     * 
     * @return
     */
    public RenderedImage getImage() throws VizException;

}
