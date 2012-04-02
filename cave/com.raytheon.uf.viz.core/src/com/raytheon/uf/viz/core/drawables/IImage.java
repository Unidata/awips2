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

import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Describes a generic Image resource. The IImage resource is an interface
 * handle to an image. The image resource manages the lifecycle of the
 * underlying image.
 * 
 * <P>
 * At its lowest level, the image is simply a handle to the resource (either an
 * image file or an java.awt.BufferedImage). Depending on the target mechanism,
 * the image may need to be loaded into memory or staged in graphics memory.
 * 
 * <P>
 * The image resource is abstract and should be implemented in accordance with
 * the particular target (e.g. see GLTarget).
 * 
 * <P>
 * Typical method invocation:
 * <OL>
 * <LI>The specific constructor of the implementation is called
 * <LI>When the image is to be prepared, the target(IGraphicsTarget) method is
 * called
 * <LI>The image is drawn by calling IGraphicsTarget.paint(Image, ...);
 * </OL>
 * 
 * @see com.raytheon.uf.viz.core.IGraphicsTarget <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public interface IImage {

    /** Enumeration: is loading, staged in memory, fully loaded or unloaded */
    public static enum Status {
        UNLOADED, STAGED, LOADED, LOADING, FAILED, INVALID
    };

    /**
     * Stages any data required for the image to load/draw
     */
    public abstract void stage() throws VizException;

    /**
     * @return the status
     */
    public abstract Status getStatus();

    /**
     * @param isInterpolated
     *            the isInterpolated to set
     */
    public abstract void setInterpolated(boolean isInterpolated);

    /**
     * Release the image resources
     * 
     */
    public abstract void dispose();

    /**
     * Get the width of the image
     * 
     * @return the image width in pixels
     */
    public abstract int getWidth();

    /**
     * Get the height of the image
     * 
     * @return the image height in pixels
     */
    public abstract int getHeight();

    /**
     * @param brightness
     *            the brightness to set
     */
    public abstract void setBrightness(float brightness);

    /**
     * @param contrast
     *            the contrast to set
     */
    public abstract void setContrast(float contrast);

    /**
     * Gets the extension class for this image
     * 
     * @return
     */
    public abstract Class<? extends IImagingExtension> getExtensionClass();
}