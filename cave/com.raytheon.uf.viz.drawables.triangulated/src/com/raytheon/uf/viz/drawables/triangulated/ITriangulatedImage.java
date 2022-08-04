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
package com.raytheon.uf.viz.drawables.triangulated;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Interface for a colormapped image that is composed of multiple triangles with
 * data values at the vertices.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 18, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public interface ITriangulatedImage {

    public void setBrightness(float brightness);

    public void setContrast(float contrast);

    /**
     * Extract an interpolated data value from the triangle that intersects the
     * provided coordinates. The provided coordinate is in render space.
     */
    public abstract double getDataValue(double x, double y);

    /**
     * Prepare the image for rendering. This should be called before the first
     * time the image is painted. This will call use the callbacks provided
     * during construction to retrieve the data and location information and
     * convert it into a format appropriate for the {@link IGraphicsTarget}.
     * This method should be called on a background thread to avoid delays
     * during paint.
     */
    public void stage() throws VizException;

    /**
     * MUST be called when the iamge is no longer going to be used to free up
     * any graphics resourcs.
     */
    public void dispose();

}