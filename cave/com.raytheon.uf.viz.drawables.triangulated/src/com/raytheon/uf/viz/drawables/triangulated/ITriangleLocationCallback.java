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

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Callback interface for getting the location information for an
 * {@link ITriangulatedImage}. NOTE: IT IS HIGHLY RECOMMENDED TO PROPERLY
 * IMPLEMENT hashCode() as well as equals() methods to maximize caching ability
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
public interface ITriangleLocationCallback {

    /**
     * 
     * @return an array of coordinates. The resulting array should have a shape
     *         of [n][2], where n is the number of vertices, which also must be
     *         the buffer length of the {@link ColorMapData} used for the
     *         triangulated images. The coordinates must be in render space.
     * @throws VizException
     *             if the coordinates cannot be loaded.
     */
    public double[][] getCoordinates() throws VizException;

    /**
     * @return the indexes to use into the coordinate and data arrays to form
     *         triangles. Every three ints will form a triangle. This structure
     *         is used so that when multiple triangles share an edge, the
     *         coordinate and data do not need to be repeated, only the indexes
     *         need to be repeated.
     * @throws VizException
     *             if the indices cannot be loaded.
     */
    public int[] getTriangleIndices() throws VizException;

}