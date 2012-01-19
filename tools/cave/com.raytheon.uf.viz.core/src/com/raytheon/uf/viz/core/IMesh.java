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
package com.raytheon.uf.viz.core;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;

/**
 * Base for any mesh 2D/3D, Quad/Triangle -- etc
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */

public interface IMesh extends IRenderable {

    /**
     * Calculate all the mesh vertices and texture coordinates
     * 
     * @param pc
     * @param tile
     * @param toLatLon
     *            translate the tile coordinates to lat/lon coords if the tile
     *            envelope is not already
     */
    public abstract void calculateMesh(PixelCoverage pc, ImageTile tile,
            MathTransform toLatLon);

    /**
     * Calculate all the mesh vertices and texture coordinates
     * 
     * @param pc
     * @param gg
     */
    public void calculateMesh(PixelCoverage pc, GridGeometry2D gg);

    /**
     * Dispose of the mesh data
     */
    public void dispose();

    /**
     * Does the mesh intersect the extent
     * 
     * @param extent
     */
    public boolean intersects(IExtent extent);
}
