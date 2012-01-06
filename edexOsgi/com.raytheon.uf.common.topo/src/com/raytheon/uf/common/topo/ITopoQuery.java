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
package com.raytheon.uf.common.topo;

import org.geotools.coverage.grid.GridGeometry2D;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for topo query
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface ITopoQuery {
    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinate.
     * 
     * @param coord
     *            should contain lon/lat in degrees
     * @return
     */
    public double getHeight(Coordinate coord);

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinates.
     * 
     * @param coords
     *            should contain lon/lat in degrees
     * @return
     */
    public double[] getHeight(Coordinate[] coords);

    /**
     * Retrieves topo height in meters above mean sea level reprojected and
     * interpolated to the specified grid geometry.
     * 
     * @param targetGeom
     * @return the topo data array in row major order
     */
    public float[] getHeight(GridGeometry2D targetGeom);
}
