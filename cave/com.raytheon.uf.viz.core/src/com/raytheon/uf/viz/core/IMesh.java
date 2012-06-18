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

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Base for any mesh 2D/3D, Quad/Triangle -- etc. See {@link PixelCoverage} /
 * {@link DrawableImage}
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

public interface IMesh {

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

    /**
     * use clone instead, this method will be removed in future version.
     * 
     * @param targetGeometry
     * @return
     * @throws VizException
     */
    @Deprecated
    public IMesh reproject(GeneralGridGeometry targetGeometry)
            throws VizException;

    /**
     * Create a mesh identical to this mesh but for a different target geometry.
     * If this mesh is no longer in use than dispose should be called after
     * cloning.
     * 
     * @param targetGeometry
     * @return
     * @throws VizException
     */
    public IMesh clone(GeneralGridGeometry targetGeometry) throws VizException;
}
