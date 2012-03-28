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
package com.raytheon.uf.viz.core.map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Graphics extension for creating map based image mesh objections
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IMapMeshExtension extends IGraphicsExtensionInterface {

    /**
     * Constructs a mesh for mapping the imageGeometry onto the targetGeometry
     * 
     * @param imageGeometry
     * @param targetGeometry
     * @return
     * @throws VizException
     */
    public abstract IMesh constructMesh(GridGeometry2D imageGeometry,
            GeneralGridGeometry targetGeometry) throws VizException;

    /**
     * Convenient method for constructing a mesh for mapping the imageGeometry
     * onto the targetDescriptor. Same as calling
     * {@link #constructMesh(GridGeometry2D, GeneralGridGeometry)} passing in
     * target.getGridGeometry()
     * 
     * @param imageGeometry
     * @param targetDescriptor
     * @return
     * @throws VizException
     */
    public abstract IMesh constructMesh(GridGeometry2D imageGeometry,
            IDescriptor targetDescriptor) throws VizException;

}
