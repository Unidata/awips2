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
package com.raytheon.viz.core.rsc.displays;

import java.nio.Buffer;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.tile.BufferTileImageCreator;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;

/**
 * 
 * Gridded Buffer object, just a convenience class for thing already using it.
 * Just extends {@link TileSetRenderable} and uses the
 * {@link BufferTileImageCreator} as the {@link TileImageCreator} for the
 * {@link TileSetRenderable}. Also projects on construction as convenience so
 * creators don't have to
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class GriddedImageDisplay2 extends TileSetRenderable {

    /**
     * 
     * @param size
     *            size of imageTile, default to 512
     * @param data
     * @param descriptor
     * @param gridGeometry
     * @param rsc
     * @param viewType
     * @throws VizException
     */
    public GriddedImageDisplay2(int size, Buffer data,
            GridGeometry2D gridGeometry,
            AbstractVizResource<?, ? extends IMapDescriptor> rsc)
            throws VizException {
        super(rsc.getCapability(ImagingCapability.class), gridGeometry,
                new BufferTileImageCreator(data, gridGeometry.getGridRange2D()
                        .getBounds(),
                        rsc.getCapability(ColorMapCapability.class)), 1, size);
        project(rsc.getDescriptor().getGridGeometry());
    }

    /**
     * 
     * @param data
     * @param descriptor
     * @param gridGeometry
     * @param rsc
     * @param viewType
     * @throws VizException
     */
    public GriddedImageDisplay2(Buffer data, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ? extends IMapDescriptor> rsc)
            throws VizException {
        this(512, data, gridGeometry, rsc);
    }

}