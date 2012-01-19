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
package com.raytheon.viz.hydrocommon.resource;

import java.awt.Rectangle;

import org.eclipse.swt.graphics.RGB;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * HydroView and MPE HRAP overlay resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2010 1783       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HRAPOverlayResource extends
        AbstractVizResource<HRAPOverlayResourceData, MapDescriptor> {
    private Rectangle hrapExtent;

    private IWireframeShape grid = null;

    protected HRAPOverlayResource(HRAPOverlayResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /**
     * Paint the grid on the map.
     * 
     * @param target
     *            The IGraphicsTarget
     */
    private void paintGrid(IGraphicsTarget target) {
        if ((grid != null)
                && (getCapability(OutlineCapability.class).isOutlineOn())) {
            try {
                target.drawWireframeShape(grid, new RGB(255, 255, 255),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(),
                        getCapability(OutlineCapability.class).getLineStyle());
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Load the IWireframeShape
     * 
     * @param target
     *            The IGraphicsTarget
     * @return The IWireframeShape
     */
    private IWireframeShape loadGrid(IGraphicsTarget target) {
        int x1;
        int y1;
        int x2;
        int y2;

        if (hrapExtent == null) {
            try {
                hrapExtent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e) {
                e.printStackTrace();
                return null;
            }
        }
        IWireframeShape grid = target.createWireframeShape(false, descriptor);

        HRAP hrap = HRAP.getInstance();
        try {
            HRAPSubGrid subGrid = hrap.getHRAPSubGrid(hrapExtent);
            int minX = subGrid.getExtent().x;
            int minY = subGrid.getExtent().y;
            int nx = subGrid.getNx();
            int ny = subGrid.getNy();
            int maxX = minX + nx;
            int maxY = minY + ny;

            // Vertical Lines
            for (int i = 0; i < nx + 1; i++) {
                x1 = minX + i;
                y1 = minY;
                x2 = minX + i;
                y2 = maxY;
                Coordinate[] ca = new Coordinate[2];
                ca[0] = new Coordinate(x1, y1);
                ca[0] = hrap.gridCoordinateToLatLon(ca[0],
                        PixelOrientation.CENTER);
                ca[1] = new Coordinate(x2, y2);
                ca[1] = hrap.gridCoordinateToLatLon(ca[1],
                        PixelOrientation.CENTER);
                grid.addLineSegment(ca);
            }

            // Horizontal lines
            for (int i = 0; i < ny + 1; i++) {
                x1 = minX;
                y1 = minY + i;
                x2 = maxX;
                y2 = minY + i;

                Coordinate[] ca = new Coordinate[2];
                ca[0] = new Coordinate(x1, y1);
                ca[0] = hrap.gridCoordinateToLatLon(ca[0],
                        PixelOrientation.CENTER);
                ca[1] = new Coordinate(x2, y2);
                ca[1] = hrap.gridCoordinateToLatLon(ca[1],
                        PixelOrientation.CENTER);
                grid.addLineSegment(ca);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return grid;
    }

    @Override
    protected void disposeInternal() {
        if (grid != null) {
            grid.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        grid = loadGrid(target);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        paintGrid(target);
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        recycle();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return resourceData.getMapName();
    }
}
