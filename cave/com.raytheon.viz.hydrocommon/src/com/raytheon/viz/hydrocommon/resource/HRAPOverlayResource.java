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

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.TransformFactory;
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

    private IWireframeShape grid;

    protected HRAPOverlayResource(HRAPOverlayResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void disposeInternal() {
        if (grid != null) {
            grid.dispose();
            grid = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        grid = target.createWireframeShape(false, descriptor);
        try {
            GridGeometry2D gridGeometry = MapUtil
                    .getGridGeometry(new HRAPSubGrid(HRAPCoordinates
                            .getHRAPCoordinates()));
            MathTransform mt = TransformFactory.gridCellToGridCell(
                    gridGeometry, PixelInCell.CELL_CORNER,
                    descriptor.getGridGeometry(), PixelInCell.CELL_CENTER);
            GridEnvelope ge = gridGeometry.getGridRange();
            int minX = ge.getLow(0);
            int width = ge.getSpan(0) + 1;
            int minY = ge.getLow(1);
            int height = ge.getSpan(1) + 1;

            for (int x = 0; x < width; ++x) {
                double[][] line = new double[height][];
                for (int y = 0; y < height; ++y) {
                    double[] out = new double[2];
                    mt.transform(new double[] { minX + x, minY + y }, 0, out,
                            0, 1);
                    line[y] = out;
                }
                grid.addLineSegment(line);
            }

            for (int y = 0; y < height; ++y) {
                double[][] line = new double[height][];
                for (int x = 0; x < width; ++x) {
                    double[] out = new double[2];
                    mt.transform(new double[] { minX + x, minY + y }, 0, out,
                            0, 1);
                    line[x] = out;
                }
                grid.addLineSegment(line);
            }

            grid.compile();
        } catch (Exception e) {
            throw new VizException("Error creating HRAP Overlay", e);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (grid != null) {
            OutlineCapability outline = getCapability(OutlineCapability.class);
            if (outline.isOutlineOn()) {
                target.drawWireframeShape(grid, new RGB(255, 255, 255),
                        outline.getOutlineWidth(), outline.getLineStyle());
            }
        }
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
