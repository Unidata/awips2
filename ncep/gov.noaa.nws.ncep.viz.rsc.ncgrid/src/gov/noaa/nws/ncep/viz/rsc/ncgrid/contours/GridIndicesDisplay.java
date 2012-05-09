package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display grid Indices
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April, 2011              M. Li     	Initial creation
 * Nov 02,2011              X. Guo      Used coverage to get nx/ny
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class GridIndicesDisplay implements IRenderable {

    private final IMapDescriptor descriptor;

    private final GeneralGridGeometry gridGeometryOfGrid;

    private final int[] gridDims;

    private RGB gridIndiceColor;

    public GridIndicesDisplay(RGB color, IMapDescriptor descriptor,
            ISpatialObject gridLocation) {

        this.descriptor = descriptor;
        this.gridGeometryOfGrid = MapUtil.getGridGeometry(gridLocation);
        this.gridDims = new int[] { gridLocation.getNx(), gridLocation.getNy() };
        this.gridIndiceColor = color;
    }

    @SuppressWarnings("deprecation")
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        /*
         * Calculate center point
         */
        IExtent screenExtentInPixels = paintProps.getView().getExtent();
        double ratio = screenExtentInPixels.getWidth()
                / paintProps.getCanvasBounds().width;
        int interval0 = (int) (10 * ratio / (Math.min(2.0, 1.0 /*
                                                                * paintProps.
                                                                * getDensity()
                                                                */)));
        // int interv = (int) ((interval0 + 15) / 15.0);
        int interv = (int) Math.log(interval0);
        if (interv < 0)
            interv = 0;
        if (interv > 4)
            interv = 4;

        double centerX = 0.5 * (screenExtentInPixels.getMinX() + screenExtentInPixels
                .getMaxX());
        double centerY = 0.5 * (screenExtentInPixels.getMinY() + screenExtentInPixels
                .getMaxY());

        int centerI = 0;
        int centerJ = 0;
        double distance = 100000.0;
        for (int i = 0; i < gridDims[0]; i++) {
            for (int j = 0; j < gridDims[1]; j++) {
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate(i, j), gridGeometryOfGrid,
                        Type.GRID_CENTER);

                try {
                    double dx = (centerX - c.asPixel(descriptor
                            .getGridGeometry()).x);
                    double dy = (centerY - c.asPixel(descriptor
                            .getGridGeometry()).y);
                    double dist = Math.sqrt(dx * dx + dy * dy);

                    if (dist < distance) {
                        distance = dist;
                        centerI = i;
                        centerJ = j;
                    }

                } catch (TransformException e) {
                    continue;
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
        }

        /*
         * Draw grid indices
         */
        for (int i = 0; i < gridDims[0]; i += interv + 1) {
            ReferencedCoordinate c = new ReferencedCoordinate(new Coordinate(i,
                    centerJ), gridGeometryOfGrid, Type.GRID_CENTER);

            try {
                double[] d = this.descriptor.worldToPixel(new double[] {
                        (double) c.asLatLon().x, (double) c.asLatLon().y });

                if (!screenExtentInPixels.contains(d)) {
                    continue;
                }

                String indiceText = String.valueOf(i);
                try {
                    target.drawString(null, indiceText, d[0], d[1], 0.0,
                            IGraphicsTarget.TextStyle.NORMAL, gridIndiceColor,
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, null);
                } catch (VizException e) {
                    e.printStackTrace();
                }

            } catch (TransformException e) {
                e.printStackTrace();
            } catch (FactoryException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < gridDims[1]; i += interv + 1) {
            ReferencedCoordinate c = new ReferencedCoordinate(new Coordinate(
                    centerI, i), gridGeometryOfGrid, Type.GRID_CENTER);

            try {
                if (i == 0 && c.asLatLon().y == 90)
                    c.asLatLon().y = 89.98;
                if (i == gridDims[1] - 1 && c.asLatLon().y == -90)
                    c.asLatLon().y = -89.98;
                // if (c.asLatLon().x == 360) c.asLatLon().x = 359.98;

                double[] d = this.descriptor.worldToPixel(new double[] {
                        (double) c.asLatLon().x, (double) c.asLatLon().y });

                if (d == null)
                    continue;

                if (!screenExtentInPixels.contains(d)) {
                    continue;
                }

                String indiceText = String.valueOf(i);
                try {
                    target.drawString(null, indiceText, d[0], d[1], 0.0,
                            IGraphicsTarget.TextStyle.NORMAL, gridIndiceColor,
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, null);
                } catch (VizException e) {
                    e.printStackTrace();
                }

            } catch (TransformException e) {
                e.printStackTrace();
            } catch (FactoryException e) {
                e.printStackTrace();
            }
        }

    }
}
