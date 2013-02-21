/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.gempak.parameters.marker.MARKER;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;

import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display grid point values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June, 2010    164        M. Li     	Initial creation
 * Aug., 2012    655        B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */

public class GridPointMarkerDisplay implements IRenderable {

    private final IMapDescriptor descriptor;

    private SymbolLocationSet gridPointMarkerSet;

    public GridPointMarkerDisplay(String markerAttr, IMapDescriptor descriptor,
            ISpatialObject gridLocation) {

        this.descriptor = descriptor;

        gridPointMarkerSet = null;

        int nx = gridLocation.getNx();
        int ny = gridLocation.getNy();
        Coordinate[] locations = new Coordinate[nx * ny];
        String markerName = "PLUS_SIGN";
        Color markerColor = new Color(255, 255, 255);
        double markerSize = 1.0;
        float markerWidth = 1.0f;

        if (markerAttr != null) {
            MARKER marker = new MARKER(markerAttr);
            RGB color = marker.getMarkerColor();
            markerColor = new Color(color.red, color.green, color.blue);
            markerName = marker.getMarkerName();
            markerSize = marker.getMarkerSize();
            markerWidth = marker.getMarkerWidth();
        }

        Color[] colors = new Color[] { markerColor };
        GridGeometry2D gridGeom = MapUtil.getGridGeometry(gridLocation);

        int n = 0;
        for (int i = 0; i < nx; i++) {
            for (int j = 0; j < ny; j++) {
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate(i, j), gridGeom, Type.GRID_CENTER);

                try {
                    if (c != null) {
                        double lat = c.asLatLon().y;
                        double lon = c.asLatLon().x;

                        locations[n++] = new Coordinate(lon, lat);
                    }
                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
        }

        gridPointMarkerSet = new SymbolLocationSet(null, colors, markerWidth,
                markerSize, false, locations, "Markers", markerName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (paintProps.isZooming()) {
            return;
        }

        if (gridPointMarkerSet != null) {
            DisplayElementFactory df = new DisplayElementFactory(target,
                    this.descriptor);
            ArrayList<IDisplayable> elements = df.createDisplayElements(
                    gridPointMarkerSet, paintProps);
            for (IDisplayable each : elements) {
                if (each == null)
                    continue;

                each.draw(target, paintProps);
                each.dispose();
            }
        }

    }

}
