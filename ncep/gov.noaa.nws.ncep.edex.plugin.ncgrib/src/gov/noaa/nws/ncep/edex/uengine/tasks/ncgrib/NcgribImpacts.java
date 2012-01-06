/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
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

package gov.noaa.nws.ncep.edex.uengine.tasks.ncgrib;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * GribImpacts task derived from original uEngine GribImpacts task.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 12, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class NcgribImpacts extends ScriptTask {

    private CoordinateReferenceSystem crs;

    private GridGeometry2D geom;

    private float[] gribData;

    private int minValue = -1;

    private List<Integer> keys = new ArrayList<Integer>();

    public NcgribImpacts(float[] aGribData, GridGeometry2D aGeometry,
            CoordinateReferenceSystem aCrs) {
        gribData = aGribData;
        geom = aGeometry;
        crs = aCrs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        Coordinate[] coords = null;
        GeometryFactory factory = new GeometryFactory();
        Map<Integer, Geometry> polygons = new HashMap<Integer, Geometry>();

        // set up the transform from grid coordinates to lon/lat

        MathTransform fullTransform = null;
        try {
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            fullTransform = dmtf.createConcatenatedTransform(
                    geom.getGridToCRS(), MapUtil.getTransformToLatLon(crs));

        } catch (FactoryException e) {
            logger.error("Unable to create MathTransform instances - "
                    + "cannot transform from GRID space to Lat/Lon", e);
            throw new MicroEngineException(
                    "Unable to create MathTransform instances - cannot transform from GRID space to Lat/Lon",
                    e);
        }

        // determine the grid width and length
        int nx = geom.getGridRange().getSpan(0);
        int ny = geom.getGridRange().getSpan(1);

        // verify that the grib data length matches the
        // grid dimensions
        if (gribData.length != (nx * ny)) {
            throw new MicroEngineException("Data is corrupt - dx: " + nx
                    + ", dy:" + ny + ", size:" + gribData.length);
        }

        // create an array to hold the max number of verices for a row
        double[] xyCoord = new double[4 * (nx + 1)];

        int gribIdx = 0; // index of the first cell in the current row
        int v; // value of the current cell
        int n; // number of adjacent cells with the same value
        int x; // x coordinate of the first cell in the current string

        // for each row in the grid
        for (int y = ny - 1; y >= 0; --y) {
            // set x to the first cell in the row
            x = 0;

            // loop over the cells in the row
            do { // while x < nx

                // skip over cells with vales < the minValue
                do {
                    v = (int) gribData[gribIdx + x];
                    if (v >= minValue) {
                        break;
                    }
                    x++;
                } while (x < nx);

                // if we're at the end of the row get out of this loop
                if (x >= nx) {
                    break;
                }

                // count up adjacent cells with the same value
                n = 1;
                while ((x + n < nx) && (v == (int) gribData[gribIdx + x + n])) {
                    n++;
                }

                // compute the grid coordinates of the vertices surrounding
                // this string of adjacent cells
                for (int j = 0; j <= n; j++) {
                    xyCoord[4 * j] = x + j - .5;
                    xyCoord[4 * j + 1] = y + .5;

                    xyCoord[4 * j + 2] = x + j - .5;
                    xyCoord[4 * j + 3] = y - .5;
                }

                // move x to the next cell after the current string
                x += n;

                // transform the grid coordinates to lon/lat
                try {
                    fullTransform
                            .transform(xyCoord, 0, xyCoord, 0, 2 * (n + 1));
                } catch (TransformException e) {
                    logger.error("Unable to create a polygon", e);
                    throw new MicroEngineException(
                            "Unable to create a polygon", e);
                }

                // convert the lon/lats to coordinates
                int m = 2 * (n + 1);
                coords = new Coordinate[m + 1];
                for (int j = 0; j <= n; j++) {
                    coords[j] = new Coordinate(xyCoord[4 * j],
                            xyCoord[4 * j + 1]);
                    coords[m - j - 1] = new Coordinate(xyCoord[4 * j + 2],
                            xyCoord[4 * j + 3]);
                }
                // duplicate the first point as the last to close the
                // polygon
                coords[m] = new Coordinate(coords[0]);

                // create a coordinate sequence
                CoordinateSequence cs = new CoordinateArraySequence(coords);

                // create a polygon and add it to the list
                LinearRing linearRing = new LinearRing(cs, factory);
                Polygon polygon = new Polygon(linearRing, null, factory);

                // see if we already have a polygon for this cell value
                Geometry g = polygons.get(v);
                if (g == null) {
                    // if not then add a new polygon
                    polygons.put(v, polygon);
                } else {
                    // if so then merge this polygon with the existing one
                    g = g.union(polygon);
                    polygons.put(v, g);
                }
            } while (x < nx);

            // increment the grib data index to the next row
            gribIdx += nx;
        }

        // create a geometry collection from the polygon map
        // and put it in the chain data
        GeometryCollection collection = new GeometryCollection(polygons
                .values().toArray(new Geometry[] {}), factory);

        // create a map of attribute names/values
        keys = new ArrayList<Integer>(polygons.keySet());
        return collection;
    }

    public CoordinateReferenceSystem getCrs() {
        return crs;
    }

    public void setCrs(CoordinateReferenceSystem aCrs) {
        crs = aCrs;
    }

    public GridGeometry2D getGeom() {
        return geom;
    }

    public void setGeom(GridGeometry2D aGeom) {
        geom = aGeom;
    }

    public float[] getGribData() {
        return gribData;
    }

    public void setGribData(float[] aGribData) {
        gribData = aGribData;
    }

    public int getMinValue() {
        return minValue;
    }

    public void setMinValue(int aMinValue) {
        minValue = aMinValue;
    }

    public List<Integer> getKeys() {
        return keys;
    }

}
