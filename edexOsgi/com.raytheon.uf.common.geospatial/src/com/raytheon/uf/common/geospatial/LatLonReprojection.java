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
package com.raytheon.uf.common.geospatial;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Convert a {@link GeneralGridGeometry} to Lat/Lon projection, with methods for
 * retrieving just the lats and just the lons
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LatLonReprojection {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LatLonReprojection.class);

    /**
     * Take a {@link GeneralGridGeometry} and reproject it to lat/lon space
     * 
     * @param source
     * @return float[] of all lat/lon points
     */
    private static float[] reproject(GeneralGridGeometry source) {
        MathTransform gridToCRS = source.getGridToCRS(PixelInCell.CELL_CENTER);
        DefaultMathTransformFactory mtf = new DefaultMathTransformFactory();

        int sourceNx = source.getGridRange().getSpan(0);
        int sourceNy = source.getGridRange().getSpan(1);

        float[] transformTable = new float[sourceNx * sourceNy * 2];
        try {
            // create a concatenated transform with the one above and to
            // lat/lon
            MathTransform finalTransform = null;
            finalTransform = mtf.createConcatenatedTransform(gridToCRS,
                    MapUtil.getTransformToLatLon(source
                            .getCoordinateReferenceSystem()));
            int index = 0;
            for (int j = 0; j < sourceNy; j++) {
                for (int i = 0; i < sourceNx; i++) {
                    transformTable[index++] = i;
                    transformTable[index++] = j;
                }
            }
            finalTransform.transform(transformTable, 0, transformTable, 0,
                    sourceNx * sourceNy);
        } catch (ProjectionException e) {
            // do nothing
        } catch (TransformException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to transform to Lat/Lon projection", e);
        } catch (InvalidGridGeometryException e) {
            statusHandler.handle(Priority.ERROR, "Grid geometry is invalid", e);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return transformTable;
    }

    /**
     * Get the latitudes as an array after being reprojected
     * 
     * @param source
     * @return
     */
    public static LatLonWrapper getLatLons(GeneralGridGeometry source) {
        float[] latlons = reproject(source);
        float[] lats = new float[latlons.length / 2];
        float[] lons = new float[latlons.length / 2];

        for (int i = 0; i < lats.length; i++) {
            int index = i * 2;
            lons[i] = latlons[index];
            lats[i] = latlons[index + 1];
        }

        LatLonWrapper wrapper = new LatLonWrapper(lats, lons);
        return wrapper;
    }
}
