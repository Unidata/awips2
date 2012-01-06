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
package com.raytheon.uf.viz.objectiveanalysis.rsc;

import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class OAGridTransformer {

    protected MathTransform latLonToGrid;

    protected GridGeometry2D gridGeom;

    protected int nx;

    protected int ny;

    protected ReferencedEnvelope latLonEnv;

    protected float smoothPts;

    protected ParameterDescription parmDescription;

    public OAGridTransformer(GeneralGridGeometry geom2D,
            CoordinateReferenceSystem crs, int gridSize, float smoothPts)
            throws VizException {

        Envelope envelope = geom2D.getEnvelope();
        double aspect = envelope.getSpan(0) / envelope.getSpan(1);
        nx = (aspect > 1.0 ? (int) Math.round(aspect * gridSize) : gridSize);
        ny = (aspect > 1.0 ? gridSize : (int) Math.round(gridSize / aspect));

        gridGeom = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { nx, ny }, false), envelope);
        try {
            MathTransform latLonToCrs = MapUtil.getTransformFromLatLon(crs);
            MathTransform crsToGrid = gridGeom.getGridToCRS(
                    PixelInCell.CELL_CENTER).inverse();
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            latLonToGrid = dmtf.createConcatenatedTransform(latLonToCrs,
                    crsToGrid);

            ReferencedEnvelope refEnv = new ReferencedEnvelope(envelope);
            latLonEnv = refEnv.transform(MapUtil.getLatLonProjection(), true);

            // TODO: get smoothing distance from some config file
            this.smoothPts = smoothPts;
        } catch (Exception e) {
            throw new VizException("Error preparing Objective Analysis Grid", e);
        }
    }

    public float[] computeGrid(String parameter,
            Map<String, RequestConstraint> constraints, String levelKey)
            throws VizException {
        addLatLonConstraints(constraints);
        String pluginName = constraints.get("pluginName").getConstraintValue();

        String[] parameters = { "latitude", "longitude", parameter };

        PointDataContainer pdc = DataCubeContainer.getPointData(pluginName,
                parameters, levelKey, constraints);
        if (pdc == null) {
            return null;
        }
        parmDescription = pdc.getDescription(parameter);
        float[] grid = getGrid(pdc, parameter);
        if (pdc.getParameters().contains(parameter + "[1]")) {
            float[] dir = getGrid(pdc, parameter + "[1]");
            float[] combined = new float[nx * ny * 2];
            System.arraycopy(grid, 0, combined, 0, nx * ny);
            System.arraycopy(dir, 0, combined, nx * ny, nx * ny);
            return combined;
        }
        return grid;
    }

    private float[] getGrid(PointDataContainer pdc, String parameter)
            throws VizException {
        int size = pdc.getCurrentSz();
        if (size < 1) {
            return null;
        }
        float[] xind = new float[size];
        float[] yind = new float[size];
        float[] values = new float[size];
        double[] input = new double[2];
        double[] output = new double[2];
        boolean hasData = false;
        for (int i = 0; i < size; i++) {
            PointDataView pdv = pdc.readRandom(i);
            input[0] = pdv.getFloat("longitude");
            input[1] = pdv.getFloat("latitude");
            try {
                latLonToGrid.transform(input, 0, output, 0, 1);
            } catch (TransformException e) {
                throw new VizException(e);
            }
            xind[i] = (float) output[0];
            yind[i] = (float) output[1];

            float value = pdv.getFloat(parameter);
            if (value <= -9999 || xind[i] < 0 || yind[i] < 0 || xind[i] >= nx
                    || yind[i] >= ny) {
                values[i] = 1e37f; // fill value used by OA
            } else {
                values[i] = value;
                hasData = true;
            }
        }
        if (!hasData) {
            return null;
        }
        float[] grid = new float[nx * ny];
        Controller.scaleless_analysis(xind, yind, values, size, nx, ny, grid);

        grid = Controller.dist_filter(grid, smoothPts, nx, 0, 0, nx, ny);

        for (int i = 0; i < grid.length; i++) {
            if (grid[i] > 1e36f) {
                grid[i] = Util.GRID_FILL_VALUE;
            }
        }
        return grid;
    }

    /**
     * @param constraints
     */
    public void addLatLonConstraints(Map<String, RequestConstraint> constraints) {
        double lat0 = latLonEnv.getMinY();
        double lat1 = latLonEnv.getMaxY();
        double lon0 = latLonEnv.getMinX();
        double lon1 = latLonEnv.getMaxX();

        RequestConstraint longitudeRequestConstraint = new RequestConstraint();
        longitudeRequestConstraint.setBetweenValueList(new String[] {
                lon0 + "", lon1 + "" });
        longitudeRequestConstraint.setConstraintType(ConstraintType.BETWEEN);

        RequestConstraint latitudeRequestConstraint = new RequestConstraint();
        latitudeRequestConstraint.setBetweenValueList(new String[] { lat0 + "",
                lat1 + "" });
        latitudeRequestConstraint.setConstraintType(ConstraintType.BETWEEN);

        constraints.put("location.latitude", latitudeRequestConstraint);
        constraints.put("location.longitude", longitudeRequestConstraint);
    }

    /**
     * @return the parmDescription
     */
    public ParameterDescription getParmDescription() {
        return parmDescription;
    }

    /**
     * @return the gridGeom
     */
    public GridGeometry2D getGridGeom() {
        return gridGeom;
    }

    /**
     * @return the nx
     */
    public int getNx() {
        return nx;
    }

    /**
     * @return the ny
     */
    public int getNy() {
        return ny;
    }

}
