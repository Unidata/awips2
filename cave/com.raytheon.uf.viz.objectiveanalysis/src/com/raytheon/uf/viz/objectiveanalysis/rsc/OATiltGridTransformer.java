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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;
import com.raytheon.viz.grid.util.TiltUtils;

/**
 * OA Tilt Derived Parameters Requests full columns of point data and performs
 * OA gridding for several MB levels and calculates the Tilt by slicing these
 * grids using derived parameters. This results in nicer data than the Regular
 * Grid Transformer which attempts to slice columns at Tilt levels and makes a
 * single grid. The problem is that for steep tilts there is not enough columns
 * of data nearby to make any relevant grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2010            bsteffen    Initial creation
 * Jun 04, 2013 2041       bsteffen    Switch derived parameters to use
 *                                     concurrent python for threading.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class OATiltGridTransformer extends OAGridTransformer {

    public OATiltGridTransformer(GeneralGridGeometry geom2D,
            CoordinateReferenceSystem crs, int gridSize, float smoothPts)
            throws VizException {
        super(geom2D, crs, gridSize, smoothPts);
    }

    /**
     * Given a pdc and pressure level determine the value of the parameter by
     * performing one d column interpolation and then OA gridding the result
     * 
     * @param parameter
     * @param pdc
     *            a PontDataContainer which must have lat, lon, multidimensional
     *            P and the parameter
     * @param presLvl
     * @return
     * @throws VizException
     */
    private float[] computeGrid(String parameter, PointDataContainer pdc,
            Number presLvl) throws VizException {
        int size = pdc.getCurrentSz();

        parmDescription = pdc.getDescription(parameter);

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
            Number[] paramValues = pdv.getNumberAllLevels(parameter);
            Number[] presValues = pdv.getNumberAllLevels("P");
            float value = getValue(paramValues, presValues, presLvl);

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
     * Column interpolation to get the value of a parameter at a given pressure
     * level.
     * 
     * @param paramCol
     * @param presCol
     * @param presLvl
     * @return
     */
    private float getValue(Number[] paramCol, Number[] presCol, Number presLvl) {
        int above = -1;
        int below = -1;
        for (int i = 0; i < presCol.length; i++) {
            if (presCol[i].floatValue() == presLvl.floatValue()) {
                return paramCol[i].floatValue();
            }
            if (presCol[i].floatValue() < presLvl.floatValue()
                    && (below == -1 || presCol[i].floatValue() > presCol[below]
                            .floatValue())) {
                below = i;
            }
            if (presCol[i].floatValue() > presLvl.floatValue()
                    && (above == -1 || presCol[i].floatValue() < presCol[above]
                            .floatValue())) {
                above = i;
            }
        }
        if (above == -1 || below == -1) {
            return -9999f;
        }
        return paramCol[below].floatValue()
                + (presLvl.floatValue() - presCol[below].floatValue())
                * (paramCol[above].floatValue() - paramCol[below].floatValue())
                / (presCol[above].floatValue() - presCol[below].floatValue());
    }

    public float[] computeGrid(String parameter,
            Map<String, RequestConstraint> constraints, String levelKey)
            throws VizException {
        addLatLonConstraints(constraints);
        String pluginName = constraints.get("pluginName").getConstraintValue();

        double tilt = Double.parseDouble(levelKey.replace("deg", ""));
        List<CubeLevel<Object, Object>> cube = new ArrayList<CubeLevel<Object, Object>>(
                21);
        String[] parameters = { "latitude", "longitude", "P", parameter };

        PointDataContainer pdc = DataCubeContainer.getPointData(pluginName,
                parameters, constraints);

        if (pdc == null || pdc.getCurrentSz() < 1) {
            return null;
        }
        for (int i = 1050; i >= 150; i -= 100) {
            float[] grid = computeGrid(parameter, pdc, i);
            if (grid == null) {
                continue;
            }
            FloatDataRecord fdr = new FloatDataRecord(parmDescription
                    .getParameterName(), "", grid, 2, new long[] { nx, ny });
            cube.add(new CubeLevel<Object, Object>((float) i, fdr));
        }
        if (cube.size() < 3) {
            return null;
        }
        FloatDataRecord heightRecord;
        try {
            heightRecord = TiltUtils.getInstance().getHeightGrid(
                    gridGeom.getGridRange2D(), latLonToGrid.inverse(), tilt);
        } catch (Exception e) {
            throw new VizException("Error building TiltHeight", e);
        }
        DerivedParameterRequest presRequest = new DerivedParameterRequest();
        presRequest.setMethod("Hgt2Pres");
        presRequest.setArgumentRecords(new Object[] { heightRecord });
        DerivedParameterRequest sliceRequest = new DerivedParameterRequest();
        sliceRequest.setMethod("Slice");
        sliceRequest.setArgumentRecords(new Object[] { cube, presRequest, -1 });
        try {
            return ((FloatDataRecord) DerivedParameterGenerator.calculate(
                    sliceRequest).get(0)).getFloatData();
        } catch (ExecutionException e) {
            throw new VizException(e);
        }

    }

}
