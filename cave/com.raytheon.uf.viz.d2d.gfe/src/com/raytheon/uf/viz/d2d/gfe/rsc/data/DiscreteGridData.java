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
package com.raytheon.uf.viz.d2d.gfe.rsc.data;

import java.nio.ByteBuffer;

import javax.measure.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojectionDataSource;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.PrecomputedGridReprojection;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;

/**
 *
 * A class which hold GFE discrete data for a grid.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 16, 2019  71870    tjensen   Initial creation
 *
 * </pre>
 *
 * @author tjensen
 */
public class DiscreteGridData extends GeneralGridData {
    private final GeographicDataSource discreteData;

    private DiscreteKey[] keys;

    /**
     * Create a discrete grid data object from byte data.
     *
     * @param discreteData
     * @param dataUnit
     * @return
     */
    public static DiscreteGridData createDiscreteData(
            GeneralGridGeometry gridGeometry, ByteBuffer discreteData,
            DiscreteKey[] keys, Unit<?> dataUnit) {
        DataSource discreteSource = new GeographicDataSource(discreteData,
                gridGeometry);
        return createDiscreteData(gridGeometry, discreteSource, keys, dataUnit);
    }

    /**
     * Create a discrete grid data object from any data source
     *
     * @param discreteData
     * @param dataUnit
     * @return
     */
    public static DiscreteGridData createDiscreteData(
            GeneralGridGeometry gridGeometry, DataSource discreteData,
            DiscreteKey[] keys, Unit<?> dataUnit) {
        return new DiscreteGridData(gridGeometry, discreteData, keys, dataUnit);
    }

    private DiscreteGridData(GeneralGridGeometry gridGeometry,
            DataSource discreteData, DiscreteKey[] keys, Unit<?> dataUnit) {
        super(gridGeometry, dataUnit);
        this.discreteData = GeographicDataSource.wrap(discreteData,
                this.gridGeometry);
        this.keys = keys;
    }

    @Override
    public boolean convert(Unit<?> unit) {
        // Do not attempt to convert discrete products
        return false;
    }

    @Override
    public DiscreteGridData reproject(GeneralGridGeometry newGridGeometry,
            Interpolation interpolation)
            throws FactoryException, TransformException {
        // interpolation should always be done using NearestNeighbor
        if (!(interpolation instanceof NearestNeighborInterpolation)) {
            interpolation = new NearestNeighborInterpolation();
        }
        GridGeometry2D newGeom = GridGeometry2D.wrap(newGridGeometry);
        GridReprojection reproj = PrecomputedGridReprojection
                .getReprojection(gridGeometry, newGeom);
        GridSampler sampler = new GridSampler(interpolation);

        sampler.setSource(getDiscreteData());
        return createDiscreteData(newGridGeometry,
                new GridReprojectionDataSource(reproj, sampler), keys,
                dataUnit);
    }

    public GeographicDataSource getDiscreteData() {
        return discreteData;
    }

    public DiscreteKey[] getKeys() {
        return keys;
    }

    public void setKeys(DiscreteKey[] keys) {
        this.keys = keys;
    }

    @Override
    public GeographicDataSource getData() {
        return getDiscreteData();
    }

    @Override
    public GeneralGridData mergeData(GeneralGridData other,
            GridEnvelope2D range1, GridEnvelope2D range2,
            GridGeometry2D geometry) {
        throw new UnsupportedOperationException(
                "Cannot merge discrete grid data products.");
    }

    public Grid2DBit eq(DiscreteKey value) {
        if (!value.isValid()) {
            throw new IllegalArgumentException(
                    "Supplied key is invalid: " + value);
        }

        int x = gridGeometry.getGridRange2D().width;
        int y = gridGeometry.getGridRange2D().height;

        Grid2DBit bits = new Grid2DBit(x, y);

        // Get or make a Discrete
        byte dByte = 0;
        boolean found = false;
        for (int k = 0; k < keys.length; k++) {
            if (keys[k].equals(value)) {
                dByte = (byte) k;
                found = true;
            }
        }

        if (!found) {
            return bits;
        }

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                if (discreteData.getDataValue(i, j) == dByte) {
                    bits.set(i, j);
                }
            }
        }

        return bits;
    }
}
