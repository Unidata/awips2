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

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
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
 * A class which hold GFE weather data for a grid.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 16, 2019  71870    tjensen   Initial creation
 * Dec 13, 2019  72475    tjensen   Add eq()
 *
 * </pre>
 *
 * @author tjensen
 */
public class WeatherGridData extends GeneralGridData {
    private final GeographicDataSource weatherData;

    private WeatherKey[] keys;

    /**
     * Create a weather grid data object from byte data.
     *
     * @param gridGeometry
     * @param weatherData
     * @param keys
     * @param dataUnit
     * @return
     */
    public static WeatherGridData createWeatherData(
            GeneralGridGeometry gridGeometry, ByteBuffer weatherData,
            WeatherKey[] keys, Unit<?> dataUnit) {
        DataSource weatherSource = new GeographicDataSource(weatherData,
                gridGeometry);

        return createWeatherData(gridGeometry, weatherSource, keys, dataUnit);
    }

    /**
     * Create a weather grid data object from any data source
     *
     * @param gridGeometry
     * @param weatherData
     * @param keys
     * @param dataUnit
     * @return
     */
    public static WeatherGridData createWeatherData(
            GeneralGridGeometry gridGeometry, DataSource weatherData,
            WeatherKey[] keys, Unit<?> dataUnit) {
        return new WeatherGridData(gridGeometry, weatherData, keys, dataUnit);
    }

    private WeatherGridData(GeneralGridGeometry gridGeometry,
            DataSource weatherData, WeatherKey[] keys, Unit<?> dataUnit) {
        super(gridGeometry, dataUnit);
        this.weatherData = GeographicDataSource.wrap(weatherData,
                this.gridGeometry);
        this.keys = keys;
    }

    @Override
    public boolean convert(Unit<?> unit) {
        // Do not attempt to convert weather products
        return false;
    }

    @Override
    public WeatherGridData reproject(GeneralGridGeometry newGridGeometry,
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
        return createWeatherData(newGridGeometry,
                new GridReprojectionDataSource(reproj, sampler), keys,
                dataUnit);
    }

    public GeographicDataSource getDiscreteData() {
        return weatherData;
    }

    public WeatherKey[] getKeys() {
        return keys;
    }

    public void setKeys(WeatherKey[] keys) {
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
                "Cannot merge weather grid data products.");
    }

    /**
     * Returns a Grid2DBit that corresponds to the grid cells that are equal to
     * the specified WeatherKey value.
     *
     * @param value
     *            the WeatherKey to test for
     * @return a Grid2DBit with bits set that match the input WeatherKey
     */
    public Grid2DBit eq(WeatherKey value) {
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
                if (weatherData.getDataValue(i, j) == dByte) {
                    bits.set(i, j);
                }
            }
        }

        return bits;
    }
}
