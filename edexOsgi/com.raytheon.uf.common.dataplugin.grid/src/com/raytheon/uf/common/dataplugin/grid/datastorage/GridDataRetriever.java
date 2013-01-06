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
package com.raytheon.uf.common.dataplugin.grid.datastorage;

import java.io.File;
import java.io.FileNotFoundException;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Convenience class for requesting grid data. This class provides automatic
 * unit conversion and also the ability to allow worldwide data to overlap.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridDataRetriever {

    protected static String serverDataDir;

    protected GridRecord record;

    protected GridCoverage requestCoverage;

    protected Request request = Request.ALL;

    protected int worldWrapColumns = -1;

    protected Unit<?> unit;

    protected UnitConverter converter;

    /**
     * Construct a GridDataRetriever that will retrieve data for the provided
     * GridRecord.
     * 
     * @param record
     *            GridRecord for which data is retrieved.
     */
    public GridDataRetriever(GridRecord record) {
        this.record = record;
    }

    /**
     * Construct a GridDataRetriever that will retrieve data for the provided
     * dataURI. The dataURI must be a valid dataURI for a grid record.
     * 
     * @param dataUri
     *            dataURI for grid data to retrieve.
     */
    public GridDataRetriever(String dataUri) {
        this.record = new GridRecord(dataUri);
        this.record.setParameter(ParameterLookup.getInstance().getParameter(
                record.getParameter().getAbbreviation()));
    }

    /**
     * For data that wraps around the world, specify the number of redundant
     * columns to add. For data that does not wrap around the world this method
     * call has no affect and the full data set will be retrieved.
     * 
     * @param worldWrapColumns
     *            The number of redundant columns to include in the x direction
     *            for wrapping grids.
     * @return a boolean true if the data is world wrapping, false if not(in
     *         which case this method call has no affect).
     * @throws GridCoverageException
     *             thrown when there are errors constructing a wrapping
     *             GridCoverage
     */
    public boolean setWorldWrapColumns(int worldWrapColumns)
            throws GridCoverageException {
        GridCoverage dataLoc = record.getLocation();
        GridGeometry2D dataGeom = dataLoc.getGridGeometry();
        int wrapX = GridGeometryWrapChecker.checkForWrapping(dataGeom);
        if (wrapX != -1) {
            int newX = wrapX + worldWrapColumns;
            if (newX == dataLoc.getNx()) {
                this.request = Request.ALL;
            } else if (newX < dataLoc.getNx()) {
                Coordinate upperRight = new Coordinate(newX - 1, 0);
                upperRight = MapUtil.gridCoordinateToLatLon(upperRight,
                        PixelOrientation.CENTER, dataLoc);
                setRequestArea(dataLoc.getLowerLeftLon(),
                        dataLoc.getLowerLeftLat(), upperRight.x, upperRight.y);
            } else {
                this.request = Request.ALL;
                if (dataLoc instanceof LatLonGridCoverage) {
                    LatLonGridCoverage newLoc = new LatLonGridCoverage(
                            (LatLonGridCoverage) dataLoc);
                    newLoc.setLa2(0);
                    newLoc.setLo2(0);
                    requestCoverage = newLoc;
                } else if (dataLoc instanceof MercatorGridCoverage) {
                    MercatorGridCoverage newLoc = new MercatorGridCoverage(
                            (MercatorGridCoverage) dataLoc);
                    newLoc.setLa2(null);
                    newLoc.setLo2(null);
                    requestCoverage = newLoc;
                } else if (dataLoc instanceof LambertConformalGridCoverage) {
                    requestCoverage = new LambertConformalGridCoverage(
                            (LambertConformalGridCoverage) dataLoc);
                } else if (dataLoc instanceof PolarStereoGridCoverage) {
                    // I really doubt it is possible to world wrap a
                    // PolarStereoCoverage, but just in case...
                    requestCoverage = new PolarStereoGridCoverage(
                            (PolarStereoGridCoverage) dataLoc);
                } else {
                    throw new GridCoverageException(
                            "Cannot wrap data for projection of type "
                                    + dataLoc.getClass().getName());
                }
                requestCoverage.setNx(newX);
                requestCoverage.setGridGeometry(null);
                requestCoverage.initialize();
                this.worldWrapColumns = newX - dataLoc.getNx();
            }
            return true;
        } else {
            return false;
        }
    }

    /**
     * Set the requested area to be a subgrid of the total area. This uses the
     * trim functionality of GridCoverage to generate an area which has corners
     * at the provided latitude and longitude.
     * 
     * @param lon1
     * @param lat1
     * @param lon2
     * @param lat2
     * @throws GridDataRetrievalException
     * @throws GridCoverageException
     */
    protected void setRequestArea(double lon1, double lat1, double lon2,
            double lat2) throws GridCoverageException {
        SubGrid subGrid = new SubGrid();
        subGrid.setLowerLeftLat(Math.min(lat1, lat2));
        subGrid.setLowerLeftLon(Math.min(lat1, lat2));
        subGrid.setUpperRightLat(Math.min(lat1, lat2));
        subGrid.setUpperRightLon(Math.min(lat1, lat2));
        requestCoverage = record.getLocation().trim(subGrid);
        int[] minIndex = { subGrid.getUpperLeftX(), subGrid.getUpperLeftY() };
        int[] maxIndex = { subGrid.getUpperLeftX() + subGrid.getNX(),
                subGrid.getUpperLeftY() + subGrid.getNY() };
        request = Request.buildSlab(minIndex, maxIndex);
        requestCoverage.initialize();
    }

    /**
     * Set the desired unit for the data being retrieved. If this method is
     * successful the data will automatically be converted when it is retrieved.
     * 
     * @param unit
     *            the desired unit for the data.
     * @throws ConversionException
     *             occurs when the provided unit is incompatible with the
     *             storage unit of the data.
     */
    public void setUnit(Unit<?> unit) throws ConversionException {
        this.converter = record.getParameter().getUnit().getConverterTo(unit);
        this.unit = unit;
    }

    /**
     * retrieve the grid data. This method will automatically perform any
     * requested unit conversion or world wrap adjustments.
     * 
     * @return FloatDataRecord containing the raw values for this grid.
     * @throws StorageException
     *             if anything goes wrong while retrieving the data, for example
     *             if the data cannot be found in the datastore or if there are
     *             problems communicating with the servers.
     */
    public FloatDataRecord getDataRecord() throws StorageException {
        IDataStore ds = DataStoreFactory.getDataStore(findStorageLocation());
        IDataRecord dataRecord;
        try {
            dataRecord = ds.retrieve(getGroup(), "Data", request);
        } catch (FileNotFoundException e) {
            throw new StorageException(e.getLocalizedMessage(), null, e);
        }
        FloatDataRecord floatRecord = (FloatDataRecord) dataRecord;
        boolean cloned = false;
        if (converter != null) {
            if (!cloned) {
                floatRecord = (FloatDataRecord) floatRecord.clone();
                cloned = true;
            }
            float[] data = floatRecord.getFloatData();
            for (int i = 0; i < data.length; i += 1) {
                if (data[i] <= -9999) {
                    // -9999 and -999999 are both commonly used no data values,
                    // and rarely are such extremes valid. No Data Value.
                    data[i] = data[i];
                } else {
                    data[i] = (float) converter.convert(data[i]);
                }
            }
        }
        if (worldWrapColumns > 0) {
            if (!cloned) {
                floatRecord = (FloatDataRecord) floatRecord.clone();
                cloned = true;
            }
            int nx = (int) floatRecord.getSizes()[0];
            int newNx = nx + worldWrapColumns;
            int ny = (int) floatRecord.getSizes()[1];
            float[] oldData = floatRecord.getFloatData();
            float[] newData = new float[newNx * ny];
            for (int y = 0; y < ny; y += 1) {
                // this is in a loop so it works correctly when extraColumns >
                // nx
                for (int numCopied = 0; numCopied < newNx; numCopied += nx) {
                    System.arraycopy(oldData, y * nx, newData, y * newNx
                            + numCopied, Math.min(nx, newNx - numCopied));
                }
            }
            floatRecord.setSizes(new long[] { newNx, ny });
            floatRecord.setFloatData(newData);

        }
        return floatRecord;
    }

    /**
     * Get a GridCoverage describing the spatial area covered by the retrieved
     * data. If setWorldWrapColumns or setRequestArea was used then this will
     * return a coverage describing the modified area, otherwise this is the
     * same as the coverage in the GridRecord.
     * 
     * @return grid coverage of the data record.
     */
    public GridCoverage getCoverage() {
        if (requestCoverage == null) {
            return record.getLocation();
        } else {
            return requestCoverage;
        }
    }

    /**
     * Get a unit object describing the retrieved data. If setUnit was
     * successfully used then this will return the same unit, otherwise it
     * returns the storage unit of the data.
     * 
     * @return unit of the data record
     */
    public Unit<?> getUnit() {
        if (unit == null) {
            return record.getParameter().getUnit();
        }
        return unit;
    }

    private String getGroup() {
        if (GridPathProvider.STATIC_PARAMETERS.contains(record.getParameter()
                .getAbbreviation())) {
            return DataURI.SEPARATOR + record.getLocation().getId()
                    + DataURI.SEPARATOR
                    + record.getParameter().getAbbreviation();
        } else {
            return record.getDataURI();
        }
    }

    private File findStorageLocation() throws StorageException {
        IHDFFilePathProvider pathProvider = record.getHDFPathProvider();

        String path = pathProvider.getHDFPath(record.getPluginName(), record);
        String fileName = pathProvider.getHDFFileName(record.getPluginName(),
                record);

        return new File(getServerDataDir() + IPathManager.SEPARATOR
                + record.getPluginName() + IPathManager.SEPARATOR + path
                + IPathManager.SEPARATOR + fileName);
    }

    private static synchronized String getServerDataDir()
            throws StorageException {
        if (serverDataDir == null) {
            // TODO cave already knows the server data dir in VizApp, and edex
            // has it in system properties but we can't access either because
            // this is common code, architecturally we need some way around
            // this. For now this will send it's own request which is slightly
            // wasteful but not terribly harmful.
            try {
                GetServersResponse response = (GetServersResponse) RequestRouter
                        .route(new GetServersRequest());
                serverDataDir = response.getServerDataDir();
            } catch (Exception e) {
                throw new StorageException("Error communicating with server.",
                        null, e);
            }
        }
        return serverDataDir;
    }
}
