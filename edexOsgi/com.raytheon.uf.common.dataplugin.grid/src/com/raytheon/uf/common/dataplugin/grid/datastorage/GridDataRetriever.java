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

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;

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
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;

/**
 * Convenience class for requesting grid data. This class provides automatic
 * unit conversion and also the ability to allow worldwide data to overlap.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 14, 2012           bsteffen  Initial creation
 * Jan 14, 2013  1469     bkowal    No longer needs to retrieve the location of
 *                                  the hdf5 data directory.
 * Dec 16, 2013  2574     bsteffen  Fixed bugs in setRequestArea.
 * Mar 04, 2015  3959     rjpeter   Update for grid based subgridding.
 * Mar 03, 2016  5439     bsteffen  Make retrieve public static, fix static
 *                                  parameters.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridDataRetriever {

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
        int wrapX = GridGeometryWrapChecker.checkForWrapping(dataLoc
                .getGridGeometry());

        if (wrapX != GridGeometryWrapChecker.NO_WRAP) {
            int newX = wrapX + worldWrapColumns;

            if (newX == dataLoc.getNx()) {
                this.request = Request.ALL;
            } else {
                /*
                 * Let subgridding handle getting the full grid in case grid
                 * stored stored with world wrap columns.
                 */
                setRequestArea(wrapX, dataLoc.getNy());
                this.worldWrapColumns = worldWrapColumns;

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
            }

            return true;
        } else {
            return false;
        }
    }

    /**
     * Set the requested area to be a subgrid of the total area. This uses the
     * trim functionality of GridCoverage to generate an area based on the
     * passed nx/ny.
     * 
     * @param nx
     * @param ny
     */
    protected void setRequestArea(int nx, int ny) {
        SubGrid subGrid = new SubGrid();
        // leave UL point as 0,0
        subGrid.setNX(nx);
        subGrid.setNY(ny);
        record.getLocation().trim(subGrid);
        int[] minIndex = { subGrid.getUpperLeftX(), subGrid.getUpperLeftY() };
        int[] maxIndex = { subGrid.getUpperLeftX() + subGrid.getNX(),
                subGrid.getUpperLeftY() + subGrid.getNY() };
        request = Request.buildSlab(minIndex, maxIndex);
    }

    /**
     * Set the desired unit for the data being retrieved. If this method is
     * successful the data will automatically be converted when it is retrieved.
     * 
     * @param unit
     *            the desired unit for the data.
     * @throws IncommensurableException
     *             occurs when the provided unit is incompatible with the
     *             storage unit of the data.
     * @throws UnconvertibleException
     *             occurs when the provided unit is incompatible with the
     *             storage unit of the data.
     */
    public void setUnit(Unit<?> unit) throws IncommensurableException, UnconvertibleException {
        this.converter = record.getParameter().getUnit().getConverterToAny(unit);
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

        IDataRecord dataRecord = retrieve(record, request);
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

    public static IDataRecord retrieve(GridRecord record, Request request)
            throws StorageException {
        String group = record.getDataURI();
        String dataset = DataStoreFactory.DEF_DATASET_NAME;
        if (GridPathProvider.STATIC_PARAMETERS.contains(record.getParameter()
                .getAbbreviation())) {
            group = DataURI.SEPARATOR + record.getLocation().getId();
            dataset = record.getParameter().getAbbreviation();
        }

        IDataStore ds = DataStoreFactory
                .getDataStore(findStorageLocation(record));
        try {
            return ds.retrieve(group, dataset, request);
        } catch (FileNotFoundException e) {
            throw new StorageException(e.getLocalizedMessage(), null, e);
        }

    }

    private static File findStorageLocation(GridRecord record) {
        IHDFFilePathProvider pathProvider = record.getHDFPathProvider();

        String path = pathProvider.getHDFPath(record.getPluginName(), record);
        String fileName = pathProvider.getHDFFileName(record.getPluginName(),
                record);

        return new File(record.getPluginName() + IPathManager.SEPARATOR + path
                + IPathManager.SEPARATOR + fileName);
    }
}
