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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.EnvelopeProjectionException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.vividsolutions.jts.geom.Envelope;

/**
 * An abstract factory for getting grid data from plugins that use
 * PluginDataObject.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Jan 17, 2013           bsteffen    Initial creation
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Jan 14, 2014  2667     mnash       Remove getGeometryData methods
 * Feb 04, 2014  2672     bsteffen    Enable subgridding when envelopes are
 *                                    requested
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGridDataPluginFactory extends
        AbstractDataPluginFactory {

    /**
     * Executes the provided DbQueryRequest and returns an array of IGridData
     * 
     * @param request
     *            the original grid request
     * @param dbQueryRequest
     *            the db query request to execute
     * @return an array of IGridData
     */
    protected IGridData[] getGridData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {

        List<IGridData> gridData = new ArrayList<IGridData>();
        for (Map<String, Object> resultMap : dbQueryResponse.getResults()) {
            if (resultMap.containsKey(null) == false) {
                throw new DataRetrievalException(
                        "The results of the DbQueryRequest do not consist of PluginDataObject objects as expected.");
            }
            if ((resultMap.get(null) instanceof PluginDataObject) == false) {
                throw new DataRetrievalException(
                        "The objects returned by the DbQueryRequest are not of type PluginDataObject as expected.");
            }

            PluginDataObject pdo = (PluginDataObject) resultMap.get(null);

            /*
             * Extract the grid geometry.
             */
            GridGeometry2D gridGeometry = getGridGeometry(pdo);

            DataSource dataSource = null;

            Envelope envelope = request.getEnvelope();
            if (envelope != null) {
                ReferencedEnvelope requestEnv = new ReferencedEnvelope(
                        envelope, DefaultGeographicCRS.WGS84);
                SubGridGeometryCalculator subGrid = calculateSubGrid(
                        requestEnv, gridGeometry);
                if (subGrid == null || !subGrid.isEmpty()) {
                    dataSource = getDataSource(pdo, subGrid);
                    if (subGrid != null) {
                        gridGeometry = subGrid.getZeroedSubGridGeometry();
                    }
                }
            } else {
                dataSource = getDataSource(pdo, null);
            }

            if (dataSource != null) {
                gridData.add(this.constructGridDataResponse(request, pdo,
                        gridGeometry, dataSource));
            }
        }

        return gridData.toArray(new IGridData[gridData.size()]);
    }

    /**
     * Generate a SubGridGeometryCalculator appropriate for determining what
     * area of data to request for this dataType. A return type of null can be
     * used to indicate the entire gridGeometry should be used.
     * 
     * @param envelope
     *            The requested envelope in WGS84
     * @param gridGeometry
     *            The gridGeometry.
     * @return a SubGridGeometryCalculator.
     * @throws EnvelopeProjectionException
     */
    protected SubGridGeometryCalculator calculateSubGrid(
            ReferencedEnvelope envelope, GridGeometry2D gridGeometry)
            throws EnvelopeProjectionException {
        try {
            return new SubGridGeometryCalculator(envelope, gridGeometry);
        } catch (TransformException e) {
            throw new EnvelopeProjectionException(
                    "Error determining subgrid from envelope: " + envelope, e);
        }
    }

    /**
     * Request the raw data for a pdo.
     * 
     * @param pdo
     *            the pdo with metadata popualted
     * @param subGrid
     *            object describing area requested.
     * @return a DataSource holding the raw data.
     */
    protected DataSource getDataSource(PluginDataObject pdo,
            SubGridGeometryCalculator subGrid) {
        try {
            IDataRecord dataRecord = null;
            if (subGrid == null || subGrid.isFull()) {
                dataRecord = PDOUtil.getDataRecord(pdo, "Data", Request.ALL);
            } else if (!subGrid.isEmpty()) {
                Request dataStoreReq = Request.buildSlab(
                        subGrid.getGridRangeLow(true),
                        subGrid.getGridRangeHigh(false));
                dataRecord = PDOUtil.getDataRecord(pdo, "Data", dataStoreReq);
            } else {
                return null;
            }
            return DataWrapperUtil.constructArrayWrapper(dataRecord, false);
        } catch (Exception e) {
            throw new DataRetrievalException(
                    "Failed to retrieve the IDataRecord for PluginDataObject: "
                            + pdo.toString(), e);
        }
    }

    protected GridGeometry2D getGridGeometry(PluginDataObject pdo) {
        return PDOUtil.retrieveGeometry(pdo);
    }

    /**
     * Builds an IGridData with the information that is supplied
     * 
     * @param request
     *            the original grid request
     * @param pdo
     *            a record that was retrieved from the database
     * @param gridGeometry
     *            the geometry extracted from the pdo
     * @param dataRecord
     *            the raw data
     * @return the IGridData that was constructed
     */
    protected abstract IGridData constructGridDataResponse(
            IDataRequest request, PluginDataObject pdo,
            GridGeometry2D gridGeometry, DataSource dataSource);

}
